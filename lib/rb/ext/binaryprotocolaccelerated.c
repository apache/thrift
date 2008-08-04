// Half of this file comes from contributions from Nitay Joffe (nitay@powerset.com)
// Much of the rest (almost) directly ported (or pulled) from thrift-py's fastbinary.c
// Everything else via Kevin Clark (kevin@powerset.com)
#include <stdint.h>
#include <stdbool.h>

#include <ruby.h>
#include <st.h>
#include <netinet/in.h>

// #define __DEBUG__

#ifndef HAVE_STRLCPY

static
size_t
strlcpy (char *dst, const char *src, size_t dst_sz)
{
    size_t n;

    for (n = 0; n < dst_sz; n++) {
      if ((*dst++ = *src++) == '\0')
        break;
    }

    if (n < dst_sz)
      return n;
    if (n > 0)
      *(dst - 1) = '\0';
    return n + strlen (src);
}

#endif

#define dbg() fprintf(stderr, "%s:%d\n", __FUNCTION__, __LINE__)


// TODO (kevinclark): This was here from the patch/python. Not sure
// If it's actually that big a pain. Need to look into pulling
// From the right place

// Stolen out of TProtocol.h.
// It would be a huge pain to have both get this from one place.

enum TType {
  T_STOP       = 0,
  T_BOOL       = 2,
  T_BYTE       = 3,
  T_I16        = 6,
  T_I32        = 8,
  T_I64        = 10,
  T_DBL        = 4,
  T_STR        = 11,
  T_STRCT      = 12,
  T_MAP        = 13,
  T_SET        = 14,
  T_LIST       = 15
  // T_VOID       = 1,
  // T_I08        = 3,
  // T_U64        = 9,
  // T_UTF7       = 11,
  // T_UTF8       = 16,
  // T_UTF16      = 17
};

#define IS_CONTAINER(x) (x == T_MAP || x == T_SET || x == T_LIST)

// Same comment as the enum.  Sorry.
#ifdef HAVE_ENDIAN_H
#include <endian.h>
#endif

#ifndef __BYTE_ORDER
# if defined(BYTE_ORDER) && defined(LITTLE_ENDIAN) && defined(BIG_ENDIAN)
#  define __BYTE_ORDER BYTE_ORDER
#  define __LITTLE_ENDIAN LITTLE_ENDIAN
#  define __BIG_ENDIAN BIG_ENDIAN
# else
#  error "Cannot determine endianness"
# endif
#endif

#if __BYTE_ORDER == __BIG_ENDIAN
# define ntohll(n) (n)
# define htonll(n) (n)
#elif __BYTE_ORDER == __LITTLE_ENDIAN
# if defined(__GNUC__) && defined(__GLIBC__)
#  include <byteswap.h>
#  define ntohll(n) bswap_64(n)
#  define htonll(n) bswap_64(n)
# else /* GNUC & GLIBC */
#  define ntohll(n) ( (((unsigned long long)ntohl(n)) << 32) + ntohl(n >> 32) )
#  define htonll(n) ( (((unsigned long long)htonl(n)) << 32) + htonl(n >> 32) )
# endif /* GNUC & GLIBC */
#else /* __BYTE_ORDER */
# error "Can't define htonll or ntohll!"
#endif


// -----------------------------------------------------------------------------
// Cached interned strings and such
// -----------------------------------------------------------------------------

static VALUE class_tbpa;
static VALUE m_thrift;
static VALUE rb_cSet;
static ID type_sym;
static ID class_sym;
static ID key_sym;
static ID value_sym;
static ID element_sym;
static ID name_sym;
static ID fields_id;
static ID consume_bang_id;
static ID string_buffer_id;
static ID borrow_id;
static ID keys_id;
static ID entries_id;

static const uint32_t VERSION_MASK = 0xffff0000;
static const uint32_t VERSION_1 = 0x80010000;

// -----------------------------------------------------------------------------
// Structs so I don't have to keep calling rb_hash_aref
// -----------------------------------------------------------------------------

// { :type => field[:type],
//   :class => field[:class],
//   :key => field[:key],
//   :value => field[:value],
//   :element => field[:element] }

struct _thrift_map;
struct _field_spec;

typedef union {
  VALUE class;
  struct _thrift_map* map;
  struct _field_spec* element;
} container_data;

typedef struct _field_spec {
  int type;
  char* name;
  container_data data;
} field_spec;

typedef struct _thrift_map {
  field_spec* key;
  field_spec* value; 
} thrift_map;


static void free_field_spec(field_spec* spec) {
  switch(spec->type) {
    case T_LIST:
    case T_SET:
      free_field_spec(spec->data.element);
      break;
    
    case T_MAP:
      free_field_spec(spec->data.map->key);
      free_field_spec(spec->data.map->value);
      free(spec->data.map);
      break;
  }
  
  free(spec);
}

// Parses a ruby field spec into a C struct
//
// Simple fields look like:
// { :name => .., :type => .. }
// Structs add the :class attribute
// Maps adds :key and :value attributes, field specs
// Lists and Sets add an :element, a field spec
static field_spec* parse_field_spec(VALUE field_data) {
  int type = NUM2INT(rb_hash_aref(field_data, type_sym));
  VALUE name = rb_hash_aref(field_data, name_sym);
  field_spec* spec = (field_spec *) malloc(sizeof(field_spec));

#ifdef __DEBUG__ // No need for this in prod since I set all the fields
  bzero(spec, sizeof(field_spec));
#endif

  spec->type = type;
  
  if (!NIL_P(name)) {
    spec->name = StringValuePtr(name);
  } else {
    spec->name = NULL;
  }
  
  switch(type) {
    case T_STRCT: {
      spec->data.class = rb_hash_aref(field_data, class_sym);
      break;
    }
    
    case T_MAP: {
      VALUE key_fields = rb_hash_aref(field_data, key_sym);
      VALUE value_fields = rb_hash_aref(field_data, value_sym);
      thrift_map* map = (thrift_map *) malloc(sizeof(thrift_map));
      
      map->key = parse_field_spec(key_fields);
      map->value = parse_field_spec(value_fields);
      spec->data.map = map;
      
      break;
    }
    
    case T_LIST: 
    case T_SET:
    {
      VALUE list_fields = rb_hash_aref(field_data, element_sym);
      spec->data.element = parse_field_spec(list_fields);
      break;
    }
  }
  
  return spec;
}


// -----------------------------------------------------------------------------
// Serialization routines
// -----------------------------------------------------------------------------


// write_*(VALUE buf, ...) takes a value and adds it to a Ruby string buffer,
// in network order
static void write_byte(VALUE buf, int8_t val) {
  rb_str_buf_cat(buf, (char*)&val, sizeof(int8_t));
}

static void write_i16(VALUE buf, int16_t val) {
  int16_t net = (int16_t)htons(val);
  rb_str_buf_cat(buf, (char*)&net, sizeof(int16_t));
}

static void write_i32(VALUE buf, int32_t val) {
  int32_t net = (int32_t)htonl(val);
  rb_str_buf_cat(buf, (char*)&net, sizeof(int32_t));
}

static void write_i64(VALUE buf, int64_t val) {
  int64_t net = (int64_t)htonll(val);
  rb_str_buf_cat(buf, (char*)&net, sizeof(int64_t));
}

static void write_double(VALUE buf, double dub) {
  // Unfortunately, bitwise_cast doesn't work in C.  Bad C!
  union {
    double f;
    int64_t t;
  } transfer;
  transfer.f = dub;
  write_i64(buf, transfer.t);
}

static void write_string(VALUE buf, char* str, size_t len) {
  write_i32(buf, len);
  rb_str_buf_cat(buf, str, len);
}

// Some functions macro'd out because they're nops for the binary protocol
// but placeholders were desired in case things change
#define write_struct_begin(buf)
#define write_struct_end(buf)

static void write_field_begin(VALUE buf, char* name, int type, int fid) {
#ifdef __DEBUG__
  fprintf(stderr, "Writing field beginning: %s %d %d\n", name, type, fid);
#endif

  write_byte(buf, (int8_t)type);
  write_i16(buf, (int16_t)fid);
}

#define write_field_end(buf)

static void write_field_stop(VALUE buf) {
  write_byte(buf, T_STOP);
}

static void write_map_begin(VALUE buf, int8_t ktype, int8_t vtype, int32_t sz) {
  write_byte(buf, ktype);
  write_byte(buf, vtype);
  write_i32(buf, sz);
}

#define write_map_end(buf);

static void write_list_begin(VALUE buf, int type, int sz) {
  write_byte(buf, type);
  write_i32(buf, sz);
}

#define write_list_end(buf)

static void write_set_begin(VALUE buf, int type, int sz) {
  write_byte(buf, type);
  write_i32(buf, sz);
}

#define write_set_end(buf)

static void binary_encoding(VALUE buf, VALUE obj, int type);

// Handles container types: Map, Set, List
static void write_container(VALUE buf, VALUE value, field_spec* spec) {
  int sz, i;
  
  switch(spec->type) {
    case T_MAP: {
      VALUE keys;
      VALUE key;
      VALUE val;

      Check_Type(value, T_HASH);
      
      keys = rb_funcall(value, keys_id, 0);
      
      sz = RARRAY(keys)->len;
      
      write_map_begin(buf, spec->data.map->key->type, spec->data.map->value->type, sz);
      
      for (i = 0; i < sz; i++) {
        key = rb_ary_entry(keys, i);
        val = rb_hash_aref(value, key);
        
        if (IS_CONTAINER(spec->data.map->key->type)) {
          write_container(buf, key, spec->data.map->key);
        } else {
          binary_encoding(buf, key, spec->data.map->key->type);
        }
        
        if (IS_CONTAINER(spec->data.map->value->type)) {
          write_container(buf, val, spec->data.map->value);
        } else {
          binary_encoding(buf, val, spec->data.map->value->type);
        }
      }
      
      write_map_end(buf);

      break;
    }
    
    case T_LIST: {
      Check_Type(value, T_ARRAY);

      sz = RARRAY(value)->len;
      
      write_list_begin(buf, spec->data.element->type, sz);
      for (i = 0; i < sz; ++i) {
        VALUE val = rb_ary_entry(value, i);
        if (IS_CONTAINER(spec->data.element->type)) {
          write_container(buf, val, spec->data.element);
        } else {
          binary_encoding(buf, val, spec->data.element->type);
        }
      }
      write_list_end(buf);
      break;
    }

    case T_SET: {
      VALUE items;

      if (TYPE(value) == T_ARRAY) {
        items = value;
      } else {        
        if (rb_cSet == CLASS_OF(value)) {
          items = rb_funcall(value, entries_id, 0);
        } else {
          Check_Type(value, T_HASH);
          items = rb_funcall(value, keys_id, 0);
        }
      }

      sz = RARRAY(items)->len;
      
      write_set_begin(buf, spec->data.element->type, sz);
      
      for (i = 0; i < sz; i++) {
        VALUE val = rb_ary_entry(items, i);
        if (IS_CONTAINER(spec->data.element->type)) {
          write_container(buf, val, spec->data.element);
        } else {
          binary_encoding(buf, val, spec->data.element->type);
        }
      }
      
      write_set_end(buf);
      break;
    }
  }
}

// Takes the field id, data to be encoded, buffer and enclosing object
// to be encoded. buf and obj passed as a ruby array for rb_hash_foreach.
// TODO(kevinclark): See if they can be passed individually to avoid object
// creation
static int encode_field(VALUE fid, VALUE data, VALUE ary) {
  field_spec *spec = parse_field_spec(data);
  
  VALUE buf = rb_ary_entry(ary, 0);
  VALUE obj = rb_ary_entry(ary, 1);
  char name_buf[128];
  
  name_buf[0] = '@';
  strlcpy(&name_buf[1], spec->name, sizeof(name_buf) - 1);
  
  VALUE value = rb_ivar_get(obj, rb_intern(name_buf));
  
  if (NIL_P(value)) {
    free_field_spec(spec);
    return 0;
  }
     
  write_field_begin(buf, spec->name, spec->type, NUM2INT(fid));
  
  if (IS_CONTAINER(spec->type)) {
    write_container(buf, value, spec);
  } else {
    binary_encoding(buf, value, spec->type);
  }
  write_field_end(buf);
  
  free_field_spec(spec);
  
  return 0;
}

// -----------------------------------------------------------------------------
// TFastBinaryProtocol's main encoding loop
// -----------------------------------------------------------------------------

static void binary_encoding(VALUE buf, VALUE obj, int type) {
#ifdef __DEBUG__
  rb_p(rb_str_new2("Encoding binary (buf, obj, type)"));
  rb_p(rb_inspect(buf));
  rb_p(rb_inspect(obj));
  rb_p(rb_inspect(INT2FIX(type)));
#endif

  switch(type) {
    case T_BOOL:
      if RTEST(obj) {
        write_byte(buf, 1);
      }
      else {
        write_byte(buf, 0);
      }
      
      break;
    
    case T_BYTE:
      write_byte(buf, NUM2INT(obj));
      break;
    
    case T_I16:
      write_i16(buf, NUM2INT(obj));
      break;
    
    case T_I32:
      write_i32(buf, NUM2INT(obj));
      break;
    
    case T_I64:
      write_i64(buf, rb_num2ll(obj));
      break;
    
    case T_DBL:
      write_double(buf, NUM2DBL(obj));
      break;

    case T_STR: {
      // make sure to call StringValuePtr before calling RSTRING
      char *ptr = StringValuePtr(obj);
      write_string(buf, ptr, RSTRING(obj)->len);
      break;
    }
          
    case T_STRCT: {
      // rb_hash_foreach has to take args as a ruby array
      VALUE args = rb_ary_new3(2, buf, obj);
      VALUE fields = rb_const_get(CLASS_OF(obj), fields_id);
      
      write_struct_begin(buf);
      
      rb_hash_foreach(fields, encode_field, args);
      
      write_field_stop(buf);
      write_struct_end(buf);
      break;
    }
    
    default: {
      rb_raise(rb_eNotImpError, "Unknown type for binary_encoding: %d", type);
    }
  }
}

// obj is always going to be a TSTRCT
static VALUE tbpa_encode_binary(VALUE self, VALUE obj) {
  VALUE buf = rb_str_buf_new(1024);
  binary_encoding(buf, obj, T_STRCT);
  return buf;
}


// -----------------------------------------------------------------------------
// Read stuff
// -----------------------------------------------------------------------------

typedef struct {
  char* name;
  int8_t type;
  int16_t id;
} field_header;

typedef struct {
  int8_t key_type;
  int8_t val_type;
  int num_entries;
} map_header;

typedef struct {
  int8_t type;
  int num_elements;
} list_header;

typedef list_header set_header;

typedef struct {
  char* data;
  int pos;
  int len;
  VALUE trans;
} decode_buffer;

typedef struct {
  char* ptr;
  int len;
} thrift_string;

#define read_struct_begin(buf)
#define read_struct_end(buf)

// This prototype is required to be able to run a call through rb_protect
// which rescues from ruby exceptions
static VALUE protectable_consume(VALUE args) {
  VALUE trans = rb_ary_entry(args, 0);
  VALUE size = rb_ary_entry(args, 1);
  
  return rb_funcall(trans, consume_bang_id, 1, size);
}

// Clears size bytes from the transport's string buffer
static bool consume(decode_buffer* buf, int32_t size) {
  if (size != 0) {
    VALUE ret;
    VALUE args = rb_ary_new3(2, buf->trans, INT2FIX(size));
    int status = 0;
    
    ret = rb_protect(protectable_consume, args, &status);
    
    if (status) {
      return false;
    } else {
      return true;
    }
  }
  
  // Nothing to consume, we're all good
  return true;
}

// This prototype is required to be able to run a call through rb_protect
// which rescues from ruby exceptions
static VALUE protectable_borrow(VALUE args) {
  VALUE trans = rb_ary_entry(args, 0);
  
  switch(RARRAY(args)->len) {
    case 1:
      return rb_funcall(trans, borrow_id, 0);
    
    case 2: {
      VALUE size = rb_ary_entry(args, 1);
      return rb_funcall(trans, borrow_id, 1, size);
    }
  }
  
  return Qnil;
}

// Calls into the transport to get the available string buffer
static bool borrow(decode_buffer* buf, int32_t size, VALUE* dst) {
  int status = 0;
  VALUE args;
  
  if (size == 0) {
    args = rb_ary_new3(1, buf->trans);
  } else {
    args = rb_ary_new3(2, buf->trans, INT2FIX(size));
  }

  *dst = rb_protect(protectable_borrow, args, &status);
  
  return (status == 0);
}

// Refills the buffer by calling borrow. If buf->pos is nonzero that number of bytes
// is cleared through consume.
//
// returns: 0 on success, non-zero on failure. On error buf is unchanged.
static int fill_buffer(decode_buffer* buf, int32_t req_len) {
  VALUE refill;
  
  if (!consume(buf, buf->pos)) {
    return -1;
  }

  if (!borrow(buf, req_len, &refill)) {
    return -2;
  }
  
  buf->data = StringValuePtr(refill);
  buf->len = RSTRING(refill)->len;
  buf->pos = 0;
  
  return 0;
}


// read_bytes pulls a number of bytes (size) from the buffer, refilling if needed,
// and places them in dst. This should _always_ be used used when reading from the buffer
// or buffered transports will be upset with you.
static bool read_bytes(decode_buffer* buf, void* dst, size_t size) {
  int avail = (buf->len - buf->pos);
  
  if (size <= avail) {
    memcpy(dst, buf->data + buf->pos, size);
    buf->pos += size;
  } else {
    
    if (avail > 0) {
      // Copy what we can
      memcpy(dst, buf->data + buf->pos, avail);
      buf->pos += avail;
    }

    if (fill_buffer(buf, size - avail) < 0) {
      return false;
    }
    
    memcpy(dst + avail, buf->data, size - avail);
    buf->pos += size - avail;
  }
  
  return true;
}

// -----------------------------------------------------------------------------
// Helpers for grabbing specific types from the buffer
// -----------------------------------------------------------------------------

static bool read_byte(decode_buffer* buf, int8_t* data) {
  return read_bytes(buf, data, sizeof(int8_t));
}

static bool read_int16(decode_buffer* buf, int16_t* data) {
  bool success = read_bytes(buf, data, sizeof(int16_t));
  *data = ntohs(*data);
  
  return success;
}

static bool read_int32(decode_buffer* buf, int32_t* data) {
  bool success = read_bytes(buf, data, sizeof(int32_t));
  *data = ntohl(*data);
  
  return success;
}

static bool read_int64(decode_buffer* buf, int64_t* data) {
  bool success = read_bytes(buf, data, sizeof(int64_t));
  *data = ntohll(*data);
  
  return success;
}

static bool read_double(decode_buffer* buf, double* data) {
  return read_int64(buf, (int64_t*)data);
}

static bool read_string(decode_buffer* buf, VALUE* data) {
  int len;
  
  if (!read_int32(buf, &len)) {
    return false;
  }
  
  if (buf->len - buf->pos >= len) {
    *data = rb_str_new(buf->data + buf->pos, len);
    buf->pos += len;
  } 
  else {
    char* str;
    
    if ((str = (char*) malloc(len)) == NULL) {
      return false;
    }
    
    if (!read_bytes(buf, str, len)) {
      free(str);
      return false;
    }
    
    *data = rb_str_new(str, len);
    
    free(str);
  }
  
  return true;
}

static bool read_field_begin(decode_buffer* buf, field_header* header) {
#ifdef __DEBUG__ // No need for this in prod since I set all the fields
  bzero(header, sizeof(field_header));
#endif

  header->name = NULL;
  
  if (!read_byte(buf, &header->type)) {
    return false;
  }
  
  if (header->type == T_STOP) {
    header->id = 0;
  } else {
    if (!read_int16(buf, &header->id)) {
      return false;
    }
  }
  
  return true;
}

#define read_field_end(buf)

static bool read_map_begin(decode_buffer* buf, map_header* header) {
#ifdef __DEBUG__ // No need for this in prod since I set all the fields
  bzero(header, sizeof(map_header));
#endif
  
  return (read_byte(buf, &header->key_type) && 
          read_byte(buf, &header->val_type) &&
          read_int32(buf, &header->num_entries));
}

#define read_map_end(buf)

static bool read_list_begin(decode_buffer* buf, list_header* header) {
#ifdef __DEBUG__ // No need for this in prod since I set all the fields
  bzero(header, sizeof(list_header));
#endif
  
  if (!read_byte(buf, &header->type) || !read_int32(buf, &header->num_elements)) {
    return false;
  } else {
    return true;
  }
}

#define read_list_end(buf)

#define read_set_begin read_list_begin
#define read_set_end read_list_end


// High level reader function with ruby type coercion
static bool read_type(int type, decode_buffer* buf, VALUE* dst) {
  switch(type) {
    case T_BOOL: {
      int8_t byte;
      
      if (!read_byte(buf, &byte)) {
        return false;
      }
      
      if (0 == byte) {
        *dst = Qfalse;
      } else {
        *dst = Qtrue;
      }
      
      break;
    }
    
    case T_BYTE: {
      int8_t byte;

      if (!read_byte(buf, &byte)) {
        return false;
      }

      *dst = INT2FIX(byte);
      break;
    }
    
    case T_I16: {
      int16_t i16;
      
      if (!read_int16(buf, &i16)) {
        return false;
      }
      
      *dst = INT2FIX(i16);
      break;
    }

    case T_I32: {
      int32_t i32;
      
      if (!read_int32(buf, &i32)) {
        return false;
      }
      
      *dst = INT2NUM(i32);
      break;
    }

    case T_I64: {
      int64_t i64;
      
      if (!read_int64(buf, &i64)) {
        return false;
      }
      
      *dst = rb_ll2inum(i64);
      break;
    }
    
    case T_DBL: {
      double dbl;
      
      if (!read_double(buf, &dbl)) {
        return false;
      }
      
      *dst = rb_float_new(dbl);
      break;
    }

    case T_STR: {
      VALUE str;
      
      if (!read_string(buf, &str)) {
        return false;
      }
      
      *dst = str;
      break;
    }
  }
  
  return true;
}

// TODO(kevinclark): Now that read_string does a malloc,
// This maybe could be modified to avoid that, and the type coercion

// Read the bytes but don't do anything with the value
static bool skip_type(int type, decode_buffer* buf) {
  switch (type) {
    case T_STRCT:
      read_struct_begin(buf);
      while (true) {
        field_header header;

        if (!read_field_begin(buf, &header)) {
          return false;
        }

        if (header.type == T_STOP) {
          break;
        }

        if (!skip_type(header.type, buf)) {
          return false;
        }

        read_field_end(buf);
      }
      read_struct_end(buf);

      break;

    case T_MAP: {
      int i;
      map_header header;

      if (!read_map_begin(buf, &header)) {
        return false;
      }

      for (i = 0; i < header.num_entries; ++i) {
        if (!skip_type(header.key_type, buf)) {
          return false;
        }
        if (!skip_type(header.val_type, buf)) {
          return false;
        }
      }

      read_map_end(buf);
      break;
    }

    case T_SET: {
      int i;
      set_header header;

      if (!read_set_begin(buf, &header)) {
        return false;
      }

      for (i = 0; i < header.num_elements; ++i) {
        if (!skip_type(header.type, buf)) {
          return false;
        }
      }

      read_set_end(buf);
      break;
    }

    case T_LIST: {
      int i;
      list_header header;

      if (!read_list_begin(buf, &header)) {
        return false;
      }

      for (i = 0; i < header.num_elements; ++i) {
        if (!skip_type(header.type, buf)) {
          return false;
        }
      }

      read_list_end(buf);
      break;
    }

    default: {
      VALUE v;
      if (!read_type(type, buf, &v)) {
        return false;
      }
    }
  }

  return true;
}


static VALUE read_struct(VALUE obj, decode_buffer* buf);

// Read the right thing from the buffer given the field spec
// and return the ruby object
static bool read_field(decode_buffer* buf, field_spec* spec, VALUE* dst) {
  switch (spec->type) {
    case T_STRCT: {
      VALUE obj = rb_class_new_instance(0, NULL, spec->data.class);
      
      *dst = read_struct(obj, buf);
      break;
    }
    
    case T_MAP: {
      map_header hdr;
      VALUE hsh;
      int i;
      
      read_map_begin(buf, &hdr); 
      hsh = rb_hash_new();
      
      for (i = 0; i < hdr.num_entries; ++i) {
        VALUE key, val;
        
        if (!read_field(buf, spec->data.map->key, &key)) {
          return false;
        }
        
        if (!read_field(buf, spec->data.map->value, &val)) {
          return false;
        }
        
        rb_hash_aset(hsh, key, val);
      }
      
      read_map_end(buf);
      
      *dst = hsh;
      break;
    }
    
    case T_LIST: {
      list_header hdr;
      VALUE arr, element;
      int i;
      
      read_list_begin(buf, &hdr);
      arr = rb_ary_new2(hdr.num_elements);
      
      for (i = 0; i < hdr.num_elements; ++i) {
        if (!read_field(buf, spec->data.element, &element)) {
          return false;
        }

        rb_ary_push(arr, element);
      }
      
      read_list_end(buf);
      
      *dst = arr;
      break;
    }
    
    case T_SET: {
      VALUE items, item;
      set_header hdr;
      int i;
      
      read_set_begin(buf, &hdr);
      items = rb_ary_new2(hdr.num_elements);
      
      for (i = 0; i < hdr.num_elements; ++i) {
        if (!read_field(buf, spec->data.element, &item)) {
          return false;
        }
        
        rb_ary_push(items, item);
      }
      
      *dst = rb_class_new_instance(1, &items, rb_cSet);
      break;
    }
    
    
    default:
      return read_type(spec->type, buf, dst);
  }
  
  return true;
}

static void handle_read_error() {
  // If it was an exception, reraise
  if (!NIL_P(ruby_errinfo)) {
    rb_exc_raise(ruby_errinfo);
  } else {
    // Something else went wrong, no idea what would call this yet
    // So far, the only thing to cause failures underneath is ruby
    // exceptions. Follow up on this regularly -- Kevin Clark (TODO)
    rb_raise(rb_eStandardError, "[BUG] Something went wrong in the field reading, but not a ruby exception");
  }
}

// Fill in the instance variables in an object (thrift struct)
// from the decode buffer
static VALUE read_struct(VALUE obj, decode_buffer* buf) {
  VALUE field;
  field_header f_header;
  VALUE value = Qnil;
  VALUE fields = rb_const_get(CLASS_OF(obj), fields_id);
  field_spec* spec;
  char name_buf[128];
    
  read_struct_begin(buf);
  
  while(true) {
    if (!read_field_begin(buf, &f_header)) {
      handle_read_error();
    }
    
    if (T_STOP == f_header.type) {
      break;
    }
    
    field = rb_hash_aref(fields, INT2FIX(f_header.id));
    
    if (NIL_P(field)) {
      if (!skip_type(f_header.type, buf)) {
        handle_read_error();
        return Qnil;
      }
    } 
    else {
      spec = parse_field_spec(field);

      if (spec->type != f_header.type) {
        if (!skip_type(spec->type, buf)) {
          free_field_spec(spec);
          handle_read_error();
          return Qnil;
        }
      } else {
        // Read busted somewhere (probably borrow/consume), bail
        if (!read_field(buf, spec, &value)) {
          free_field_spec(spec);
          handle_read_error();
          return Qnil;
        }
        
        name_buf[0] = '@';
        strlcpy(&name_buf[1], spec->name, sizeof(name_buf) - 1);
        
        rb_iv_set(obj, name_buf, value);
      }
      
      free_field_spec(spec);
    }
    
    read_field_end(buf);
  }
  
  read_struct_end(buf);
  
  return obj;
}


// Takes an object and transport, and decodes the values in the transport's
// buffer to fill the object.
static VALUE tbpa_decode_binary(VALUE self, VALUE obj, VALUE transport) {
  decode_buffer buf;
  VALUE ret_val;

  buf.pos = 0;    // This needs to be set so an arbitrary number of bytes isn't consumed
  buf.trans = transport;       // We need to hold this so the buffer can be refilled
  
  if (fill_buffer(&buf, 0) < 0) {
    handle_read_error();
    return Qnil;
  }

#ifdef __DEBUG__
  rb_p(rb_str_new2("Running decode binary with data:"));
  rb_p(rb_inspect(rb_str_new2(buf.data)));
#endif
 
  ret_val = read_struct(obj, &buf);
  
  // Consume whatever was read
  consume(&buf, buf.pos);
  
  return ret_val;
}

// -----------------------------------------------------------------------------
// These methods are used by the thrift library and need to handled
// seperately from encode and decode
// -----------------------------------------------------------------------------

// Read the message header and return it as a ruby array
static VALUE tbpa_read_message_begin(VALUE self) {
  decode_buffer buf;
  int32_t version, seqid;
  int8_t type;
  VALUE name;
  
  VALUE trans = rb_iv_get(self, "@trans");
  
  buf.pos = 0;              // This needs to be set so fill_buffer doesn't consume
  buf.trans = trans;        // We need to hold this so the buffer can be refilled
  

  if (fill_buffer(&buf, 0) < 0 || !read_int32(&buf, &version)) {
    // Consume whatever was read
    consume(&buf, buf.pos);
    handle_read_error();
    return Qnil;
  }
  
  if ((version & VERSION_MASK) != VERSION_1) {
    VALUE tprotocol_exception = rb_const_get(m_thrift, rb_intern("ProtocolException"));
    VALUE exception = rb_funcall(tprotocol_exception, rb_intern("new"), 2, rb_const_get(tprotocol_exception, rb_intern("BAD_VERSION")), rb_str_new2("Missing version identifier"));
    rb_exc_raise(exception);
  }
  
  type = version & 0x000000ff;
  
  if (!read_string(&buf, &name) || !read_int32(&buf, &seqid)) {
    // Consume whatever was read
    consume(&buf, buf.pos);
    handle_read_error();
    return Qnil;
  }
  
  // Consume whatever was read
  if (consume(&buf, buf.pos) < 0) {
    handle_read_error();
    return Qnil;
  }
  
  return rb_ary_new3(3, name, INT2FIX(type), INT2FIX(seqid));
}

void Init_binaryprotocolaccelerated()
{
  m_thrift = rb_const_get(rb_cObject, rb_intern("Thrift"));
  VALUE class_tbinproto = rb_const_get(m_thrift, rb_intern("BinaryProtocol"));
  class_tbpa = rb_define_class_under(m_thrift, "BinaryProtocolAccelerated", class_tbinproto);
  type_sym = ID2SYM(rb_intern("type"));
  class_sym = ID2SYM(rb_intern("class"));
  key_sym = ID2SYM(rb_intern("key"));
  value_sym = ID2SYM(rb_intern("value"));
  name_sym = ID2SYM(rb_intern("name"));
  fields_id = rb_intern("FIELDS");
  element_sym = ID2SYM(rb_intern("element"));
  consume_bang_id = rb_intern("consume!");
  string_buffer_id = rb_intern("string_buffer");
  borrow_id = rb_intern("borrow");
  keys_id = rb_intern("keys");
  entries_id = rb_intern("entries");
  rb_cSet = rb_const_get(rb_cObject, rb_intern("Set"));
  
  // For fast access
  rb_define_method(class_tbpa, "encode_binary", tbpa_encode_binary, 1);
  rb_define_method(class_tbpa, "decode_binary", tbpa_decode_binary, 2);
  rb_define_method(class_tbpa, "read_message_begin", tbpa_read_message_begin, 0);
  
}

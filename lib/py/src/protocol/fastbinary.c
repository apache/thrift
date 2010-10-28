/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#include <Python.h>
#include "cStringIO.h"
#include <stdbool.h>
#include <stdint.h>
#include <netinet/in.h>

/* Fix endianness issues on Solaris */
#if defined (__SVR4) && defined (__sun)
 #if defined(__i386) && !defined(__i386__)
  #define __i386__
 #endif

 #ifndef BIG_ENDIAN
  #define BIG_ENDIAN (4321)
 #endif
 #ifndef LITTLE_ENDIAN
  #define LITTLE_ENDIAN (1234)
 #endif

 /* I386 is LE, even on Solaris */
 #if !defined(BYTE_ORDER) && defined(__i386__)
  #define BYTE_ORDER LITTLE_ENDIAN
 #endif
#endif

// TODO(dreiss): defval appears to be unused.  Look into removing it.
// TODO(dreiss): Make parse_spec_args recursive, and cache the output
//               permanently in the object.  (Malloc and orphan.)
// TODO(dreiss): Why do we need cStringIO for reading, why not just char*?
//               Can cStringIO let us work with a BufferedTransport?
// TODO(dreiss): Don't ignore the rv from cwrite (maybe).

/* ====== BEGIN UTILITIES ====== */

#define INIT_OUTBUF_SIZE 128

// Stolen out of TProtocol.h.
// It would be a huge pain to have both get this from one place.
typedef enum TType {
  T_STOP       = 0,
  T_VOID       = 1,
  T_BOOL       = 2,
  T_BYTE       = 3,
  T_I08        = 3,
  T_I16        = 6,
  T_I32        = 8,
  T_U64        = 9,
  T_I64        = 10,
  T_DOUBLE     = 4,
  T_STRING     = 11,
  T_UTF7       = 11,
  T_STRUCT     = 12,
  T_MAP        = 13,
  T_SET        = 14,
  T_LIST       = 15,
  T_UTF8       = 16,
  T_UTF16      = 17
} TType;

#ifndef __BYTE_ORDER
# if defined(BYTE_ORDER) && defined(LITTLE_ENDIAN) && defined(BIG_ENDIAN)
#  define __BYTE_ORDER BYTE_ORDER
#  define __LITTLE_ENDIAN LITTLE_ENDIAN
#  define __BIG_ENDIAN BIG_ENDIAN
# else
#  error "Cannot determine endianness"
# endif
#endif

// Same comment as the enum.  Sorry.
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

// Doing a benchmark shows that interning actually makes a difference, amazingly.
#define INTERN_STRING(value) _intern_ ## value

#define INT_CONV_ERROR_OCCURRED(v) ( ((v) == -1) && PyErr_Occurred() )
#define CHECK_RANGE(v, min, max) ( ((v) <= (max)) && ((v) >= (min)) )

// Py_ssize_t was not defined before Python 2.5
#if (PY_VERSION_HEX < 0x02050000)
typedef int Py_ssize_t;
#endif

/**
 * A cache of the spec_args for a set or list,
 * so we don't have to keep calling PyTuple_GET_ITEM.
 */
typedef struct {
  TType element_type;
  PyObject* typeargs;
} SetListTypeArgs;

/**
 * A cache of the spec_args for a map,
 * so we don't have to keep calling PyTuple_GET_ITEM.
 */
typedef struct {
  TType ktag;
  TType vtag;
  PyObject* ktypeargs;
  PyObject* vtypeargs;
} MapTypeArgs;

/**
 * A cache of the spec_args for a struct,
 * so we don't have to keep calling PyTuple_GET_ITEM.
 */
typedef struct {
  PyObject* klass;
  PyObject* spec;
} StructTypeArgs;

/**
 * A cache of the item spec from a struct specification,
 * so we don't have to keep calling PyTuple_GET_ITEM.
 */
typedef struct {
  int tag;
  TType type;
  PyObject* attrname;
  PyObject* typeargs;
  PyObject* defval;
} StructItemSpec;

/**
 * A cache of the two key attributes of a CReadableTransport,
 * so we don't have to keep calling PyObject_GetAttr.
 */
typedef struct {
  PyObject* stringiobuf;
  PyObject* refill_callable;
} DecodeBuffer;

/** Pointer to interned string to speed up attribute lookup. */
static PyObject* INTERN_STRING(cstringio_buf);
/** Pointer to interned string to speed up attribute lookup. */
static PyObject* INTERN_STRING(cstringio_refill);

static inline bool
check_ssize_t_32(Py_ssize_t len) {
  // error from getting the int
  if (INT_CONV_ERROR_OCCURRED(len)) {
    return false;
  }
  if (!CHECK_RANGE(len, 0, INT32_MAX)) {
    PyErr_SetString(PyExc_OverflowError, "string size out of range");
    return false;
  }
  return true;
}

static inline bool
parse_pyint(PyObject* o, int32_t* ret, int32_t min, int32_t max) {
  long val = PyInt_AsLong(o);

  if (INT_CONV_ERROR_OCCURRED(val)) {
    return false;
  }
  if (!CHECK_RANGE(val, min, max)) {
    PyErr_SetString(PyExc_OverflowError, "int out of range");
    return false;
  }

  *ret = (int32_t) val;
  return true;
}


/* --- FUNCTIONS TO PARSE STRUCT SPECIFICATOINS --- */

static bool
parse_set_list_args(SetListTypeArgs* dest, PyObject* typeargs) {
  if (PyTuple_Size(typeargs) != 2) {
    PyErr_SetString(PyExc_TypeError, "expecting tuple of size 2 for list/set type args");
    return false;
  }

  dest->element_type = PyInt_AsLong(PyTuple_GET_ITEM(typeargs, 0));
  if (INT_CONV_ERROR_OCCURRED(dest->element_type)) {
    return false;
  }

  dest->typeargs = PyTuple_GET_ITEM(typeargs, 1);

  return true;
}

static bool
parse_map_args(MapTypeArgs* dest, PyObject* typeargs) {
  if (PyTuple_Size(typeargs) != 4) {
    PyErr_SetString(PyExc_TypeError, "expecting 4 arguments for typeargs to map");
    return false;
  }

  dest->ktag = PyInt_AsLong(PyTuple_GET_ITEM(typeargs, 0));
  if (INT_CONV_ERROR_OCCURRED(dest->ktag)) {
    return false;
  }

  dest->vtag = PyInt_AsLong(PyTuple_GET_ITEM(typeargs, 2));
  if (INT_CONV_ERROR_OCCURRED(dest->vtag)) {
    return false;
  }

  dest->ktypeargs = PyTuple_GET_ITEM(typeargs, 1);
  dest->vtypeargs = PyTuple_GET_ITEM(typeargs, 3);

  return true;
}

static bool
parse_struct_args(StructTypeArgs* dest, PyObject* typeargs) {
  if (PyTuple_Size(typeargs) != 2) {
    PyErr_SetString(PyExc_TypeError, "expecting tuple of size 2 for struct args");
    return false;
  }

  dest->klass = PyTuple_GET_ITEM(typeargs, 0);
  dest->spec = PyTuple_GET_ITEM(typeargs, 1);

  return true;
}

static int
parse_struct_item_spec(StructItemSpec* dest, PyObject* spec_tuple) {

  // i'd like to use ParseArgs here, but it seems to be a bottleneck.
  if (PyTuple_Size(spec_tuple) != 5) {
    PyErr_SetString(PyExc_TypeError, "expecting 5 arguments for spec tuple");
    return false;
  }

  dest->tag = PyInt_AsLong(PyTuple_GET_ITEM(spec_tuple, 0));
  if (INT_CONV_ERROR_OCCURRED(dest->tag)) {
    return false;
  }

  dest->type = PyInt_AsLong(PyTuple_GET_ITEM(spec_tuple, 1));
  if (INT_CONV_ERROR_OCCURRED(dest->type)) {
    return false;
  }

  dest->attrname = PyTuple_GET_ITEM(spec_tuple, 2);
  dest->typeargs = PyTuple_GET_ITEM(spec_tuple, 3);
  dest->defval = PyTuple_GET_ITEM(spec_tuple, 4);
  return true;
}

/* ====== END UTILITIES ====== */


/* ====== BEGIN WRITING FUNCTIONS ====== */

/* --- LOW-LEVEL WRITING FUNCTIONS --- */

static void writeByte(PyObject* outbuf, int8_t val) {
  int8_t net = val;
  PycStringIO->cwrite(outbuf, (char*)&net, sizeof(int8_t));
}

static void writeI16(PyObject* outbuf, int16_t val) {
  int16_t net = (int16_t)htons(val);
  PycStringIO->cwrite(outbuf, (char*)&net, sizeof(int16_t));
}

static void writeI32(PyObject* outbuf, int32_t val) {
  int32_t net = (int32_t)htonl(val);
  PycStringIO->cwrite(outbuf, (char*)&net, sizeof(int32_t));
}

static void writeI64(PyObject* outbuf, int64_t val) {
  int64_t net = (int64_t)htonll(val);
  PycStringIO->cwrite(outbuf, (char*)&net, sizeof(int64_t));
}

static void writeDouble(PyObject* outbuf, double dub) {
  // Unfortunately, bitwise_cast doesn't work in C.  Bad C!
  union {
    double f;
    int64_t t;
  } transfer;
  transfer.f = dub;
  writeI64(outbuf, transfer.t);
}


/* --- MAIN RECURSIVE OUTPUT FUCNTION -- */

static int
output_val(PyObject* output, PyObject* value, TType type, PyObject* typeargs) {
  /*
   * Refcounting Strategy:
   *
   * We assume that elements of the thrift_spec tuple are not going to be
   * mutated, so we don't ref count those at all. Other than that, we try to
   * keep a reference to all the user-created objects while we work with them.
   * output_val assumes that a reference is already held. The *caller* is
   * responsible for handling references
   */

  switch (type) {

  case T_BOOL: {
    int v = PyObject_IsTrue(value);
    if (v == -1) {
      return false;
    }

    writeByte(output, (int8_t) v);
    break;
  }
  case T_I08: {
    int32_t val;

    if (!parse_pyint(value, &val, INT8_MIN, INT8_MAX)) {
      return false;
    }

    writeByte(output, (int8_t) val);
    break;
  }
  case T_I16: {
    int32_t val;

    if (!parse_pyint(value, &val, INT16_MIN, INT16_MAX)) {
      return false;
    }

    writeI16(output, (int16_t) val);
    break;
  }
  case T_I32: {
    int32_t val;

    if (!parse_pyint(value, &val, INT32_MIN, INT32_MAX)) {
      return false;
    }

    writeI32(output, val);
    break;
  }
  case T_I64: {
    int64_t nval = PyLong_AsLongLong(value);

    if (INT_CONV_ERROR_OCCURRED(nval)) {
      return false;
    }

    if (!CHECK_RANGE(nval, INT64_MIN, INT64_MAX)) {
      PyErr_SetString(PyExc_OverflowError, "int out of range");
      return false;
    }

    writeI64(output, nval);
    break;
  }

  case T_DOUBLE: {
    double nval = PyFloat_AsDouble(value);
    if (nval == -1.0 && PyErr_Occurred()) {
      return false;
    }

    writeDouble(output, nval);
    break;
  }

  case T_STRING: {
    Py_ssize_t len = PyString_Size(value);

    if (!check_ssize_t_32(len)) {
      return false;
    }

    writeI32(output, (int32_t) len);
    PycStringIO->cwrite(output, PyString_AsString(value), (int32_t) len);
    break;
  }

  case T_LIST:
  case T_SET: {
    Py_ssize_t len;
    SetListTypeArgs parsedargs;
    PyObject *item;
    PyObject *iterator;

    if (!parse_set_list_args(&parsedargs, typeargs)) {
      return false;
    }

    len = PyObject_Length(value);

    if (!check_ssize_t_32(len)) {
      return false;
    }

    writeByte(output, parsedargs.element_type);
    writeI32(output, (int32_t) len);

    iterator =  PyObject_GetIter(value);
    if (iterator == NULL) {
      return false;
    }

    while ((item = PyIter_Next(iterator))) {
      if (!output_val(output, item, parsedargs.element_type, parsedargs.typeargs)) {
        Py_DECREF(item);
        Py_DECREF(iterator);
        return false;
      }
      Py_DECREF(item);
    }

    Py_DECREF(iterator);

    if (PyErr_Occurred()) {
      return false;
    }

    break;
  }

  case T_MAP: {
    PyObject *k, *v;
    Py_ssize_t pos = 0;
    Py_ssize_t len;

    MapTypeArgs parsedargs;

    len = PyDict_Size(value);
    if (!check_ssize_t_32(len)) {
      return false;
    }

    if (!parse_map_args(&parsedargs, typeargs)) {
      return false;
    }

    writeByte(output, parsedargs.ktag);
    writeByte(output, parsedargs.vtag);
    writeI32(output, len);

    // TODO(bmaurer): should support any mapping, not just dicts
    while (PyDict_Next(value, &pos, &k, &v)) {
      // TODO(dreiss): Think hard about whether these INCREFs actually
      //               turn any unsafe scenarios into safe scenarios.
      Py_INCREF(k);
      Py_INCREF(v);

      if (!output_val(output, k, parsedargs.ktag, parsedargs.ktypeargs)
          || !output_val(output, v, parsedargs.vtag, parsedargs.vtypeargs)) {
        Py_DECREF(k);
        Py_DECREF(v);
        return false;
      }
      Py_DECREF(k);
      Py_DECREF(v);
    }
    break;
  }

  // TODO(dreiss): Consider breaking this out as a function
  //               the way we did for decode_struct.
  case T_STRUCT: {
    StructTypeArgs parsedargs;
    Py_ssize_t nspec;
    Py_ssize_t i;

    if (!parse_struct_args(&parsedargs, typeargs)) {
      return false;
    }

    nspec = PyTuple_Size(parsedargs.spec);

    if (nspec == -1) {
      return false;
    }

    for (i = 0; i < nspec; i++) {
      StructItemSpec parsedspec;
      PyObject* spec_tuple;
      PyObject* instval = NULL;

      spec_tuple = PyTuple_GET_ITEM(parsedargs.spec, i);
      if (spec_tuple == Py_None) {
        continue;
      }

      if (!parse_struct_item_spec (&parsedspec, spec_tuple)) {
        return false;
      }

      instval = PyObject_GetAttr(value, parsedspec.attrname);

      if (!instval) {
        return false;
      }

      if (instval == Py_None) {
        Py_DECREF(instval);
        continue;
      }

      writeByte(output, (int8_t) parsedspec.type);
      writeI16(output, parsedspec.tag);

      if (!output_val(output, instval, parsedspec.type, parsedspec.typeargs)) {
        Py_DECREF(instval);
        return false;
      }

      Py_DECREF(instval);
    }

    writeByte(output, (int8_t)T_STOP);
    break;
  }

  case T_STOP:
  case T_VOID:
  case T_UTF16:
  case T_UTF8:
  case T_U64:
  default:
    PyErr_SetString(PyExc_TypeError, "Unexpected TType");
    return false;

  }

  return true;
}


/* --- TOP-LEVEL WRAPPER FOR OUTPUT -- */

static PyObject *
encode_binary(PyObject *self, PyObject *args) {
  PyObject* enc_obj;
  PyObject* type_args;
  PyObject* buf;
  PyObject* ret = NULL;

  if (!PyArg_ParseTuple(args, "OO", &enc_obj, &type_args)) {
    return NULL;
  }

  buf = PycStringIO->NewOutput(INIT_OUTBUF_SIZE);
  if (output_val(buf, enc_obj, T_STRUCT, type_args)) {
    ret = PycStringIO->cgetvalue(buf);
  }

  Py_DECREF(buf);
  return ret;
}

/* ====== END WRITING FUNCTIONS ====== */


/* ====== BEGIN READING FUNCTIONS ====== */

/* --- LOW-LEVEL READING FUNCTIONS --- */

static void
free_decodebuf(DecodeBuffer* d) {
  Py_XDECREF(d->stringiobuf);
  Py_XDECREF(d->refill_callable);
}

static bool
decode_buffer_from_obj(DecodeBuffer* dest, PyObject* obj) {
  dest->stringiobuf = PyObject_GetAttr(obj, INTERN_STRING(cstringio_buf));
  if (!dest->stringiobuf) {
    return false;
  }

  if (!PycStringIO_InputCheck(dest->stringiobuf)) {
    free_decodebuf(dest);
    PyErr_SetString(PyExc_TypeError, "expecting stringio input");
    return false;
  }

  dest->refill_callable = PyObject_GetAttr(obj, INTERN_STRING(cstringio_refill));

  if(!dest->refill_callable) {
    free_decodebuf(dest);
    return false;
  }

  if (!PyCallable_Check(dest->refill_callable)) {
    free_decodebuf(dest);
    PyErr_SetString(PyExc_TypeError, "expecting callable");
    return false;
  }

  return true;
}

static bool readBytes(DecodeBuffer* input, char** output, int len) {
  int read;

  // TODO(dreiss): Don't fear the malloc.  Think about taking a copy of
  //               the partial read instead of forcing the transport
  //               to prepend it to its buffer.

  read = PycStringIO->cread(input->stringiobuf, output, len);

  if (read == len) {
    return true;
  } else if (read == -1) {
    return false;
  } else {
    PyObject* newiobuf;

    // using building functions as this is a rare codepath
    newiobuf = PyObject_CallFunction(
        input->refill_callable, "s#i", *output, read, len, NULL);
    if (newiobuf == NULL) {
      return false;
    }

    // must do this *AFTER* the call so that we don't deref the io buffer
    Py_CLEAR(input->stringiobuf);
    input->stringiobuf = newiobuf;

    read = PycStringIO->cread(input->stringiobuf, output, len);

    if (read == len) {
      return true;
    } else if (read == -1) {
      return false;
    } else {
      // TODO(dreiss): This could be a valid code path for big binary blobs.
      PyErr_SetString(PyExc_TypeError,
          "refill claimed to have refilled the buffer, but didn't!!");
      return false;
    }
  }
}

static int8_t readByte(DecodeBuffer* input) {
  char* buf;
  if (!readBytes(input, &buf, sizeof(int8_t))) {
    return -1;
  }

  return *(int8_t*) buf;
}

static int16_t readI16(DecodeBuffer* input) {
  char* buf;
  if (!readBytes(input, &buf, sizeof(int16_t))) {
    return -1;
  }

  return (int16_t) ntohs(*(int16_t*) buf);
}

static int32_t readI32(DecodeBuffer* input) {
  char* buf;
  if (!readBytes(input, &buf, sizeof(int32_t))) {
    return -1;
  }
  return (int32_t) ntohl(*(int32_t*) buf);
}


static int64_t readI64(DecodeBuffer* input) {
  char* buf;
  if (!readBytes(input, &buf, sizeof(int64_t))) {
    return -1;
  }

  return (int64_t) ntohll(*(int64_t*) buf);
}

static double readDouble(DecodeBuffer* input) {
  union {
    int64_t f;
    double t;
  } transfer;

  transfer.f = readI64(input);
  if (transfer.f == -1) {
    return -1;
  }
  return transfer.t;
}

static bool
checkTypeByte(DecodeBuffer* input, TType expected) {
  TType got = readByte(input);
  if (INT_CONV_ERROR_OCCURRED(got)) {
    return false;
  }

  if (expected != got) {
    PyErr_SetString(PyExc_TypeError, "got wrong ttype while reading field");
    return false;
  }
  return true;
}

static bool
skip(DecodeBuffer* input, TType type) {
#define SKIPBYTES(n) \
  do { \
    if (!readBytes(input, &dummy_buf, (n))) { \
      return false; \
    } \
  } while(0)

  char* dummy_buf;

  switch (type) {

  case T_BOOL:
  case T_I08: SKIPBYTES(1); break;
  case T_I16: SKIPBYTES(2); break;
  case T_I32: SKIPBYTES(4); break;
  case T_I64:
  case T_DOUBLE: SKIPBYTES(8); break;

  case T_STRING: {
    // TODO(dreiss): Find out if these check_ssize_t32s are really necessary.
    int len = readI32(input);
    if (!check_ssize_t_32(len)) {
      return false;
    }
    SKIPBYTES(len);
    break;
  }

  case T_LIST:
  case T_SET: {
    TType etype;
    int len, i;

    etype = readByte(input);
    if (etype == -1) {
      return false;
    }

    len = readI32(input);
    if (!check_ssize_t_32(len)) {
      return false;
    }

    for (i = 0; i < len; i++) {
      if (!skip(input, etype)) {
        return false;
      }
    }
    break;
  }

  case T_MAP: {
    TType ktype, vtype;
    int len, i;

    ktype = readByte(input);
    if (ktype == -1) {
      return false;
    }

    vtype = readByte(input);
    if (vtype == -1) {
      return false;
    }

    len = readI32(input);
    if (!check_ssize_t_32(len)) {
      return false;
    }

    for (i = 0; i < len; i++) {
      if (!(skip(input, ktype) && skip(input, vtype))) {
        return false;
      }
    }
    break;
  }

  case T_STRUCT: {
    while (true) {
      TType type;

      type = readByte(input);
      if (type == -1) {
        return false;
      }

      if (type == T_STOP)
        break;

      SKIPBYTES(2); // tag
      if (!skip(input, type)) {
        return false;
      }
    }
    break;
  }

  case T_STOP:
  case T_VOID:
  case T_UTF16:
  case T_UTF8:
  case T_U64:
  default:
    PyErr_SetString(PyExc_TypeError, "Unexpected TType");
    return false;

  }

  return true;

#undef SKIPBYTES
}


/* --- HELPER FUNCTION FOR DECODE_VAL --- */

static PyObject*
decode_val(DecodeBuffer* input, TType type, PyObject* typeargs);

static bool
decode_struct(DecodeBuffer* input, PyObject* output, PyObject* spec_seq) {
  int spec_seq_len = PyTuple_Size(spec_seq);
  if (spec_seq_len == -1) {
    return false;
  }

  while (true) {
    TType type;
    int16_t tag;
    PyObject* item_spec;
    PyObject* fieldval = NULL;
    StructItemSpec parsedspec;

    type = readByte(input);
    if (type == -1) {
      return false;
    }
    if (type == T_STOP) {
      break;
    }
    tag = readI16(input);
    if (INT_CONV_ERROR_OCCURRED(tag)) {
      return false;
    }
    if (tag >= 0 && tag < spec_seq_len) {
      item_spec = PyTuple_GET_ITEM(spec_seq, tag);
    } else {
      item_spec = Py_None;
    }

    if (item_spec == Py_None) {
      if (!skip(input, type)) {
        return false;
      } else {
        continue;
      }
    }

    if (!parse_struct_item_spec(&parsedspec, item_spec)) {
      return false;
    }
    if (parsedspec.type != type) {
      if (!skip(input, type)) {
        PyErr_SetString(PyExc_TypeError, "struct field had wrong type while reading and can't be skipped");
        return false;
      } else {
        continue;
      }
    }

    fieldval = decode_val(input, parsedspec.type, parsedspec.typeargs);
    if (fieldval == NULL) {
      return false;
    }

    if (PyObject_SetAttr(output, parsedspec.attrname, fieldval) == -1) {
      Py_DECREF(fieldval);
      return false;
    }
    Py_DECREF(fieldval);
  }
  return true;
}


/* --- MAIN RECURSIVE INPUT FUCNTION --- */

// Returns a new reference.
static PyObject*
decode_val(DecodeBuffer* input, TType type, PyObject* typeargs) {
  switch (type) {

  case T_BOOL: {
    int8_t v = readByte(input);
    if (INT_CONV_ERROR_OCCURRED(v)) {
      return NULL;
    }

    switch (v) {
    case 0: Py_RETURN_FALSE;
    case 1: Py_RETURN_TRUE;
    // Don't laugh.  This is a potentially serious issue.
    default: PyErr_SetString(PyExc_TypeError, "boolean out of range"); return NULL;
    }
    break;
  }
  case T_I08: {
    int8_t v = readByte(input);
    if (INT_CONV_ERROR_OCCURRED(v)) {
      return NULL;
    }

    return PyInt_FromLong(v);
  }
  case T_I16: {
    int16_t v = readI16(input);
    if (INT_CONV_ERROR_OCCURRED(v)) {
      return NULL;
    }
    return PyInt_FromLong(v);
  }
  case T_I32: {
    int32_t v = readI32(input);
    if (INT_CONV_ERROR_OCCURRED(v)) {
      return NULL;
    }
    return PyInt_FromLong(v);
  }

  case T_I64: {
    int64_t v = readI64(input);
    if (INT_CONV_ERROR_OCCURRED(v)) {
      return NULL;
    }
    // TODO(dreiss): Find out if we can take this fastpath always when
    //               sizeof(long) == sizeof(long long).
    if (CHECK_RANGE(v, LONG_MIN, LONG_MAX)) {
      return PyInt_FromLong((long) v);
    }

    return PyLong_FromLongLong(v);
  }

  case T_DOUBLE: {
    double v = readDouble(input);
    if (v == -1.0 && PyErr_Occurred()) {
      return false;
    }
    return PyFloat_FromDouble(v);
  }

  case T_STRING: {
    Py_ssize_t len = readI32(input);
    char* buf;
    if (!readBytes(input, &buf, len)) {
      return NULL;
    }

    return PyString_FromStringAndSize(buf, len);
  }

  case T_LIST:
  case T_SET: {
    SetListTypeArgs parsedargs;
    int32_t len;
    PyObject* ret = NULL;
    int i;

    if (!parse_set_list_args(&parsedargs, typeargs)) {
      return NULL;
    }

    if (!checkTypeByte(input, parsedargs.element_type)) {
      return NULL;
    }

    len = readI32(input);
    if (!check_ssize_t_32(len)) {
      return NULL;
    }

    ret = PyList_New(len);
    if (!ret) {
      return NULL;
    }

    for (i = 0; i < len; i++) {
      PyObject* item = decode_val(input, parsedargs.element_type, parsedargs.typeargs);
      if (!item) {
        Py_DECREF(ret);
        return NULL;
      }
      PyList_SET_ITEM(ret, i, item);
    }

    // TODO(dreiss): Consider biting the bullet and making two separate cases
    //               for list and set, avoiding this post facto conversion.
    if (type == T_SET) {
      PyObject* setret;
#if (PY_VERSION_HEX < 0x02050000)
      // hack needed for older versions
      setret = PyObject_CallFunctionObjArgs((PyObject*)&PySet_Type, ret, NULL);
#else
      // official version
      setret = PySet_New(ret);
#endif
      Py_DECREF(ret);
      return setret;
    }
    return ret;
  }

  case T_MAP: {
    int32_t len;
    int i;
    MapTypeArgs parsedargs;
    PyObject* ret = NULL;

    if (!parse_map_args(&parsedargs, typeargs)) {
      return NULL;
    }

    if (!checkTypeByte(input, parsedargs.ktag)) {
      return NULL;
    }
    if (!checkTypeByte(input, parsedargs.vtag)) {
      return NULL;
    }

    len = readI32(input);
    if (!check_ssize_t_32(len)) {
      return false;
    }

    ret = PyDict_New();
    if (!ret) {
      goto error;
    }

    for (i = 0; i < len; i++) {
      PyObject* k = NULL;
      PyObject* v = NULL;
      k = decode_val(input, parsedargs.ktag, parsedargs.ktypeargs);
      if (k == NULL) {
        goto loop_error;
      }
      v = decode_val(input, parsedargs.vtag, parsedargs.vtypeargs);
      if (v == NULL) {
        goto loop_error;
      }
      if (PyDict_SetItem(ret, k, v) == -1) {
        goto loop_error;
      }

      Py_DECREF(k);
      Py_DECREF(v);
      continue;

      // Yuck!  Destructors, anyone?
      loop_error:
      Py_XDECREF(k);
      Py_XDECREF(v);
      goto error;
    }

    return ret;

    error:
    Py_XDECREF(ret);
    return NULL;
  }

  case T_STRUCT: {
    StructTypeArgs parsedargs;
    if (!parse_struct_args(&parsedargs, typeargs)) {
      return NULL;
    }

    PyObject* ret = PyObject_CallObject(parsedargs.klass, NULL);
    if (!ret) {
      return NULL;
    }

    if (!decode_struct(input, ret, parsedargs.spec)) {
      Py_DECREF(ret);
      return NULL;
    }

    return ret;
  }

  case T_STOP:
  case T_VOID:
  case T_UTF16:
  case T_UTF8:
  case T_U64:
  default:
    PyErr_SetString(PyExc_TypeError, "Unexpected TType");
    return NULL;
  }
}


/* --- TOP-LEVEL WRAPPER FOR INPUT -- */

static PyObject*
decode_binary(PyObject *self, PyObject *args) {
  PyObject* output_obj = NULL;
  PyObject* transport = NULL;
  PyObject* typeargs = NULL;
  StructTypeArgs parsedargs;
  DecodeBuffer input = {};

  if (!PyArg_ParseTuple(args, "OOO", &output_obj, &transport, &typeargs)) {
    return NULL;
  }

  if (!parse_struct_args(&parsedargs, typeargs)) {
    return NULL;
  }

  if (!decode_buffer_from_obj(&input, transport)) {
    return NULL;
  }

  if (!decode_struct(&input, output_obj, parsedargs.spec)) {
    free_decodebuf(&input);
    return NULL;
  }

  free_decodebuf(&input);

  Py_RETURN_NONE;
}

/* ====== END READING FUNCTIONS ====== */


/* -- PYTHON MODULE SETUP STUFF --- */

static PyMethodDef ThriftFastBinaryMethods[] = {

  {"encode_binary",  encode_binary, METH_VARARGS, ""},
  {"decode_binary",  decode_binary, METH_VARARGS, ""},

  {NULL, NULL, 0, NULL}        /* Sentinel */
};

PyMODINIT_FUNC
initfastbinary(void) {
#define INIT_INTERN_STRING(value) \
  do { \
    INTERN_STRING(value) = PyString_InternFromString(#value); \
    if(!INTERN_STRING(value)) return; \
  } while(0)

  INIT_INTERN_STRING(cstringio_buf);
  INIT_INTERN_STRING(cstringio_refill);
#undef INIT_INTERN_STRING

  PycString_IMPORT;
  if (PycStringIO == NULL) return;

  (void) Py_InitModule("thrift.protocol.fastbinary", ThriftFastBinaryMethods);
}

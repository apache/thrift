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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#include <netinet/in.h>
#include <unistd.h>
#include <endian.h>
#include <byteswap.h>

#if __BYTE_ORDER == __LITTLE_ENDIAN
#define ntohll(x) bswap_64(x)
#else
#define ntohll(x) x
#endif

enum TType {
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
};

#include "php.h"
#include "php_thrift_protocol.h"

static function_entry thrift_protocol_functions[] = {
  PHP_FE(thrift_protocol_binary_deserialize, NULL)
  {NULL, NULL, NULL}
} ;

zend_module_entry thrift_protocol_module_entry = {
  STANDARD_MODULE_HEADER,
  "thrift_protocol",
  thrift_protocol_functions,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  "1.0",
  STANDARD_MODULE_PROPERTIES
};

#ifdef COMPILE_DL_THRIFT_PROTOCOL
ZEND_GET_MODULE(thrift_protocol)
#endif

class PHPTransport {
public:
  PHPTransport(zval* _p, size_t _buffer_size = 1024) :  buffer(reinterpret_cast<char*>(emalloc(_buffer_size))), buffer_remaining(0), buffer_size(_buffer_size), p(_p) {
    ZVAL_STRING(&funcname, "read", 0);
    // Get the transport for the passed protocol
    zval gettransport;
    ZVAL_STRING(&gettransport, "getTransport", 0);
    MAKE_STD_ZVAL(t);
    ZVAL_NULL(t);
    TSRMLS_FETCH();
    call_user_function(EG(function_table), &p, &gettransport, t, 0, NULL TSRMLS_CC);
  }

  ~PHPTransport() {
    put_back();
    efree(buffer);
    zval_ptr_dtor(&t);
  }

  void put_back() {
    if (buffer_remaining) {
      zval putbackfn;
      ZVAL_STRING(&putbackfn, "putBack", 0);

      char* newbuf = (char*)emalloc(buffer_remaining + 1);
      memcpy(newbuf, buffer_ptr, buffer_remaining);
      newbuf[buffer_remaining] = '\0';

      zval *args[1];
      MAKE_STD_ZVAL(args[0]);
      ZVAL_STRINGL(args[0], newbuf, buffer_remaining, 0);

      TSRMLS_FETCH();

      zval ret;
      call_user_function(EG(function_table), &t, &putbackfn, &ret, 1, args TSRMLS_CC);
      zval_ptr_dtor(args);
      zval_dtor(&ret);
    }
    buffer_remaining = 0;
    buffer_ptr = buffer;
  }

  zval* protocol() { return p; }
  zval* transport() { return t; }

  void readBytes(void* buf, size_t len) {
    while (len) {
      size_t chunk_size = MIN(len, buffer_remaining);
      if (chunk_size) {
        memcpy(buf, buffer_ptr, chunk_size);
        buffer_ptr = reinterpret_cast<char*>(buffer_ptr) + chunk_size;
        buffer_remaining -= chunk_size;
        buf = reinterpret_cast<char*>(buf) + chunk_size;
        len -= chunk_size;
      }
      if (! len) break;
      refill();
    }
  }

protected:
  void refill() {
    assert(buffer_remaining == 0);
    zval retval;
    ZVAL_NULL(&retval);

    zval *args[1];
    MAKE_STD_ZVAL(args[0]);
    ZVAL_LONG(args[0], buffer_size);

    TSRMLS_FETCH();

    call_user_function(EG(function_table), &t, &funcname, &retval, 1, args TSRMLS_CC);
    zval_ptr_dtor(args);

    buffer_remaining = Z_STRLEN(retval);
    memcpy(buffer, Z_STRVAL(retval), buffer_remaining);
    zval_dtor(&retval);

    buffer_ptr = buffer;
  }

  char* buffer;
  char* buffer_ptr;
  size_t buffer_remaining;
  size_t buffer_size;

  zval* p; 
  zval* t;
  zval funcname;
};

// Does not call the ctor on the object, all fields will be NULL
void createObject(char* obj_typename, zval* return_value) {
  TSRMLS_FETCH();
  size_t obj_typename_len = strlen(obj_typename);
  zend_class_entry* ce = zend_fetch_class(obj_typename, obj_typename_len, ZEND_FETCH_CLASS_DEFAULT TSRMLS_CC);
  if (! ce) {
    php_error_docref(NULL TSRMLS_CC, E_ERROR, "Class %s does not exist", obj_typename);
    RETURN_NULL();
  } 

  object_and_properties_init(return_value, ce, NULL);
}

void binary_deserialize(long thrift_typeID, PHPTransport& transport, zval* return_value, char* structType) {
  Z_TYPE_P(return_value) = IS_NULL; // just in case

  switch (thrift_typeID) {
    case T_STOP:
    case T_VOID:
      RETURN_NULL();
      return; 
    case T_STRUCT: {
      assert(structType);
      createObject(structType, return_value);
      zval retval;
      ZVAL_NULL(&retval);
      zval *args[1];
      args[0] = transport.protocol();
      zval funcname;
      ZVAL_STRING(&funcname, "read", 0);
      transport.put_back(); // return our buffer to the userland T{Framed,Buffered}Transport for reading the field headers and such
      TSRMLS_FETCH();
      call_user_function(EG(function_table), &return_value, &funcname, &retval, 1, args TSRMLS_CC);
      zval_dtor(&retval);
      return;
    } break;
    case T_BOOL: {
      uint8_t c;
      transport.readBytes(&c, 1);
      RETURN_BOOL(c != 0);
    }
  //case T_I08: // same numeric value as T_BYTE
    case T_BYTE: { 
      uint8_t c;
      transport.readBytes(&c, 1);
      RETURN_LONG(c); 
    }
    case T_I16: {
      uint16_t c;
      transport.readBytes(&c, 2);
      RETURN_LONG(ntohs(c));
    }
    case T_I32: {
      uint32_t c;
      transport.readBytes(&c, 4);
      RETURN_LONG(ntohl(c));
    }
    case T_U64:
    case T_I64: {
      uint64_t c;
      transport.readBytes(&c, 8);
      RETURN_LONG(ntohll(c));
    }
    case T_DOUBLE: {
      union {
        uint64_t c;
        double d;
      } a;
      transport.readBytes(&(a.c), 8);
      a.c = ntohll(a.c);
      RETURN_DOUBLE(a.d);
    }
    //case T_UTF7: // aliases T_STRING
    case T_UTF8:
    case T_UTF16:
    case T_STRING: {
      uint32_t size;
      transport.readBytes(&size, 4);
      size = ntohl(size);
      char* strbuf = (char*) emalloc(size + 1);
      if (size) {
        transport.readBytes(strbuf, size);
      }
      strbuf[size] = '\0';
      ZVAL_STRINGL(return_value, strbuf, size, 0);
      return;
    }
    case T_MAP: { // array of key -> value
      uint8_t types[2];
      uint32_t size;
      transport.readBytes(types, 2);
      transport.readBytes(&size, 4);
      size = ntohl(size);
      array_init(return_value);

      for (uint32_t s = 0; s < size; ++s) {
        zval *value;
        MAKE_STD_ZVAL(value);

        zval* key;
        MAKE_STD_ZVAL(key);

        binary_deserialize(types[0], transport, key, NULL);
        binary_deserialize(types[1], transport, value, structType);
        if (Z_TYPE_P(key) == IS_LONG) {
          zend_hash_index_update(return_value->value.ht, Z_LVAL_P(key), &value, sizeof(zval *), NULL);
        }
        else {
          convert_to_string_ex(&key);
          zend_hash_update(return_value->value.ht, Z_STRVAL_P(key), Z_STRLEN_P(key) + 1, &value, sizeof(zval *), NULL);
        }
        zval_ptr_dtor(&key);
      }
      return; // return_value already populated
    }
    case T_LIST: { // array with autogenerated numeric keys
      uint8_t type;
      uint32_t size;
      transport.readBytes(&type, 1);
      transport.readBytes(&size, 4);
      size = ntohl(size);

      array_init(return_value);
      for (uint32_t s = 0; s < size; ++s) {
        zval *value;
        MAKE_STD_ZVAL(value);
        binary_deserialize(type, transport, value, structType);
        zend_hash_next_index_insert(return_value->value.ht, &value, sizeof(zval *), NULL);
      }
      return;
    }
    case T_SET: { // array of key -> TRUE
      uint8_t type;
      uint32_t size;
      transport.readBytes(&type, 1);
      transport.readBytes(&size, 4);
      size = ntohl(size);
      array_init(return_value);

      for (uint32_t s = 0; s < size; ++s) {
        zval* key;
        zval* value;
        MAKE_STD_ZVAL(key);
        MAKE_STD_ZVAL(value);
        ZVAL_TRUE(value);

        binary_deserialize(type, transport, key, NULL);

        if (Z_TYPE_P(key) == IS_LONG) {
          zend_hash_index_update(return_value->value.ht, Z_LVAL_P(key), &value, sizeof(zval *), NULL);
        }
        else {
          convert_to_string_ex(&key);
          zend_hash_update(return_value->value.ht, Z_STRVAL_P(key), Z_STRLEN_P(key) + 1, &value, sizeof(zval *), NULL);
        }
        zval_ptr_dtor(&key);
      }
      return;
    }
    default:
      TSRMLS_FETCH();
      php_error_docref(NULL TSRMLS_CC, E_ERROR, "Unknown thrift typeID %ld", thrift_typeID);
      RETURN_NULL();
  };
  assert(0); // should never get here...
}

PHP_FUNCTION(thrift_protocol_binary_deserialize) {
  int argc = ZEND_NUM_ARGS();
  long thrift_typeID;

  if (argc < 2) {
    WRONG_PARAM_COUNT;
  }

  zval ***args = (zval***) emalloc(argc * sizeof(zval**));
  zend_get_parameters_array_ex(argc, args);
  convert_to_long_ex(args[0]);
  thrift_typeID = Z_LVAL_PP(args[0]);

  if (Z_TYPE_PP(args[1]) != IS_OBJECT) {
    php_error_docref(NULL TSRMLS_CC, E_ERROR, "2nd parameter is not an object");
    efree(args);
    RETURN_NULL();
  }
 
  char* structType = NULL;
  if (argc >= 3) {
    if (Z_TYPE_PP(args[2]) == IS_STRING) {
      for (int s = 0; s < Z_STRLEN_PP(args[2]); ++s) {
        if (isalpha(Z_STRVAL_PP(args[2])[s])) Z_STRVAL_PP(args[2])[s] = tolower(Z_STRVAL_PP(args[2])[s]);
      }
      structType = Z_STRVAL_PP(args[2]);
    } else {
      php_error_docref(NULL TSRMLS_CC, E_ERROR, "3rd parameter (if present) must be a string");
      efree(args);
      RETURN_NULL();
    }
  }

  PHPTransport transport(*args[1]);

  binary_deserialize(thrift_typeID, transport, return_value, structType);
  efree(args);
}


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

#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>

#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/thrift_base64_utils.h>
#include <thrift/c_glib/protocol/thrift_protocol.h>
#include <thrift/c_glib/protocol/thrift_json_protocol.h>
#include <thrift/c_glib/protocol/thrift_json_pair_context.h>

G_DEFINE_TYPE(ThriftJsonProtocol, thrift_json_protocol, THRIFT_TYPE_PROTOCOL)

/* object properties */
enum _ThriftJsonProtocolProperties
{
  PROP_0,
  PROP_THRIFT_JSON_PROTOCOL_TRANSPORT,
  PROP_THRIFT_JSON_PROTOCOL_LOOK_AHEAD_READER,
};

static const guint8 KJSONObjectStart = '{';
static const guint8 KJSONObjectEnd = '}';
static const guint8 KJSONArrayStart = '[';
static const guint8 KJSONArrayEnd = ']';
static const guint8 KJSONPairSeparator = ':';
static const guint8 KJSONElemSeparator = ',';
static const guint8 KJSONBackslash = '\\';
static const guint8 KJSONStringDelimiter = '"';
static const guint8 KJSONEscapeChar = 'u';

static const gchar *KJSONEscapePrefix = "\\u00";

static const guint32 KThriftVersion1 = 1;

static const gchar *KThriftNan = "NaN";
static const gchar *KThriftInfinity = "Infinity";
static const gchar *KThriftNegativeInfinity = "-Infinity";

static const gchar *KTypeNameBool = "tf";
static const gchar *KTypeNameByte = "i8";
static const gchar *KTypeNameI16 = "i16";
static const gchar *KTypeNameI32 = "i32";
static const gchar *KTypeNameI64 = "i64";
static const gchar *KTypeNameDouble = "dbl";
static const gchar *KTypeNameStruct = "rec";
static const gchar *KTypeNameString = "str";
static const gchar *KTypeNameMap = "map";
static const gchar *KTypeNameList = "lst";
static const gchar *KTypeNameSet = "set";

/* This table decribes the handling for the first 0x30 characters
 * 0 : escape using "\u00xx" notation
 * 1 : just output index
 * <other> : escape using "\<other>" notation
 */
static const guint8 KJSONCharTable[0x30] = {
  /* 0 1 2 3 4 5 6 7 8 9 A B C D E F */
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  'b',
  't',
  'n',
  0,
  'f',
  'r',
  0,
  0, /* 0 */
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0, /* 1 */
  1,
  1,
  '"',
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1, /* 2 */
};

static const gchar *KEscapeChars = "\"\\bfnrt/";

static const uint8_t kEscapeCharVals[8] = {
    '"',
    '\\',
    '\b',
    '\f',
    '\n',
    '\r',
    '\t',
    '/',
};

static const gchar*
thrift_json_protocol_get_typename_for_typeid (ThriftJsonProtocol *protocol, 
                                              ThriftType typeID, GError ** error)
{
  THRIFT_UNUSED_VAR (protocol);

  switch (typeID) {
    case T_BOOL:
      return KTypeNameBool;
    case T_BYTE:
      return KTypeNameByte;
    case T_I16:
      return KTypeNameI16;
    case T_I32:
      return KTypeNameI32;
    case T_I64:
      return KTypeNameI64;
    case T_DOUBLE:
      return KTypeNameDouble;
    case T_STRING:
      return KTypeNameString;
    case T_STRUCT:
      return KTypeNameStruct;
    case T_MAP:
      return KTypeNameMap;
    case T_SET:
      return KTypeNameSet;
    case T_LIST:
      return KTypeNameList;
    default:
      g_set_error (error,
                   THRIFT_PROTOCOL_ERROR,
		   THRIFT_PROTOCOL_ERROR_UNKNOWN,
                   "unknown type");
      return NULL;
  }
}

static ThriftType 
thrift_json_protocol_get_typeid_for_typename(ThriftJsonProtocol *protocol, 
                                             const gchar* str, GError **error) 
{
  THRIFT_UNUSED_VAR (protocol);

  ThriftType result = T_STOP; // Sentinel value
  if (strlen (str) > 1) {
    switch (str[0]) {
    case 'd':
      result = T_DOUBLE;
      break;
    case 'i':
      switch (str[1]) {
      case '8':
        result = T_BYTE;
        break;
      case '1':
        result = T_I16;
        break;
      case '3':
        result = T_I32;
        break;
      case '6':
        result = T_I64;
        break;
      }
      break;
    case 'l':
      result = T_LIST;
      break;
    case 'm':
      result = T_MAP;
      break;
    case 'r':
      result = T_STRUCT;
      break;
    case 's':
      if (str[1] == 't') {
        result = T_STRING;
      } else if (str[1] == 'e') {
        result = T_SET;
      }
      break;
    case 't':
      result = T_BOOL;
      break;
    }
  }
  if (result == T_STOP) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_UNKNOWN,
                 "unknown type");
    return T_STOP;
  }
  return result;
}

static gint32
thrift_json_protocol_read_syntax_char (ThriftJsonProtocol *protocol, 
                                       guint8 ch, GError **error)
{
  guint8 ch2 = THRIFT_LOOK_AHEAD_READER_GET_CLASS (protocol->reader)->read (protocol->reader, error);
  if (ch2 != ch) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                 "Expected erorr char");
    return -1;
  }
  return 1;
}

static guint8
thrift_json_protocol_hex_val (guint8 ch, GError **error)
{
  if ((ch >= '0') && (ch <= '9')) {
    return ch - '0';
  } else if ((ch >= 'a') && (ch <= 'f')) {
    return ch - 'a' + 10;
  } else {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_NEGATIVE_SIZE,
                 "Expected hex val ([0-9a-f])");
    return 0xff;
  }
}

static guint8
thrift_json_protocol_hex_char (guint8 val) 
{
  val &= 0x0F;
  if (val < 10) {
    return val + '0';
  } else {
    return val - 10 + 'a';
  }
}

static gboolean
thrift_json_protocol_is_json_numeric (guint8 ch)
{
  switch (ch) {
  case '+':
  case '-':
  case '.':
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
  case 'E':
  case 'e':
    return TRUE;
  }
  return FALSE;
}

static gboolean
thrift_json_protocol_is_high_surrogate (guint16 val)
{
 return ((val >= 0xD800) && (val <= 0xDBFF));
}

static gboolean
thrift_json_protocol_is_low_surrogate (guint16 val)
{
  return ((val >= 0xDC00) && (val <= 0xDFFF));
}

void thrift_json_protocol_destroy_context (gpointer data)
{
  if (data != NULL) {
    if (THRIFT_IS_JSON_PAIR_CONTEXT (data)) {
      ThriftJsonPairContext *tjc = THRIFT_JSON_PAIR_CONTEXT (data);
      g_object_unref (tjc);
    } else if (THRIFT_IS_JSON_LIST_CONTEXT (data)) {
      ThriftJsonListContext *tjc = THRIFT_JSON_LIST_CONTEXT (data);
      g_object_unref (tjc);
    }
  }
}

void 
thrift_json_protocol_push_context (ThriftJsonProtocol *protocol, ThriftJsonContext *context)
{
  protocol->context_list = g_list_append (protocol->context_list, context);
  protocol->context = context;
}

void
thrift_json_protocol_pop_context (ThriftJsonProtocol *protocol)
{
  GList *first = g_list_first (protocol->context_list);
  if (first) {
    if (first->data) {
      protocol->context = THRIFT_JSON_CONTEXT (first->data);
      protocol->context_list = g_list_delete_link (protocol->context_list, first);
    } else {
      protocol->context = NULL;
    }
  }
}

gint32
thrift_json_protocol_escape_char (ThriftJsonProtocol *protocol, const guint8 ch, GError **error)
{
  ThriftProtocol *p = THRIFT_PROTOCOL (protocol);
  /* length of KJSONEscapePrefix is 4 */
  thrift_transport_write (p->transport, (const gpointer)KJSONEscapePrefix, 4, error);
  guint8 outCh = thrift_json_protocol_hex_char (ch >> 4);
  thrift_transport_write (p->transport, &outCh, 1, error);
  outCh = thrift_json_protocol_hex_char (ch);
  thrift_transport_write (p->transport, &outCh, 1, error);
  return 6;
}

gint32 
thrift_json_protocol_write_json_char (ThriftJsonProtocol *protocol, const guint8 *ch, GError **error)
{
  ThriftProtocol *p = THRIFT_PROTOCOL (protocol);
  if (*ch >= 0x30) {
    if (*ch == KJSONBackslash) {
      thrift_transport_write (p->transport, (const gpointer)&KJSONBackslash, 1, error);
      thrift_transport_write (p->transport, (const gpointer)&KJSONBackslash, 1, error);
      return 2;
    } else {
      thrift_transport_write (p->transport, (const gpointer)ch, 1, error);
      return 1;
    }
  } else {
    guint8 outCh = KJSONCharTable[*ch];
    if (outCh == 1) {
      thrift_transport_write (p->transport, (const gpointer)ch, 1, error);
      return 2;
    } else if (outCh > 1) {
      thrift_transport_write (p->transport, (const gpointer)&KJSONBackslash, 1, error);
      thrift_transport_write (p->transport, (const gpointer)&outCh, 1, error);
    } else {
      return thrift_json_protocol_escape_char (protocol, *ch, error);
    }
  }
  return 0;
}

gint32
thrift_json_protocol_write_json_string (ThriftJsonProtocol *protocol, const gchar* str,  GError **error)
{
  gint32 ret;
  gint32 xfer = 0;

  ThriftProtocol *p = THRIFT_PROTOCOL (protocol);

  if (str == NULL) {
    g_set_error (error,
		 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                 "str is NULL");
    return -1;
  }

  if (protocol->context != NULL) {
    if ((ret = THRIFT_JSON_CONTEXT_GET_CLASS(protocol->context)->write (protocol->context, p->transport, error)) < 0) {
      return -1;
    }
    xfer += ret;
  }

  xfer += 2; /* For quotes */

  thrift_transport_write (p->transport, (const gpointer)&KJSONStringDelimiter, 1, error);

  guint32 i = 0;
  guint32 len = strlen (str);

  while (len && (str[i] != '\0')) {
    if ((ret = thrift_json_protocol_write_json_char (protocol, (const guint8*)&str[i], error)) < 0) {
      return -1;
    }
    xfer += ret;
    len--;
    i++;
  }
  thrift_transport_write (p->transport, (const gpointer)&KJSONStringDelimiter, 1, error);
  return xfer;
}

gint32
thrift_json_protocol_write_json_base64 (ThriftJsonProtocol *protocol, gchar *str, guint32 len, GError **error)
{
  gint32 ret;
  gint32 xfer = 0;
  guint8 b[4];

  ThriftProtocol *p = THRIFT_PROTOCOL (protocol);

  if (protocol->context != NULL) {
    if ((ret = THRIFT_JSON_CONTEXT_GET_CLASS(protocol->context)->write (protocol->context, p->transport, error)) < 0) {
      return -1;
    }
    xfer += ret;
  }
  xfer += 2;

  thrift_transport_write (p->transport, (const gpointer)&KJSONStringDelimiter, 1, error);

  if (len > INT_MAX) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
		 THRIFT_PROTOCOL_ERROR_NEGATIVE_SIZE,
		 "size limit %d", len);
   return -1;
  }

  while (len >= 3) {
    /* Encode 3 bytes at a time */
    base64_encode ((guint8*)str, 3, b);
    thrift_transport_write (p->transport, b, 4, error);
    xfer += 4;
    str += 3;
    len -= 3; 
  }
  if (len) {
    base64_encode ((guint8*)str, len, b);
    thrift_transport_write (p->transport, (const gpointer)b, len + 1, error);
    xfer += len + 1;
  }
  thrift_transport_write (p->transport, (const gpointer)&KJSONStringDelimiter, 1, error);
  return xfer;
}

gint32
thrift_json_protocol_write_json_integer (ThriftJsonProtocol *protocol, glong num, GError **error)
{
  gboolean escapeNum = FALSE;
  gint32 xfer = 0;
  gint32 ret;

  ThriftProtocol *p = THRIFT_PROTOCOL (protocol);

  if (protocol->context != NULL) {
    if ((ret = THRIFT_JSON_CONTEXT_GET_CLASS (protocol->context)->write(protocol->context, p->transport, error)) < 0) {
      return -1;
    }
    xfer += ret;
  }

  gchar *str = g_strdup_printf ("%ld", num);
  if (str == NULL) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                 "str malloc fail.");
    return -1;
  }

  if (protocol->context != NULL) {
    escapeNum = THRIFT_JSON_CONTEXT_GET_CLASS (protocol->context)->escape_num (protocol->context);
  } else {
    escapeNum = FALSE;
  }
  if (escapeNum) {
    thrift_transport_write (p->transport, (const gpointer)&KJSONStringDelimiter, 1, error);
    xfer += 1;
  }
  if (strlen (str) > INT_MAX) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
		 THRIFT_PROTOCOL_ERROR_NEGATIVE_SIZE,
		 "size limit %ld", strlen (str));
    g_free (str);
    return -1;
  }
  thrift_transport_write (p->transport, (const gpointer)str, strlen (str), error);
  xfer += strlen (str);    
  if (escapeNum) {
    thrift_transport_write (p->transport, (const gpointer)&KJSONStringDelimiter, 1, error);
    xfer += 1;
  }
  g_free (str);
  return xfer;
}

gint32
thrift_json_protocol_write_json_double (ThriftJsonProtocol *protocol, double num, GError **error)
{
  gboolean escapeNum = FALSE;
  gint32 result = 0;
  gchar var[100] = {0};

  ThriftProtocol *p = THRIFT_PROTOCOL (protocol);

  if (protocol->context != NULL) {
    result = THRIFT_JSON_CONTEXT_GET_CLASS (protocol->context)->write (protocol->context, p->transport, error);
  }

  gboolean special = FALSE;
  switch (fpclassify (num)) {
    case FP_INFINITE:
      if (signbit(num)) {
        g_strlcpy (var, KThriftNegativeInfinity, 10);
      } else {
        g_strlcpy (var, KThriftInfinity, 9);
      }
      special = TRUE;
      break;
    case FP_NAN:
      g_strlcpy (var, KThriftNan, 4);
      special = TRUE;
      break;
    default:
      g_ascii_formatd (var, G_ASCII_DTOSTR_BUF_SIZE, "%g", num);
      break;
  }
  if (protocol->context != NULL) {
    escapeNum = special || THRIFT_JSON_CONTEXT_GET_CLASS (protocol->context)->escape_num (protocol->context);
  } else {
    escapeNum = special;
  }
  if (escapeNum) {
    thrift_transport_write (p->transport, (const gpointer)&KJSONStringDelimiter, 1, error);
    result += 1;
  }
  if (strlen (var) > INT_MAX) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_NEGATIVE_SIZE,
                 "size limit %ld", strlen (var));
    return -1;
  }
  thrift_transport_write (p->transport, (const gpointer)var, strlen (var), error);
  result += strlen (var);
  if (escapeNum) {
    thrift_transport_write (p->transport, (const gpointer)&KJSONStringDelimiter, 1, error);
    result += 1;
  }
  return result;
}

gint32
thrift_json_protocol_write_json_object_start (ThriftJsonProtocol *protocol, GError **error)
{
  gint32 result = 0;

  ThriftProtocol *p = THRIFT_PROTOCOL (protocol);

  if (protocol->context != NULL) {
    result = THRIFT_JSON_CONTEXT_GET_CLASS (protocol->context)->write (protocol->context, p->transport, error);
  }

  thrift_transport_write (p->transport, (const gpointer)&KJSONObjectStart, 1, error);
  ThriftJsonPairContext *new_node = g_object_new (THRIFT_TYPE_JSON_PAIR_CONTEXT, NULL);
  thrift_json_protocol_push_context (protocol, THRIFT_JSON_CONTEXT (new_node));
  return result + 1;
}

gint32
thrift_json_protocol_write_json_object_end (ThriftJsonProtocol *protocol, GError **error)
{
  ThriftProtocol *p = THRIFT_PROTOCOL (protocol);
  thrift_json_protocol_pop_context (protocol);
  thrift_transport_write (p->transport, (const gpointer)&KJSONObjectEnd, 1, error);
  return 1;
}

gint32
thrift_json_protocol_write_json_array_start (ThriftJsonProtocol *protocol, GError **error)
{
  gint32 result = 0;

  ThriftProtocol *p = THRIFT_PROTOCOL (protocol);

  if (protocol->context != NULL) {
    result = THRIFT_JSON_CONTEXT_GET_CLASS (protocol->context)->write (protocol->context, p->transport, error);
  }

  thrift_transport_write (p->transport, (const gpointer)&KJSONArrayStart, 1, error);
  ThriftJsonPairContext *new_node = g_object_new (THRIFT_TYPE_JSON_LIST_CONTEXT, NULL);
  thrift_json_protocol_push_context (protocol, THRIFT_JSON_CONTEXT (new_node));
  return result + 1;
}

gint32
thrift_json_protocol_write_json_array_end (ThriftJsonProtocol *protocol, GError **error)
{
  ThriftProtocol *p = THRIFT_PROTOCOL (protocol);
  thrift_json_protocol_pop_context (protocol);
  thrift_transport_write (p->transport, (const gpointer)&KJSONArrayEnd, 1, error);
  return 1;
}

gint32
thrift_json_protocol_write_message_begin (ThriftProtocol *protocol,
                                          const gchar *name, const ThriftMessageType message_type,
                                          const gint32 seqid, GError **error)
{
  gint32 ret;
  gint32 xfer = 0;

  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  if ((ret = thrift_json_protocol_write_json_array_start (p, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_write_json_integer (p, KThriftVersion1, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_write_json_string (p, name, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_write_json_integer (p, message_type, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_write_json_integer (p, seqid, error)) < 0) {
    return -1;
  }
  xfer += ret;

  return xfer;
}

gint32
thrift_json_protocol_write_message_end (ThriftProtocol *protocol, GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_write_json_array_end (p, error);
}

gint32
thrift_json_protocol_write_struct_begin (ThriftProtocol *protocol, 
                                         const gchar *name,
                                         GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  THRIFT_UNUSED_VAR (name);
  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_write_json_object_start (p, error);
}

gint32
thrift_json_protocol_write_struct_end (ThriftProtocol *protocol, GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_write_json_object_end (p, error);
}

gint32
thrift_json_protocol_write_field_begin (ThriftProtocol *protocol,
                                        const gchar *name,
                                        const ThriftType field_type,
                                        const gint16 field_id,
                                        GError **error)
{
  gint32 ret;
  gint32 xfer = 0;

  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  THRIFT_UNUSED_VAR (name);
  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  if ((ret = thrift_json_protocol_write_json_integer (p, field_id, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_write_json_object_start (p, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_write_json_string (p, 
                                                    thrift_json_protocol_get_typename_for_typeid (p, field_type, error), 
                                                    error)) < 0) {
    return -1;
  }
  xfer += ret;

  return xfer;
}

gint32
thrift_json_protocol_write_field_end (ThriftProtocol *protocol, GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_write_json_object_end (p, error);
}

gint32
thrift_json_protocol_write_field_stop (ThriftProtocol *protocol, GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  THRIFT_UNUSED_VAR (protocol);
  THRIFT_UNUSED_VAR (*error);
  return 0;
}
	
gint32
thrift_json_protocol_write_map_begin (ThriftProtocol *protocol,
                                      const ThriftType keyType,
                                      const ThriftType valType,
                                      const guint32 size,
                                      GError **error)
{
  gint32 ret;
  gint32 xfer = 0;  

  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  if ((ret = thrift_json_protocol_write_json_array_start (p, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_write_json_string (p,
                                                    thrift_json_protocol_get_typename_for_typeid (p, keyType, error),
						    error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_write_json_string (p,
                                                    thrift_json_protocol_get_typename_for_typeid (p, valType, error),
						    error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_write_json_integer (p, (gint64)size, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_write_json_object_start (p, error)) < 0) {
    return -1;
  }
  xfer += ret;

  return xfer;
}

gint32
thrift_json_protocol_write_map_end (ThriftProtocol *protocol, GError **error)
{
  gint32 ret;
  gint32 xfer = 0; 

  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  if ((ret = thrift_json_protocol_write_json_object_end (p, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_write_json_array_end (p, error)) < 0) {
    return -1;
  }
  xfer += ret;

  return xfer;
}

gint32
thrift_json_protocol_write_list_begin (ThriftProtocol *protocol,
                                       const ThriftType elemType,
                                       const guint32 size,
                                       GError **error)
{
  gint32 ret;
  gint32 xfer = 0;
 
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  if ((ret = thrift_json_protocol_write_json_array_start (p, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_write_json_string (p,
                                                    thrift_json_protocol_get_typename_for_typeid (p, elemType, error),
                                                    error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_write_json_integer (p, (gint64)size, error)) < 0) {
    return -1;
  }
  xfer += ret;

  return xfer;
}

gint32
thrift_json_protocol_write_list_end (ThriftProtocol *protocol, GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_write_json_array_end (p, error);
}

gint32
thrift_json_protocol_write_set_begin (ThriftProtocol *protocol,
                                      const ThriftType elemType,
                                      const guint32 size,
                                      GError **error)
{
  return thrift_json_protocol_write_list_begin (protocol, elemType, size, error);
}

gint32
thrift_json_protocol_write_set_end (ThriftProtocol *protocol, GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_write_json_array_end (p, error);
}

gint32
thrift_json_protocol_write_bool (ThriftProtocol *protocol,
                                 const gboolean value, GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_write_json_integer (p, value, error);
}

gint32
thrift_json_protocol_write_byte (ThriftProtocol *protocol,
                                 const gint8 byte, GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_write_json_integer (p, byte, error);
}

gint32
thrift_json_protocol_write_i16 (ThriftProtocol *protocol,
                                const gint16 value, GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_write_json_integer (p, value, error);
}

gint32
thrift_json_protocol_write_i32 (ThriftProtocol *protocol,
                                const gint32 value, GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_write_json_integer (p, value, error);
}
                              
gint32
thrift_json_protocol_write_i64 (ThriftProtocol *protocol,
                                const gint64 value, GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_write_json_integer (p, value, error);
}

gint32
thrift_json_protocol_write_double (ThriftProtocol *protocol,
                                   const gdouble value, GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);
	
  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_write_json_double (p, value, error);
}

gint32
thrift_json_protocol_write_string (ThriftProtocol *protocol,
                                   const gchar *str, GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_write_json_string (p, str, error);
}

gint32
thrift_json_protocol_write_binary (ThriftProtocol *protocol,
                                   const gpointer buf,
                                   const guint32 len, GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_write_json_base64 (p, buf, len, error);
}

/*!
 *  Reading functions
 */

gint32
thrift_json_protocol_read_json_syntax_char (ThriftJsonProtocol *protocol,
                                            guint8 ch, GError **error)
{
  return thrift_json_protocol_read_syntax_char (protocol, ch, error);
}

gint32
thrift_json_protocol_read_json_escape_char (ThriftJsonProtocol *protocol,
                                            guint16 *out, GError **error)
{
  guint8 b[4];
  b[0] = THRIFT_LOOK_AHEAD_READER_GET_CLASS (protocol->reader)->read (protocol->reader, error);
  b[1] = THRIFT_LOOK_AHEAD_READER_GET_CLASS (protocol->reader)->read (protocol->reader, error);
  b[2] = THRIFT_LOOK_AHEAD_READER_GET_CLASS (protocol->reader)->read (protocol->reader, error);
  b[3] = THRIFT_LOOK_AHEAD_READER_GET_CLASS (protocol->reader)->read (protocol->reader, error);

  *out = (thrift_json_protocol_hex_val(b[0], error) << 12)
     + (thrift_json_protocol_hex_val (b[1], error) << 8) + (thrift_json_protocol_hex_val (b[2], error) << 4) + thrift_json_protocol_hex_val (b[3], error);

  return 4;
}

gsize
thrift_json_protocol_char_find (const gchar *str, guint32 len, guint8 ch)
{
  gsize pos = 0;
  while (str) {
    if (ch == *str) {
      break;
    }
    len--;
    if (len == 0) {
      pos = 0xFF;
      break;
    }
    pos++;
    str++;
  }
  return pos;
}

gint32
thrift_json_protocol_read_json_string (ThriftJsonProtocol *protocol,
                                       gchar **str,
                                       gboolean skipContext, GError **error)
{
  gint32 xfer = 0;

  gint32 ret;
  guint8 ch;
  gsize pos;
  gchar *tmp = NULL;
  guint16 cp;

  if (protocol->context != NULL) {
    xfer = (skipContext ? 0 : THRIFT_JSON_CONTEXT_GET_CLASS (protocol->context)->read (protocol->context, protocol->reader, error));
  }

  GByteArray *condeunits = g_byte_array_new();
  GByteArray *buf = g_byte_array_new();

  if ((ret = thrift_json_protocol_read_json_syntax_char (protocol, KJSONStringDelimiter, error)) < 0) {

    g_byte_array_unref (buf);
    g_byte_array_unref (condeunits);
    return -1;
  }
  xfer += ret;
  while (TRUE) {
    ch = THRIFT_LOOK_AHEAD_READER_GET_CLASS (protocol->reader)->read (protocol->reader, error);
    ++xfer;
    if (ch == KJSONStringDelimiter) {
      break;
    }
    if (ch == KJSONBackslash) {
      ch = THRIFT_LOOK_AHEAD_READER_GET_CLASS (protocol->reader)->read (protocol->reader, error);
      if (ch == KJSONEscapeChar) {
        if ((ret = thrift_json_protocol_read_json_escape_char (protocol, &cp, error)) < 0) {
          g_byte_array_unref (buf);
          g_byte_array_unref (condeunits);
          return -1;
        }
        xfer += ret;
      
        if (thrift_json_protocol_is_high_surrogate (cp)) {
          condeunits = g_byte_array_append (condeunits, (const guint8*)&cp, 1);
        } else {
          if (thrift_json_protocol_is_low_surrogate (cp) && condeunits->len) {
            g_set_error (error,
                         THRIFT_PROTOCOL_ERROR,
                         THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                         "Missing UTF-16 high surrogate pair.");
            g_byte_array_unref (buf);
            g_byte_array_unref (condeunits);
	    return -1;
	  }
          condeunits = g_byte_array_append (condeunits, (const guint8*)&cp, 1);
          tmp = g_new0 (gchar, condeunits->len);
          memcpy (tmp, condeunits->data, condeunits->len);
          g_byte_array_remove_range (condeunits, 0, condeunits->len);
          buf = g_byte_array_append (buf, (const guint8*)tmp, condeunits->len);
        }
        continue;
      } else {
        pos = thrift_json_protocol_char_find (KEscapeChars, strlen (KEscapeChars), ch);
        if (pos == 0xFF) {
            g_set_error (error,
                         THRIFT_PROTOCOL_ERROR,
                         THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                         "Expected control char error.");
            g_byte_array_unref (buf);
            g_byte_array_unref (condeunits);
	    return -1;
        }
        ch = kEscapeCharVals[pos];
      }
    }
    if (condeunits->len != 0) {
        g_set_error (error,
                     THRIFT_PROTOCOL_ERROR,
                     THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                     "Missing UTF-16 high surrogate pair.");
        g_byte_array_unref (buf);
        g_byte_array_unref (condeunits);
	return -1;
    }
    buf = g_byte_array_append (buf, &ch, 1);
  }

  if (condeunits->len != 0) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                 "Missing UTF-16 high surrogate pair.");
    g_byte_array_unref (buf);
    g_byte_array_unref (condeunits);
    return -1;
  }

  *str = g_new0 (gchar, buf->len+1);
  memcpy (*str, buf->data, buf->len);
  g_byte_array_unref (buf);
  g_byte_array_unref (condeunits);
  return xfer;
}

gint32
thrift_json_protocol_read_json_base64 (ThriftJsonProtocol *protocol,
                                       gchar **str,
                                       guint32 *len, GError **error)
{
  gchar *tmp = NULL;

  guint32 i;
  gchar *b = NULL;
  gint32 xfer = 0;
  gint32 ret;
  guint32 len_t;

  GByteArray *buf = g_byte_array_new();

  if ((ret = thrift_json_protocol_read_json_string (protocol, &tmp, FALSE, error)) < 0) {
    g_byte_array_unref (buf);
    return -1;
  }
  xfer += ret;

  len_t = strlen ((char*)tmp);
  b = tmp;
  if (len_t > INT_MAX) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                 "SIZE_LIMIT.");
    g_byte_array_unref (buf);
    g_free (tmp);
    return -1;
  }

  if (len_t >= 2) {
    guint32 bound = len_t - 2;
    for (i = len_t - 1; i >= bound && b[i] == '='; i--) {
      len_t--;
    }
  }

  while (len_t >= 4) {
    base64_decode ((guint8 *)b, 4);
    buf = g_byte_array_append (buf, (const guint8*)b, 3);
    b += 4;
    len_t -= 4;
  }

  if (len_t > 1) {
    base64_decode ((guint8 *)b, len_t);
    buf = g_byte_array_append (buf, (const guint8*)b, len_t-1);
  }

  *len = buf->len;
  *str = g_new0 (gchar, buf->len+1);
  memcpy (*str, buf->data, buf->len);
  g_byte_array_unref (buf);
  g_free (tmp);
  return xfer;
}

gint32
thrift_json_protocol_read_json_numeric_chars (ThriftJsonProtocol *protocol,
                                              gchar** str, GError **error)
{
  gint32 result = 0;
  guint8 ch = 0;

  GByteArray *buf = g_byte_array_new();

  while (TRUE) {
    ch = THRIFT_LOOK_AHEAD_READER_GET_CLASS (protocol->reader)->peek (protocol->reader, error); 
    if (!thrift_json_protocol_is_json_numeric (ch)) {
      break;
    }
    ch = THRIFT_LOOK_AHEAD_READER_GET_CLASS (protocol->reader)->read (protocol->reader, error);
    buf = g_byte_array_append (buf, &ch, 1);
    ++result;
  }
  if (buf->len) {
    *str = g_new0 (gchar, buf->len+1);
    memcpy (*str, buf->data, buf->len);
  }
  g_byte_array_unref (buf);
  return result;
}

gint32
thrift_json_protocol_read_json_integer (ThriftJsonProtocol *protocol,
                                        gint64 *num, GError **error)
{
  gint32 xfer = 0;
  gint32 ret;
  gchar *str = NULL;
  
  if (protocol->context != NULL) {
    if ((ret = THRIFT_JSON_CONTEXT_GET_CLASS (protocol->context)->read (protocol->context, protocol->reader, error)) < 0) {
      return -1;
    }
    xfer += ret;
  }

  if (protocol->context != NULL) {
    if (THRIFT_JSON_CONTEXT_GET_CLASS (protocol->context)->escape_num (protocol->context)) {
      if ((ret = thrift_json_protocol_read_json_syntax_char (protocol, KJSONStringDelimiter, error)) < 0) {
        return -1;
      }
      xfer += ret;
    }
  }

  if ((ret = thrift_json_protocol_read_json_numeric_chars (protocol, &str, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if (str == NULL) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                 "It not a number");
    return -1;
  }

  if (thrift_json_protocol_char_find (str, strlen (str), '.') != 0xFF) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                 "Expected numeric value;");
    g_free (str);
    return -1;
  }

  *num = g_ascii_strtoll (str, NULL, 0);

  if (protocol->context != NULL) {
    if (THRIFT_JSON_CONTEXT_GET_CLASS (protocol->context)->escape_num (protocol->context)) {
      if ((ret = thrift_json_protocol_read_json_syntax_char (protocol, KJSONStringDelimiter, error)) < 0) {
        return -1;
      }
      xfer += ret;
    }
  }

  return xfer;
}

gint32
thrift_json_protocol_read_json_double (ThriftJsonProtocol *protocol,
                                       gdouble *num, GError **error)
{
  gint32 xfer = 0;
  gint32 ret;
  gchar *str = NULL;
  gint8 ch;

  if (protocol->context != NULL) {
    if ((ret = THRIFT_JSON_CONTEXT_GET_CLASS (protocol->context)->read (protocol->context, protocol->reader, error)) < 0) {
      g_free (str);
      return -1;
    }
    xfer += ret;
  }

  if (protocol->reader->hasData) {
    protocol->reader->hasData = FALSE;
  }
  if ((ch=THRIFT_LOOK_AHEAD_READER_GET_CLASS (protocol->reader)->peek (protocol->reader,
                                                                             error)) == KJSONStringDelimiter) {
    if ((ret = thrift_json_protocol_read_json_string (protocol, &str, TRUE, error)) < 0) {
      return -1;
    }
    xfer += ret;
    if (g_strcmp0 (str, KThriftNan) == 0) {
      *num = HUGE_VAL / HUGE_VAL;  /* set to not a number */
    } else if (g_strcmp0 (str, KThriftInfinity) == 0) {
      *num = HUGE_VAL;
    } else if (g_strcmp0 (str, KThriftNegativeInfinity) == 0) {
      *num = -HUGE_VAL;
    } else {
      if (protocol->context != NULL) {
        if (!THRIFT_JSON_CONTEXT_GET_CLASS (protocol->context)->escape_num (protocol->context)) {
          g_set_error (error,
                       THRIFT_PROTOCOL_ERROR,
                       THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                       "Numeric data unexpectedly quoted");
          g_free (str);
          return -1;
        }
      }

      *num = g_strtod (str, NULL);
      if (*num == 0) {
        g_set_error (error,
                     THRIFT_PROTOCOL_ERROR,
                     THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                     "Num is error");
        g_free (str);
	return -1;
      }

    }
  } else {
    if (protocol->context != NULL) {
      if (THRIFT_JSON_CONTEXT_GET_CLASS (protocol->context)->escape_num (protocol->context)) {
        thrift_json_protocol_read_json_syntax_char (protocol, KJSONStringDelimiter, error);
      }
    }

    if ((ret = thrift_json_protocol_read_json_numeric_chars (protocol, &str, error)) < 0) {
      g_free (str);
      return -1;
    }
    xfer += ret;

    *num = g_strtod (str, NULL);
    if (*num == 0) {
      g_set_error (error,
                   THRIFT_PROTOCOL_ERROR,
                   THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                   "Num is error");
      g_free (str);
      return -1;
    }
  }

  g_free (str);
  return xfer;
}

gint32
thrift_json_protocol_read_json_object_start (ThriftJsonProtocol *protocol, GError **error)
{
  gint32 xfer = 0;
  gint32 ret;

  if (protocol->context != NULL) {
    if ((ret = THRIFT_JSON_CONTEXT_GET_CLASS (protocol->context)->read (protocol->context, protocol->reader, error)) < 0) {
      return -1;
    }
    xfer += ret;
  }

  if ((ret = thrift_json_protocol_read_json_syntax_char (protocol, KJSONObjectStart, error)) < 0) {
    return -1;
  }
  xfer += ret;

  thrift_json_protocol_pop_context (protocol);

  return xfer;
}

gint32
thrift_json_protocol_read_json_object_end (ThriftJsonProtocol *protocol, GError **error)
{
  gint32 result = thrift_json_protocol_read_json_syntax_char (protocol, KJSONObjectEnd, error);
  thrift_json_protocol_pop_context (protocol);
  return result;
}

gint32
thrift_json_protocol_read_json_array_start (ThriftJsonProtocol *protocol, GError **error)
{
  gint32 xfer = 0;
  gint32 ret;

  if (protocol->context != NULL) {
    if ((ret = THRIFT_JSON_CONTEXT_GET_CLASS (protocol->context)->read (protocol->context, protocol->reader, error)) < 0) {
      return -1;
    }
    xfer += ret;
  }

  if ((ret = thrift_json_protocol_read_json_syntax_char (protocol, KJSONArrayStart, error)) < 0) {
    return -1;
  }
  xfer += ret;

  ThriftJsonListContext *context = g_object_new (THRIFT_TYPE_JSON_LIST_CONTEXT, NULL);
  thrift_json_protocol_push_context (protocol, THRIFT_JSON_CONTEXT (context));

  return xfer;
}

gint32
thrift_json_protocol_read_json_array_end (ThriftJsonProtocol *protocol, GError **error)
{
  gint32 result = 0;

  if ((result = thrift_json_protocol_read_json_syntax_char (protocol, KJSONArrayEnd, error)) < 0) {
    return -1;
  }

  thrift_json_protocol_pop_context (protocol);
  return result;
}

gint32
thrift_json_protocol_read_message_begin (ThriftProtocol *protocol,
                                         gchar **name, 
                                         ThriftMessageType *message_type,
                                         gint32 *seqid, GError **error)
{
  gint32 xfer = 0;
  gint32 ret;
  gint64 tmpVal = 0;

   g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  if ((ret = thrift_json_protocol_read_json_array_start(p, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_read_json_integer (p, &tmpVal, error)) < 0) {
    thrift_json_protocol_pop_context (p);
    if (p->reader->hasData) {
      p->reader->hasData = FALSE;
    }
    return -1;
  }
  xfer += ret;

  if (tmpVal != KThriftVersion1) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_BAD_VERSION,
                 "Message contatined bad version.");
    thrift_json_protocol_pop_context (p);
    if (p->reader->hasData) {
      p->reader->hasData = FALSE;
    }
    return -1;
  }

  if ((ret = thrift_json_protocol_read_json_string (p, name, FALSE, error)) < 0) {
    thrift_json_protocol_pop_context (p);
    if (p->reader->hasData) {
      p->reader->hasData = FALSE;
    }
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_read_json_integer (p, &tmpVal, error)) < 0) {
    thrift_json_protocol_pop_context (p);
    if (p->reader->hasData) {
      p->reader->hasData = FALSE;
    }
    return -1;
  }
  xfer += ret;

  *message_type = (ThriftMessageType)tmpVal;

  if ((ret = thrift_json_protocol_read_json_integer (p, &tmpVal, error)) < 0) {
    thrift_json_protocol_pop_context (p);
    if (p->reader->hasData) {
      p->reader->hasData = FALSE;
    }
    return -1;
  }
  xfer += ret;

  if (tmpVal > INT_MAX || tmpVal < INT_MIN) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                 "sequence id is not int32.");
    thrift_json_protocol_pop_context (p);
    if (p->reader->hasData) {
      p->reader->hasData = FALSE;
    }
    return -1;
  }
  *seqid = (gint32)tmpVal;

  return xfer;
}	

gint32
thrift_json_protocol_read_message_end (ThriftProtocol *protocol, GError **error)
{
  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  if (p->reader->hasData) {
    p->reader->hasData = FALSE;
  }
  return thrift_json_protocol_read_json_array_end (p, error);
}

gint32
thrift_json_protocol_read_struct_begin (ThriftProtocol *protocol,
                                        gchar **name, GError **error)
{
  THRIFT_UNUSED_VAR (*name);
  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_read_json_object_start(p, error);
}

gint32
thrift_json_protocol_read_struct_end (ThriftProtocol *protocol, GError **error)
{
  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_read_json_object_end (p, error);
}

gint32
thrift_json_protocol_read_field_begin (ThriftProtocol *protocol,
                                       gchar **name,
                                       ThriftType *field_type,
                                       gint16 *field_id,
                                       GError **error)
{
  THRIFT_UNUSED_VAR (*name);

  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  gint32 xfer = 0;
  gint32 ret;
  gint64 tmpVal = 0;
  gchar *tmpStr = NULL;

  gint8 ch = THRIFT_LOOK_AHEAD_READER_GET_CLASS (p->reader)->peek (p->reader, error);

  if (ch == KJSONObjectEnd) {
    *field_type = T_STOP;
  } else {
    if ((ret = thrift_json_protocol_read_json_integer (p, &tmpVal, error)) < 0) {
      return -1;
    }
    xfer += ret;

    if (tmpVal > INT_MAX) {
      g_set_error (error,
                   THRIFT_PROTOCOL_ERROR,
                   THRIFT_PROTOCOL_ERROR_SIZE_LIMIT,
                   "tmpVal limit size.");
      return -1;
    }
    *field_id = (gint16)tmpVal;

    if ((ret = thrift_json_protocol_read_json_object_start (p, error)) < 0) {
      return -1;
    }
    xfer += ret;

    if ((ret = thrift_json_protocol_read_json_string (p, &tmpStr, FALSE, error)) < 0) {
      return -1;
    }
    xfer += ret;

    *field_type = thrift_json_protocol_get_typeid_for_typename(p, tmpStr, error);
    if (*field_type == T_STOP) {
      g_free (tmpStr);
      return -1;
    }
  }
  
  g_free (tmpStr);
  return xfer; 
}

gint32
thrift_json_protocol_read_field_end (ThriftProtocol *protocol, 
                                     GError **error)
{
  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_read_json_object_end (p, error);
}

gint32
thrift_json_protocol_read_map_begin (ThriftProtocol *protocol,
                                     ThriftType *key_type,
                                     ThriftType *value_type,
                                     guint32 *size,
                                     GError **error)
{
  gint64 sizei = 0;
  gint32 ret;
  gint32 xfer = 0;

  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);
  ThriftTransportClass *ttc = THRIFT_TRANSPORT_GET_CLASS (protocol->transport);

  gchar *tmpStr = NULL;

  if ((ret = thrift_json_protocol_read_json_array_start (p, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_read_json_string (p, &tmpStr, FALSE, error)) < 0) {
    return -1;
  }
  xfer += ret;

  *key_type = thrift_json_protocol_get_typeid_for_typename (p, tmpStr, error);
  if (*key_type == T_STOP) {
    g_free (tmpStr);
    return -1;
  }

  g_free (tmpStr);
  tmpStr = NULL;
  if ((ret = thrift_json_protocol_read_json_string (p, &tmpStr, FALSE, error)) < 0) {
    return -1;
  }
  xfer += ret;

  *value_type = thrift_json_protocol_get_typeid_for_typename (p, tmpStr, error);
  if (*value_type == T_STOP) {
    g_free (tmpStr);
    return -1;
  }

  if ((ret = thrift_json_protocol_read_json_integer (p, &sizei, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if (sizei > INT_MAX) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_SIZE_LIMIT,
                 "size limit.");
    g_free (tmpStr);
    return -1;
  }
  *size = (gint32)sizei;
  if (!ttc->checkReadBytesAvailable (THRIFT_TRANSPORT(protocol->transport),
                                    sizei * thrift_json_protocol_get_min_serialized_size (protocol, *key_type, error) +
                                    sizei * thrift_json_protocol_get_min_serialized_size (protocol, *value_type, error),
                                    error)) {
    g_free (tmpStr);
    return -1;
  }

  if ((ret = thrift_json_protocol_read_json_object_start (p, error)) < 0) {
    return -1;
  }
  g_free (tmpStr);
  xfer += ret;
  return xfer;
}

gint32
thrift_json_protocol_read_map_end (ThriftProtocol *protocol,
                                   GError **error)
{
  gint32 xfer = 0;
  gint32 ret; 

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  if ((ret = thrift_json_protocol_read_json_object_end (p, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_read_json_array_end (p, error)) < 0) {
    return -1;
  }
  xfer += ret;

  return xfer;
}

gint32
thrift_json_protocol_read_list_begin (ThriftProtocol *protocol,
                                      ThriftType *element_type,
                                      guint32 *size,
                                      GError **error)
{
  gint64 sizei = 0;
  gint32 xfer = 0;
  gint32 ret;
  gchar *tmpstr = NULL;

  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);
  ThriftTransportClass *ttc = THRIFT_TRANSPORT_GET_CLASS (protocol->transport);
 
  if ((ret = thrift_json_protocol_read_json_array_start (p, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if ((ret = thrift_json_protocol_read_json_string (p, &tmpstr, FALSE, error)) < 0) {
    return -1;
  }
  xfer += ret;

  *element_type = thrift_json_protocol_get_typeid_for_typename (p, tmpstr, error);
  if (*element_type == T_STOP) {
    return -1;
  }

  if ((ret = thrift_json_protocol_read_json_integer (p, &sizei, error)) < 0) {
    g_free (tmpstr);
    return -1;
  }
  xfer += ret;

  if (sizei > INT_MAX) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_SIZE_LIMIT,
                 "size limit.");
    g_free (tmpstr);
    return -1;
  }

  if (!ttc->checkReadBytesAvailable (THRIFT_TRANSPORT(protocol->transport),
                                     (sizei * thrift_json_protocol_get_min_serialized_size(protocol, *element_type, error)),
                                     error)) {
    g_free (tmpstr);
    return -1;
  }
  *size = (guint32)sizei;

  return xfer;
}

gint32
thrift_json_protocol_read_list_end (ThriftProtocol *protocol,
                                    GError **error)
{
  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  if (p->reader->hasData){
    p->reader->hasData = FALSE;
  }
  return thrift_json_protocol_read_json_array_end (p, error);
}

gint32
thrift_json_protocol_read_set_begin (ThriftProtocol *protocol,
                                     ThriftType *element_type,
                                     guint32 *size, GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  return thrift_protocol_read_list_begin (protocol, element_type, size, error);
}

gint32
thrift_json_protocol_read_set_end (ThriftProtocol *protocol,
                                   GError **error)
{
  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_read_json_array_end (p, error);
}

gint32
thrift_json_protocol_read_bool (ThriftProtocol *protocol, gboolean *value,
                                GError **error)
{
  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  return thrift_json_protocol_read_json_integer (p, (gint64*)value, error);
}

gint32
thrift_json_protocol_read_byte (ThriftProtocol *protocol, gint8 *value,
                                GError **error)
{
  gint32 xfer = 0;
  gint32 ret;
  gint64 tmp;

  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  if ((ret = thrift_json_protocol_read_json_integer (p, &tmp, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if (tmp > UINT8_MAX) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_SIZE_LIMIT,
		 "byte size limit.");
    return -1;
  }

  *value = (gint8)tmp;
  return xfer;
}

gint32
thrift_json_protocol_read_i16 (ThriftProtocol *protocol, gint16 *value,
                               GError **error)
{
  gint32 ret;
  gint32 xfer = 0;
  gint64 tmp;

  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  if ((ret = thrift_json_protocol_read_json_integer (p, &tmp, error)) < 0) {
    return -1;
  }
  xfer += ret;

  if (tmp > UINT16_MAX) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_SIZE_LIMIT,
		 "i16 size limit.");
    return -1;
  }
  *value = (gint16)tmp;

  return xfer;
}

gint32
thrift_json_protocol_read_i32 (ThriftProtocol *protocol, gint32 *value,
                               GError **error)
{
  gint32 ret;
  gint64 tmp;

  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  if ((ret = thrift_json_protocol_read_json_integer (p, &tmp, error)) < 0) {
    return -1;
  }

  if (tmp > UINT32_MAX) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_SIZE_LIMIT,
		 "i32 size limit.");
    return -1;
  }
  *value = (gint32)tmp;

  return ret;
}

gint32
thrift_json_protocol_read_i64 (ThriftProtocol *protocol, gint64 *value,
                               GError **error)
{
  gint32 ret;
  gint64 tmp;

  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  if ((ret = thrift_json_protocol_read_json_integer (p, &tmp, error)) < 0) {
    return -1;
  }

  if (tmp > UINT64_MAX) {
    g_set_error (error,
                 THRIFT_PROTOCOL_ERROR,
                 THRIFT_PROTOCOL_ERROR_SIZE_LIMIT,
		 "i64 size limit.");
    return -1;
  }
  *value = (gint64)tmp;

  return ret;
}

gint32
thrift_json_protocol_read_double (ThriftProtocol *protocol, 
                                  gdouble *value, GError **error)
{
  gint32 ret;
  gdouble tmp;

  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  if ((ret = thrift_json_protocol_read_json_double (p, &tmp, error)) < 0) {
    return -1;
  }
  *value = tmp;

  return ret;
}

gint32
thrift_json_protocol_read_string (ThriftProtocol *protocol,
                                  gchar **str, GError **error)
{
  gint32 ret;

  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

  if ((ret = thrift_json_protocol_read_json_string (p, str, FALSE, error)) < 0) {
    return -1;
  }

  return ret;
}

gint32
thrift_json_protocol_read_binary (ThriftProtocol *protocol,
                                  gpointer *buf, guint32 *len,
                                  GError **error)
{
  gint32 ret;

  g_return_val_if_fail (THRIFT_IS_JSON_PROTOCOL (protocol), -1);

  ThriftJsonProtocol *p = THRIFT_JSON_PROTOCOL (protocol);

 if ((ret = thrift_json_protocol_read_json_base64 (p, (gchar **)buf, len, error)) < 0) {
   *len = 0;
   return -1;
 }

 return ret;
}

gint
thrift_json_protocol_get_min_serialized_size(ThriftProtocol *protocol, ThriftType type, GError **error)
{
  THRIFT_UNUSED_VAR (protocol);

  switch (type)
  {
    case T_STOP:
         return 0;
    case T_VOID:
         return 0;
    case T_BOOL:
         return 1;
    case T_BYTE:
         return 1;
    case T_DOUBLE:
         return 1;
    case T_I16:
         return 1;
    case T_I32:
         return 1;
    case T_I64:
         return 1;
    case T_STRING:
         return 2;
    case T_STRUCT:
         return 2;
    case T_MAP:
         return 2;
    case T_SET:
         return 2;
    case T_LIST:
         return 2;
    default:
         g_set_error(error,
                     THRIFT_PROTOCOL_ERROR,
                     THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                     "unrecognized type");			    
         return -1;
  }
}

/* initializes the instance */
static void
thrift_json_protocol_init (ThriftJsonProtocol *protocol)
{
  protocol->context_list = NULL;
  protocol->context = NULL;
  protocol->context_list = g_list_append (protocol->context_list, protocol->context);
}

/* destructor */
static void
thrift_json_protocol_finalize (GObject *object)
{
  ThriftJsonProtocol *protocol = THRIFT_JSON_PROTOCOL (object);
  g_list_free_full(protocol->context_list, thrift_json_protocol_destroy_context);
}

/* property accessor */
void
thrift_json_protocol_get_property (GObject *object, guint property_id,
                                   GValue *value, GParamSpec *pspec)
{
  ThriftJsonProtocol *protocol = THRIFT_JSON_PROTOCOL (object);
  ThriftProtocol *p = THRIFT_PROTOCOL (object);

  THRIFT_UNUSED_VAR (pspec);

  switch (property_id)
  {
    case PROP_THRIFT_JSON_PROTOCOL_TRANSPORT:
     g_value_set_object (value, p->transport);
     break;
    case PROP_THRIFT_JSON_PROTOCOL_LOOK_AHEAD_READER:
     g_value_set_object (value, protocol->reader);
     break;
  }
}

/* property mutator */
void
thrift_json_protocol_set_property (GObject *object, guint property_id,
                                   const GValue *value, GParamSpec *pspec)
{
  ThriftJsonProtocol *protocol = THRIFT_JSON_PROTOCOL (object);
  ThriftProtocol *p = THRIFT_PROTOCOL (object);

  THRIFT_UNUSED_VAR (pspec);

  switch (property_id)
  {
    case PROP_THRIFT_JSON_PROTOCOL_TRANSPORT:
     p->transport = g_value_dup_object (value);
     break;
    case PROP_THRIFT_JSON_PROTOCOL_LOOK_AHEAD_READER:
     protocol->reader = g_value_dup_object (value);
     break;
  }
}

/* initialize the class */
static void
thrift_json_protocol_class_init (ThriftJsonProtocolClass *klass)
{
  ThriftProtocolClass *tpc = THRIFT_PROTOCOL_CLASS (klass);
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GParamSpec *param_spec = NULL;

  /* setup accessors and mutators */
  gobject_class->get_property = thrift_json_protocol_get_property;
  gobject_class->set_property = thrift_json_protocol_set_property;

  param_spec = g_param_spec_object ("transport", "transport (construct)",
                                    "Thrift transport",
                                    THRIFT_TYPE_TRANSPORT,
                                    G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_JSON_PROTOCOL_TRANSPORT,
                                   param_spec);

  param_spec = g_param_spec_object ("look ahead reader", "look_ahead_reader (construct)",
                                    "look ahead reader",
                                    THRIFT_TYPE_LOOK_AHEAD_READER,
                                    G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_JSON_PROTOCOL_LOOK_AHEAD_READER,
                                   param_spec);

  gobject_class->finalize = thrift_json_protocol_finalize;

  tpc->write_message_begin = thrift_json_protocol_write_message_begin;
  tpc->write_message_end = thrift_json_protocol_write_message_end;
  tpc->write_list_begin = thrift_json_protocol_write_list_begin;
  tpc->write_list_end = thrift_json_protocol_write_list_end;
  tpc->write_set_begin = thrift_json_protocol_write_set_begin;
  tpc->write_set_end = thrift_json_protocol_write_set_end;
  tpc->write_bool = thrift_json_protocol_write_bool;
  tpc->write_byte = thrift_json_protocol_write_byte;
  tpc->write_i16 = thrift_json_protocol_write_i16;
  tpc->write_i32 = thrift_json_protocol_write_i32;
  tpc->write_i64 = thrift_json_protocol_write_i64;
  tpc->write_double = thrift_json_protocol_write_double;
  tpc->write_string = thrift_json_protocol_write_string;
  tpc->write_binary = thrift_json_protocol_write_binary;
  tpc->read_message_begin = thrift_json_protocol_read_message_begin;
  tpc->read_message_end = thrift_json_protocol_read_message_end;
  tpc->read_struct_begin = thrift_json_protocol_read_struct_begin;
  tpc->read_struct_end = thrift_json_protocol_read_struct_end;
  tpc->read_field_begin = thrift_json_protocol_read_field_begin;
  tpc->read_field_end = thrift_json_protocol_read_field_end;
  tpc->read_map_begin = thrift_json_protocol_read_map_begin;
  tpc->read_map_end = thrift_json_protocol_read_map_end;
  tpc->read_list_begin = thrift_json_protocol_read_list_begin;
  tpc->read_list_end = thrift_json_protocol_read_list_end;
  tpc->read_set_begin = thrift_json_protocol_read_set_begin;
  tpc->read_set_end = thrift_json_protocol_read_set_end;
  tpc->read_bool = thrift_json_protocol_read_bool;
  tpc->read_byte = thrift_json_protocol_read_byte;
  tpc->read_i16 = thrift_json_protocol_read_i16;
  tpc->read_i32 = thrift_json_protocol_read_i32;
  tpc->read_i64 = thrift_json_protocol_read_i64;
  tpc->read_double = thrift_json_protocol_read_double;
  tpc->read_string = thrift_json_protocol_read_string;
  tpc->read_binary = thrift_json_protocol_read_binary;
  tpc->get_min_serialized_size = thrift_json_protocol_get_min_serialized_size;
}

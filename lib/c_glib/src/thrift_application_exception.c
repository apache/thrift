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

#include "thrift_application_exception.h"
#include "protocol/thrift_protocol.h"

G_DEFINE_TYPE(ThriftApplicationException, thrift_application_exception, THRIFT_TYPE_STRUCT)

gint32
thrift_application_exception_read (ThriftStruct *object,
                                   ThriftProtocol *protocol, GError **error)
{
  gint32 ret;
  gint32 xfer = 0;
  gchar *name;
  ThriftType ftype;
  gint16 fid;
  ThriftApplicationException *this = THRIFT_APPLICATION_EXCEPTION (object);

  /* read the struct begin marker */
  if ((ret = thrift_protocol_read_struct_begin (protocol, &name, error)) < 0)
  {
    if (name) g_free (name);
    return -1;
  }
  xfer += ret;
  if (name) g_free (name);

  while (1)
  {
    if ((ret = thrift_protocol_read_field_begin (protocol, &name, &ftype,
                                                 &fid, error)) < 0)
    {
      if (name) g_free (name);
      return -1;
    }
    xfer += ret;
    if (name) g_free (name);

    /* break if we get a STOP field */
    if (ftype == T_STOP)
    {
      break;
    }

    switch (fid)
    {
      case 1:
        if (ftype == T_STRING)
        {
          if ((ret = thrift_protocol_read_string (protocol, &this->message,
                                                  error)) < 0)
            return -1;
          xfer += ret;
          this->__isset_message = TRUE;
        } else {
          if ((ret = thrift_protocol_skip (protocol, ftype, error)) < 0)
            return -1;
          xfer += ret;
        }
        break;
      case 2:
        if (ftype == T_I32)
        {
          if ((ret = thrift_protocol_read_i32 (protocol, &this->type,
                                               error)) < 0)
            return -1;
          xfer += ret;
          this->__isset_type = TRUE;
        } else {
          if ((ret = thrift_protocol_skip (protocol, ftype, error)) < 0)
            return -1;
          xfer += ret;
        }
        break;
      default:
        if ((ret = thrift_protocol_skip (protocol, ftype, error)) < 0)
          return -1;
        xfer += ret;
        break;
    }
    if ((ret = thrift_protocol_read_field_end (protocol, error)) < 0)
      return -1;
    xfer += ret;
  }

  if ((ret = thrift_protocol_read_struct_end (protocol, error)) < 0)
    return -1;
  xfer += ret;

  return xfer;
}

gint32
thrift_application_exception_write (ThriftStruct *object,
                                    ThriftProtocol *protocol, GError **error)
{
  gint32 ret;
  gint32 xfer = 0;

  ThriftApplicationException *this = THRIFT_APPLICATION_EXCEPTION (object);

  if ((ret = thrift_protocol_write_struct_begin (protocol,
                                                 "TApplicationException",
                                                 error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_field_begin (protocol, "message",
                                                T_STRING, 1, error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_string (protocol, this->message, error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_field_end (protocol, error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_field_begin (protocol, "type",
                                                T_I32, 2, error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_i32 (protocol, this->type, error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_field_end (protocol, error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_field_stop (protocol, error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_struct_end (protocol, error)) < 0)
    return -1;
  xfer += ret;

  return xfer;
}


/* GError domain */
#define THRIFT_APPLICATION_EXCEPTION_ERROR_DOMAIN "thrift-application-exception-error-quark"

GQuark
thrift_application_exception_error_quark (void)
{
  return g_quark_from_static_string (THRIFT_APPLICATION_EXCEPTION_ERROR_DOMAIN);
}

void
thrift_application_exception_init (ThriftApplicationException *object)
{
  object->type = 0;
  object->__isset_type = FALSE;
  object->message = NULL;
  object->__isset_message = FALSE;
}

void
thrift_application_exception_class_init (ThriftApplicationExceptionClass *class)
{
  ThriftStructClass *cls = THRIFT_STRUCT_CLASS(class);
  cls->read = thrift_application_exception_read;
  cls->write = thrift_application_exception_write;
}

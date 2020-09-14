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

#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/protocol/thrift_protocol.h>
#include <thrift/c_glib/protocol/thrift_look_ahead_reader.h>

/* object properties */
enum _ThriftLookAheadReaderProperties
{
  PROP_0,
  PROP_THRIFT_LOOK_AHEAD_READER_TRANSPORT
};

G_DEFINE_TYPE(ThriftLookAheadReader, thrift_look_ahead_reader, G_TYPE_OBJECT)

gint8
thrift_look_ahead_reader_read (ThriftLookAheadReader *reader, GError **error)
{
  ThriftLookAheadReader *r = THRIFT_LOOK_AHEAD_READER (reader);
  if (r->hasData) {
    r->hasData = FALSE;
  } else {
    if (thrift_transport_read_all (r->transport, &(r->data), 1, error) < 0) {
      return -1;
    }
  }
  return r->data;
}

gint8
thrift_look_ahead_reader_peek (ThriftLookAheadReader *reader, GError **error)
{
  ThriftLookAheadReader *r = THRIFT_LOOK_AHEAD_READER (reader);

  if (!r->hasData) {
    if (thrift_transport_read_all (r->transport, &(r->data), 1, error) < 0) {
      return -1;
    }
  }
  r->hasData = TRUE;
  return r->data;
}

gint32
thrift_look_ahead_reader_read_syntax_char (ThriftLookAheadReader *reader, gint8 ch, GError **error)
{
  gint8 ch2 = thrift_look_ahead_reader_read(reader, error);

  if (ch2 != ch) {
    g_set_error (error, THRIFT_PROTOCOL_ERROR,
                THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                "Expected \'#{ch}\' got \'#{ch2}\'.");
    return -1;
  }
  return 1;
}

/* initializes the instance */
static void 
thrift_look_ahead_reader_init (ThriftLookAheadReader *reader)
{
  reader->hasData = FALSE;
  reader->data = 0;
}

/* destructor */
static void
thrift_look_ahead_reader_finalize (GObject *object)
{
  THRIFT_UNUSED_VAR (object);
}

/* property accessor */
void
thrift_look_ahead_reader_get_property (GObject *object, guint property_id,
                                       GValue *value, GParamSpec *pspec)
{
  ThriftLookAheadReader *r = THRIFT_LOOK_AHEAD_READER (object);
  
  THRIFT_UNUSED_VAR (pspec);

  switch (property_id) {
    case PROP_THRIFT_LOOK_AHEAD_READER_TRANSPORT:
      g_value_set_object (value, r->transport);
      break;
    default:
      break;
  }
}

/* property mutator */
void
thrift_look_ahead_reader_set_property (GObject *object, guint property_id,
                                       const GValue *value, GParamSpec *pspec)
{
  ThriftLookAheadReader *r = THRIFT_LOOK_AHEAD_READER (object);
  
  THRIFT_UNUSED_VAR (pspec);

  switch (property_id) {
    case PROP_THRIFT_LOOK_AHEAD_READER_TRANSPORT:
      r->transport = g_value_dup_object (value);
      break;
    default:
      break;
  }
}

/* initializes the class */
static void
thrift_look_ahead_reader_class_init (ThriftLookAheadReaderClass *cls)
{
  ThriftLookAheadReaderClass *tlarc = THRIFT_LOOK_AHEAD_READER_CLASS (cls);
  GObjectClass *gobject_class = G_OBJECT_CLASS (cls);
  GParamSpec *param_spec = NULL;

  /* setup accessors and mutators */
  gobject_class->get_property = thrift_look_ahead_reader_get_property;
  gobject_class->set_property = thrift_look_ahead_reader_set_property;

  param_spec = g_param_spec_object ("transport", "transport (construct)",
                                    "Thrift transport",
                                    THRIFT_TYPE_TRANSPORT,
                                    G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_LOOK_AHEAD_READER_TRANSPORT,
                                   param_spec);
  gobject_class->finalize = thrift_look_ahead_reader_finalize;
  tlarc->read = thrift_look_ahead_reader_read;
  tlarc->peek = thrift_look_ahead_reader_peek;
}

/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements. See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership. The ASF licenses this file
 *  to you under the Apache License, Version 2.0(the
 *  "License"); you may not use this file except in compliance
 *  with the License. You may obtain a copy of the License at
 *    
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distuributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
 *  KIND, either express or implied. See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */

#include <thrift/c_glib/thrift.h>
#include "thrift_configuration.h"

/* object properties */
enum _ThriftConfigurationProperties
{
  PROP_0,
  PROP_THRIFT_CONFIGURATION_MAX_MESSAGE_SIZE,
  PROP_THRIFT_CONFIGURATION_MAX_FRAME_SIZE,
  PROP_THRIFT_CONFIGURATION_RECURSION_LIMIT
};

G_DEFINE_TYPE(ThriftConfiguration, thrift_configuration, G_TYPE_OBJECT)

/* property accessor */
void
thrift_configuration_get_property(GObject *object, guint property_id,
		                   GValue *value, GParamSpec *pspec)
{
  ThriftConfiguration *configuration = THRIFT_CONFIGURATION(object);

  THRIFT_UNUSED_VAR (pspec);

  switch (property_id)
  {
    case PROP_THRIFT_CONFIGURATION_MAX_MESSAGE_SIZE:
         g_value_set_int(value, configuration->maxMessageSize_);
	 break;
    case PROP_THRIFT_CONFIGURATION_MAX_FRAME_SIZE:
         g_value_set_int(value, configuration->maxFrameSize_);
         break;
    case PROP_THRIFT_CONFIGURATION_RECURSION_LIMIT:
	 g_value_set_int(value, configuration->recursionLimit_);
	 break;
   }
}

/* property mutator */
void
thrift_configuration_set_property(GObject *object, guint property_id, 
		                  const GValue *value, GParamSpec *pspec)
{
  ThriftConfiguration *configuration = THRIFT_CONFIGURATION (object);

  THRIFT_UNUSED_VAR (pspec);

  switch (property_id)
  {
    case PROP_THRIFT_CONFIGURATION_MAX_MESSAGE_SIZE:
	 configuration->maxMessageSize_ = g_value_get_int (value);
	 break;
    case PROP_THRIFT_CONFIGURATION_MAX_FRAME_SIZE:
	 configuration->maxFrameSize_ = g_value_get_int (value);
	 break;
    case PROP_THRIFT_CONFIGURATION_RECURSION_LIMIT:
	 configuration->recursionLimit_ = g_value_get_int (value);
	 break;
  }
}
	
static void
thrift_configuration_class_init (ThriftConfigurationClass *cls)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (cls);
  GParamSpec *param_spec = NULL;

  /* setup accessors and mutators */
  gobject_class->get_property = thrift_configuration_get_property;
  gobject_class->set_property = thrift_configuration_set_property;

  param_spec = g_param_spec_int ("max_message_size",
                                 "max_message_size (construct)",
                                 "Set the max size of the message",
                                 0, /* min */
                                 G_MAXINT32, /* max */
                                 DEFAULT_MAX_MESSAGE_SIZE, /* default by convention */
                                 G_PARAM_CONSTRUCT_ONLY |
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_THRIFT_CONFIGURATION_MAX_MESSAGE_SIZE,
                                   param_spec);

  param_spec = g_param_spec_int ("max_frame_size",
                                 "max_frame_size (construct)",
                                 "Set the max size of the frame",
                                 0, /* min */                         
                                 G_MAXINT32, /* max */
                                 DEFAULT_MAX_FRAME_SIZE, /* default by convention */
                                 G_PARAM_CONSTRUCT_ONLY |
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_THRIFT_CONFIGURATION_MAX_FRAME_SIZE,
                                   param_spec);

  param_spec = g_param_spec_int ("recursion_limit",
                                 "recursion_limit (construct)",
                                 "Set the limit of the resursion",
                                 0, /* min */
                                 G_MAXINT32, /* max */
                                 DEFAULT_RECURSION_DEPTH, /* default by convention */
                                 G_PARAM_CONSTRUCT_ONLY |
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_THRIFT_CONFIGURATION_RECURSION_LIMIT,
                                   param_spec);

}

static void
thrift_configuration_init (ThriftConfiguration *configuration)
{
  THRIFT_UNUSED_VAR (configuration);
}

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

#ifndef _THRIFT_CONFIGURATION_H
#define _THRIFT_CONFIGURATION_H

#include <glib-object.h>

G_BEGIN_DECLS

/* type macros */
#define THRIFT_TYPE_CONFIGURATION (thrift_configuration_get_type ())
#define THRIFT_CONFIGURATION(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), THRIFT_TYPE_CONFIGURATION, ThriftConfiguration))
#define THRIFT_IS_CONFIGURATION(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), THRIFT_TYPE_CONFIGURATION))		
#define THRIFT_CONFIGURATTION_CLASS(c) (G_TYPE_CHECK_CLASS_CAST ((c), THRIFT_TYPE_CONFIGURATION, ThriftConfigurationClass))
#define THRIFT_IS_CONFIGURATION_CLASS(c) (G_TYPE_CHECK_CLASS_TYPE ((c), THRIFT_TYPE_CONFIGURATION))
#define THRIFT_CONFIGURATION_GET_CLASS(obj) (G_TYPE_INSTAANCE_GET_CLASS ((obj), THRIFT_TYPE_CONFIGURATION, ThriftconfigurationClass))

typedef struct _ThriftConfiguration ThriftConfiguration;

/*!
 * Thrift Configuration object
 */
struct _ThriftConfiguration
{
    GObject parent;

    /* private */
    int maxMessageSize_;
    int maxFrameSize_;
    int recursionLimit_;
};

typedef struct _ThriftConfigurationClass ThriftConfigurationClass;

/*!
 *  Thrift Configuration class
 */
struct _ThriftConfigurationClass
{
    GObjectClass parent;
};

/* used by THRIFT_TYPE_CONFIGURATION */
GType thrift_configuration_get_type(void);

#define DEFAULT_MAX_MESSAGE_SIZE  (100 * 1024 * 1024)
#define DEFAULT_MAX_FRAME_SIZE    (16384000)
#define DEFAULT_RECURSION_DEPTH   (64)

G_END_DECLS

#endif /* #ifndef _THRIFT_CONFIGURATION_H */

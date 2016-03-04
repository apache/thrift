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

#include <string.h>
#include <stdio.h>
#include <glib.h>
#include <glib-object.h>

#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/protocol/thrift_protocol.h>
#include <thrift/c_glib/protocol/thrift_multiplexed_protocol.h>

G_DEFINE_TYPE(ThriftMultiplexedProtocol, thrift_multiplexed_protocol, THRIFT_TYPE_PROTOCOL)


static GParamSpec *thrift_multiplexed_protocol_obj_properties[N_PROPERTIES] = { NULL, };

gint32
thrift_multiplexed_protocol_write_message_begin (ThriftProtocol *protocol,
		const gchar *name, const ThriftMessageType message_type,
		const gint32 seqid, GError **error)
{
	gint32 version = (THRIFT_MULTIPLEXED_PROTOCOL_VERSION_1)
                				   | ((gint32) message_type);
	gint32 ret;
	gint32 xfer = 0;

	g_return_val_if_fail (THRIFT_IS_MULTIPLEXED_PROTOCOL (protocol), -1);

	ThriftMultiplexedProtocol *self = THRIFT_MULTIPLEXED_PROTOCOL (protocol);



	if ((ret = thrift_protocol_write_i32 (protocol, version, error)) < 0)
	{
		return -1;
	}
	xfer += ret;

	if( (message_type == T_CALL || message_type == T_ONEWAY) && self->service_name != NULL) {
		gchar *service_name = g_strdup_printf("%s%s%s", self->service_name, self->separator, name);
		if ((ret = thrift_protocol_write_string (protocol, name, error)) < 0)
		{
			g_free(service_name);
			return -1;
		}
		g_free(service_name);

	}else{
		if ((ret = thrift_protocol_write_string (protocol, name, error)) < 0)
		{
			return -1;
		}
	}
	xfer += ret;

	if ((ret = thrift_protocol_write_i32 (protocol, seqid, error)) < 0)
	{
		return -1;
	}
	xfer += ret;
	return xfer;
}




static void
thrift_multiplexed_protocol_set_property (GObject      *object,
		guint         property_id,
		const GValue *value,
		GParamSpec   *pspec)
{
	ThriftMultiplexedProtocol *self = THRIFT_MULTIPLEXED_PROTOCOL (object);

	switch (property_id)
	{
	case PROP_SERVICE_NAME:
		if(self->service_name!=NULL)
			g_free (self->service_name);
		self->service_name= g_value_dup_string (value);
		break;

	case PROP_SEPARATOR:
		if(self->separator!=NULL)
			g_free (self->separator);
		self->separator= g_value_dup_string (value);
		break;

	default:
		/* We don't have any other property... */
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}

static void
thrift_multiplexed_protocol_get_property (GObject    *object,
		guint       property_id,
		GValue     *value,
		GParamSpec *pspec)
{
	ThriftMultiplexedProtocol *self = THRIFT_MULTIPLEXED_PROTOCOL (object);

	switch (property_id)
	{
	case PROP_SERVICE_NAME:
		g_value_set_string (value, self->service_name);
		break;

	case PROP_SEPARATOR:
		g_value_set_string (value, self->separator);
		break;

	default:
		/* We don't have any other property... */
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}


static void
thrift_multiplexed_protocol_init (ThriftMultiplexedProtocol *protocol)
{
	//  THRIFT_UNUSED_VAR (protocol);
	protocol->separator = g_value_dup_string (THRIFT_MULTIPLEXED_PROTOCOL_DEFAULT_SEPARATOR);
	protocol->service_name = NULL;
}

static void
thrift_multiplexed_protocol_finalize (ThriftMultiplexedProtocol *protocol)
{
	if(protocol->separator){
		g_free(protocol->separator);
		protocol->separator = NULL;
	}
	if(protocol->service_name){
		g_free(protocol->service_name);
		protocol->service_name = NULL;
	}
	/* Always chain up to the parent class; there is no need to check if
	 * the parent class implements the dispose() virtual function: it is
	 * always guaranteed to do so
	 */
	G_OBJECT_CLASS (protocol)->finalize(protocol);
}

/* initialize the class */
static void
thrift_multiplexed_protocol_class_init (ThriftMultiplexedProtocolClass *klass)
{
	ThriftProtocolClass *cls = THRIFT_PROTOCOL_CLASS (klass);
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	cls->write_message_begin = thrift_multiplexed_protocol_write_message_begin;


	object_class->set_property = thrift_multiplexed_protocol_set_property;
	object_class->get_property = thrift_multiplexed_protocol_get_property;
	object_class->finalize = thrift_multiplexed_protocol_finalize;

	thrift_multiplexed_protocol_obj_properties[PROP_SERVICE_NAME] =
			g_param_spec_string ("service-name",
					"Service name the protocol points to",
					"Set the service name",
					NULL /* default value */,
					(G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE));
	thrift_multiplexed_protocol_obj_properties[PROP_SEPARATOR] =
			g_param_spec_string ("separator",
					"Separator for service name and pointer",
					"Set service name separator",
					NULL /* default value */,
					G_PARAM_READWRITE);

	g_object_class_install_properties (object_class,
			N_PROPERTIES,
			thrift_multiplexed_protocol_obj_properties);
}

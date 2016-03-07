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

#ifndef _THRIFT_SSL_SOCKET_H
#define _THRIFT_SSL_SOCKET_H

#include <glib-object.h>
#include <glib.h>
#include <openssl/err.h>
#include <openssl/rand.h>
#include <openssl/ssl.h>
#include <openssl/x509v3.h>
#include <sys/socket.h>

#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_socket.h>
#include <thrift/c_glib/transport/thrift_platform_socket.h>

G_BEGIN_DECLS

/*! \file thrift_ssl_socket.h
 *  \brief SSL Socket implementation of a Thrift transport.  Subclasses the
 *         ThriftSocket class. Based on plain openssl.
 *         In the future we should take a look to https://issues.apache.org/jira/browse/THRIFT-1016
 */

/* type macros */
#define THRIFT_TYPE_SSL_SOCKET (thrift_ssl_socket_get_type ())
#define THRIFT_SSL_SOCKET(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), THRIFT_TYPE_SSL_SOCKET, ThriftSSLSocket))
#define THRIFT_IS_SSL_SOCKET(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), THRIFT_TYPE_SSL_SOCKET))
#define THRIFT_SSL_SOCKET_CLASS(c) (G_TYPE_CHECK_CLASS_CAST ((c), THRIFT_TYPE_SSL_SOCKET, ThriftSSLSocketClass))
#define THRIFT_IS_SSL_SOCKET_CLASS(c) (G_TYPE_CHECK_CLASS_TYPE ((c), THRIFT_TYPE_SSL_SOCKET))
#define THRIFT_SSL_SOCKET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), THRIFT_TYPE_SSL_SOCKET, ThriftSSLSocketClass))


/* define error/exception types */
typedef enum
{
  THRIFT_SSL_SOCKET_ERROR_TRANSPORT=7,
  THRIFT_SSL_SOCKET_ERROR_CIPHER_NOT_AVAILABLE,
  THRIFT_SSL_SOCKET_ERROR_SSL
} ThriftSSLSocketError;


typedef struct _ThriftSSLSocket ThriftSSLSocket;

/*!
 * Thrift SSL Socket instance.
 */
struct _ThriftSSLSocket
{
  ThriftSocket parent;

  /* private */
  SSL *ssl;
  SSL_CTX* ctx;
  gboolean server;
  /*
  gchar *hostname;
  gshort port;
  int sd;
  guint8 *buf;
  guint32 buf_size;
  guint32 buf_len;
  */
};

typedef struct _ThriftSSLSocketClass ThriftSSLSocketClass;
typedef gboolean (* AUTHORIZATION_MANAGER_CALLBACK) (ThriftTransport * transport, X509 *cert, struct sockaddr_storage *addr, GError **error);
/*!
 * Thrift Socket class.
 */
struct _ThriftSSLSocketClass
{
	ThriftSocketClass parent;

	gboolean (* handle_handshake) (ThriftTransport * transport, GError **error);
	gboolean (* create_ssl_context) (ThriftTransport * transport, GError **error);
	gboolean (* authorize_peer) (ThriftTransport * transport, X509 *cert, struct sockaddr_storage *addr, GError **error);

	/* Padding to allow adding up to 12 new virtual functions without
	 * breaking ABI. */
	gpointer padding[12];
};

enum _ThriftSSLSocketProtocol {
  SSLTLS  = 0,  // Supports SSLv2 and SSLv3 handshake but only negotiates at TLSv1_0 or later.
//SSLv2   = 1,  // HORRIBLY INSECURE!
  SSLv3   = 2,  // Supports SSLv3 only - also horribly insecure!
  TLSv1_0 = 3,  // Supports TLSv1_0 or later.
  TLSv1_1 = 4,  // Supports TLSv1_1 or later.
  TLSv1_2 = 5,  // Supports TLSv1_2 or later.
  LATEST  = TLSv1_2
};
typedef enum _ThriftSSLSocketProtocol ThriftSSLSocketProtocol;


/* Internal functions */
gboolean
thrift_ssl_socket_context_initialize(ThriftSSLSocketProtocol ssl_protocol, GError **error);


/* used by THRIFT_TYPE_SSL_SOCKET */
GType thrift_ssl_socket_get_type (void);
void thrift_ssl_socket_get_error(GError **error, const guchar *error_msg, guint thrift_error_no);


void thrift_ssl_socket_set_manager(ThriftSSLSocket *ssl_socket, AUTHORIZATION_MANAGER_CALLBACK callback);

/* This is the SSL API */
ThriftSSLSocket*
thrift_ssl_socket_new_with_host(ThriftSSLSocketProtocol ssl_protocol, gchar *hostname, guint port, GError **error);
ThriftSSLSocket*
thrift_ssl_socket_new(ThriftSSLSocketProtocol ssl_protocol, GError **error);
gboolean
thrift_ssl_load_cert_from_file(ThriftSSLSocket *ssl_socket, const char *file_name);
gboolean
thrift_ssl_load_cert_from_buffer(ThriftSSLSocket *ssl_socket, const char chain_certs[]);
void
thrift_ssl_socket_initialize_openssl(void);
void
thrift_ssl_socket_finalize_openssl(void);

G_END_DECLS
#endif

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
#include <netdb.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <openssl/ssl.h>

#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_socket.h>
#include <thrift/c_glib/transport/thrift_ssl_socket.h>

#define OPENSSL_VERSION_NO_THREAD_ID 0x10000000L


/* object properties */
enum _ThriftSSLSocketProperties
{
	PROP_THRIFT_SSL_SOCKET_CONTEXT = 3
};

/* for errors coming from socket() and connect() */
// extern int errno;

// To hold a global state management of openssl for all instances
static gboolean thrift_ssl_socket_openssl_initialized=FALSE;
static SSL_CTX* thrift_ssl_socket_global_context=NULL; // Should this be keept at class level?

#if (OPENSSL_VERSION_NUMBER < OPENSSL_VERSION_NO_THREAD_ID)
static unsigned long callbackThreadID() {
	return (unsigned long)pthread_self();
}
#endif


static void thrift_ssl_socket_static_locking_callback(int mode, int n, const char* unk, int id) {
	if (mode & CRYPTO_LOCK) {
		//		g_printf("We should lock thread %d\n", n);
		//mutexes[n].lock();
	} else {
		//		g_printf("We should unlock thread %d\n", n);
		//mutexes[n].unlock();
	}
}

static void* thrift_ssl_socket_dyn_lock_create_callback(const char* unk, int id) {
	g_print("We should create a lock\n");
	return NULL;
}

static void thrift_ssl_socket_dyn_lock_callback(int mode, void* lock, const char* unk, int id) {
	if (lock != NULL) {
		if (mode & CRYPTO_LOCK) {
			g_printf("We should lock thread %d\n");
		} else {
			g_printf("We should unlock thread %d\n");
		}
	}
}

static void thrift_ssl_socket_dyn_lock_destroy_callback(void* lock, const char* unk, int id) {
	g_printf("We must destroy the lock\n");
}



G_DEFINE_TYPE(ThriftSSLSocket, thrift_ssl_socket, THRIFT_TYPE_SOCKET)


/* implements thrift_transport_is_open */
gboolean
thrift_ssl_socket_is_open (ThriftTransport *transport)
{
	return thrift_socket_is_open(transport);
}

/* overrides thrift_transport_peek */
gboolean
thrift_ssl_socket_peek (ThriftTransport *transport, GError **error)
{
	gboolean retval = FALSE;
	ThriftSSLSocket *ssl_socket = THRIFT_SSL_SOCKET (transport);
	if(ssl_socket!=NULL && THRIFT_SSL_SOCKET_GET_CLASS(ssl_socket)->handle_handshake(transport, error)){
		if (thrift_ssl_socket_is_open (transport))
		{
			int rc;
			gchar byte;
			rc = SSL_peek(ssl_socket->ssl, &byte, 1);
			if (rc < 0) {
				g_set_error (error,
						THRIFT_TRANSPORT_ERROR,
						THRIFT_SSL_SOCKET_ERROR_SSL,
						"failed to peek at socket - id?");
				//			    int errno_copy = THRIFT_GET_SOCKET_ERROR;
				//			    string errors;
				//			    buildErrors(errors, errno_copy);
				//			    throw TSSLException("SSL_peek: " + errors);
			}
			if (rc == 0) {
				ERR_clear_error();
			}
			retval = (rc > 0);
		}
	}
	return retval;
}

/* implements thrift_transport_open */
gboolean
thrift_ssl_socket_open (ThriftTransport *transport, GError **error)
{
	return thrift_socket_open(transport, error);
}

/* implements thrift_transport_close */
gboolean
thrift_ssl_socket_close (ThriftTransport *transport, GError **error)
{
	gboolean retval = FALSE;
	if(THRIFT_SSL_SOCKET(transport)->ssl) {
		int rc = SSL_shutdown(THRIFT_SSL_SOCKET(transport)->ssl);
		if (rc < 0) {
			int errno_copy = THRIFT_SSL_SOCKET_ERROR_SSL;
			// FIXME build error
			//	      string errors;
			//	      buildErrors(errors, errno_copy);
			//	      GlobalOutput(("SSL_shutdown: " + errors).c_str());
		}
		SSL_free(THRIFT_SSL_SOCKET(transport)->ssl);
		THRIFT_SSL_SOCKET(transport)->ssl = NULL;
		ERR_remove_state(0);
	}
	return thrift_socket_close(transport, error);
}

/* implements thrift_transport_read */
gint32
thrift_ssl_socket_read (ThriftTransport *transport, gpointer buf,
		guint32 len, GError **error)
{
	guint maxRecvRetries_ = 10;
	ThriftSSLSocket *ssl_socket = THRIFT_SSL_SOCKET (transport);
	guint bytes = 0;
	guint retries = 0;
	if(ssl_socket!=NULL && THRIFT_SSL_SOCKET_GET_CLASS(ssl_socket)->handle_handshake(transport, error)){
		for (retries=0; retries < maxRecvRetries_; retries++) {
			bytes = SSL_read(ssl_socket->ssl, buf, len);
			if (bytes >= 0)
				break;
			int errno_copy = THRIFT_GET_SOCKET_ERROR;
			if (SSL_get_error(ssl_socket->ssl, bytes) == SSL_ERROR_SYSCALL) {
				if (ERR_get_error() == 0 && errno_copy == THRIFT_EINTR) {
					continue;
				}
			}
			g_set_error (error, THRIFT_TRANSPORT_ERROR,
					THRIFT_TRANSPORT_ERROR_RECEIVE,
					"failed to read %d bytes - %s", len, strerror(errno));
			return -1;
			//			string errors;
			//			buildErrors(errors, errno_copy);
			//			throw TSSLException("SSL_read: " + errors);
		}
	}
	return bytes;
}

/* implements thrift_transport_read_end
 * called when write is complete.  nothing to do on our end. */
gboolean
thrift_ssl_socket_read_end (ThriftTransport *transport, GError **error)
{
	/* satisfy -Wall */
	THRIFT_UNUSED_VAR (transport);
	THRIFT_UNUSED_VAR (error);
	return TRUE;
}

/* implements thrift_transport_write */
gboolean
thrift_ssl_socket_write (ThriftTransport *transport, const gpointer buf,
		const guint32 len, GError **error)
{
	ThriftSSLSocket *ssl_socket = THRIFT_SSL_SOCKET (transport);
	gint ret = 0;
	guint sent = 0;
	if(THRIFT_SSL_SOCKET_GET_CLASS(ssl_socket)->handle_handshake(transport, error)){

		ThriftSocket *socket = THRIFT_SSL_SOCKET (transport);
		g_return_val_if_fail (socket->sd != THRIFT_INVALID_SOCKET, FALSE);

		while (sent < len)
		{
			ret = SSL_write (ssl_socket->ssl, (guint8 *)buf + sent, len - sent);
			if (ret < 0)
			{
				g_set_error (error, THRIFT_TRANSPORT_ERROR,
						THRIFT_TRANSPORT_ERROR_SEND,
						"failed to send %d bytes - %s", len, strerror(errno));
				return FALSE;
			}
			sent += ret;
		}

	}
	return sent==len;
}

/* implements thrift_transport_write_end
 * called when write is complete.  nothing to do on our end. */
gboolean
thrift_ssl_socket_write_end (ThriftTransport *transport, GError **error)
{
	/* satisfy -Wall */
	THRIFT_UNUSED_VAR (transport);
	THRIFT_UNUSED_VAR (error);
	return TRUE;
}

/* implements thrift_transport_flush
 * flush pending data.  since we are not buffered, this is a no-op */
gboolean
thrift_ssl_socket_flush (ThriftTransport *transport, GError **error)
{
	ThriftSSLSocket *ssl_socket = THRIFT_SSL_SOCKET (transport);
	gint ret = 0;
	guint sent = 0;
	if(THRIFT_SSL_SOCKET_GET_CLASS(ssl_socket)->handle_handshake(transport, error)){

		BIO* bio = SSL_get_wbio(ssl_socket->ssl);
		if (bio == NULL) {
			g_set_error (error, THRIFT_TRANSPORT_ERROR,
					THRIFT_TRANSPORT_ERROR_SEND,
					"failed to flush, wbio returned null");
			//			    throw TSSLException("SSL_get_wbio returns NULL");
		}
		if (BIO_flush(bio) != 1) {
			g_set_error (error, THRIFT_TRANSPORT_ERROR,
					THRIFT_TRANSPORT_ERROR_SEND,
					"failed to flush it returned error");
			//			    int errno_copy = THRIFT_GET_SOCKET_ERROR;
			//			    string errors;
			//			    buildErrors(errors, errno_copy);
			//			    throw TSSLException("BIO_flush: " + errors);
			return FALSE;
		}

	}
	return TRUE;
}


gboolean
thrift_ssl_socket_handle_handshake(ThriftTransport * transport, GError **error)
{
	ThriftSSLSocket *ssl_socket = THRIFT_SSL_SOCKET (transport);
	ThriftSocket *socket = THRIFT_SOCKET (transport);
	g_return_val_if_fail (thrift_transport_is_open (transport), FALSE);

	if(ssl_socket==NULL || ssl_socket->ssl!=NULL){
		return TRUE;
	}

	if(THRIFT_SSL_SOCKET_GET_CLASS(ssl_socket)->create_ssl_context(transport, error)){
		// Context created
		SSL_set_fd(ssl_socket->ssl, socket->sd);
		int rc;
		if(ssl_socket->server){
			rc = SSL_accept(ssl_socket->ssl);
		}else{
			rc = SSL_connect(ssl_socket->ssl);
		}
		if (rc <= 0) {
			fprintf(stderr,"The error returned was %d\n", SSL_get_error(ssl_socket->ssl, rc));
			thrift_ssl_socket_get_error(error, "Not possible to connect", THRIFT_SSL_SOCKET_ERROR_CIPHER_NOT_AVAILABLE);
			return FALSE;
			//		    int errno_copy = THRIFT_GET_SOCKET_ERROR;
			//		    string fname(server() ? "SSL_accept" : "SSL_connect");
			//		    string errors;
			//		    buildErrors(errors, errno_copy);
			//		    throw TSSLException(fname + ": " + errors);
		}
	}else
		return FALSE;

	return thrift_ssl_socket_authorize(transport, error);
}

gboolean
thrift_ssl_socket_create_ssl_context(ThriftTransport * transport, GError **error)
{
	ThriftSSLSocket *socket = THRIFT_SSL_SOCKET (transport);

	if(socket->ctx!=NULL){
		if(socket->ssl!=NULL) {
			return TRUE;
		}

		socket->ssl = SSL_new(socket->ctx);
		if (socket->ssl == NULL) {
			g_set_error (error, THRIFT_TRANSPORT_ERROR,
					THRIFT_SSL_SOCKET_ERROR_TRANSPORT,
					"Unable to create SSL context");
			return FALSE;
		}
	}

	return TRUE;
}

/**
 *
 * @param ssl_socket The ssl socket
 * @param file_name The file name of the PEM certificate chain
 * @return
 */
gboolean thrift_ssl_load_cert_from_file(ThriftSSLSocket *ssl_socket, const char *file_name)
{
	int rc = SSL_CTX_load_verify_locations(ssl_socket->ctx, file_name, NULL);
	if (rc != 1) { // verify authentication result
		g_warning("Load of certificates failed!: %s", X509_verify_cert_error_string(ERR_get_error()));
		return FALSE;
	}
	return TRUE;
}

/**
 * Load a certificate chain from memory
 * @param ssl_socket the ssl socket
 * @param chain_certs the buffer to load PEM from
 * @return
 */
gboolean thrift_ssl_load_cert_from_buffer(ThriftSSLSocket *ssl_socket, const char chain_certs[])
{
	gboolean retval = FALSE;
	// Load chain of certs
	X509 *cacert=NULL;
	BIO *mem = BIO_new_mem_buf(chain_certs,strlen(chain_certs));
	X509_STORE *cert_store = SSL_CTX_get_cert_store(ssl_socket->ctx);

	if(cert_store!=NULL){
		int index = 0;
		while ((cacert = PEM_read_bio_X509(mem, NULL, 0, NULL))!=NULL) {
			if(cacert) {
				g_debug("Our certificate name is %s", cacert->name);
				X509_STORE_add_cert(cert_store, cacert);
				X509_free(cacert);
				cacert=NULL;
			} /* Free immediately */
			index++;
		}
		retval=TRUE;
	}
	BIO_free(mem);
	return retval;
}

gboolean
thrift_ssl_socket_authorize(ThriftTransport * transport, GError **error)
{
	ThriftSocket *socket = THRIFT_SOCKET (transport);
	ThriftSSLSocket *ssl_socket = THRIFT_SSL_SOCKET (transport);
	ThriftSSLSocketClass *cls = THRIFT_SSL_SOCKET_GET_CLASS(ssl_socket);
	gboolean authorization_result = FALSE;
	// We still don't support it
	if(cls!=NULL && ssl_socket->ssl!=NULL){
		int rc = SSL_get_verify_result(ssl_socket->ssl);
		if (rc != X509_V_OK) { // verify authentication result
			g_warning("The certificate verification failed!: %s", X509_verify_cert_error_string(rc));
			return authorization_result;
		}
		X509* cert = SSL_get_peer_certificate(ssl_socket->ssl);
		if (cert == NULL) {
			// certificate is not present
			if (SSL_get_verify_mode(ssl_socket->ssl) & SSL_VERIFY_FAIL_IF_NO_PEER_CERT) {
				g_warning("There's no certificate present!");
				return authorization_result;
				//throw TSSLException("authorize: required certificate not present");
			}
			// certificate was optional: didn't intend to authorize remote
			if (ssl_socket->server && cls->authorize_peer != NULL) {
				g_warning("Certificate required for authorization!");
				return authorization_result;
				//				throw TSSLException("authorize: certificate required for authorization");
			}
			return authorization_result;
		}
		// certificate is present, since we don't support access manager we are done
		if (cls->authorize_peer == NULL) {
			X509_free(cert);
			return authorization_result;
		}else{
			// both certificate and access manager are present
			struct sockaddr_storage sa;
			socklen_t saLength = sizeof(struct sockaddr_storage);

			if (getpeername(socket->sd, (struct sockaddr*)&sa, &saLength) != 0) {
				sa.ss_family = AF_UNSPEC;
			}
			authorization_result = cls->authorize_peer(transport, cert, &sa, error);
		}
		if(cert!=NULL){
			X509_free(cert);
		}
	}

	return authorization_result;
}


/* initializes the instance */
static void
thrift_ssl_socket_init (ThriftSSLSocket *socket)
{
	socket->ssl = NULL;
	socket->ctx = NULL;
	socket->server = FALSE;
}

/* destructor */
static void
thrift_ssl_socket_finalize (GObject *object)
{
	ThriftSSLSocket *socket = THRIFT_SSL_SOCKET (object);
	GError *error=NULL;
	if(socket->ssl != NULL)
	{
		thrift_ssl_socket_close(THRIFT_TRANSPORT(object), &error);
		socket->ssl=NULL;
		socket->ctx=NULL;
	}
}

/* property accessor */
void
thrift_ssl_socket_get_property (GObject *object, guint property_id,
		GValue *value, GParamSpec *pspec)
{
	ThriftSSLSocket *socket = THRIFT_SSL_SOCKET (object);

	THRIFT_UNUSED_VAR (pspec);
	g_test_message("Getting property id %d", value);

	switch (property_id)
	{
	case PROP_THRIFT_SSL_SOCKET_CONTEXT:
		g_value_set_pointer (value, socket->ctx);
		break;
	}
}

/* property mutator */
void
thrift_ssl_socket_set_property (GObject *object, guint property_id,
		const GValue *value, GParamSpec *pspec)
{
	//ThriftSocketClass *tsc= THRIFT_SOCKET_CLASS(object);
	ThriftSSLSocket *socket = THRIFT_SSL_SOCKET (object);

	THRIFT_UNUSED_VAR (pspec);
	g_test_message("Setting property id %d", property_id);
	switch (property_id)
	{
	case PROP_THRIFT_SSL_SOCKET_CONTEXT:
		socket->ctx = g_value_get_pointer(value); // We copy the context
		break;
		//	default:
		//		thrift_socket_set_property(object, property_id, value, pspec);
		//		break;
	}
}

void
thrift_ssl_socket_initialize_openssl(void)
{
	if(thrift_ssl_socket_openssl_initialized){
		return;
	}
	thrift_ssl_socket_openssl_initialized=TRUE;
	SSL_library_init();
	ERR_load_crypto_strings();
	SSL_load_error_strings();
	ERR_load_BIO_strings();

#if (OPENSSL_VERSION_NUMBER < OPENSSL_VERSION_NO_THREAD_ID)
	CRYPTO_set_id_callback(callbackThreadID);
#endif
	CRYPTO_set_locking_callback(thrift_ssl_socket_static_locking_callback);
	// dynamic locking
	CRYPTO_set_dynlock_create_callback(thrift_ssl_socket_dyn_lock_create_callback);
	CRYPTO_set_dynlock_lock_callback(thrift_ssl_socket_dyn_lock_callback);
	CRYPTO_set_dynlock_destroy_callback(thrift_ssl_socket_dyn_lock_destroy_callback);
}


void thrift_ssl_socket_finalize_openssl(void)
{

	// FIXME This should not be here
	if (thrift_ssl_socket_global_context != NULL) {
		SSL_CTX_free(thrift_ssl_socket_global_context);
		thrift_ssl_socket_global_context = NULL;
	}

	if (!thrift_ssl_socket_openssl_initialized) {
		return;
	}
	thrift_ssl_socket_openssl_initialized = FALSE;
#if (OPENSSL_VERSION_NUMBER < OPENSSL_VERSION_NO_THREAD_ID)
	CRYPTO_set_id_callback(NULL);
#endif
	CRYPTO_set_locking_callback(NULL);
	CRYPTO_set_dynlock_create_callback(NULL);
	CRYPTO_set_dynlock_lock_callback(NULL);
	CRYPTO_set_dynlock_destroy_callback(NULL);
	ERR_free_strings();
	EVP_cleanup();
	CRYPTO_cleanup_all_ex_data();
	ERR_remove_state(0);


}


/* initializes the class */
static void
thrift_ssl_socket_class_init (ThriftSSLSocketClass *cls)
{
	ThriftTransportClass *ttc = THRIFT_TRANSPORT_CLASS (cls);
	GObjectClass *gobject_class = G_OBJECT_CLASS (cls);
	GParamSpec *param_spec = NULL;
	g_test_message("Init of class\n");
	/* setup accessors and mutators */
	gobject_class->get_property = thrift_ssl_socket_get_property;
	gobject_class->set_property = thrift_ssl_socket_set_property;
	param_spec = g_param_spec_pointer ("ssl_context",
			"ssl_context (construct)",
			"Set the ssl context for handshake with the remote host",
			G_PARAM_CONSTRUCT_ONLY |
			G_PARAM_READWRITE);
	g_object_class_install_property (gobject_class, PROP_THRIFT_SSL_SOCKET_CONTEXT,
			param_spec);
	//
	//  param_spec = g_param_spec_uint ("port",
	//                                  "port (construct)",
	//                                  "Set the port of the remote host",
	//                                  0, /* min */
	//                                  65534, /* max */
	//                                  9090, /* default by convention */
	//                                  G_PARAM_CONSTRUCT_ONLY |
	//                                  G_PARAM_READWRITE);
	//  g_object_class_install_property (gobject_class, PROP_THRIFT_SSL_SOCKET_PORT,
	//                                   param_spec);



	// Class methods
	cls->handle_handshake = thrift_ssl_socket_handle_handshake;
	cls->create_ssl_context = thrift_ssl_socket_create_ssl_context;

	// Override
	gobject_class->finalize = thrift_ssl_socket_finalize;
	ttc->is_open = thrift_ssl_socket_is_open;
	ttc->peek = thrift_ssl_socket_peek;
	ttc->open = thrift_ssl_socket_open;
	ttc->close = thrift_ssl_socket_close;
	ttc->read = thrift_ssl_socket_read;
	ttc->read_end = thrift_ssl_socket_read_end;
	ttc->write = thrift_ssl_socket_write;
	ttc->write_end = thrift_ssl_socket_write_end;
	ttc->flush = thrift_ssl_socket_flush;
}


/*
 * Public API
 */
ThriftSSLSocket*
thrift_ssl_socket_new(ThriftSSLSocketProtocol ssl_protocol, GError **error)
{
	ThriftSSLSocket *thriftSSLSocket = NULL;
	// Create the context
	if(thrift_ssl_socket_global_context==NULL){
		if(!thrift_ssl_socket_context_initialize(ssl_protocol, error)){
			// FIXME Do error control
			return thriftSSLSocket;
		}
	}
	// FIXME if the protocol is different?
	thriftSSLSocket = g_object_new (THRIFT_TYPE_SSL_SOCKET, "ssl_context", thrift_ssl_socket_global_context, NULL);
	return thriftSSLSocket;
}

void thrift_ssl_socket_set_manager(ThriftSSLSocket *ssl_socket, AUTHORIZATION_MANAGER_CALLBACK callback)
{
	ThriftSSLSocketClass *sslSocketClass = THRIFT_SSL_SOCKET_GET_CLASS (ssl_socket);
	if(sslSocketClass){
		sslSocketClass->authorize_peer = callback;
	}
}


ThriftSSLSocket*
thrift_ssl_socket_new_with_host(ThriftSSLSocketProtocol ssl_protocol, gchar *hostname, guint port, GError **error)
{
	ThriftSSLSocket *thriftSSLSocket = NULL;
	// Create the context
	if(thrift_ssl_socket_global_context==NULL){
		if(!thrift_ssl_socket_context_initialize(ssl_protocol, error)){
			// FIXME Do error control
			return thriftSSLSocket;
		}
	}
	// FIXME if the protocol is different?
	thriftSSLSocket = g_object_new (THRIFT_TYPE_SSL_SOCKET, "ssl_context", thrift_ssl_socket_global_context, "hostname", hostname, "port", port, NULL);
	return thriftSSLSocket;
}

gboolean
thrift_ssl_socket_context_initialize(ThriftSSLSocketProtocol ssl_protocol, GError **error)
{
	switch(ssl_protocol){
	case SSLTLS:
		thrift_ssl_socket_global_context = SSL_CTX_new(SSLv23_method());
		break;
	case SSLv3:
		thrift_ssl_socket_global_context = SSL_CTX_new(SSLv3_method());
		break;
	case TLSv1_0:
		thrift_ssl_socket_global_context = SSL_CTX_new(TLSv1_method());
		break;
	case TLSv1_1:
		thrift_ssl_socket_global_context = SSL_CTX_new(TLSv1_1_method());
		break;
	case TLSv1_2:
		thrift_ssl_socket_global_context = SSL_CTX_new(TLSv1_2_method());
		break;
	default:
		g_set_error (error, THRIFT_TRANSPORT_ERROR,
				THRIFT_SSL_SOCKET_ERROR_CIPHER_NOT_AVAILABLE,
				"The SSL protocol is unknown for %d", ssl_protocol);
		return FALSE;
		break;
	}

	if (thrift_ssl_socket_global_context == NULL) {
		thrift_ssl_socket_get_error(error, "No cipher overlay", THRIFT_SSL_SOCKET_ERROR_CIPHER_NOT_AVAILABLE);
		return FALSE;
	}
	SSL_CTX_set_mode(thrift_ssl_socket_global_context, SSL_MODE_AUTO_RETRY);

	// Disable horribly insecure SSLv2 and SSLv3 protocols but allow a handshake
	// with older clients so they get a graceful denial.
	if (ssl_protocol == SSLTLS) {
		SSL_CTX_set_options(thrift_ssl_socket_global_context, SSL_OP_NO_SSLv2);
		SSL_CTX_set_options(thrift_ssl_socket_global_context, SSL_OP_NO_SSLv3);   // THRIFT-3164
	}

	return TRUE;
}


void thrift_ssl_socket_get_error(GError **error, const guchar *error_msg, guint thrift_error_no)
{
	unsigned long error_code;
	while ((error_code = ERR_get_error()) != 0) {
		const char* reason = ERR_reason_error_string(error_code);
		if (reason == NULL) {
			g_set_error (error, THRIFT_TRANSPORT_ERROR,
					thrift_error_no,
					"SSL error %lX: %s", error_code, error_msg);
		}else{
			g_set_error (error, THRIFT_TRANSPORT_ERROR,
					thrift_error_no,
					"SSL error %lX %s: %s", error_code,reason, error_msg);
		}
	}
}


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

#include <assert.h>
#include <netdb.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>

#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_buffered_transport.h>
#include <thrift/c_glib/transport/thrift_server_transport.h>
#include <thrift/c_glib/transport/thrift_server_socket.h>
#include <thrift/c_glib/transport/thrift_ssl_socket.h>

//#define TEST_DATA { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j' }
#define TEST_DATA { "GET / HTTP/1.1\n\n" }


/* substituted functions to test failures of system and library calls */
static int socket_error = 0;
int
my_socket(int domain, int type, int protocol)
{
	if (socket_error == 0)
	{
		return socket (domain, type, protocol);
	}
	return -1;
}

static int recv_error = 0;
ssize_t
my_recv(int socket, void *buffer, size_t length, int flags)
{
	if (recv_error == 0)
	{
		return recv (socket, buffer, length, flags);
	}
	return -1;
}

static int send_error = 0;
ssize_t
my_send(int socket, const void *buffer, size_t length, int flags)
{
	if (send_error == 0)
	{
		return send (socket, buffer, length, flags);
	}
	return -1;
}

#define socket my_socket
#define recv my_recv
#define send my_send
#include "../src/thrift/c_glib/transport/thrift_ssl_socket.c"
#undef socket
#undef recv
#undef send

static void thrift_ssl_socket_server (const int port);

/* test object creation and destruction */
static void
test_ssl_create_and_destroy(void)
{
	gchar *hostname = NULL;
	guint port = 0;

	GObject *object = NULL;
	object = g_object_new (THRIFT_TYPE_SSL_SOCKET, NULL);
	assert (object != NULL);
	g_object_get (G_OBJECT(object), "hostname", &hostname, "port", &port, NULL);
	g_free (hostname);
	g_object_unref (object);
}

static void
test_ssl_create_and_set_properties(void)
{
	gchar *hostname = NULL;
	guint port = 0;
	SSL_CTX* ssl_ctx= NULL;
	GError *error=NULL;

	GObject *object = NULL;
	object = thrift_ssl_socket_new(SSLTLS, &error);
	g_object_get (G_OBJECT(object), "hostname", &hostname, "port", &port, "ssl_context", &ssl_ctx, NULL);
	assert (ssl_ctx!=NULL);

	g_free (hostname);
	g_object_unref (object);
}

static void
test_ssl_open_and_close(void)
{
	ThriftSSLSocket *tSSLSocket = NULL;
	ThriftTransport *transport = NULL;
	GError *error=NULL;

	/* open a connection and close it */
	tSSLSocket = thrift_ssl_socket_new_with_host(SSLTLS, "localhost", 51188, &error);

	transport = THRIFT_TRANSPORT (tSSLSocket);
	thrift_ssl_socket_open (transport, NULL);
	assert (thrift_ssl_socket_is_open (transport) == TRUE);
	thrift_ssl_socket_close (transport, NULL);
	assert (thrift_ssl_socket_is_open (transport) == FALSE);

	/* test close failure */
	THRIFT_SOCKET(tSSLSocket)->sd = -1;
	thrift_ssl_socket_close (transport, NULL);
	g_object_unref (tSSLSocket);

	/* try a hostname lookup failure */
	tSSLSocket = thrift_ssl_socket_new_with_host(SSLTLS, "localhost.broken", 51188, &error);
	transport = THRIFT_TRANSPORT (tSSLSocket);
	assert (thrift_ssl_socket_open (transport, &error) == FALSE);
	g_object_unref (tSSLSocket);
	g_error_free (error);
	error = NULL;

	/* try an error call to socket() */
	tSSLSocket = thrift_ssl_socket_new_with_host(SSLTLS, "localhost", 51188, &error);
	transport = THRIFT_TRANSPORT (tSSLSocket);
	socket_error = 1;
	assert (thrift_ssl_socket_open (transport, &error) == FALSE);
	socket_error = 0;
	g_object_unref (tSSLSocket);
	g_error_free (error);
}



/**
 * Print the common name of certificate
 */
unsigned char * get_cn_name(X509_NAME* const name)
{
	int idx = -1;
	unsigned char *utf8 = NULL;

	do
	{
		if(!name) break; /* failed */

		idx = X509_NAME_get_index_by_NID(name, NID_commonName, -1);
		if(!(idx > -1))  break; /* failed */

		X509_NAME_ENTRY* entry = X509_NAME_get_entry(name, idx);
		if(!entry) break; /* failed */

		ASN1_STRING* data = X509_NAME_ENTRY_get_data(entry);
		if(!data) break; /* failed */

		int length = ASN1_STRING_to_UTF8(&utf8, data);
		if(!utf8 || !(length > 0))  break; /* failed */

	} while (0);
	return utf8;
}

/*
 * Handle IPV4 and IPV6 addr
 */
void *get_in_addr(struct sockaddr *sa)
{
	if (sa->sa_family == AF_INET)
		return &(((struct sockaddr_in*)sa)->sin_addr);
	return &(((struct sockaddr_in6*)sa)->sin6_addr);
}

int verify_ip(char * hostname, struct sockaddr_storage *addr)
{
	struct addrinfo *addr_info,*p;
	struct addrinfo hints;
	int res;
	int retval = 0;


	memset(&hints, 0, sizeof hints);
	hints.ai_family = AF_UNSPEC; // use AF_INET6 to force IPv6
	hints.ai_socktype = SOCK_STREAM;


	if ( (res = getaddrinfo(hostname, NULL, &hints, &addr_info) ) != 0)
	{
		// get the host info
		g_error("Cannot get the host address");
		return retval;
	}
	// loop through all the results and connect to the first we can
	char dnshost[INET6_ADDRSTRLEN]; // bigger addr supported IPV6
	char socket_ip[INET6_ADDRSTRLEN];
	if(inet_ntop(addr->ss_family, get_in_addr(addr), socket_ip, INET6_ADDRSTRLEN)==socket_ip){
		g_debug("We are connected to host %s checking against certificate...", socket_ip);
		int sizeip = socket_ip!=NULL ? strlen(socket_ip) : 0;
		for(p = addr_info; p != NULL; p = p->ai_next) {
			if(inet_ntop(p->ai_family, get_in_addr((struct sockaddr *)p->ai_addr), dnshost, INET6_ADDRSTRLEN)==dnshost){
				if(dnshost!=NULL){
					g_info("DNS address [%i -> %s]", p->ai_addr, dnshost);
					if(!strncmp(dnshost, socket_ip, sizeip)){
						retval=1;
						break; // if we get here, we must have connected successfully
					}
				}
			}
		}
	}

	if(addr_info)
		freeaddrinfo(addr_info);

	return retval;
}

static void
read_from_file(char *buffer, long size, const char *file_name)
{
	   char ch;
	   long index=0;
	   FILE *fp;

	   fp = fopen(file_name,"r"); // read mode

	   if( fp == NULL )
	   {
	      perror("Error while opening the file.\n");
	      exit(EXIT_FAILURE);
	   }

	   printf("The contents of %s file are :\n", file_name);

	   while(index<size && ( ch = fgetc(fp) ) != EOF ){
		   buffer[index++] = ch;
	   }

	   fclose(fp);
}

#define ISSUER_CN_PINNING "The Apache Software Foundation"
#define SUBJECT_CN_PINNING "localhost"
#define CERT_SERIAL_NUMBER "1"

gboolean verify_certificate_sn(X509 *cert, const unsigned char *serial_number)
{
	gboolean retval = FALSE;

	ASN1_INTEGER *serial = X509_get_serialNumber(cert);

	BIGNUM *bn = ASN1_INTEGER_to_BN(serial, NULL);
	if (!bn) {
		fprintf(stderr, "unable to convert ASN1INTEGER to BN\n");
		return EXIT_FAILURE;
	}
	char *tmp = BN_bn2dec(bn);
	if (!tmp) {
		g_warning(stderr, "unable to convert BN to decimal string.\n");
		BN_free(bn);
		return EXIT_FAILURE;
	}
//	if (strlen(tmp) >= len) {
//		g_warn(stderr, "buffer length shorter than serial number\n");
//		BN_free(bn);
//		OPENSSL_free(tmp);
//		return EXIT_FAILURE;
//	}
	if(!strncmp(serial_number, tmp, strlen(serial_number))){
		retval=TRUE;
	}else{
		g_warning("Serial number is not valid");
	}

	BN_free(bn);
	OPENSSL_free(tmp);
	return retval;
}

gboolean my_access_manager(ThriftTransport * transport, X509 *cert, struct sockaddr_storage *addr, GError **error)
{
	ThriftSSLSocket *sslSocket = THRIFT_SSL_SOCKET (transport);

	g_info("Processing access to the server");
	X509_NAME* iname = cert ? X509_get_issuer_name(cert) : NULL;
	X509_NAME* sname = cert ? X509_get_subject_name(cert) : NULL;

	/* Issuer is the authority we trust that warrants nothing useful */
	const unsigned char * issuer = get_cn_name(iname);
	if(issuer){
		gboolean valid = TRUE;
		g_info("Issuer (cn) %s", issuer);

		// Issuer pinning
		if(strncmp(ISSUER_CN_PINNING, issuer, strlen(ISSUER_CN_PINNING))){
			g_warning("The Issuer of the certificate is not valid");
			valid=FALSE;
		}
		OPENSSL_free(issuer);
		if(!valid)
			return valid;
	}


	/* Subject is who the certificate is issued to by the authority  */
	const unsigned char * subject = get_cn_name(sname);
	if(subject){
		g_info("Subject (cn) %s", subject);
		gboolean valid = TRUE;

		// Subject pinning
		if(strncmp(SUBJECT_CN_PINNING, subject, strlen(SUBJECT_CN_PINNING))){
			g_warning("The subject of the certificate is not valid");
			valid=FALSE;
		}

		if(!valid)
			return valid;

		// Host pinning
		if(verify_ip(subject, addr)){
			g_info("Verified subject");
		}else{
			g_info("Cannot verify subject");
			valid=FALSE;
		}
		OPENSSL_free(subject);

		if(!valid)
			return valid;
	}

	if(!verify_certificate_sn(cert, CERT_SERIAL_NUMBER)){
		return FALSE;
	}else{
		g_info("Verified serial number");
	}

	return TRUE;

}




#ifdef BUILD_SERVER
static void
test_ssl_authorization_manager(void)
{
	int status=0;
	pid_t pid;
	ThriftSSLSocket *tSSLsocket = NULL;
	ThriftTransport *transport = NULL;
	//  int port = 51199;
	int port = 443;
	GError *error=NULL;

	guchar buf[17] = TEST_DATA; /* a buffer */

	//  pid = fork ();
	//  assert ( pid >= 0 );
	//
	//  if ( pid == 0 )
	//  {
	//    /* child listens */
	//    thrift_ssl_socket_server (port);
	//    exit (0);
	//  } else {
	/* parent connects, wait a bit for the socket to be created */
	sleep (1);

	// Test against level2 owncloud certificate
	tSSLsocket = thrift_ssl_socket_new_with_host(SSLTLS, "localhost", port, &error);
	thrift_ssl_socket_set_manager(tSSLsocket, my_access_manager); 					// Install pinning manager
	//thrift_ssl_load_cert_from_file(tSSLsocket, "./owncloud.level2crm.pem");
	unsigned char cert_buffer[65534];
	read_from_file(cert_buffer, 65534, "../../keys/client.pem");
	if(!thrift_ssl_load_cert_from_buffer(tSSLsocket, cert_buffer)){
		g_warning("Certificates cannot be loaded!");
	}

	transport = THRIFT_TRANSPORT (tSSLsocket);
	assert (thrift_ssl_socket_open (transport, NULL) == TRUE);
	assert (thrift_ssl_socket_is_open (transport));

	thrift_ssl_socket_write (transport, buf, 17, NULL);

	/* write fail */
	send_error = 1;
	//    thrift_ssl_socket_write (transport, buf, 1, NULL);
	//   send_error = 0;

	//    thrift_ssl_socket_write_end (transport, NULL);
	//    thrift_ssl_socket_flush (transport, NULL);
	thrift_ssl_socket_close (transport, NULL);
	g_object_unref (tSSLsocket);

	//    assert ( wait (&status) == pid );
	assert ( status == 0 );
	//  }
}
#endif


/* test ThriftSocket's peek() implementation */
//static void
//test_ssl_peek(void)
//{
//  gint status;
//  pid_t pid;
//  guint port = 51199;
//  gchar data = 'A';
//  ThriftTransport *client_transport;
//  GError *error = NULL;
//
//  client_transport = g_object_new (THRIFT_TYPE_SSL_SOCKET,
//                                   "hostname", "localhost",
//                                   "port",     port,
//                                   NULL);
//
//  /* thrift_transport_peek returns FALSE when the socket is closed */
//  g_assert (thrift_transport_is_open (client_transport) == FALSE);
//  g_assert (thrift_transport_peek (client_transport, &error) == FALSE);
//  g_assert (error == NULL);
//
//  pid = fork ();
//  g_assert (pid >= 0);
//
//  if (pid == 0)
//  {
//    ThriftServerTransport *server_transport = NULL;
//
//    g_object_unref (client_transport);
//
//    /* child listens */
//    server_transport = g_object_new (THRIFT_TYPE_SERVER_SOCKET,
//                                     "port", port,
//                                     NULL);
//    g_assert (server_transport != NULL);
//
//    thrift_server_transport_listen (server_transport, &error);
//    g_assert (error == NULL);
//
//    client_transport = g_object_new
//      (THRIFT_TYPE_BUFFERED_TRANSPORT,
//       "transport",  thrift_server_transport_accept (server_transport, &error),
//       "r_buf_size", 0,
//       "w_buf_size", sizeof data,
//       NULL);
//    g_assert (error == NULL);
//    g_assert (client_transport != NULL);
//
//    /* write exactly one character to the client */
//    g_assert (thrift_transport_write (client_transport,
//                                      &data,
//                                      sizeof data,
//                                      &error) == TRUE);
//
//    thrift_transport_flush (client_transport, &error);
//    thrift_transport_write_end (client_transport, &error);
//    thrift_transport_close (client_transport, &error);
//
//    g_object_unref (client_transport);
//    g_object_unref (server_transport);
//
//    exit (0);
//  }
//  else {
//    /* parent connects, wait a bit for the socket to be created */
//    sleep (1);
//
//    /* connect to the child */
//    thrift_transport_open (client_transport, &error);
//    g_assert (error == NULL);
//    g_assert (thrift_transport_is_open (client_transport) == TRUE);
//
//    /* thrift_transport_peek returns TRUE when the socket is open and there is
//       data available to be read */
//    g_assert (thrift_transport_peek (client_transport, &error) == TRUE);
//    g_assert (error == NULL);
//
//    /* read exactly one character from the server */
//    g_assert_cmpint (thrift_transport_read (client_transport,
//                                            &data,
//                                            sizeof data,
//                                            &error), ==, sizeof data);
//
//    /* thrift_transport_peek returns FALSE when the socket is open but there is
//       no (more) data available to be read */
//    g_assert (thrift_transport_is_open (client_transport) == TRUE);
//    g_assert (thrift_transport_peek (client_transport, &error) == FALSE);
//    g_assert (error == NULL);
//
//    thrift_transport_read_end (client_transport, &error);
//    thrift_transport_close (client_transport, &error);
//
//    g_object_unref (client_transport);
//
//    g_assert (wait (&status) == pid);
//    g_assert (status == 0);
//  }
//}

static void
thrift_ssl_socket_server (const int port)
{
	int bytes = 0;
	ThriftServerTransport *transport = NULL;
	ThriftTransport *client = NULL;
	guchar buf[10]; /* a buffer */
	guchar match[10] = TEST_DATA;

	ThriftServerSocket *tsocket = g_object_new (THRIFT_TYPE_SERVER_SOCKET,
			"port", port, NULL);

	transport = THRIFT_SERVER_TRANSPORT (tsocket);
	thrift_server_transport_listen (transport, NULL);
	client = thrift_server_transport_accept (transport, NULL);
	assert (client != NULL);

	/* read 10 bytes */
	bytes = thrift_ssl_socket_read (client, buf, 10, NULL);
	assert (bytes == 10); /* make sure we've read 10 bytes */
	assert ( memcmp(buf, match, 10) == 0 ); /* make sure what we got matches */

	/* failed read */
	recv_error = 1;
	thrift_ssl_socket_read (client, buf, 1, NULL);
	recv_error = 0;

	thrift_ssl_socket_read_end (client, NULL);
	thrift_ssl_socket_close (client, NULL);
	g_object_unref (tsocket);
	g_object_unref (client);
}

int
main(int argc, char *argv[])
{
	int retval;
#if (!GLIB_CHECK_VERSION (2, 36, 0))
	g_type_init();
#endif

	g_test_init (&argc, &argv, NULL);

	thrift_ssl_socket_initialize_openssl();

	g_test_add_func ("/testtransportsslsocket/CreateAndDestroy", test_ssl_create_and_destroy);
	g_test_add_func ("/testtransportsslsocket/CreateAndSetProperties", test_ssl_create_and_set_properties);
	g_test_add_func ("/testtransportsslsocket/OpenAndClose", test_ssl_open_and_close);
	// This test is disabled because server is not ready
	// g_test_add_func ("/testtransportsslsocket/AuthorizationManagerPinning", test_ssl_authorization_manager);
	//  g_test_add_func ("/testtransportsslsocket/Peek", test_ssl_peek);

	retval = g_test_run ();

	thrift_ssl_socket_finalize_openssl();

	return retval;
}


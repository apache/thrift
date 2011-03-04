Notes on Thrift/SSL

Author: Ping Li <pingli@facebook.com>

1. Scope

   This SSL only supports blocking mode socket I/O. It can only be used with
   TSimpleServer, TThreadedServer, and TThreadPoolServer.

2. Implementation

   There're two main classes TSSLSocketFactory and TSSLSocket. Instances of
   TSSLSocket are always created from TSSLSocketFactory.

   PosixSSLThreadFactory creates PosixSSLThread. The only difference from the
   PthreadThread type is that it cleanups OpenSSL error queue upon exiting
   the thread. Ideally, OpenSSL APIs should only be called from PosixSSLThread.

3. How to use SSL APIs

   // This is for demo. In real code, typically only one TSSLSocketFactory
   // instance is needed.
   shared_ptr<TSSLSocketFactory> getSSLSocketFactory() {
     shared_ptr<TSSLSocketFactory> factory(new TSSLSocketFactory());
     // client: load trusted certificates
     factory->loadTrustedCertificates("my-trusted-ca-certificates.pem");
     // client: optionally set your own access manager, otherwise,
     //         the default client access manager will be loaded.

     factory->loadCertificate("my-certificate-signed-by-ca.pem");
     factory->loadPrivateKey("my-private-key.pem");
     // server: optionally setup access manager
     // shared_ptr<AccessManager> accessManager(new MyAccessManager);
     // factory->access(accessManager);
     ...
   }

   // client code sample
   shared_ptr<TSSLSocketFactory> factory = getSSLSocketFactory();
   shared_ptr<TSocket> socket = factory.createSocket(host, port);
   shared_ptr<TBufferedTransport> transport(new TBufferedTransport(socket));
   ...

   // server code sample
   shared_ptr<TSSLSocketFactory> factory = getSSLSocketFactory();
   shared_ptr<TSSLServerSocket> socket(new TSSLServerSocket(port, factory));
   shared_ptr<TTransportFactory> transportFactory(new TBufferedTransportFactory));
   ...

4. AccessManager

   AccessManager defines a callback interface. It has three callback methods:

   (a) Decision verify(const sockaddr_storage& sa);
   (b) Decision verify(const string& host, const char* name, int size);
   (c) Decision verify(const sockaddr_storage& sa, const char* data, int size);

   After SSL handshake completes, additional checks are conducted. Application
   is given the chance to decide whether or not to continue the conversation
   with the remote. Application is queried through the above three "verify"
   method. They are called at different points of the verification process.

   Decisions can be one of ALLOW, DENY, and SKIP. ALLOW and DENY means the
   conversation should be continued or disconnected, respectively. ALLOW and
   DENY decision stops the verification process. SKIP means there's no decision
   based on the given input, continue the verification process.

   First, (a) is called with the remote IP. It is called once at the beginning.
   "sa" is the IP address of the remote peer.

   Then, the certificate of remote peer is loaded. SubjectAltName extensions
   are extracted and sent to application for verification. When a DNS
   subjectAltName field is extracted, (b) is called. When an IP subjectAltName
   field is extracted, (c) is called.

   The "host" in (b) is the value from TSocket::getHost() if this is a client
   side socket, or TSocket::getPeerHost() if this is a server side socket. The
   reason is client side socket initiates the connection. TSocket::getHost()
   is the remote host name. On server side, the remote host name is unknown
   unless it's retrieved through TSocket::getPeerHost(). Either way, "host"
   should be the remote host name. Keep in mind, if TSocket::getPeerHost()
   failed, it would return the remote host name in numeric format.

   If all subjectAltName extensions were "skipped", the common name field would
   be checked. It is sent to application through (c), where "sa" is the remote
   IP address. "data" is the IP address extracted from subjectAltName IP
   extension, and "size" is the length of the extension data.

   If any of the above "verify" methods returned a decision ALLOW or DENY, the
   verification process would be stopped.

   If any of the above "verify" methods returned SKIP, that decision would be
   ignored and the verification process would move on till the last item is
   examined. At that point, if there's still no decision, the connection is
   terminated.

   Thread safety, an access manager should not store state information if it's
   to be used by many SSL sockets.

5. SIGPIPE signal

   Applications running OpenSSL over network connections may crash if SIGPIPE
   is not ignored. This happens when they receive a connection reset by remote
   peer exception, which somehow triggers a SIGPIPE signal. If not handled,
   this signal would kill the application.

6. How to run test client/server in SSL mode

   The server expects the followings from the current working directory,
   - "server-certificate.pem"
   - "server-private-key.pem"

   The client loads "trusted-ca-certificate.pem" from current directory.

   The file names are hard coded in the source code. You need to create these
   certificates before you can run the test code in SSL mode. Make sure at least
   one of the followings is included in "server-certificate.pem",
   - subjectAltName, DNS localhost
   - subjectAltName, IP  127.0.0.1
   - common name,    localhost

   Run,
   - "./test_server --ssl" to start server
   - "./test_client --ssl" to run client

   If "-h <host>" is used to run client, the above "localhost" in the above
   server-certificate.pem has to be replaced with that host name.

7. TSSLSocketFactory::randomize()

   The default implementation of OpenSSLSocketFactory::randomize() simply calls
   OpenSSL's RAND_poll() when OpenSSL library is first initialized.

   The PRNG seed is key to the application security. This method should be
   overridden if it's not strong enough for you.

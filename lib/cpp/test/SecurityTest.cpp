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

#define BOOST_TEST_MODULE SecurityTest
#include <boost/test/unit_test.hpp>
#include <condition_variable>
#include <fstream>
#include <memory>
#include <mutex>
#include <thread>
#include <openssl/opensslv.h>
#include <thrift/transport/TSSLServerSocket.h>
#include <thrift/transport/TSSLSocket.h>
#include <thrift/transport/TTransport.h>
#include <vector>
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

using apache::thrift::transport::TSSLServerSocket;
using apache::thrift::transport::SSLContext;
using apache::thrift::transport::SSLContextFactory;
using apache::thrift::transport::TSSLException;
using apache::thrift::transport::TServerTransport;
using apache::thrift::transport::TSSLSocket;
using apache::thrift::transport::TSSLSocketFactory;
using apache::thrift::transport::TTransport;
using apache::thrift::transport::TTransportException;
using apache::thrift::transport::TTransportFactory;

using std::bind;
using std::shared_ptr;

std::string keyDir;
std::string certFile(const std::string& filename)
{
    return keyDir + "/" + filename;
}
std::mutex gMutex;

bool fileExists(const std::string& path)
{
    return std::ifstream(path.c_str()).good();
}

struct GlobalFixture
{
    GlobalFixture()
    {
        using namespace boost::unit_test::framework;
    for (int i = 0; i < master_test_suite().argc; ++i)
    {
      BOOST_TEST_MESSAGE("argv[" << i << "] = \"" << master_test_suite().argv[i] << "\"");
    }

    #ifdef __linux__
    // OpenSSL calls send() without MSG_NOSIGPIPE so writing to a socket that has
    // disconnected can cause a SIGPIPE signal...
    signal(SIGPIPE, SIG_IGN);
    #endif

    TSSLSocketFactory::setManualOpenSSLInitialization(true);
    apache::thrift::transport::initializeOpenSSL();

    keyDir = "../../../test/keys";
    if (!fileExists(certFile("server.crt")))
    {
      keyDir = master_test_suite().argv[master_test_suite().argc - 1];
      if (!fileExists(certFile("server.crt")))
      {
        throw std::invalid_argument("The last argument to this test must be the directory containing the test certificate(s).");
      }
    }
    }

    virtual ~GlobalFixture()
    {
    apache::thrift::transport::cleanupOpenSSL();
#ifdef __linux__
    signal(SIGPIPE, SIG_DFL);
#endif
    }
};

#if (BOOST_VERSION >= 105900)
BOOST_GLOBAL_FIXTURE(GlobalFixture);
#else
BOOST_GLOBAL_FIXTURE(GlobalFixture)
#endif

struct SecurityFixture
{
    void server(apache::thrift::transport::SSLProtocol protocol)
    {
        try
        {
            std::unique_lock<std::mutex> lock(mMutex);

            shared_ptr<TSSLSocketFactory> pServerSocketFactory;
            shared_ptr<TSSLServerSocket> pServerSocket;

            pServerSocketFactory.reset(new TSSLSocketFactory(static_cast<apache::thrift::transport::SSLProtocol>(protocol)));
            #if OPENSSL_VERSION_NUMBER >= 0x10100000L
                // OpenSSL 1.1.0 introduced @SECLEVEL. Modern distributions limit TLS 1.0/1.1
                // to @SECLEVEL=0 or 1, so specify it to test all combinations.
                pServerSocketFactory->ciphers("ALL:!ADH:!LOW:!EXP:!MD5:@SECLEVEL=0:@STRENGTH");
            #else
                pServerSocketFactory->ciphers("ALL:!ADH:!LOW:!EXP:!MD5:@STRENGTH");
            #endif
            pServerSocketFactory->loadCertificate(certFile("server.crt").c_str());
            pServerSocketFactory->loadPrivateKey(certFile("server.key").c_str());
            pServerSocketFactory->server(true);
            pServerSocket.reset(new TSSLServerSocket("localhost", 0, pServerSocketFactory));
            shared_ptr<TTransport> connectedClient;

            try
            {
                pServerSocket->listen();
                mPort = pServerSocket->getPort();
                mCVar.notify_one();
                lock.unlock();

                connectedClient = pServerSocket->accept();
                uint8_t buf[2];
                buf[0] = 'O';
                buf[1] = 'K';
                connectedClient->write(&buf[0], 2);
                connectedClient->flush();
            }

            catch (apache::thrift::transport::TTransportException& ex)
            {
                std::lock_guard<std::mutex> lock(gMutex);
                BOOST_TEST_MESSAGE("SRV " << std::this_thread::get_id() << " Exception: " << ex.what());
            }

            if (connectedClient)
            {
                connectedClient->close();
                connectedClient.reset();
            }

            pServerSocket->close();
            pServerSocket.reset();
        }
        catch (std::exception& ex)
        {
            BOOST_FAIL(typeid(ex).name() << ": " << ex.what());
        }
    }

    void client(apache::thrift::transport::SSLProtocol protocol)
    {
        try
        {
            shared_ptr<TSSLSocketFactory> pClientSocketFactory;
            shared_ptr<TSSLSocket> pClientSocket;

            try
            {
                pClientSocketFactory.reset(new TSSLSocketFactory(static_cast<apache::thrift::transport::SSLProtocol>(protocol)));
                pClientSocketFactory->authenticate(true);
                #if OPENSSL_VERSION_NUMBER >= 0x10100000L
                    // OpenSSL 1.1.0 introduced @SECLEVEL. Modern distributions limit TLS 1.0/1.1
                    // to @SECLEVEL=0 or 1, so specify it to test all combinations.
                    pClientSocketFactory->ciphers("ALL:!ADH:!LOW:!EXP:!MD5:@SECLEVEL=0");
                #endif
                pClientSocketFactory->loadCertificate(certFile("client.crt").c_str());
                pClientSocketFactory->loadPrivateKey(certFile("client.key").c_str());
                pClientSocketFactory->loadTrustedCertificates(certFile("CA.pem").c_str());
                pClientSocket = pClientSocketFactory->createSocket("localhost", mPort);
                pClientSocket->open();

                uint8_t buf[3];
                buf[0] = 0;
                buf[1] = 0;
                BOOST_CHECK_EQUAL(2, pClientSocket->read(&buf[0], 2));
                BOOST_CHECK_EQUAL(0, memcmp(&buf[0], "OK", 2));
                mConnected = true;
            }
            catch (apache::thrift::transport::TTransportException& ex)
            {
                std::lock_guard<std::mutex> lock(gMutex);
                BOOST_TEST_MESSAGE("CLI " << std::this_thread::get_id() << " Exception: " << ex.what());
            }

            if (pClientSocket)
            {
                pClientSocket->close();
                pClientSocket.reset();
            }
        }
        catch (std::exception& ex)
        {
            BOOST_FAIL(typeid(ex).name() << ": " << ex.what());
        }
    }

    static const char *protocol2str(size_t protocol)
    {
        static const char *strings[apache::thrift::transport::LATEST + 1] =
        {
                "SSLTLS",
                "SSLv2",
                "SSLv3",
                "TLSv1_0",
                "TLSv1_1",
                "TLSv1_2"
        };
        return strings[protocol];
    }

    std::mutex mMutex;
    std::condition_variable mCVar;
    int mPort;
    bool mConnected;
};

BOOST_FIXTURE_TEST_SUITE(BOOST_TEST_MODULE, SecurityFixture)

BOOST_AUTO_TEST_CASE(default_ssl_context_options)
{
    apache::thrift::transport::SSLContext context;
    const auto options = SSL_CTX_get_options(context.get());

    if (SSL_OP_NO_SSLv2 != 0) {
        BOOST_CHECK((options & SSL_OP_NO_SSLv2) != 0);
    }
    if (SSL_OP_NO_SSLv3 != 0) {
        BOOST_CHECK((options & SSL_OP_NO_SSLv3) != 0);
    }
    if (SSL_OP_NO_TLSv1 != 0) {
        BOOST_CHECK((options & SSL_OP_NO_TLSv1) != 0);
    }
    if (SSL_OP_NO_TLSv1_1 != 0) {
        BOOST_CHECK((options & SSL_OP_NO_TLSv1_1) != 0);
    }
}

BOOST_AUTO_TEST_CASE(custom_ssl_context_options)
{
    class CustomSSLContext : public apache::thrift::transport::SSLContext
    {
    public:
        CustomSSLContext() : SSLContext()
        {
            SSL_CTX_clear_options(get(), SSL_OP_NO_TLSv1_1);
        }
    };

    std::shared_ptr<apache::thrift::transport::SSLContext> context;
    TSSLSocketFactory factory([&context]() {
        context = std::make_shared<CustomSSLContext>();
        return context;
    });
    const auto options = SSL_CTX_get_options(context->get());

    if (SSL_OP_NO_TLSv1 != 0) {
        BOOST_CHECK((options & SSL_OP_NO_TLSv1) != 0);
    }
    if (SSL_OP_NO_TLSv1_1 != 0) {
        BOOST_CHECK((options & SSL_OP_NO_TLSv1_1) == 0);
    }
    context.reset();
}

BOOST_AUTO_TEST_CASE(wrapped_ssl_context)
{
    SSL_CTX* raw = SSL_CTX_new(TLS_method());
    BOOST_REQUIRE(raw != nullptr);
    SSL_CTX_set_mode(raw, SSL_MODE_AUTO_RETRY);

    std::shared_ptr<SSLContext> context;
    TSSLSocketFactory factory([&context, raw]() {
        context = std::make_shared<SSLContext>(raw);
        return context;
    });
    BOOST_CHECK(context->get() == raw);
    context.reset();
}

BOOST_AUTO_TEST_CASE(wrapped_ssl_context_null)
{
    try
    {
        (void)std::make_shared<SSLContext>(nullptr);
        BOOST_FAIL("Expected null SSL_CTX to throw");
    }
    catch (const TSSLException& ex)
    {
        BOOST_CHECK_EQUAL("SSLContext: ctx must not be null", std::string(ex.what()));
    }
}

BOOST_AUTO_TEST_CASE(custom_ssl_context_factory_validation)
{
    try
    {
        SSLContextFactory contextFactory;
        TSSLSocketFactory factory(contextFactory);
        BOOST_FAIL("Expected empty SSLContextFactory to throw");
    }
    catch (const TSSLException& ex)
    {
        BOOST_CHECK_EQUAL("SSLContextFactory must not be empty", std::string(ex.what()));
    }

    try
    {
        TSSLSocketFactory factory([]() {
            return std::shared_ptr<apache::thrift::transport::SSLContext>();
        });
        BOOST_FAIL("Expected null SSLContextFactory result to throw");
    }
    catch (const TSSLException& ex)
    {
        BOOST_CHECK_EQUAL("SSLContextFactory must not return null", std::string(ex.what()));
    }
}

BOOST_AUTO_TEST_CASE(explicit_protocol_version_window)
{
    // An explicitly requested protocol has to pin the context to exactly that
    // version, the way the per-version TLSv1_x_method() functions it replaces
    // did. Checking the window rather than just "a context came back" is what
    // catches a remapping that silently leaves the floor to the library
    // default, which would make an explicit request laxer than SSLTLS.
    //
    // SSL_CTX_get_min_proto_version() and SSL_CTX_get_max_proto_version() were
    // added in OpenSSL 1.1.1a.
#if OPENSSL_VERSION_NUMBER >= 0x1010101fL && !defined(LIBRESSL_VERSION_NUMBER)
    struct Expectation
    {
        apache::thrift::transport::SSLProtocol protocol;
        const char* name;
        int version;
    };

    const Expectation expectations[] = {
        { apache::thrift::transport::TLSv1_0, "TLSv1_0", TLS1_VERSION   },
        { apache::thrift::transport::TLSv1_1, "TLSv1_1", TLS1_1_VERSION },
        { apache::thrift::transport::TLSv1_2, "TLSv1_2", TLS1_2_VERSION },
        { apache::thrift::transport::LATEST,  "LATEST",  TLS1_2_VERSION }
    };

    for (const auto& expected : expectations)
    {
        BOOST_TEST_MESSAGE("TEST: protocol = " << expected.name);
        apache::thrift::transport::SSLContext context(expected.protocol);
        BOOST_CHECK_EQUAL(expected.version, SSL_CTX_get_min_proto_version(context.get()));
        BOOST_CHECK_EQUAL(expected.version, SSL_CTX_get_max_proto_version(context.get()));
    }
#endif

#if defined(OPENSSL_NO_SSL3) || OPENSSL_VERSION_NUMBER >= 0x40000000L
    // SSLv3 is not available in this build of the library. That has to be
    // reported rather than quietly satisfied with some other version.
    try
    {
        apache::thrift::transport::SSLContext context(apache::thrift::transport::SSLv3);
        BOOST_FAIL("Expected unavailable SSLv3 to throw");
    }
    catch (const TSSLException&)
    {
    }
#endif
}

BOOST_AUTO_TEST_CASE(ssl_security_matrix)
{
    try
    {
        // matrix of connection success between client and server with different SSLProtocol selections
        static_assert(apache::thrift::transport::LATEST == 5, "Mismatch in assumed number of ssl protocols");
        bool matrix[apache::thrift::transport::LATEST + 1][apache::thrift::transport::LATEST + 1] =
        {
    //   server    = SSLTLS   SSLv2    SSLv3    TLSv1_0  TLSv1_1  TLSv1_2
    // client
    /* SSLTLS  */  { true,    false,   false,   false,   false,   true    },
    /* SSLv2   */  { false,   false,   false,   false,   false,   false   },
    /* SSLv3   */  { false,   false,   true,    false,   false,   false   },
    /* TLSv1_0 */  { false,   false,   false,   true,    false,   false   },
    /* TLSv1_1 */  { false,   false,   false,   false,   true,    false   },
    /* TLSv1_2 */  { true,    false,   false,   false,   false,   true    }
        };

        for (size_t si = 0; si <= apache::thrift::transport::LATEST; ++si)
        {
            for (size_t ci = 0; ci <= apache::thrift::transport::LATEST; ++ci)
            {
                if (si == 1 || ci == 1)
                {
                    // Skip all SSLv2 cases - protocol not supported
                    continue;
                }

#if defined(OPENSSL_NO_SSL3) || OPENSSL_VERSION_NUMBER >= 0x40000000L
                if (si == 2 || ci == 2)
                {
                    // Skip all SSLv3 cases - protocol not supported
                    continue;
                }
#endif

                std::unique_lock<std::mutex> lock(mMutex);

                BOOST_TEST_MESSAGE("TEST: Server = " << protocol2str(si) << ", Client = " << protocol2str(ci));

                mConnected = false;
                std::thread serverThread(bind(&SecurityFixture::server, this, static_cast<apache::thrift::transport::SSLProtocol>(si)));
                mCVar.wait(lock);           // wait for listen() to succeed
                lock.unlock();
                std::thread clientThread(bind(&SecurityFixture::client, this, static_cast<apache::thrift::transport::SSLProtocol>(ci)));
                clientThread.join();
                serverThread.join();

                BOOST_CHECK_MESSAGE(mConnected == matrix[ci][si],
                        "      Server = " << protocol2str(si) << ", Client = " << protocol2str(ci)
                            << " expected mConnected == " << matrix[ci][si] << " but was " << mConnected);
            }
        }
    }
    catch (std::exception& ex)
    {
        BOOST_FAIL(typeid(ex).name() << ": " << ex.what());
    }
}

BOOST_AUTO_TEST_SUITE_END()

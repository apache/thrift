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

#define BOOST_TEST_MODULE SecurityFromBufferTest
#include <boost/test/unit_test.hpp>
#include <condition_variable>
#include <stdexcept>
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

using apache::thrift::transport::TServerTransport;
using apache::thrift::transport::TSSLServerSocket;
using apache::thrift::transport::TSSLSocket;
using apache::thrift::transport::TSSLSocketFactory;
using apache::thrift::transport::TTransport;
using apache::thrift::transport::TTransportException;
using apache::thrift::transport::TTransportFactory;

using std::bind;
using std::shared_ptr;

std::string keyDir;
std::string certFile(const std::string& filename) {
  return keyDir + "/" + filename;
}
bool fileExists(const std::string& path) {
  return std::ifstream(path.c_str()).good();
}
std::string certString(const std::string& filename) {
  std::ifstream ifs(certFile(filename).c_str());
  if(!ifs.is_open() || !ifs.good()) {
    throw(std::runtime_error("Failed to open key file " + filename + " for reading"));
  }
  std::stringstream buffer;
  buffer << ifs.rdbuf();
  return buffer.str();
}
std::mutex gMutex;

struct GlobalFixture {
  GlobalFixture() {
    using namespace boost::unit_test::framework;
    for (int i = 0; i < master_test_suite().argc; ++i) {
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
    if (!fileExists(certFile("server.crt"))) {
      keyDir = master_test_suite().argv[master_test_suite().argc - 1];
      if (!fileExists(certFile("server.crt"))) {
        throw std::invalid_argument("The last argument to this test must be the directory containing the test certificate(s).");
      }
    }
  }

  virtual ~GlobalFixture() {
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

struct SecurityFromBufferFixture {
  void server(apache::thrift::transport::SSLProtocol protocol) {
    try {
      std::unique_lock<std::mutex> lock(mMutex);

      shared_ptr<TSSLSocketFactory> pServerSocketFactory;
      shared_ptr<TSSLServerSocket> pServerSocket;

      pServerSocketFactory.reset(new TSSLSocketFactory(static_cast<apache::thrift::transport::SSLProtocol>(protocol)));
      #if OPENSSL_VERSION_NUMBER >= 0x10100000L
          // OpenSSL 1.1.0 introduced @SECLEVEL. Modern distributions limit TLS 1.0/1.1
          // to @SECLEVEL=0 or 1, so specify it to test all combinations.
          pServerSocketFactory->ciphers("ALL:!ADH:!LOW:!EXP:!MD5:@SECLEVEL=0");
      #else
          pServerSocketFactory->ciphers("ALL:!ADH:!LOW:!EXP:!MD5:@STRENGTH");
      #endif
      pServerSocketFactory->loadCertificateFromBuffer(certString("server.crt").c_str());
      pServerSocketFactory->loadPrivateKeyFromBuffer(certString("server.key").c_str());
      pServerSocketFactory->server(true);
      pServerSocket.reset(new TSSLServerSocket("localhost", 0, pServerSocketFactory));
      shared_ptr<TTransport> connectedClient;

      try {
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

      catch (apache::thrift::transport::TTransportException& ex) {
        std::lock_guard<std::mutex> lock(gMutex);
        BOOST_TEST_MESSAGE("SRV " << std::this_thread::get_id() << " Exception: " << ex.what());
      }

      if (connectedClient) {
        connectedClient->close();
        connectedClient.reset();
      }

      pServerSocket->close();
      pServerSocket.reset();
    } catch (std::exception& ex) {
      BOOST_FAIL(typeid(ex).name() << ": " << ex.what());
    }
  }

  void client(apache::thrift::transport::SSLProtocol protocol) {
    try {
      shared_ptr<TSSLSocketFactory> pClientSocketFactory;
      shared_ptr<TSSLSocket> pClientSocket;

      try {
        pClientSocketFactory.reset(new TSSLSocketFactory(static_cast<apache::thrift::transport::SSLProtocol>(protocol)));
        pClientSocketFactory->authenticate(true);
        #if OPENSSL_VERSION_NUMBER >= 0x10100000L
            // OpenSSL 1.1.0 introduced @SECLEVEL. Modern distributions limit TLS 1.0/1.1
            // to @SECLEVEL=0 or 1, so specify it to test all combinations.
            pClientSocketFactory->ciphers("ALL:!ADH:!LOW:!EXP:!MD5:@SECLEVEL=0");
        #endif
        pClientSocketFactory->loadCertificateFromBuffer(certString("client.crt").c_str());
        pClientSocketFactory->loadPrivateKeyFromBuffer(certString("client.key").c_str());
        pClientSocketFactory->loadTrustedCertificatesFromBuffer(certString("CA.pem").c_str());
        pClientSocket = pClientSocketFactory->createSocket("localhost", mPort);
        pClientSocket->open();

        uint8_t buf[3];
        buf[0] = 0;
        buf[1] = 0;
        BOOST_CHECK_EQUAL(2, pClientSocket->read(&buf[0], 2));
        BOOST_CHECK_EQUAL(0, memcmp(&buf[0], "OK", 2));
        mConnected = true;
      } catch (apache::thrift::transport::TTransportException& ex) {
        std::lock_guard<std::mutex> lock(gMutex);
        BOOST_TEST_MESSAGE("CLI " << std::this_thread::get_id() << " Exception: " << ex.what());
      }

      if (pClientSocket) {
        pClientSocket->close();
        pClientSocket.reset();
      }
    } catch (std::exception& ex) {
      BOOST_FAIL(typeid(ex).name() << ": " << ex.what());
    }
  }

  static const char* protocol2str(size_t protocol) {
    static const char* strings[apache::thrift::transport::LATEST + 1]
        = {"SSLTLS", "SSLv2", "SSLv3", "TLSv1_0", "TLSv1_1", "TLSv1_2"};
    return strings[protocol];
  }

  std::mutex mMutex;
  std::condition_variable mCVar;
  int mPort;
  bool mConnected;
};

BOOST_FIXTURE_TEST_SUITE(BOOST_TEST_MODULE, SecurityFromBufferFixture)

BOOST_AUTO_TEST_CASE(ssl_security_matrix) {
  try {
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

    for (size_t si = 0; si <= apache::thrift::transport::LATEST; ++si) {
      for (size_t ci = 0; ci <= apache::thrift::transport::LATEST; ++ci) {
        if (si == 1 || ci == 1) {
          // Skip all SSLv2 cases - protocol not supported
          continue;
        }

#if defined(OPENSSL_NO_SSL3) || OPENSSL_VERSION_NUMBER >= 0x40000000L
        if (si == 2 || ci == 2) {
          // Skip all SSLv3 cases - protocol not supported
          continue;
        }
#endif

        std::unique_lock<std::mutex> lock(mMutex);

        BOOST_TEST_MESSAGE("TEST: Server = " << protocol2str(si) << ", Client = " << protocol2str(ci));

        mConnected = false;
        std::thread serverThread(bind(&SecurityFromBufferFixture::server, this,
                                      static_cast<apache::thrift::transport::SSLProtocol>(si)));
        mCVar.wait(lock); // wait for listen() to succeed
        lock.unlock();
        std::thread clientThread(bind(&SecurityFromBufferFixture::client, this,
                                      static_cast<apache::thrift::transport::SSLProtocol>(ci)));
        clientThread.join();
        serverThread.join();

        BOOST_CHECK_MESSAGE(mConnected == matrix[ci][si],
            "      Server = " << protocol2str(si) << ", Client = " << protocol2str(ci)
                << " expected mConnected == " << matrix[ci][si] << " but was " << mConnected);
      }
    }
  } catch (std::exception& ex) {
    BOOST_FAIL(typeid(ex).name() << ": " << ex.what());
  }
}

BOOST_AUTO_TEST_SUITE_END()

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

#define BOOST_TEST_MODULE TSSLSocketMatchNameTest
#include <boost/test/unit_test.hpp>
#include <boost/thread.hpp>
#include <thrift/transport/TSSLSocket.h>
#include <thrift/transport/TSSLServerSocket.h>
#include <thrift/transport/TTransportException.h>

#include <openssl/pem.h>
#include <openssl/x509v3.h>

#include <string>

using apache::thrift::transport::AccessManager;
using apache::thrift::transport::DefaultClientAccessManager;
using apache::thrift::transport::TSSLSocket;
using apache::thrift::transport::TSSLSocketFactory;
using apache::thrift::transport::TSSLServerSocket;
using apache::thrift::transport::TTransport;
using apache::thrift::transport::TTransportException;

BOOST_AUTO_TEST_SUITE(TSSLSocketMatchNameTest)

// Helper: ALLOW means match, SKIP means no match.
static bool allows(const std::string& host, const std::string& pattern) {
  DefaultClientAccessManager mgr;
  return mgr.verify(host, pattern.c_str(), static_cast<int>(pattern.size()))
         == AccessManager::ALLOW;
}

BOOST_AUTO_TEST_CASE(standard_wildcard_matches) {
  BOOST_CHECK(allows("foo.example.com", "*.example.com"));
  BOOST_CHECK(allows("FOO.EXAMPLE.COM", "*.example.com"));  // case-insensitive
  BOOST_CHECK(allows("a.b.c.example.com", "*.b.c.example.com"));  // leftmost wildcard
}

BOOST_AUTO_TEST_CASE(exact_match) {
  BOOST_CHECK(allows("example.com", "example.com"));
  BOOST_CHECK(allows("foo.example.com", "foo.example.com"));
}

BOOST_AUTO_TEST_CASE(wildcard_must_not_span_labels) {
  BOOST_CHECK(!allows("foo.bar.example.com", "*.example.com"));
}

BOOST_AUTO_TEST_CASE(wildcard_must_be_in_leftmost_label) {
  // RFC 6125 §6.4.3: wildcard must not appear outside the leftmost label.
  BOOST_CHECK(!allows("example.foo.com",  "example.*.com"));
  BOOST_CHECK(!allows("a.evil.com",       "a.ev*.com"));
}

BOOST_AUTO_TEST_CASE(no_suffix_bypass) {
  BOOST_CHECK(!allows("evil.com.attacker.com", "evil.com"));
}


// ---------------------------------------------------------------------------
// Which identity in the certificate is consulted, and in what order.
//
// The matcher above answers "does this name match this pattern". Which name is
// offered to it is decided in TSSLSocket::authorize(), and that is a separate
// question: RFC 6125 6.4.4 and RFC 9525 6.3 make a dNSName in the
// subjectAltName the identity, so commonName may only be reached by a
// certificate that carries no dNSName at all. Testing that needs a real
// handshake, so these cases run one, against certificates built here rather
// than against fixtures, since the point is the relationship between the two
// fields.
// ---------------------------------------------------------------------------

namespace {

struct Pem {
  std::string certificate;
  std::string privateKey;
};

std::string readBio(BIO* bio) {
  char* data = nullptr;
  const long length = BIO_get_mem_data(bio, &data);
  return std::string(data, static_cast<size_t>(length));
}

// A self-signed certificate for `commonName`, carrying `subjectAltName` when
// that is not empty (in OpenSSL's config syntax, e.g. "DNS:other.example").
Pem selfSigned(const std::string& commonName, const std::string& subjectAltName) {
  EVP_PKEY* key = nullptr;
  EVP_PKEY_CTX* keyCtx = EVP_PKEY_CTX_new_id(EVP_PKEY_RSA, nullptr);
  BOOST_REQUIRE(keyCtx != nullptr);
  BOOST_REQUIRE(EVP_PKEY_keygen_init(keyCtx) > 0);
  BOOST_REQUIRE(EVP_PKEY_CTX_set_rsa_keygen_bits(keyCtx, 2048) > 0);
  BOOST_REQUIRE(EVP_PKEY_keygen(keyCtx, &key) > 0);
  EVP_PKEY_CTX_free(keyCtx);

  X509* cert = X509_new();
  BOOST_REQUIRE(cert != nullptr);
  X509_set_version(cert, 2);
  ASN1_INTEGER_set(X509_get_serialNumber(cert), 1);
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
  X509_gmtime_adj(X509_getm_notBefore(cert), 0);
  X509_gmtime_adj(X509_getm_notAfter(cert), 3600);
#else
  X509_gmtime_adj(X509_get_notBefore(cert), 0);
  X509_gmtime_adj(X509_get_notAfter(cert), 3600);
#endif
  X509_set_pubkey(cert, key);

  X509_NAME* subject = X509_get_subject_name(cert);
  X509_NAME_add_entry_by_txt(subject, "CN", MBSTRING_ASC,
                             reinterpret_cast<const unsigned char*>(commonName.c_str()),
                             -1, -1, 0);
  X509_set_issuer_name(cert, subject);

  if (!subjectAltName.empty()) {
    X509V3_CTX extCtx;
    X509V3_set_ctx_nodb(&extCtx);
    X509V3_set_ctx(&extCtx, cert, cert, nullptr, nullptr, 0);
    X509_EXTENSION* ext = X509V3_EXT_conf_nid(nullptr, &extCtx, NID_subject_alt_name,
                                              const_cast<char*>(subjectAltName.c_str()));
    BOOST_REQUIRE(ext != nullptr);
    X509_add_ext(cert, ext, -1);
    X509_EXTENSION_free(ext);
  }

  BOOST_REQUIRE(X509_sign(cert, key, EVP_sha256()) > 0);

  Pem pem;
  BIO* bio = BIO_new(BIO_s_mem());
  PEM_write_bio_X509(bio, cert);
  pem.certificate = readBio(bio);
  BIO_free(bio);

  bio = BIO_new(BIO_s_mem());
  PEM_write_bio_PrivateKey(bio, key, nullptr, nullptr, 0, nullptr, nullptr);
  pem.privateKey = readBio(bio);
  BIO_free(bio);

  X509_free(cert);
  EVP_PKEY_free(key);
  return pem;
}

// Serves exactly one connection with the given identity, and reports whether
// the client was accepted by the *client's* own checks -- which is what the
// return value of connectTo() below says.
class OneShotServer {
public:
  explicit OneShotServer(const Pem& pem) {
    auto factory = std::make_shared<TSSLSocketFactory>();
    factory->server(true);
    factory->loadCertificateFromBuffer(pem.certificate.c_str());
    factory->loadPrivateKeyFromBuffer(pem.privateKey.c_str());
    socket_ = std::make_shared<TSSLServerSocket>("127.0.0.1", 0, factory);
    socket_->listen();
    port_ = socket_->getPort();
    thread_ = boost::thread(&OneShotServer::run, this);
  }

  ~OneShotServer() {
    // interrupt() before close(): a client that never arrives, or that walks
    // away mid-handshake, leaves accept() blocked, and close() alone does not
    // wake it.
    socket_->interrupt();
    socket_->close();
    thread_.join();
  }

  int port() const { return port_; }

private:
  void run() {
    try {
      std::shared_ptr<TTransport> client = socket_->accept();
      if (client) {
        // The client has to read something, or it never handshakes at all --
        // see connectAccepts().
        uint8_t ok[2] = {'O', 'K'};
        client->write(ok, sizeof(ok));
        client->flush();
        client->close();
      }
    } catch (...) {
      // A client that refuses the certificate leaves the handshake unfinished;
      // that is the case under test, not a failure of the server.
    }
  }

  std::shared_ptr<TSSLServerSocket> socket_;
  boost::thread thread_;
  int port_{0};
};

// true when the client accepted the server's identity.
//
// It has to read, not just open: TSSLSocket::open() performs no handshake at
// all -- it is deferred to the first I/O through checkHandshake() -- so a test
// that only opens and closes passes whatever the certificate says, including
// against unfixed code. That mistake was made here once already.
bool connectAccepts(const Pem& pem, int port) {
  auto factory = std::make_shared<TSSLSocketFactory>();
  // Self-signed, so trusting the certificate itself puts the chain beyond
  // question and leaves only the name check under test.
  factory->loadTrustedCertificatesFromBuffer(pem.certificate.c_str());
  std::shared_ptr<TSSLSocket> client = factory->createSocket("127.0.0.1", port);
  try {
    client->open();
    uint8_t buffer[2];
    client->readAll(buffer, sizeof(buffer));
    client->close();
    return true;
  } catch (const TTransportException&) {
    try {
      client->close();
    } catch (...) {
    }
    return false;
  }
}

} // namespace

BOOST_AUTO_TEST_CASE(subject_alt_name_matching_is_accepted) {
  // Control: without this, the assertion below is satisfied by a handshake
  // that fails for some unrelated reason.
  Pem pem = selfSigned("no.match.example", "IP:127.0.0.1");
  OneShotServer server(pem);
  BOOST_CHECK(connectAccepts(pem, server.port()));
}

BOOST_AUTO_TEST_CASE(common_name_is_not_consulted_when_a_dns_san_is_present) {
  // The commonName is exactly what is being connected to, and would be
  // accepted on its own; the certificate says otherwise in its subjectAltName,
  // and the subjectAltName is the one that counts.
  Pem pem = selfSigned("127.0.0.1", "DNS:other.example");
  OneShotServer server(pem);
  BOOST_CHECK(!connectAccepts(pem, server.port()));
}

BOOST_AUTO_TEST_CASE(common_name_still_answers_when_no_san_is_present) {
  // Certificates predating subjectAltName must keep working.
  Pem pem = selfSigned("127.0.0.1", "");
  OneShotServer server(pem);
  BOOST_CHECK(connectAccepts(pem, server.port()));
}

BOOST_AUTO_TEST_SUITE_END()

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
module thrift.internal.ssl;

import core.memory : GC;
import core.stdc.config;
import core.stdc.errno : errno;
import core.stdc.string : strerror;
import deimos.openssl.err;
import deimos.openssl.ssl;
import deimos.openssl.x509v3;
import std.array : empty, appender;
import std.conv : to;
import std.socket : Address, parseAddress, SocketException;
import thrift.transport.ssl;

version (use_openssl_3) {
  // OpenSSL 3.0 renamed SSL_get_peer_certificate to SSL_get1_peer_certificate
  // and left the old name only as a header macro, so it is no longer an
  // exported symbol the pinned deimos bindings can reach. Declare the current
  // name here until those bindings carry it.
  private extern (C) X509* SSL_get1_peer_certificate(const(SSL)* ssl);
}

/**
 * Checks if the peer is authorized after the SSL handshake has been
 * completed on the given connection and throws an TSSLException if not.
 *
 * Params:
 *   ssl = The SSL connection to check.
 *   accessManager = The access manager to check the peer against.
 *   peerAddress = The (IP) address of the peer.
 *   hostName = The host name of the peer.
 *   serverSide = Whether ssl is the server end of the connection.
 */
void authorize(SSL* ssl, TAccessManager accessManager,
  Address peerAddress, lazy string hostName, bool serverSide
) {
  alias TAccessManager.Decision Decision;

  auto rc = SSL_get_verify_result(ssl);
  if (rc != X509_V_OK) {
    throw new TSSLException("SSL_get_verify_result(): " ~
      to!string(X509_verify_cert_error_string(rc)));
  }

  version (use_openssl_3) {
    auto cert = SSL_get1_peer_certificate(ssl);
  } else {
    auto cert = SSL_get_peer_certificate(ssl);
  }
  if (cert is null) {
    // Certificate is not present.
    if (SSL_get_verify_mode(ssl) & SSL_VERIFY_FAIL_IF_NO_PEER_CERT) {
      throw new TSSLException(
        "Authorize: Required certificate not present.");
    }

    // If we don't have an access manager set, we don't intend to authorize
    // the client, so everything's fine.
    if (accessManager) {
      throw new TSSLException(
        "Authorize: Certificate required for authorization.");
    }
    return;
  }

  if (accessManager is null) {
    // No access manager set, can return immediately as the cert is valid
    // and all peers are authorized.
    X509_free(cert);
    return;
  }

  // both certificate and access manager are present
  auto decision = accessManager.verify(peerAddress);

  if (decision != Decision.SKIP) {
    X509_free(cert);
    if (decision != Decision.ALLOW) {
      throw new TSSLException("Authorize: Access denied based on remote IP.");
    }
    return;
  }

  // In client mode the iPAddress entries are consulted only when the host the
  // socket was opened with is itself an address literal (RFC 9525 6.1.1): an
  // address that a host name resolved to is not a reference identifier
  // (RFC 9525 2), so a client that was asked for a host name is answered by the
  // dNSName entries and the common name. A socket opened without a host, on an
  // existing connection, keeps the peer address as its only reference
  // identifier.
  bool consultAddressEntries = true;
  if (!serverSide) {
    auto clientHost = hostName;
    if (!clientHost.empty) {
      consultAddressEntries = isAddressLiteral(clientHost);
    }
  }

  // Check subjectAltName(s), if present.
  auto alternatives = cast(STACK_OF!(GENERAL_NAME)*)
    X509_get_ext_d2i(cert, NID_subject_alt_name, null, null);

  version(use_openssl_1_0_x) {
    enum _GEN_DNS = GENERAL_NAME.GEN_DNS;
    enum _GEN_IPADD = GENERAL_NAME.GEN_IPADD;
  } else version(use_openssl_1_1_x) {
    enum _GEN_DNS = GEN_DNS;
    enum _GEN_IPADD = GEN_IPADD;
  } else version(use_openssl_3) {
    enum _GEN_DNS = GENERAL_NAME.GEN_DNS;
    enum _GEN_IPADD = GENERAL_NAME.GEN_IPADD;
  } else {
    static assert(false, `Must have version use_openssl_1_0_x, use_openssl_1_1_x or use_openssl_3 defined, e.g.
	"subConfigurations": {
		"apache-thrift": "use_openssl_3"
	}`);
  }

  // Whether the certificate carried a dNSName at all, tracked separately from
  // whether one matched: RFC 6125 6.4.4 and RFC 9525 6.3 make the
  // subjectAltName the identity once it is present, so the commonName below is
  // for certificates that carry none.
  bool hasDnsName;

  if (alternatives != null) {
    auto count = sk_GENERAL_NAME_num(alternatives);
    for (int i = 0; decision == Decision.SKIP && i < count; i++) {
      auto name = sk_GENERAL_NAME_value(alternatives, i);
      if (name is null) {
        continue;
      }
      auto data = ASN1_STRING_data(name.d.ia5);
      auto length = ASN1_STRING_length(name.d.ia5);

      switch (name.type) {
        case _GEN_DNS:
          hasDnsName = true;
          decision = accessManager.verify(hostName, cast(char[])data[0 .. length]);
          break;
        case _GEN_IPADD:
          if (consultAddressEntries) {
            decision = accessManager.verify(peerAddress, data[0 .. length]);
          }
          break;
        default:
          // Do nothing.
      }
    }

    // DMD @@BUG@@: Empty template arguments parens should not be needed.
    sk_GENERAL_NAME_pop_free!()(alternatives, &GENERAL_NAME_free);
  }

  // If we are already done, return.
  if (decision != Decision.SKIP) {
    X509_free(cert);
    if (decision != Decision.ALLOW) {
      throw new TSSLException("Authorize: Access denied.");
    }
    return;
  }

  // Check commonName -- but only for a certificate that presented no dNSName.
  // A dNSName that did not match is an answer, not an absence.
  auto name = hasDnsName ? null : X509_get_subject_name(cert);
  if (name !is null) {
    X509_NAME_ENTRY* entry;
    int last = -1;
    while (decision == Decision.SKIP) {
      last = X509_NAME_get_index_by_NID(name, NID_commonName, last);
      if (last == -1)
        break;
      entry = X509_NAME_get_entry(name, last);
      if (entry is null)
        continue;
      auto common = X509_NAME_ENTRY_get_data(entry);
      // ASN1_STRING_to_UTF8() leaves the output pointer untouched and returns a
      // negative length for a commonName whose string type it cannot convert, so
      // the pointer is scoped to the iteration and the result is checked before
      // the name is sliced or released.
      char* utf8;
      auto size = ASN1_STRING_to_UTF8(&utf8, common);
      if (size < 0)
        continue;
      decision = accessManager.verify(hostName, utf8[0 .. size]);
      CRYPTO_free(utf8);
    }
  }
  X509_free(cert);
  if (decision != Decision.ALLOW) {
    throw new TSSLException("Authorize: Could not authorize peer.");
  }
}

/*
 * Whether host is an address literal rather than a name. It is classified the
 * way the socket resolves it -- getaddrinfo, here with AI_NUMERICHOST -- so that
 * every form the socket connects to as an address counts as one, and a name
 * that merely begins with a dotted quad does not (RFC 9525 7.4).
 */
private bool isAddressLiteral(string host) {
  try {
    parseAddress(host);
    return true;
  } catch (SocketException) {
    return false;
  }
}

unittest {
  import std.exception : enforce;

  foreach (literal; ["127.0.0.1", "::1", "0:0:0:0:0:0:0:1"]) {
    enforce(isAddressLiteral(literal), literal);
  }
  foreach (name; ["localhost", "example.invalid", "10.1.2.3.example.com",
                  "127.0.0.1.nip.io"]) {
    enforce(!isAddressLiteral(name), name);
  }
  version (linux) {
    // Numeric forms glibc's getaddrinfo accepts, and so connects to.
    foreach (literal; ["127.1", "0x7f.0.0.1", "fe80::1%lo"]) {
      enforce(isAddressLiteral(literal), literal);
    }
  }
}

// ASN1_STRING_to_UTF8() reports a negative length and leaves its output pointer
// untouched for a string whose type it cannot convert. The commonName fallback
// in authorize() above relies on that: the pointer is scoped to each iteration
// and the result checked before the name is sliced or released, so an entry
// that does not convert is skipped rather than acted on.
unittest {
  import deimos.openssl.asn1;

  auto common = ASN1_STRING_type_new(V_ASN1_OBJECT_DESCRIPTOR);
  scope(exit) ASN1_STRING_free(common);
  static immutable ubyte[4] raw = [1, 2, 3, 4];
  ASN1_STRING_set(common, raw.ptr, cast(int) raw.length);

  char* utf8;
  auto size = ASN1_STRING_to_UTF8(&utf8, common);
  assert(size < 0, "an unconvertible commonName string type must report failure");
  assert(utf8 is null, "the output pointer must be left untouched on failure");
}

// In client mode the iPAddress entries of a certificate answer only for a host
// that is itself an address literal. test/keys/server.crt names 127.0.0.1, ::1
// and localhost, in that order: a handshake against it runs in memory, and
// authorize() is then asked about the client's host in turn.
unittest {
  import deimos.openssl.bio;
  import std.exception : enforce;
  import std.path : buildPath, dirName;
  import std.socket : Internet6Address, InternetAddress;

  auto keys = buildPath(dirName(__FILE_FULL_PATH__),
    "..", "..", "..", "..", "..", "test", "keys");

  auto serverContext = new TSSLContext();
  serverContext.serverSide = true;
  serverContext.loadCertificate(buildPath(keys, "server.crt"));
  serverContext.loadPrivateKey(buildPath(keys, "server.key"));

  auto clientContext = new TSSLContext();
  clientContext.authenticate = true;
  clientContext.loadTrustedCertificates(buildPath(keys, "CA.pem"));

  auto server = serverContext.createSSL();
  scope(exit) SSL_free(server);
  auto client = clientContext.createSSL();
  scope(exit) SSL_free(client);

  BIO* serverBio;
  BIO* clientBio;
  enforce(BIO_new_bio_pair(&serverBio, 0, &clientBio, 0) == 1);
  SSL_set_bio(server, serverBio, serverBio);
  SSL_set_bio(client, clientBio, clientBio);
  SSL_set_accept_state(server);
  SSL_set_connect_state(client);

  bool serverDone, clientDone;
  foreach (_; 0 .. 100) {
    if (!clientDone) clientDone = (SSL_do_handshake(client) == 1);
    if (!serverDone) serverDone = (SSL_do_handshake(server) == 1);
    if (clientDone && serverDone) break;
  }
  enforce(clientDone && serverDone, "the in-memory handshake did not complete");

  auto accessManager = new TDefaultClientAccessManager;
  Address v4 = new InternetAddress("127.0.0.1", 9090);
  Address v6 = new Internet6Address("::1", 9090);

  bool accepted(Address peer, string host) {
    try {
      authorize(client, accessManager, peer, host, false);
      return true;
    } catch (TSSLException) {
      return false;
    }
  }

  enforce(accepted(v6, "::1"), "an address literal is covered by its address entry");
  enforce(accepted(v6, "localhost"), "a host name is covered by its DNS entry");
  enforce(accepted(v6, null), "a socket without a host keeps the peer address as its identifier");
  enforce(!accepted(v6, "example.invalid"),
    "an address entry must not answer for a host name the certificate does not carry");
  // A client asked for a host name does not consult the address entries at
  // all, so the 127.0.0.1 entry ahead of the DNS entry is not compared either.
  enforce(accepted(v4, "localhost"), "a host name is covered by its DNS entry");
  enforce(!accepted(v4, "example.invalid"),
    "an address entry must not answer for a host name the certificate does not carry");
  enforce(!accepted(v4, "127.0.0.1.example.invalid"),
    "a host name that begins with a dotted quad is still a host name");
}

/*
 * OpenSSL error information used for storing D exceptions on the OpenSSL
 * error stack.
 */
enum ERR_LIB_D_EXCEPTION = ERR_LIB_USER;
enum ERR_F_D_EXCEPTION = 0; // function id - what to use here?
enum ERR_R_D_EXCEPTION = 1234; // 99 and above are reserved for applications
enum ERR_FILE_D_EXCEPTION = "d_exception";
enum ERR_LINE_D_EXCEPTION = 0;
enum ERR_FLAGS_D_EXCEPTION = 0;

/**
 * Returns an exception for the last.
 *
 * Params:
 *   location = An optional "location" to add to the error message (typically
 *     the last SSL API call).
 */
Exception getSSLException(string location = null, string clientFile = __FILE__,
  size_t clientLine = __LINE__
) {
  // We can return either an exception saved from D BIO code, or a "true"
  // OpenSSL error. Because there can possibly be more than one error on the
  // error stack, we have to fetch all of them, and pick the last, i.e. newest
  // one. We concatenate multiple successive OpenSSL error messages into a
  // single one, but always just return the last D exception.
  string message; // Probably better use an Appender here.
  bool hadMessage;
  Exception exception;

  void initMessage() {
    message.destroy();
    hadMessage = false;
    if (!location.empty) {
      message ~= location;
      message ~= ": ";
    }
  }
  initMessage();

  auto errn = errno;

  const(char)* file = void;
  int line = void;
  const(char)* data = void;
  int flags = void;
  c_ulong code = void;
  while ((code = ERR_get_error_line_data(&file, &line, &data, &flags)) != 0) {
    if (ERR_GET_REASON(code) == ERR_R_D_EXCEPTION) {
      initMessage();
      GC.removeRoot(cast(void*)data);
      exception = cast(Exception)data;
    } else {
      exception = null;

      if (hadMessage) {
        message ~= ", ";
      }

      auto reason = ERR_reason_error_string(code);
      if (reason) {
        message ~= "SSL error: " ~ to!string(reason);
      } else {
        message ~= "SSL error #" ~ to!string(code);
      }

      hadMessage = true;
    }
  }

  // If the last item from the stack was a D exception, throw it.
  if (exception) return exception;

  // We are dealing with an OpenSSL error that doesn't root in a D exception.
  if (!hadMessage) {
    // If we didn't get an actual error from the stack yet, try errno.
    string errnString;
    if (errn != 0) {
      errnString = to!string(strerror(errn));
    }
    if (errnString.empty) {
      message ~= "Unknown error";
    } else {
      message ~= errnString;
    }
  }

  message ~= ".";
  return new TSSLException(message, clientFile, clientLine);
}

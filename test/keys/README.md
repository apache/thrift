# Test Keys and Certificates
This folder is dedicated to test keys and certificates provided in multiple formats.
Primary use are unit test suites and cross language tests.

    test/keys

**The files in this directory must never be used on production systems.**

## SSL Keys and Certificates


## create certificates

we use the following parameters for test key and certificate creation

    C=US,
    ST=Maryland,
    L=Forest Hill,
    O=The Apache Software Foundation,
    OU=Apache Thrift,
    CN=localhost/emailAddress=dev@thrift.apache.org

### create self-signed server key and certificate

    openssl req -new -x509 -nodes  -days 3000 -out server.crt -keyout server.key
    openssl x509 -in server.crt -text > CA.pem
    cat server.crt server.key > server.pem

Export password is "thrift" without the quotes

    openssl pkcs12 -export -clcerts -in server.crt -inkey server.key -out server.p12

### create client key and certificate

    openssl genrsa -out client.key

create a signing request:

    openssl req -new -key client.key -out client.csr

sign the client certificate with the server.key

    openssl x509 -req -days 3000 -in client.csr -CA CA.pem -CAkey server.key -set_serial 01 -out client.crt

export certificate in PKCS12 format (Export password is "thrift" without the quotes)

    openssl pkcs12 -export -clcerts -in client.crt -inkey client.key -out client.p12

export certificate in PEM format for OpenSSL usage

    openssl pkcs12 -in client.p12 -out client.pem -clcerts

### create client key and certificate with altnames

`client_v3.crt` is self-signed with `client_v3.key`, carries the same
distinguished name as the other certificates, and lists the loopback addresses
and `localhost` as alternative names. It does not carry an IPv4-mapped IPv6
address (`::ffff:127.0.0.1`): Go 1.27 and later refuse to load a certificate
with one, and the Python peer matcher reduces a mapped peer address to its IPv4
form before comparing, so the entry is not needed.

Write this configuration to `client_v3.cnf`:

    [ req ]
    distinguished_name = req_distinguished_name
    x509_extensions = v3_req
    prompt = no

    [ req_distinguished_name ]
    CN = localhost
    emailAddress = dev@thrift.apache.org
    OU = Apache Thrift
    O = The Apache Software Foundation
    L = Forest Hill
    ST = Maryland
    C = US

    [ v3_req ]
    basicConstraints = CA:FALSE
    keyUsage = nonRepudiation, digitalSignature, keyEncipherment
    subjectKeyIdentifier = none
    authorityKeyIdentifier = none
    subjectAltName = @alternate_names

    [ alternate_names ]
    IP.1 = 127.0.0.1
    IP.2 = ::1
    DNS.1 = localhost

create the self-signed certificate:

    openssl req -x509 -new -key client_v3.key -days 3000 -set_serial 01 \
        -config client_v3.cnf -out client_v3.crt

## which certificate the cross tests use

Every certificate here carries the same distinguished name, and none of them
carries an authority key identifier, so OpenSSL treats each one as self-signed
and never builds a chain to `CA.pem`. A peer certificate is therefore accepted
only when the verifying side has that exact certificate in its store, and a
store can hold only one certificate for a given distinguished name -- adding a
second one for the same name shadows it rather than extending the trust.

The two test servers that ask for a client certificate,
`test/py/TestServer.py` and `test/rb/integration/TestServer.rb`, trust
`client_v3.crt`. Every cross-test client that presents a certificate to them
sends `client_v3.crt` with `client_v3.key`, including the Java and Kotlin
clients through `lib/java/src/crossTest/resources/.clientkeystore`. The Python
server additionally matches the certificate against the address the connection
arrived from, which is why it has to be the certificate carrying the
`subjectAltName` records above and not `client.crt`.

`client.crt` stays without `subjectAltName` on purpose: `lib/py/test/test_sslsocket.py`
uses it as `CLIENT_CERT_NO_IP` to exercise the rejection path.

regenerate the client keystore after changing `client_v3`

    openssl pkcs12 -export -in client_v3.crt -inkey client_v3.key \
        -name 1 -passout pass:thrift \
        -keypbe AES-256-CBC -certpbe AES-256-CBC -macalg sha256 -iter 10000 \
        -out ../../lib/java/src/crossTest/resources/.clientkeystore

## Java key and certificate import
Java Test Environment uses key and trust store password "thrift" without the quotes

list keystore entries

    keytool -list -storepass thrift -keystore ../../lib/java/test/.keystore

list truststore entries

    keytool -list -storepass thrift -keystore ../../lib/java/test/.truststore


delete an entry

    keytool -delete -storepass thrift -keystore ../../lib/java/test/.truststore -alias ssltest


import certificate into truststore

    keytool -importcert -storepass thrift -keystore ../../lib/java/test/.truststore -alias localhost --file server.crt

import key into keystore

    keytool -importkeystore -storepass thrift -keystore ../../lib/java/test/.keystore -srcstoretype pkcs12 -srckeystore server.p12

# Test SSL server and clients

    openssl s_client -connect localhost:9090
    openssl s_server -accept 9090 -www


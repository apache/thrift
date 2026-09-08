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

copy openssl.cnf from your system e.g. /etc/ssl/openssl.cnf and append following to the end of [ v3_req ]

    subjectAltName=@alternate_names

    [ alternate_names ]
    IP.1=127.0.0.1
    IP.2=::1
    IP.3=::ffff:127.0.0.1

create a signing request:

    openssl req -new -key client_v3.key -out client_v3.csr -config openssl.cnf \
        -subj "/C=US/ST=Maryland/L=Forest Hill/O=The Apache Software Foundation/OU=Apache Thrift/CN=localhost" -extensions v3_req

sign the client certificate with the server.key

    openssl x509 -req -days 3000 -in client_v3.csr -CA CA.pem -CAkey server.key -set_serial 01 -out client_v3.crt -extensions v3_req -extfile openssl.cnf

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


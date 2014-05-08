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

Export password is **thrift**

    openssl pkcs12 -export -clcerts -in server.crt -inkey server.key -out server.p12

### create client key and certificate

    openssl genrsa -out client.key

create a signing request:

    openssl req -new -key client.key -out client.csr

sign the client certificate with the server.key

    openssl x509 -req -days 365 -in client.csr -CA CA.pem -CAkey server.key -set_serial 01 -out client.crt

export certificate in PKCS12 format (Export password is **thrift**)

    openssl pkcs12 -export -clcerts -in client.crt -inkey client.key -out client.p12

export certificate in PEM format for OpenSSL usage

    openssl pkcs12 -in client.p12 -out client.pem -clcerts


## Java key and certificate import
Java Test Environment uses key and trust store password **thrift**

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


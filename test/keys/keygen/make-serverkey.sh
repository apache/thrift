#!/bin/bash
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

# tested with git bash on Windows
# probably needs a bit of tweaking for other environments

# re the "//SKIP=this" in sub see at https://stackoverflow.com/a/54924640/499466

echo init folder
rm *.p12 2> /dev/null
rm *.pem 2> /dev/null
rm *.crt 2> /dev/null
rm *.key 2> /dev/null
rm *.cfg 2> /dev/null
rm *.csr 2> /dev/null

#cp ../*.key .

echo writing config
echo '[ req ]' > my.cfg
echo 'default_bits= 4096' >> my.cfg
echo 'distinguished_name=req' >> my.cfg
echo 'x509_extensions = v3_ca' >> my.cfg
echo 'req_extensions = v3_req' >> my.cfg
echo '' >> my.cfg
echo '[ v3_req ]' >> my.cfg
echo 'basicConstraints = CA:FALSE' >> my.cfg
echo 'keyUsage = nonRepudiation, digitalSignature, keyEncipherment' >> my.cfg
echo 'subjectAltName=@alternate_names' >> my.cfg
echo '' >> my.cfg
echo '[ alternate_names ]' >> my.cfg
echo 'IP.1=127.0.0.1' >> my.cfg
echo 'IP.2=::1' >> my.cfg
echo 'IP.3=::ffff:127.0.0.1' >> my.cfg
echo 'DNS.1=localhost' >> my.cfg
echo '' >> my.cfg
echo '[ v3_ca ]' >> my.cfg
echo 'subjectKeyIdentifier=hash' >> my.cfg
echo 'authorityKeyIdentifier=keyid:always,issuer' >> my.cfg
echo 'basicConstraints = critical, CA:TRUE, pathlen:0' >> my.cfg
echo 'keyUsage = critical, cRLSign, keyCertSign, nonRepudiation, digitalSignature, keyEncipherment' >> my.cfg
echo 'extendedKeyUsage = serverAuth, clientAuth' >> my.cfg
echo 'subjectAltName=@alternate_names' >> my.cfg
echo '' >> my.cfg

echo
echo step 1a
winpty openssl req \
	-new \
	-x509 \
	-nodes  \
	-days 3000 \
	-out server.crt \
	-keyout server.key \
	-subj '//SKIP=this/CN=localhost/emailAddress=dev@thrift.apache.org/OU=Apache Thrift/O=The Apache Software Foundation/L=Forest Hill/ST=Maryland/C=US' \
	-extensions v3_ca \
	-config my.cfg

echo
echo step 1b
openssl x509 -in server.crt -text > CA.pem

echo
echo step 1c
cat server.crt server.key > server.pem

echo
echo step 2
echo 'Use "thrift" as password (without the quotes)'
winpty openssl pkcs12 -export -clcerts -in server.crt -inkey server.key -out server.p12

echo
echo step 3
winpty openssl genrsa -out client.key


echo
echo step 4
winpty openssl req \
	-new \
    -subj '//SKIP=this/CN=localhost/emailAddress=dev@thrift.apache.org/OU=Apache Thrift/O=The Apache Software Foundation/L=Forest Hill/ST=Maryland/C=US' \
	-key client.key \
	-out client.csr

echo
echo step 5
winpty openssl x509 -req -days 3000 -in client.csr -CA CA.pem -CAkey server.key -set_serial 01 -out client.crt


echo
echo step 6
winpty openssl pkcs12 -export -clcerts -in client.crt -inkey client.key -out client.p12


echo
echo step 7
winpty openssl pkcs12 -in client.p12 -out client.pem -clcerts


echo
echo step 8a
openssl genrsa -out client_v3.key

echo
echo step 8b
winpty openssl req \
	-new \
	-subj '//SKIP=this/CN=localhost/emailAddress=dev@thrift.apache.org/OU=Apache Thrift/O=The Apache Software Foundation/L=Forest Hill/ST=Maryland/C=US' \
	-key client_v3.key \
	-out client_v3.csr \
	-extensions v3_req \
	-config my.cfg

	
echo
echo step 9
winpty openssl x509 -req -days 3000 -in client_v3.csr -CA CA.pem -CAkey server.key -set_serial 01 -out client_v3.crt -extensions v3_req -extfile my.cfg

echo
echo cleanup
rm *.cfg 2> /dev/null
rm *.csr 2> /dev/null

echo
echo test
openssl s_client -connect localhost:9090 &
openssl s_server -accept 9090 -www 

echo
echo done



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

%.so:
	cd src/ext/thrift_protocol/ \
	  && $(MAKE)

if WITH_PHP_EXTENSION
phpconfdir=$(PHP_CONFIG_PREFIX)
phpconf_DATA=thrift_protocol.ini

phpmoduledir = `php-config --extension-dir`
phpmodule_SCRIPTS = src/ext/thrift_protocol/modules/thrift_protocol.so
endif

phpdir = $(PHP_PREFIX)
php_DATA =  \
  src/autoload.php \
  src/Thrift.php

phpprotocoldir = $(phpdir)/protocol
phpprotocol_DATA = \
  src/protocol/TBinaryProtocol.php \
  src/protocol/TBinarySerializer.php \
  src/protocol/TProtocol.php

phptransportdir = $(phpdir)/transport
phptransport_DATA = \
  src/transport/TBufferedTransport.php \
  src/transport/TFramedTransport.php \
  src/transport/THttpClient.php \
  src/transport/TMemoryBuffer.php \
  src/transport/TNullTransport.php \
  src/transport/TPhpStream.php \
  src/transport/TSocket.php \
  src/transport/TSocketPool.php \
  src/transport/TTransport.php

EXTRA_DIST = src thrift_protocol.ini README.apache

MAINTAINERCLEANFILES = Makefile Makefile.in

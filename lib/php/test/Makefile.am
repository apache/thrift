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

PHPUNIT=php $(top_srcdir)/vendor/bin/phpunit

stubs: Resources/ThriftTest.thrift
	mkdir -p ./Resources/packages/php
	mkdir -p ./Resources/packages/phpv
	mkdir -p ./Resources/packages/phpvo
	mkdir -p ./Resources/packages/phpjs
	mkdir -p ./Resources/packages/phpcm
	$(THRIFT) --gen php:nsglobal="Basic" -r --out ./Resources/packages/php Resources/ThriftTest.thrift
	$(THRIFT) --gen php:validate,nsglobal="Validate" -r --out ./Resources/packages/phpv Resources/ThriftTest.thrift
	$(THRIFT) --gen php:validate,oop,nsglobal="ValidateOop" -r --out ./Resources/packages/phpvo Resources/ThriftTest.thrift
	$(THRIFT) --gen php:json,nsglobal="Json" -r --out ./Resources/packages/phpjs Resources/ThriftTest.thrift
	$(THRIFT) --gen php:classmap,server,rest,nsglobal="Classmap" -r --out ./Resources/packages/phpcm Resources/ThriftTest.thrift

deps: $(top_srcdir)/composer.json
	composer install --working-dir=$(top_srcdir)

all-local: deps

check: deps stubs
	$(PHPUNIT) --log-junit=test-log-junit.xml -c phpunit.xml /

distclean-local:

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

clean-local:
	$(RM) -r ./Resources/packages
	$(RM) test-log-junit.xml

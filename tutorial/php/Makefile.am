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

# tutorial.thrift includes shared.thrift, so a change to either regenerates both.
gen-php/tutorial/CalculatorIf.php gen-php/shared/SharedServiceIf.php: $(top_srcdir)/tutorial/tutorial.thrift $(top_srcdir)/tutorial/shared.thrift
	$(THRIFT) --gen php:server -r $<

all-local: gen-php/tutorial/CalculatorIf.php

# Generate from copies of the IDL in gen-check, add a struct to the copy of
# shared.thrift, and check that make regenerates the PHP code.
check-local:
	$(RM) -r gen-check
	$(MKDIR_P) gen-check/tutorial
	cp $(top_srcdir)/tutorial/tutorial.thrift $(top_srcdir)/tutorial/shared.thrift gen-check/tutorial/
	$(MAKE) $(AM_MAKEFLAGS) -C gen-check -f $(abs_builddir)/Makefile \
	  top_srcdir=$(abs_builddir)/gen-check all-local
	sleep 1
	test ! -e gen-check/gen-php/shared/AddedByCheck.php
	echo 'struct AddedByCheck { 1: i32 key }' >> gen-check/tutorial/shared.thrift
	$(MAKE) $(AM_MAKEFLAGS) -C gen-check -f $(abs_builddir)/Makefile \
	  top_srcdir=$(abs_builddir)/gen-check all-local
	test -e gen-check/gen-php/shared/AddedByCheck.php

tutorialserver: all
	${PYTHON} runserver.py

tutorialclient: all
	${PHP} PhpClient.php --http

clean-local:
	$(RM) -r gen-*

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

EXTRA_DIST = \
	PhpServer.php \
	PhpClient.php \
	runserver.py

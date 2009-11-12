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

SUBDIRS = test

Makefile-perl.mk : Makefile.PL
	$(PERL) Makefile.PL MAKEFILE=Makefile-perl.mk INSTALLDIRS=$(INSTALLDIRS)

all-local: Makefile-perl.mk
	$(MAKE) -f Makefile-perl.mk
	find blib -name 'Makefile*' -exec rm -f {} \;

check-local:
	$(PERL) -Iblib/lib -I@abs_srcdir@ -I@builddir@/test/gen-perl \
		@abs_srcdir@/test.pl @abs_srcdir@/test/*.t

install-exec-local: Makefile-perl.mk
	$(MAKE) -f Makefile-perl.mk install DESTDIR=$(DESTDIR)/

clean-local:
	if test -f Makefile-perl.mk ; then \
		$(MAKE) -f Makefile-perl.mk clean ; \
	fi
	rm -f Makefile-perl.mk.old

EXTRA_DIST = \
	Makefile.PL \
	test.pl \
	lib/Thrift.pm \
	lib/Thrift.pm \
	lib/Thrift/BinaryProtocol.pm \
	lib/Thrift/BufferedTransport.pm \
	lib/Thrift/FramedTransport.pm \
	lib/Thrift/HttpClient.pm \
	lib/Thrift/MemoryBuffer.pm \
	lib/Thrift/Protocol.pm \
	lib/Thrift/Server.pm \
	lib/Thrift/Socket.pm \
	lib/Thrift/Transport.pm

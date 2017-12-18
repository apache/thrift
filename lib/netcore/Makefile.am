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

SUBDIRS = . 

THRIFTCODE = \
			Thrift/Thrift.csproj

all-local: \
	Thrift.dll

Thrift.dll: $(THRIFTCODE)
#	$(MKDIR_P) $(GENDIR)
#	$(THRIFT)  -gen netcore:wcf   -r  -out $(GENDIR)  $(TESTDIR)/CassandraTest.thrift
#	$(THRIFT)  -gen netcore:wcf   -r  -out $(GENDIR)  $(top_srcdir)/test/ThriftTest.thrift
#	$(THRIFT)  -gen netcore:wcf   -r  -out $(GENDIR)  $(top_srcdir)/contrib/fb303/if/fb303.thrift
	$(DOTNETCORE) build

clean-local:
	$(RM) -r Thrift/bin
	$(RM) -r Thrift/obj

EXTRA_DIST = \
			 Thrift \
			 Thrift.sln \
			 Tests \
			 README.md
			 

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

all-local:
	$(DOTNETCORE) build

check-local:
	$(DOTNETCORE) test Tests/Thrift.Tests/Thrift.Tests.csproj
	$(DOTNETCORE) test Tests/Thrift.IntegrationTests/Thrift.IntegrationTests.csproj

clean-local:
	$(RM) -r Thrift/bin
	$(RM) -r Thrift/obj
	$(RM) -r Tests/Thrift.Tests/bin
	$(RM) -r Tests/Thrift.Tests/obj
	$(RM) -r Tests/Thrift.IntegrationTests/bin
	$(RM) -r Tests/Thrift.IntegrationTests/obj
	$(RM) -r Tests/Thrift.PublicInterfaces.Compile.Tests/bin
	$(RM) -r Tests/Thrift.PublicInterfaces.Compile.Tests/obj

dist-hook:
	$(RM) -r $(distdir)/Thrift/bin
	$(RM) -r $(distdir)/Thrift/obj
	$(RM) -r $(distdir)/Tests/Thrift.Tests/bin
	$(RM) -r $(distdir)/Tests/Thrift.Tests/obj
	$(RM) -r $(distdir)/Tests/Thrift.IntegrationTests/bin
	$(RM) -r $(distdir)/Tests/Thrift.IntegrationTests/obj
	$(RM) -r $(distdir)/Tests/Thrift.PublicInterfaces.Compile.Tests/bin
	$(RM) -r $(distdir)/Tests/Thrift.PublicInterfaces.Compile.Tests/obj

DISTCLEANFILES = \
	Makefile.in

EXTRA_DIST = \
	README.md \
	Tests \
	Thrift \
	Thrift.sln \
	build.cmd \
	build.sh \
	runtests.cmd \
	runtests.sh

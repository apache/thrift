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

all-local:
	$(DOTNETCORE) build -c Release

check-local:
	$(DOTNETCORE) test Tests/Thrift.Compile.Tests/Thrift.Compile.net8/Thrift.Compile.net8.csproj
	$(DOTNETCORE) test Tests/Thrift.Compile.Tests/Thrift.Compile.net9/Thrift.Compile.net9.csproj
	$(DOTNETCORE) test Tests/Thrift.Compile.Tests/Thrift.Compile.netstd2/Thrift.Compile.netstd2.csproj
	$(DOTNETCORE) test Tests/Thrift.Tests/Thrift.Tests.csproj
	$(DOTNETCORE) test Tests/Thrift.IntegrationTests/Thrift.IntegrationTests.csproj
	$(MAKE) build-fuzzers

build-fuzzers:
	$(DOTNETCORE) build Tests/Thrift.FuzzTests/Thrift.FuzzTests.csproj -p:Protocol=Binary -p:FuzzerType=Parse -p:Engine=AFL
	$(DOTNETCORE) build Tests/Thrift.FuzzTests/Thrift.FuzzTests.csproj -p:Protocol=Binary -p:FuzzerType=Parse -p:Engine=Libfuzzer
	$(DOTNETCORE) build Tests/Thrift.FuzzTests/Thrift.FuzzTests.csproj -p:Protocol=Binary -p:FuzzerType=Roundtrip -p:Engine=AFL
	$(DOTNETCORE) build Tests/Thrift.FuzzTests/Thrift.FuzzTests.csproj -p:Protocol=Binary -p:FuzzerType=Roundtrip -p:Engine=Libfuzzer
	$(DOTNETCORE) build Tests/Thrift.FuzzTests/Thrift.FuzzTests.csproj -p:Protocol=Compact -p:FuzzerType=Parse -p:Engine=AFL
	$(DOTNETCORE) build Tests/Thrift.FuzzTests/Thrift.FuzzTests.csproj -p:Protocol=Compact -p:FuzzerType=Parse -p:Engine=Libfuzzer
	$(DOTNETCORE) build Tests/Thrift.FuzzTests/Thrift.FuzzTests.csproj -p:Protocol=Compact -p:FuzzerType=Roundtrip -p:Engine=AFL
	$(DOTNETCORE) build Tests/Thrift.FuzzTests/Thrift.FuzzTests.csproj -p:Protocol=Compact -p:FuzzerType=Roundtrip -p:Engine=Libfuzzer
	$(DOTNETCORE) build Tests/Thrift.FuzzTests/Thrift.FuzzTests.csproj -p:Protocol=Json -p:FuzzerType=Parse -p:Engine=AFL
	$(DOTNETCORE) build Tests/Thrift.FuzzTests/Thrift.FuzzTests.csproj -p:Protocol=Json -p:FuzzerType=Parse -p:Engine=Libfuzzer
	$(DOTNETCORE) build Tests/Thrift.FuzzTests/Thrift.FuzzTests.csproj -p:Protocol=Json -p:FuzzerType=Roundtrip -p:Engine=AFL
	$(DOTNETCORE) build Tests/Thrift.FuzzTests/Thrift.FuzzTests.csproj -p:Protocol=Json -p:FuzzerType=Roundtrip -p:Engine=Libfuzzer

clean-local:
	$(RM) -r Thrift/bin
	$(RM) -r Thrift/obj
	$(RM) -r Benchmarks/Thrift.Benchmarks/bin
	$(RM) -r Benchmarks/Thrift.Benchmarks/obj
	$(RM) -r Tests/Thrift.Tests/bin
	$(RM) -r Tests/Thrift.Tests/obj
	$(RM) -r Tests/Thrift.IntegrationTests/bin
	$(RM) -r Tests/Thrift.IntegrationTests/obj
	$(RM) -r Tests/Thrift.Compile.Tests/Thrift.Compile.net8/bin
	$(RM) -r Tests/Thrift.Compile.Tests/Thrift.Compile.net8/obj
	$(RM) -r Tests/Thrift.Compile.Tests/Thrift.Compile.net9/bin
	$(RM) -r Tests/Thrift.Compile.Tests/Thrift.Compile.net9/obj
	$(RM) -r Tests/Thrift.Compile.Tests/Thrift.Compile.netstd2/bin
	$(RM) -r Tests/Thrift.Compile.Tests/Thrift.Compile.netstd2/obj
	$(RM) -r Tests/Thrift.FuzzTests/bin
	$(RM) -r Tests/Thrift.FuzzTests/obj

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

EXTRA_DIST = \
	README.md \
	Directory.Build.props \
	Benchmarks/Thrift.Benchmarks \
	Tests/Thrift.IntegrationTests/Protocols \
	Tests/Thrift.IntegrationTests/Thrift.IntegrationTests.csproj \
	Tests/Thrift.Compile.Tests \
	Tests/Thrift.Compile.Tests/CassandraTest.thrift \
	Tests/Thrift.Compile.Tests/optional_required_default.thrift \
	Tests/Thrift.Compile.Tests/Thrift.Compile.net8/Thrift.Compile.net8.csproj \
	Tests/Thrift.Compile.Tests/Thrift.Compile.net9/Thrift.Compile.net9.csproj \
	Tests/Thrift.Compile.Tests/Thrift.Compile.netstd2/Thrift.Compile.netstd2.csproj \
	Tests/Thrift.FuzzTests \
	Tests/Thrift.Tests/Collections \
	Tests/Thrift.Tests/DataModel \
	Tests/Thrift.Tests/Protocols \
	Tests/Thrift.Tests/Transports \
	Tests/Thrift.Tests/Thrift.Tests.csproj \
	Thrift/.editorconfig \
	Thrift/Collections \
	Thrift/Processor \
	Thrift/Properties \
	Thrift/Protocol \
	Thrift/Server \
	Thrift/GlobalSuppressions.cs \
	Thrift/TApplicationException.cs \
	Thrift/TBaseClient.cs \
	Thrift/TConfiguration.cs \
	Thrift/TException.cs \
	Thrift/Thrift.csproj \
	Thrift/Transport \
	Thrift/*.snk \
	Thrift.sln \
	build.cmd \
	build.sh \
	runtests.cmd \
	runtests.sh

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

# Makefile for Thrift test project.
# Default target is everything

ifndef thrift_home
thrift_home=../..
endif #thrift_home

target: all

ifndef boost_home
#boost_home=../../../../../thirdparty/boost_1_33_1
boost_home=/usr/local/include/boost-1_33_1
endif #boost_home
target: all

include_paths = $(thrift_home)/lib/cpp/src \
		$(boost_home)

include_flags = $(patsubst %,-I%, $(include_paths))

# Tools
ifndef THRIFT
THRIFT = ../../compiler/cpp/thrift
endif # THRIFT

CC     = g++
LD     = g++

# Compiler flags
DCFL  = -Wall -O3 -g -I. -I./gen-cpp $(include_flags) -L$(thrift_home)/lib/cpp/.libs -lthrift -lthriftnb -levent
LFL   =  -L$(thrift_home)/lib/cpp/.libs -lthrift -lthriftnb -levent
CCFL  = -Wall -O3 -I. -I./gen-cpp $(include_flags)
CFL   = $(CCFL) $(LFL)

all: server client

debug: server-debug client-debug

stubs: ../ThriftTest.thrift
	$(THRIFT) --gen cpp ../ThriftTest.thrift

server-debug: stubs
	g++ -o TestServer $(DCFL) src/TestServer.cpp ./gen-cpp/ThriftTest.cpp ./gen-cpp/ThriftTest_types.cpp ../ThriftTest_extras.cpp

client-debug: stubs
	g++ -o TestClient $(DCFL) src/TestClient.cpp ./gen-cpp/ThriftTest.cpp ./gen-cpp/ThriftTest_types.cpp ../ThriftTest_extras.cpp

server: stubs
	g++ -o TestServer $(CFL) src/TestServer.cpp ./gen-cpp/ThriftTest.cpp ./gen-cpp/ThriftTest_types.cpp ../ThriftTest_extras.cpp

client: stubs
	g++ -o TestClient $(CFL) src/TestClient.cpp ./gen-cpp/ThriftTest.cpp ./gen-cpp/ThriftTest_types.cpp ../ThriftTest_extras.cpp

small:
	$(THRIFT) --gen cpp ../SmallTest.thrift
	g++ -c $(CCFL) ./gen-cpp/SmallService.cpp ./gen-cpp/SmallTest_types.cpp

clean:
	rm -fr *.o TestServer TestClient gen-cpp

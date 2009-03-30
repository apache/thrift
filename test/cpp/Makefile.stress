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
                $(thrift_home)/lib/cpp \
                $(thrift_home)/ \
                $(boost_home)

include_flags = $(patsubst %,-I%, $(include_paths))

# Tools
ifndef THRIFT
THRIFT = ../../compiler/cpp/thrift
endif # THRIFT

CC     = g++
LD     = g++

# Compiler flags
DCFL  = -Wall -O3 -g -I./gen-cpp $(include_flags) -L$(thrift_home)/lib/cpp/.libs -lthrift -lthriftnb -levent
CFL   = -Wall -O3 -I./gen-cpp $(include_flags) -L$(thrift_home)/lib/cpp/.libs -lthrift -lthriftnb -levent

all: stress-test stress-test-nb

debug: stress-test-debug stress-test-debug-nb

stubs: ../StressTest.thrift
	$(THRIFT) --gen cpp --gen php ../StressTest.thrift

stress-test-debug-nb: stubs
	g++ -o stress-test-nb $(DCFL) src/nb-main.cpp ./gen-cpp/Service.cpp gen-cpp/StressTest_types.cpp

stress-test-nb: stubs
	g++ -o stress-test-nb $(CFL) src/nb-main.cpp ./gen-cpp/Service.cpp gen-cpp/StressTest_types.cpp

stress-test-debug: stubs
	g++ -o stress-test $(DCFL) src/main.cpp ./gen-cpp/Service.cpp gen-cpp/StressTest_types.cpp

stress-test: stubs
	g++ -o stress-test $(CFL) src/main.cpp ./gen-cpp/Service.cpp gen-cpp/StressTest_types.cpp

clean:
	rm -fr stress-test stress-test-nb gen-cpp

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

THRIFT = thrift

CXXFLAGS += -g3 -O0

GENNAMES = Storage storage_types
GENHEADERS = $(addsuffix .h, $(GENNAMES))
GENSRCS = $(addsuffix .cpp, $(GENNAMES))
GENOBJS = $(addsuffix .o, $(GENNAMES))

PYLIBS = storage/__init__.py

PROGS =  test-client test-server test-sender test-receiver

all: $(PYLIBS) $(PROGS)

test-client: test-client.o TZmqClient.o $(GENOBJS)
	$(CXX) $^ -o $@ -lzmq -lthrift
test-server: test-server.o TZmqServer.o $(GENOBJS)
	$(CXX) $^ -o $@ -lzmq -lthrift
test-sender: test-sender.o TZmqClient.o $(GENOBJS)
	$(CXX) $^ -o $@ -lzmq -lthrift
test-receiver: test-receiver.o TZmqServer.o $(GENOBJS)
	$(CXX) $^ -o $@ -lzmq -lthrift

test-client.o test-server.o test-sender.o test-receiver.o: $(GENSRCS)

storage/__init__.py: storage.thrift
	$(RM) $(dir $@)
	$(THRIFT) --gen py $<
	mv gen-py/$(dir $@) .

$(GENSRCS): storage.thrift
	$(THRIFT) --gen cpp $<
	mv $(addprefix gen-cpp/, $(GENSRCS) $(GENHEADERS)) .

clean:
	$(RM) -r *.o $(PROGS) storage $(GENSRCS) $(GENHEADERS)

.PHONY: clean

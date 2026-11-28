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
CXXFLAGS = `pkg-config --cflags thrift thrift-nb` -levent
LDLIBS = `pkg-config --libs thrift thrift-nb` -levent
CXXFLAGS += -g -O0

GENNAMES = Aggr aggr_types
GENHEADERS = $(addsuffix .h, $(GENNAMES))
GENSRCS = $(addsuffix .cpp, $(GENNAMES))
GENOBJS = $(addsuffix .o, $(GENNAMES))

PYLIBS = aggr/__init__.py

PROGS =  test-server

all: $(PYLIBS) $(PROGS)

test-server: test-server.o $(GENOBJS)

test-server.o: $(GENSRCS)

aggr/__init__.py: aggr.thrift
	$(RM) $(dir $@)
	$(THRIFT) --gen py:newstyle $<
	mv gen-py/$(dir $@) .

$(GENSRCS): aggr.thrift
	$(THRIFT) --gen cpp:cob_style $<
	mv $(addprefix gen-cpp/, $(GENSRCS) $(GENHEADERS)) .

clean:
	$(RM) -r *.o $(PROGS) $(GENSRCS) $(GENHEADERS) gen-* aggr

.PHONY: clean

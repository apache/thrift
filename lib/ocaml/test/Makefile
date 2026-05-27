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

# Self-contained recursion-depth test (THRIFT-6051). It depends only on
# lib/ocaml/src/Thrift.ml (which holds the depth guard); it deliberately does
# not use TBinaryProtocol or generated code, neither of which compiles on
# modern OCaml. Build and run with: make
SRC = ../src
OCAMLC = ocamlc -thread unix.cma threads.cma -I $(SRC)

all: test

test: test_recursion_depth
	./test_recursion_depth

test_recursion_depth: $(SRC)/Thrift.ml test_recursion_depth.ml
	$(OCAMLC) $(SRC)/Thrift.ml test_recursion_depth.ml -o test_recursion_depth

clean:
	rm -f *.cm* *.o test_recursion_depth $(SRC)/Thrift.cm* $(SRC)/Thrift.o

.PHONY: all test clean

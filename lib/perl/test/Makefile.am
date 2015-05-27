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

THRIFT = @top_builddir@/compiler/cpp/thrift
THRIFT_IF = @top_srcdir@/test/ThriftTest.thrift
NAME_BENCHMARKSERVICE =  @top_srcdir@/lib/rb/benchmark/Benchmark.thrift
NAME_AGGR = @top_srcdir@/contrib/async-test/aggr.thrift

check-local: \
	gen-perl/ThriftTest/Types.pm \
	gen-perl/BenchmarkTest/BenchmarkService.pm \
	gen-perl/Aggr/Aggr.pm

gen-perl/ThriftTest/Types.pm: $(THRIFT_IF)
	$(THRIFT) --gen perl $(THRIFT_IF)

clean-local:
	rm -rf gen-perl
	
gen-perl/BenchmarkTest/BenchmarkService.pm: $(NAME_BENCHMARKSERVICE)
	$(THRIFT) --gen perl $(NAME_BENCHMARKSERVICE)
	
gen-perl/Aggr/Aggr.pm: $(NAME_AGGR)
	$(THRIFT) --gen perl $(NAME_AGGR)

EXTRA_DIST = memory_buffer.t processor.t multiplex.t

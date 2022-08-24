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

gen-perl/tutorial/Calculator.pm gen-perl/shared/SharedService.pm: $(top_srcdir)/tutorial/tutorial.thrift
	$(THRIFT) --gen perl -r $<

all-local: gen-perl/tutorial/Calculator.pm

tutorialserver: all
	${PERL} PerlServer.pl

tutorialclient: all
	${PERL} PerlClient.pl

clean-local:
	$(RM) -r gen-*

EXTRA_DIST = \
	PerlServer.pl \
	PerlClient.pl

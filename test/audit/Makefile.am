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

AUTOMAKE_OPTIONS = serial-tests

if HAVE_PERL_FOR_BUILD
TESTS = thrift_audit_test.pl
TESTS_ENVIRONMENT = \
	THRIFT_AUDIT_TEST_COMPILER='$(abs_top_builddir)/compiler/cpp/thrift'; \
	export THRIFT_AUDIT_TEST_COMPILER; \
	THRIFT_AUDIT_TEST_FIXTURES='$(abs_srcdir)'; \
	export THRIFT_AUDIT_TEST_FIXTURES; \
	$(PERL_FOR_BUILD)
endif

EXTRA_DIST = \
	CMakeLists.txt \
	README.md \
	thrift_audit_test.pl \
	break1.thrift \
	break2.thrift \
	break3.thrift \
	break4.thrift \
	break5.thrift \
	break6.thrift \
	break7.thrift \
	break8.thrift \
	break9.thrift \
	break10.thrift \
	break11.thrift \
	break12.thrift \
	break13.thrift \
	break14.thrift \
	break15.thrift \
	break16.thrift \
	break17.thrift \
	break18.thrift \
	break19.thrift \
	break20.thrift \
	break21.thrift \
	break22.thrift \
	break23.thrift \
	break24.thrift \
	break25.thrift \
	break26.thrift \
	break27.thrift \
	break28.thrift \
	break29.thrift \
	break30.thrift \
	break31.thrift \
	break32.thrift \
	break33.thrift \
	break34.thrift \
	default_field_old.thrift \
	optional_field_middle_old.thrift \
	optional_field_middle_removed.thrift \
	optional_field_old.thrift \
	optional_field_removed.thrift \
	required_argument_default.thrift \
	required_argument_old.thrift \
	required_field_old.thrift \
	required_to_default_new.thrift \
	required_to_default_old.thrift \
	required_to_optional_new.thrift \
	test.thrift \
	warning.thrift

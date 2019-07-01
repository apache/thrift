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
#
# Contains some contributions under the Thrift Software License.
# Please see doc/old-thrift-license.txt in the Thrift distribution for
# details.

AUTOMAKE_OPTIONS = subdir-objects nostdinc

AM_YFLAGS = -d

BUILT_SOURCES = thrift/thrifty.cc

noinst_LIBRARIES = thrift/libparse.a

thrift_libparse_a_CPPFLAGS = -I$(srcdir)
thrift_libparse_a_CXXFLAGS = -Wall -Wno-sign-compare -Wno-unused

thrift_libparse_a_SOURCES = thrift/thrifty.yy \
                     thrift/thriftl.ll

clean-local:
	$(RM) thrift/thriftl.cc thrift/thrifty.cc thrift/thrifty.h thrift/thrifty.hh

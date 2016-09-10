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

AUTOMAKE_OPTIONS = subdir-objects

AM_YFLAGS = -d

BUILT_SOURCES = thrifty.cc

noinst_LIBRARIES = libparse.a

libparse_a_CPPFLAGS = -I$(srcdir)
libparse_a_CXXFLAGS = -Wall -Wno-sign-compare -Wno-unused

libparse_a_SOURCES = thrifty.yy \
                     thriftl.ll

clean-local:
	$(RM) thriftl.cc thrifty.cc thrifty.h thrifty.hh

if WITH_PLUGIN
noinst_PROGRAMS = thrift-bootstrap

thrift_bootstrap_SOURCES = \
                 common.h \
                 common.cc \
                 audit/t_audit.h \
                 audit/t_audit.cpp \
                 generate/t_generator.cc \
                 generate/t_generator_registry.h \
                 globals.h \
                 platform.h \
                 logging.h \
                 parse/t_doc.h \
                 parse/t_type.h \
                 parse/t_base_type.h \
                 parse/t_enum.h \
                 parse/t_enum_value.h \
                 parse/t_typedef.h \
                 parse/t_typedef.cc \
                 parse/t_container.h \
                 parse/t_list.h \
                 parse/t_set.h \
                 parse/t_map.h \
                 parse/t_struct.h \
                 parse/t_field.h \
                 parse/t_service.h \
                 parse/t_function.h \
                 parse/t_program.h \
                 parse/t_scope.h \
                 parse/t_const.h \
                 parse/t_const_value.h \
                 parse/parse.cc \
                 generate/t_generator.h \
                 generate/t_oop_generator.h \
                 generate/t_html_generator.h \
                 windows/config.h \
                 windows/version.h \
                 generate/t_cpp_generator.cc \
                 main.h \
                 main.cc

main.cc: version.h

thrift_bootstrap_CXXFLAGS = -Wall -Wextra -pedantic
thrift_bootstrap_LDADD = @LEXLIB@ libparse.a
endif

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

SUBDIRS = src .
if WITH_TESTS
SUBDIRS += test
endif

bin_PROGRAMS = thrift

thrift_OBJDIR = obj

thrift_SOURCES = src/thrift/audit/t_audit.cpp \
                 src/thrift/audit/t_audit.h \
                 src/thrift/common.cc \
                 src/thrift/common.h \
                 src/thrift/generate/t_generator.cc \
                 src/thrift/generate/t_generator.h \
                 src/thrift/generate/t_generator_registry.h \
                 src/thrift/generate/t_html_generator.h \
                 src/thrift/generate/t_oop_generator.h \
                 src/thrift/globals.h \
                 src/thrift/logging.h \
                 src/thrift/main.cc \
                 src/thrift/main.h \
                 src/thrift/version.h \
                 src/thrift/parse/parse.cc \
                 src/thrift/parse/t_base_type.h \
                 src/thrift/parse/t_const.h \
                 src/thrift/parse/t_const_value.h \
                 src/thrift/parse/t_container.h \
                 src/thrift/parse/t_doc.h \
                 src/thrift/parse/t_enum.h \
                 src/thrift/parse/t_enum_value.h \
                 src/thrift/parse/t_field.h \
                 src/thrift/parse/t_function.h \
                 src/thrift/parse/t_list.h \
                 src/thrift/parse/t_map.h \
                 src/thrift/parse/t_program.h \
                 src/thrift/parse/t_scope.h \
                 src/thrift/parse/t_service.h \
                 src/thrift/parse/t_set.h \
                 src/thrift/parse/t_struct.h \
                 src/thrift/parse/t_type.h \
                 src/thrift/parse/t_typedef.cc \
                 src/thrift/parse/t_typedef.h \
                 src/thrift/platform.h

# Specific client generator source
thrift_SOURCES += src/thrift/generate/t_as3_generator.cc \
                  src/thrift/generate/t_c_glib_generator.cc \
                  src/thrift/generate/t_cl_generator.cc \
                  src/thrift/generate/t_cpp_generator.cc \
                  src/thrift/generate/t_d_generator.cc \
                  src/thrift/generate/t_dart_generator.cc \
                  src/thrift/generate/t_delphi_generator.cc \
                  src/thrift/generate/t_erl_generator.cc \
                  src/thrift/generate/t_go_generator.cc \
                  src/thrift/generate/t_gv_generator.cc \
                  src/thrift/generate/t_haxe_generator.cc \
                  src/thrift/generate/t_hs_generator.cc \
                  src/thrift/generate/t_html_generator.cc \
                  src/thrift/generate/t_markdown_generator.cc \
                  src/thrift/generate/t_java_generator.cc \
                  src/thrift/generate/t_javame_generator.cc \
                  src/thrift/generate/t_js_generator.cc \
                  src/thrift/generate/t_json_generator.cc \
                  src/thrift/generate/t_lua_generator.cc \
                  src/thrift/generate/t_netstd_generator.cc \
                  src/thrift/generate/t_netstd_generator.h \
                  src/thrift/generate/t_ocaml_generator.cc \
                  src/thrift/generate/t_perl_generator.cc \
                  src/thrift/generate/t_php_generator.cc \
                  src/thrift/generate/t_py_generator.cc \
                  src/thrift/generate/t_rb_generator.cc \
                  src/thrift/generate/t_rs_generator.cc \
                  src/thrift/generate/t_st_generator.cc \
                  src/thrift/generate/t_swift_generator.cc \
                  src/thrift/generate/t_xml_generator.cc \
                  src/thrift/generate/t_xsd_generator.cc

thrift_CPPFLAGS = -I$(srcdir)/src
thrift_CXXFLAGS = -Wall -Wextra -pedantic -Werror
thrift_LDADD = @LEXLIB@ src/thrift/libparse.a

WINDOWS_DIST = \
             compiler.sln \
             compiler.vcxproj \
             compiler.vcxproj.filters

EXTRA_DIST = \
             coding_standards.md \
             README.md \
             CMakeLists.txt \
             test \
             tests \
             $(WINDOWS_DIST)

#clean-local:
#	$(RM) version.h   -- do not delete, we need it

src/thrift/main.cc: src/thrift/version.h

style-local:
	$(CPPSTYLE_CMD)

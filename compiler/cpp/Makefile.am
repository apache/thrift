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

AM_YFLAGS = -d
BUILT_SOURCES =

bin_PROGRAMS = thrift

noinst_LIBRARIES = libparse.a

thrift_OBJDIR = obj

thrift_SOURCES = src/main.cc \
                 src/md5.c \
                 src/generate/t_generator.cc \
                 src/generate/t_generator_registry.h \
                 src/globals.h \
                 src/main.h \
                 src/platform.h \
                 src/md5.h \
                 src/parse/t_doc.h \
                 src/parse/t_type.h \
                 src/parse/t_base_type.h \
                 src/parse/t_enum.h \
                 src/parse/t_enum_value.h \
                 src/parse/t_typedef.h \
                 src/parse/t_container.h \
                 src/parse/t_list.h \
                 src/parse/t_set.h \
                 src/parse/t_map.h \
                 src/parse/t_struct.h \
                 src/parse/t_field.h \
                 src/parse/t_service.h \
                 src/parse/t_function.h \
                 src/parse/t_program.h \
                 src/parse/t_scope.h \
                 src/parse/t_const.h \
                 src/parse/t_const_value.h \
                 src/parse/parse.cc \
                 src/generate/t_generator.h \
                 src/generate/t_oop_generator.h

if THRIFT_GEN_c_glib
thrift_SOURCES += src/generate/t_c_glib_generator.cc
endif
if THRIFT_GEN_cpp
thrift_SOURCES += src/generate/t_cpp_generator.cc
endif
if THRIFT_GEN_java
thrift_SOURCES += src/generate/t_java_generator.cc
endif
if THRIFT_GEN_as3
thrift_SOURCES += src/generate/t_as3_generator.cc
endif
if THRIFT_GEN_csharp
thrift_SOURCES += src/generate/t_csharp_generator.cc
endif
if THRIFT_GEN_py
thrift_SOURCES += src/generate/t_py_generator.cc
endif
if THRIFT_GEN_rb
thrift_SOURCES += src/generate/t_rb_generator.cc
endif
if THRIFT_GEN_perl
thrift_SOURCES += src/generate/t_perl_generator.cc
endif
if THRIFT_GEN_php
thrift_SOURCES += src/generate/t_php_generator.cc
endif
if THRIFT_GEN_erl
thrift_SOURCES += src/generate/t_erl_generator.cc
endif
if THRIFT_GEN_cocoa
thrift_SOURCES += src/generate/t_cocoa_generator.cc
endif
if THRIFT_GEN_st
thrift_SOURCES += src/generate/t_st_generator.cc
endif
if THRIFT_GEN_ocaml
thrift_SOURCES += src/generate/t_ocaml_generator.cc
endif
if THRIFT_GEN_hs
thrift_SOURCES += src/generate/t_hs_generator.cc
endif
if THRIFT_GEN_xsd
thrift_SOURCES += src/generate/t_xsd_generator.cc
endif
if THRIFT_GEN_html
thrift_SOURCES += src/generate/t_html_generator.cc
endif
if THRIFT_GEN_js
thrift_SOURCES += src/generate/t_js_generator.cc
endif
if THRIFT_GEN_javame
thrift_SOURCES += src/generate/t_javame_generator.cc
endif
if THRIFT_GEN_go
thrift_SOURCES += src/generate/t_go_generator.cc
endif

thrift_CPPFLAGS = -I$(srcdir)/src
thrift_CXXFLAGS = -Wall
thrift_LDADD = @LEXLIB@ libparse.a

libparse_a_CPPFLAGS = -I$(srcdir)/src
libparse_a_CXXFLAGS = -Wall -Wno-sign-compare -Wno-unused

libparse_a_SOURCES = src/thrifty.yy \
                     src/thriftl.ll

EXTRA_DIST = README

clean-local:
	$(RM) thriftl.cc thrifty.cc thrifty.h version.h

src/main.cc: version.h

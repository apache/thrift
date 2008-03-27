AM_YFLAGS = -d

bin_PROGRAMS = thrift

thrift_OBJDIR = obj

thrift_SOURCES = src/thrifty.yy \
                 src/thriftl.ll \
                 src/main.cc \
                 src/md5c.c \
                 src/generate/t_generator.cc \
                 src/generate/t_php_generator.cc \
                 src/generate/t_py_generator.cc \
                 src/generate/t_rb_generator.cc \
                 src/generate/t_xsd_generator.cc \
                 src/generate/t_perl_generator.cc \
                 src/generate/t_erl_generator.cc \
                 src/generate/t_cocoa_generator.cc \
                 src/generate/t_csharp_generator.cc \
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
                 src/generate/t_generator.h \
                 src/generate/t_oop_generator.h \
                 src/generate/t_php_generator.h \
                 src/generate/t_py_generator.h \
                 src/generate/t_rb_generator.h \
                 src/generate/t_xsd_generator.h \
                 src/generate/t_perl_generator.h \
                 src/generate/t_erl_generator.h \
                 src/generate/t_cocoa_generator.h \
                 src/generate/t_csharp_generator.h

if THRIFT_GEN_cpp
thrift_SOURCES += src/generate/t_cpp_generator.cc
endif
if THRIFT_GEN_java
thrift_SOURCES += src/generate/t_java_generator.cc
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

thrift_CXXFLAGS = -Wall -I$(srcdir)/src $(BOOST_CPPFLAGS)
thrift_LDFLAGS = -Wall $(BOOST_LDFLAGS)

thrift_LDADD = @LEXLIB@

EXTRA_DIST = README

clean-local:
	$(RM) thriftl.cc thrifty.cc thrifty.h

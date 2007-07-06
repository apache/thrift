AM_YFLAGS = -d

bin_PROGRAMS = thrift

thrift_OBJDIR = obj

thrift_SOURCES = src/thrifty.yy \
                 src/thriftl.ll \
                 src/main.cc \
                 src/generate/t_generator.cc \
                 src/generate/t_cpp_generator.cc \
                 src/generate/t_java_generator.cc \
                 src/generate/t_php_generator.cc \
                 src/generate/t_py_generator.cc \
                 src/generate/t_rb_generator.cc \
                 src/generate/t_xsd_generator.cc \
                 src/generate/t_perl_generator.cc \
                 src/generate/t_erl_generator.cc

thrift_CXXFLAGS = -Wall -Isrc $(BOOST_CPPFLAGS)
thrift_LDFLAGS = -Wall $(BOOST_LDFLAGS)

thrift_LDADD = @LEXLIB@

clean-local:
	rm -rf thriftl.cc thrifty.cc thrifty.h

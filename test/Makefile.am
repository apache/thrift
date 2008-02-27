SUBDIRS = py

if ENABLE_JAVA
SUBDIRS += java
endif

check_PROGRAMS = \
	DebugProtoTest \
	JSONProtoTest \
	OptionalRequiredTest \
	ReflectionTest

TESTS = \
	$(check_PROGRAMS)


#
# DebugProtoTest
#
DebugProtoTest_SOURCES = \
	gen-cpp/DebugProtoTest_types.cpp \
	DebugProtoTest.cpp

DebugProtoTest_LDADD = \
	$(top_srcdir)/lib/cpp/libthrift.la


#
# JSONProtoTest
#
JSONProtoTest_SOURCES = \
	gen-cpp/DebugProtoTest_types.cpp \
	JSONProtoTest.cpp

JSONProtoTest_LDADD = \
	$(top_srcdir)/lib/cpp/libthrift.la

#
# OptionalRequiredTest
#
OptionalRequiredTest_SOURCES = \
	gen-cpp/OptionalRequiredTest_types.cpp \
	OptionalRequiredTest.cpp

OptionalRequiredTest_LDADD = \
	$(top_srcdir)/lib/cpp/libthrift.la


#
# ReflectionTest
#
ReflectionTest_SOURCES = \
	gen-cpp/DebugProtoTest_types.cpp \
	gen-cpp/PartiallyReflectable.cpp \
	gen-cpp/Service.cpp \
	gen-cpp/StressTest_types.cpp \
	ReflectionTest.cpp

ReflectionTest_LDADD = \
	$(top_srcdir)/lib/cpp/libthrift.la


#
# Common thrift code generation rules
#
THRIFT = $(top_srcdir)/compiler/cpp/thrift

gen-cpp/DebugProtoTest_types.cpp gen-cpp/PartiallyReflectable.cpp: DebugProtoTest.thrift
	$(THRIFT) --gen cpp:dense,reflection_limited $<

gen-cpp/OptionalRequiredTest_types.cpp: OptionalRequiredTest.thrift
	$(THRIFT) --gen cpp:dense $<

gen-cpp/Service.cpp gen-cpp/StressTest_types.cpp: StressTest.thrift
	$(THRIFT) --gen cpp:dense,reflection_limited $<


INCLUDES = \
	-I$(top_srcdir)/lib/cpp/src

AM_CPPFLAGS = $(BOOST_CPPFLAGS)

clean-local:
	$(RM) -r gen-cpp

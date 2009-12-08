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

AC_PREREQ(2.59)

AC_INIT([thrift], [0.2.0])

AC_CONFIG_AUX_DIR([.])

AM_INIT_AUTOMAKE

AC_ARG_VAR([PY_PREFIX], [Prefix for installing Python modules.
                         (Normal --prefix is ignored for Python because
                         Python has different conventions.)
                         Default = "/usr"])
AS_IF([test "x$PY_PREFIX" = x], [PY_PREFIX="/usr"])

AC_ARG_VAR([JAVA_PREFIX], [Prefix for installing the Java lib jar.
                           (Normal --prefix is ignored for Java because
                           Java has different conevntions.)
                           Default = "/usr/local/lib"])
AS_IF([test "x$JAVA_PREFIX" = x], [JAVA_PREFIX="/usr/local/lib"])

AC_PROG_CC
AC_PROG_CPP
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_LIBTOOL
AC_PROG_MAKE_SET
AC_PROG_YACC
AC_PROG_LEX
AM_PROG_LEX
AC_PROG_LN_S
AC_PROG_MKDIR_P

AC_LANG([C++])
AX_BOOST_BASE([1.33.1])

AX_LIB_EVENT([1.0])
AM_CONDITIONAL([AMX_HAVE_LIBEVENT], [test "$success" = "yes"])

AX_LIB_ZLIB([1.2.3])
AM_CONDITIONAL([AMX_HAVE_ZLIB], [test "$success" = "yes"])

AX_THRIFT_LIB(csharp, [C#], yes)
if test "$with_csharp" = "yes";  then
  PKG_CHECK_MODULES(MONO, mono >= 2.0.0, net_3_5=yes, net_3_5=no)
  PKG_CHECK_MODULES(MONO, mono >= 1.2.4, have_mono=yes, have_mono=no)
fi
AM_CONDITIONAL(WITH_MONO, [test "$have_mono" = "yes"])
AM_CONDITIONAL(NET_2_0, [test "$net_3_5" = "no"])

AX_THRIFT_LIB(java, [Java], yes)
if test "$with_java" = "yes";  then
  AX_JAVAC_AND_JAVA
  AC_PATH_PROG([ANT], [ant])
  AX_CHECK_JAVA_CLASS(org.slf4j.Logger)
  have_slf4j="$success"
  AX_CHECK_JAVA_CLASS(org.slf4j.impl.SimpleLogger)
  have_slf4j_simple="$success"
  AX_CHECK_JAVA_CLASS(org.apache.commons.lang.builder.HashCodeBuilder)
  have_hashcode="$success"
  if test "x$have_slf4j_simple" = "xyes" && test "x$have_slf4j" = "xyes" && test "x$have_hashcode" = "xyes" ; then
    ANT_FLAGS="$ANT_FLAGS -Dnoivy="
  fi
  AC_SUBST(CLASSPATH)
  AC_SUBST(ANT_FLAGS)
fi
AM_CONDITIONAL([WITH_JAVA],
               [test -n "$ANT" -a -n "$JAVA" -a -n "$JAVAC"])

AX_THRIFT_LIB(erlang, [Erlang], yes)
if test "$with_erlang" = "yes";  then
  AC_PATH_PROG([ERLC], [erlc])
fi
AM_CONDITIONAL(WITH_ERLANG, [test -n "$ERLC"])

AX_THRIFT_LIB(py, [Python], yes)
if test "$with_py" = "yes";  then
  AM_PATH_PYTHON(2.4,, :)
fi
AM_CONDITIONAL(WITH_PYTHON, [test -n "$PYTHON" -a "$PYTHON" != ":"])

AX_THRIFT_LIB(perl, [Perl], yes)
if test "$with_perl" = "yes"; then
  AC_PATH_PROG([PERL], [perl])
fi
AM_CONDITIONAL(WITH_PERL, [test -n "$PERL"])

AX_THRIFT_LIB(ruby, [Ruby], yes)
if test "$with_ruby" = "yes"; then
  AC_PATH_PROG([RUBY], [ruby])
  AC_PATH_PROG([RSPEC], [spec])
fi
AM_CONDITIONAL(WITH_RUBY, [test -n "$RUBY"])
AM_CONDITIONAL(HAVE_RSPEC, [test -n "$RSPEC"])


AC_C_CONST
AC_C_INLINE
AC_C_VOLATILE

AC_HEADER_STDBOOL
AC_HEADER_STDC
AC_HEADER_TIME
AC_CHECK_HEADERS([arpa/inet.h])
AC_CHECK_HEADERS([endian.h])
AC_CHECK_HEADERS([fcntl.h])
AC_CHECK_HEADERS([inttypes.h])
AC_CHECK_HEADERS([limits.h])
AC_CHECK_HEADERS([netdb.h])
AC_CHECK_HEADERS([netinet/in.h])
AC_CHECK_HEADERS([pthread.h])
AC_CHECK_HEADERS([stddef.h])
AC_CHECK_HEADERS([stdlib.h])
AC_CHECK_HEADERS([sys/socket.h])
AC_CHECK_HEADERS([sys/time.h])
AC_CHECK_HEADERS([unistd.h])
AC_CHECK_HEADERS([libintl.h])
AC_CHECK_HEADERS([malloc.h])

AC_CHECK_LIB(pthread, pthread_create)
dnl NOTE(dreiss): I haven't been able to find any really solid docs
dnl on what librt is and how it fits into various Unix systems.
dnl My best guess is that it is where glibc stashes its implementation
dnl of the POSIX Real-Time Extensions.  This seems necessary on Linux,
dnl and we haven't yet found a system where this is a problem.
AC_CHECK_LIB(rt, clock_gettime)
AC_CHECK_LIB(socket, setsockopt)

AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INT8_T
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T
AC_CHECK_TYPES([ptrdiff_t], [], [exit 1])

AC_STRUCT_TM

dnl NOTE(dreiss): AI_ADDRCONFIG is not defined on OpenBSD.
AC_CHECK_DECL([AI_ADDRCONFIG], [],
              [AC_DEFINE([AI_ADDRCONFIG], 0,
                         [Define if the AI_ADDRCONFIG symbol is unavailable])],
              [
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netdb.h>
])

AC_FUNC_ALLOCA
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_REALLOC
AC_FUNC_SELECT_ARGTYPES
AC_FUNC_STAT
AC_FUNC_STRERROR_R
AC_FUNC_STRFTIME
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([strtoul])
AC_CHECK_FUNCS([bzero])
AC_CHECK_FUNCS([ftruncate])
AC_CHECK_FUNCS([gethostbyname])
AC_CHECK_FUNCS([gettimeofday])
AC_CHECK_FUNCS([memmove])
AC_CHECK_FUNCS([memset])
AC_CHECK_FUNCS([mkdir])
AC_CHECK_FUNCS([realpath])
AC_CHECK_FUNCS([select])
AC_CHECK_FUNCS([socket])
AC_CHECK_FUNCS([strchr])
AC_CHECK_FUNCS([strdup])
AC_CHECK_FUNCS([strerror])
AC_CHECK_FUNCS([strstr])
AC_CHECK_FUNCS([strtol])
AC_CHECK_FUNCS([sqrt])
dnl The following functions are optional.
AC_CHECK_FUNCS([clock_gettime])
AC_CHECK_FUNCS([sched_get_priority_min])
AC_CHECK_FUNCS([sched_get_priority_max])

AX_SIGNED_RIGHT_SHIFT

AX_THRIFT_GEN(cpp, [C++], yes)
AM_CONDITIONAL([THRIFT_GEN_cpp], [test "$ax_thrift_gen_cpp" = "yes"])
AX_THRIFT_GEN(java, [Java], yes)
AM_CONDITIONAL([THRIFT_GEN_java], [test "$ax_thrift_gen_java" = "yes"])
AX_THRIFT_GEN(csharp, [C#], yes)
AM_CONDITIONAL([THRIFT_GEN_csharp], [test "$ax_thrift_gen_csharp" = "yes"])
AX_THRIFT_GEN(py, [Python], yes)
AM_CONDITIONAL([THRIFT_GEN_py], [test "$ax_thrift_gen_py" = "yes"])
AX_THRIFT_GEN(rb, [Ruby], yes)
AM_CONDITIONAL([THRIFT_GEN_rb], [test "$ax_thrift_gen_rb" = "yes"])
AX_THRIFT_GEN(perl, [Perl], yes)
AM_CONDITIONAL([THRIFT_GEN_perl], [test "$ax_thrift_gen_perl" = "yes"])
AX_THRIFT_GEN(php, [PHP], yes)
AM_CONDITIONAL([THRIFT_GEN_php], [test "$ax_thrift_gen_php" = "yes"])
AX_THRIFT_GEN(erl, [Erlang], yes)
AM_CONDITIONAL([THRIFT_GEN_erl], [test "$ax_thrift_gen_erl" = "yes"])
AX_THRIFT_GEN(cocoa, [Cocoa], yes)
AM_CONDITIONAL([THRIFT_GEN_cocoa], [test "$ax_thrift_gen_cocoa" = "yes"])
AX_THRIFT_GEN(st, [Smalltalk], yes)
AM_CONDITIONAL([THRIFT_GEN_st], [test "$ax_thrift_gen_st" = "yes"])
AX_THRIFT_GEN(ocaml, [OCaml], yes)
AM_CONDITIONAL([THRIFT_GEN_ocaml], [test "$ax_thrift_gen_ocaml" = "yes"])
AX_THRIFT_GEN(hs, [Haskell], yes)
AM_CONDITIONAL([THRIFT_GEN_hs], [test "$ax_thrift_gen_hs" = "yes"])
AX_THRIFT_GEN(xsd, [XSD], yes)
AM_CONDITIONAL([THRIFT_GEN_xsd], [test "$ax_thrift_gen_xsd" = "yes"])
AX_THRIFT_GEN(html, [HTML], yes)
AM_CONDITIONAL([THRIFT_GEN_html], [test "$ax_thrift_gen_html" = "yes"])

AC_CONFIG_HEADERS(config.h:config.hin)

AC_CONFIG_FILES([
  Makefile
  compiler/cpp/Makefile
  lib/Makefile
  lib/cpp/Makefile
  lib/cpp/thrift.pc
  lib/cpp/thrift-nb.pc
  lib/cpp/thrift-z.pc
  lib/csharp/Makefile
  lib/java/Makefile
  lib/perl/Makefile
  lib/perl/test/Makefile
  lib/py/Makefile
  lib/rb/Makefile
  test/Makefile
  test/py/Makefile
  test/rb/Makefile
])

AC_OUTPUT

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

AC_PREREQ(2.65)

AC_INIT([thrift], [0.7.0])

AC_CONFIG_AUX_DIR([.])

AM_INIT_AUTOMAKE([1.9 tar-ustar])
PKG_PROG_PKG_CONFIG

AC_ARG_VAR([PY_PREFIX], [Prefix for installing Python modules.
                         (Normal --prefix is ignored for Python because
                         Python has different conventions.)
                         Default = "/usr"])
AS_IF([test "x$PY_PREFIX" = x], [PY_PREFIX="/usr"])

AC_ARG_VAR([JAVA_PREFIX], [Prefix for installing the Java lib jar.
                           (Normal --prefix is ignored for Java because
                           Java has different conventions.)
                           Default = "/usr/local/lib"])
AS_IF([test "x$JAVA_PREFIX" = x], [JAVA_PREFIX="/usr/local/lib"])

AC_ARG_VAR([RUBY_PREFIX], [Prefix for installing Ruby modules.
                           (Normal --prefix is ignored for Ruby because
                           Ruby has different conventions.)
                           Default = none, let ruby setup decide])

AC_ARG_VAR([PHP_PREFIX], [Prefix for installing PHP modules.
                         (Normal --prefix is ignored for PHP because
                         PHP has different conventions.)
                         Default = "/usr/lib/php"])
AS_IF([test "x$PHP_PREFIX" = x], [PHP_PREFIX="/usr/lib/php"])

AC_ARG_VAR([PHP_CONFIG_PREFIX],
           [Prefix for installing PHP extension module .ini file.
            (Normal --prefix is ignored for PHP because PHP has
             different conventions.)
           Default = "/etc/php.d"])
AS_IF([test "x$PHP_CONFIG_PREFIX" = x], [PHP_CONFIG_PREFIX="/etc/php.d"])

AC_ARG_VAR([INSTALLDIRS], [When installing Perl modules, specifies which
                           of the sets of installation directories
                           to choose: perl, site or vendor.
                           Default = "vendor"])
AS_IF([test "x$INSTALLDIRS" = x], [INSTALLDIRS="vendor"])

AC_ARG_VAR([PERL_PREFIX], [Prefix for installing Perl modules.
                           (Normal --prefix is ignored for Perl because
                           Perl has different conventions.)
                           Ignored, when INSTALLDIRS set to site or vendor.
                           Default = "/usr/local/lib"])
AS_IF([test "x$PERL_PREFIX" = x], [PERL_PREFIX="/usr/local"])

AC_ARG_VAR([CABAL_CONFIGURE_FLAGS],
           [Extra flags to pass to cabal: "cabal Setup.lhs configure $CABAL_CONFIGURE_FLAGS".
            (Typically used to set --user or force --global.)])

AC_SUBST(CABAL_CONFIGURE_FLAGS)

AC_ARG_VAR([GOROOT], [Prefix for the Go source directory.
                           (Normal --prefix is ignored for Go because
                           Go has different conventions.)
                           Default = "/usr/local/share/go"])
AS_IF([test "x$GOROOT" = x], [GOROOT="/usr/local/share/go"])

AC_ARG_VAR([GOARCH], [Architecture default for Go.
                           Default = "amd64"])
AS_IF([test "x$GOARCH" = x], [GOARCH="amd64"])

AC_ARG_VAR([GOBIN], [Binary directory for Go.
                           Default = "/usr/local/bin"])
AS_IF([test "x$GOBIN" = x], [GOBIN="/usr/local/bin"])

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
AC_PROG_AWK
AC_PROG_RANLIB

AC_LANG([C++])

AX_THRIFT_LIB(cpp, [C++], yes)
have_cpp=no
if test "$with_cpp" = "yes";  then
  AX_BOOST_BASE([1.33.1])
  if test "x$succeeded" = "xyes" ; then
    have_cpp="yes"
  fi

  AX_LIB_EVENT([1.0])
  have_libevent=$success

  AX_LIB_ZLIB([1.2.3])
  have_zlib=$success
fi
AM_CONDITIONAL([WITH_CPP], [test "$have_cpp" = "yes"])
AM_CONDITIONAL([AMX_HAVE_LIBEVENT], [test "$have_libevent" = "yes"])
AM_CONDITIONAL([AMX_HAVE_ZLIB], [test "$have_zlib" = "yes"])

AX_THRIFT_LIB(c_glib, [C (GLib)], no)
if test "$with_c_glib" = "yes"; then
  PKG_CHECK_MODULES([GLIB], [glib-2.0 >= 2.0], have_glib2=yes, have_glib2=no)
  PKG_CHECK_MODULES([GOBJECT], [gobject-2.0 >= 2.0], have_gobject2=yes, have_gobject2=no)
  if test "$have_glib2" = "yes" -a "$have_gobject2" = "yes" ; then
    have_c_glib="yes"
  fi
fi
AM_CONDITIONAL(WITH_C_GLIB, [test "$have_glib2" = "yes" -a "$have_gobject2" = "yes"])

AX_THRIFT_LIB(csharp, [C#], yes)
if test "$with_csharp" = "yes";  then
  PKG_CHECK_MODULES(MONO, mono >= 2.0.0, net_3_5=yes, net_3_5=no)
  PKG_CHECK_MODULES(MONO, mono >= 1.2.4, have_mono=yes, have_mono=no)
  if test "$have_mono" = "yes" ; then
    have_csharp="yes"
  fi
fi
AM_CONDITIONAL(WITH_MONO, [test "$have_csharp" = "yes"])
AM_CONDITIONAL(NET_2_0, [test "$net_3_5" = "no"])

AX_THRIFT_LIB(java, [Java], yes)
if test "$with_java" = "yes";  then
  AX_JAVAC_AND_JAVA
  AC_PATH_PROG([ANT], [ant])
  AX_CHECK_ANT_VERSION($ANT, 1.7)
  AC_SUBST(CLASSPATH)
  AC_SUBST(ANT_FLAGS)
  if test "x$JAVAC" != "x" && test "x$JAVAC" != "x" && test "x$ANT" != "x" ; then
    have_java="yes"
  fi
fi
AM_CONDITIONAL([WITH_JAVA], [test "$have_java" = "yes"])

AX_THRIFT_LIB(erlang, [Erlang], yes)
if test "$with_erlang" = "yes";  then
  AC_ERLANG_PATH_ERL
  AC_ERLANG_PATH_ERLC
  if test -n "$ERLC" ; then
    AC_ERLANG_SUBST_LIB_DIR
    # Install into the detected Erlang directory instead of $libdir/erlang/lib
    ERLANG_INSTALL_LIB_DIR="$ERLANG_LIB_DIR"
    AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_NAME, AC_PACKAGE_VERSION)
  fi
  if test -n "$ERL" -a -n "$ERLC" ; then
    have_erlang="yes"
  fi
fi
AM_CONDITIONAL(WITH_ERLANG, [test "$have_erlang" = "yes"])

AX_THRIFT_LIB(python, [Python], yes)
if test "$with_python" = "yes";  then
  AM_PATH_PYTHON(2.4,, :)
  if test "x$PYTHON" != "x" && test "x$PYTHON" != "x:" ; then
    have_python="yes"
  fi
fi
AM_CONDITIONAL(WITH_PYTHON, [test "$have_python" = "yes"])

AC_PATH_PROG([TRIAL], [trial])

AX_THRIFT_LIB(perl, [Perl], yes)
if test "$with_perl" = "yes"; then
  AC_PATH_PROG([PERL], [perl])
  if test -n "$PERL" ; then
    AC_PROG_PERL_MODULES([Bit::Vector], success="yes", success="no")
    have_perl_bit_vector="$success"
  fi
  if test -n "$PERL" -a "$have_perl_bit_vector" = "yes" ; then
    have_perl="yes"
  fi
fi
AM_CONDITIONAL(WITH_PERL, [test "$have_perl" = "yes"])

AX_THRIFT_LIB(php, [PHP], yes)
if test "$with_php" = "yes"; then
  AC_PATH_PROG([PHP], [php])
  if test -n "$PHP" ; then
    have_php="yes"
  fi
fi
AM_CONDITIONAL(WITH_PHP, [test "$have_php" = "yes"])

AX_THRIFT_LIB(php_extension, [PHP_EXTENSION], yes)
if test "$with_php_extension" = "yes"; then
  AC_PATH_PROG([PHP_CONFIG], [php-config])
  if test -n "$PHP_CONFIG" ; then
    AC_CONFIG_SUBDIRS([lib/php/src/ext/thrift_protocol])
    have_php_extension="yes"
  fi
fi
AM_CONDITIONAL(WITH_PHP_EXTENSION, [test "$have_php_extension" = "yes"])

AX_THRIFT_LIB(ruby, [Ruby], yes)
have_ruby=no
if test "$with_ruby" = "yes"; then
  AC_PATH_PROG([RUBY], [ruby])
  AC_PATH_PROG([RSPEC], [spec])
  if test "x$RUBY" != "x" ; then
    have_ruby="yes"
  fi
fi
AM_CONDITIONAL(WITH_RUBY, [test "$have_ruby" = "yes"])
AM_CONDITIONAL(HAVE_RSPEC, [test "x$RSPEC" != "x"])

AX_THRIFT_LIB(haskell, [Haskell], yes)
have_haskell=no
RUNHASKELL=true
CABAL=true
if test "$with_haskell" = "yes"; then
  AC_PATH_PROG([CABAL], [cabal])
  AC_PATH_PROG([RUNHASKELL], [runhaskell])
  if test "x$CABAL" != "x" -a "x$RUNHASKELL" != "x"; then
    have_haskell="yes"
  else
    RUNHASKELL=true
    CABAL=true
  fi
fi
AC_SUBST(CABAL)
AC_SUBST(RUNHASKELL)
AM_CONDITIONAL(WITH_HASKELL, [test "$have_haskell" = "yes"])

AX_THRIFT_LIB(go, [Go], yes)
if test "$with_go" = "yes";  then
  case X"$GOARCH" in
  Xamd64)
    GOARCH_NUM=6
  	;;
  X386)
    GOARCH_NUM=8
  	;;
  Xarm)
    GOARCH_NUM=5
  	;;
  *)
    GOARCH_NUM=6
    ;;
  esac
  GO_C=${GOBIN}/${GOARCH_NUM}g
  GO_L=${GOBIN}/${GOARCH_NUM}l
  GOMAKE=${GOBIN}/gomake
  GOINSTALL=${GOBIN}/goinstall
  AC_PATH_PROG([GO_C], [${GOARCH_NUM}g])
  AC_PATH_PROG([GO_L], [${GOARCH_NUM}l])
  AC_PATH_PROG([GOMAKE], [gomake])
  AC_PATH_PROG([GOINSTALL], [goinstall])
  if [[ -x "$GO_C" -a -x "$GO_L" -a -x "$GOMAKE" -a -x "$GOINSTALL" ]] ; then
    have_go="yes"
  fi
fi
AM_CONDITIONAL(WITH_GO, [test "$have_go" = "yes"])

AC_C_CONST
AC_C_INLINE
AC_C_VOLATILE

AC_HEADER_STDBOOL
AC_HEADER_STDC
AC_HEADER_TIME
AC_HEADER_SYS_WAIT
AC_TYPE_SIGNAL
AC_CHECK_HEADERS([arpa/inet.h])
AC_CHECK_HEADERS([sys/param.h])
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
AC_CHECK_HEADERS([openssl/ssl.h])
AC_CHECK_HEADERS([openssl/rand.h])
AC_CHECK_HEADERS([openssl/x509v3.h])

AC_CHECK_LIB(pthread, pthread_create)
dnl NOTE(dreiss): I haven't been able to find any really solid docs
dnl on what librt is and how it fits into various Unix systems.
dnl My best guess is that it is where glibc stashes its implementation
dnl of the POSIX Real-Time Extensions.  This seems necessary on Linux,
dnl and we haven't yet found a system where this is a problem.
AC_CHECK_LIB(rt, clock_gettime)
AC_CHECK_LIB(socket, setsockopt)
AC_CHECK_LIB(ssl, SSL_ctrl)

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
AC_FUNC_FORK
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
AC_CHECK_FUNCS([alarm])
AC_CHECK_FUNCS([clock_gettime])
AC_CHECK_FUNCS([sched_get_priority_min])
AC_CHECK_FUNCS([sched_get_priority_max])

if test "$cross_compiling" = "no" ; then
  AX_SIGNED_RIGHT_SHIFT
fi

dnl autoscan thinks we need this macro because we have a member function
dnl called "error".  Invoke the macro but don't run the check so autoscan
dnl thinks we are in the clear.  It's highly unlikely that we will ever
dnl actually use the function that this checks for.
if false ; then
  AC_FUNC_ERROR_AT_LINE
fi

AX_THRIFT_GEN(cpp, [C++], yes)
AM_CONDITIONAL([THRIFT_GEN_cpp], [test "$ax_thrift_gen_cpp" = "yes"])
AX_THRIFT_GEN(c_glib, [C (GLib)], yes)
AM_CONDITIONAL([THRIFT_GEN_c_glib], [test "$ax_thrift_gen_c_glib" = "yes"])
AX_THRIFT_GEN(java, [Java], yes)
AM_CONDITIONAL([THRIFT_GEN_java], [test "$ax_thrift_gen_java" = "yes"])
AX_THRIFT_GEN(as3, [AS3], yes)
AM_CONDITIONAL([THRIFT_GEN_as3], [test "$ax_thrift_gen_as3" = "yes"])
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
AX_THRIFT_GEN(js, [JavaScript], yes)
AM_CONDITIONAL([THRIFT_GEN_js], [test "$ax_thrift_gen_js" = "yes"])
AX_THRIFT_GEN(javame, [JavaME], yes)
AM_CONDITIONAL([THRIFT_GEN_javame], [test "$ax_thrift_gen_javame" = "yes"])
AX_THRIFT_GEN(go, [GO_C], yes)
AM_CONDITIONAL([THRIFT_GEN_go], [test "$ax_thrift_gen_go" = "yes"])

# --- Coverage hooks ---

AC_ARG_ENABLE(coverage,
              [  --enable-coverage      turn on -fprofile-arcs -ftest-coverage],
              [case "${enableval}" in
                yes) ENABLE_COVERAGE=1 ;;
                no) ENABLE_COVERAGE=0 ;;
                *) AC_MSG_ERROR(bad value ${enableval} for --enable-cov) ;;
              esac],
              [ENABLE_COVERAGE=2])

if test "x[$]ENABLE_COVERAGE" = "x1"; then
  AC_MSG_WARN(enable coverage)
  GCOV_CFLAGS="`echo \"[$]CFLAGS\" | perl -pe 's/-O\d+//g;'` -fprofile-arcs -ftest-coverage"
  GCOV_CXXFLAGS="`echo \"[$]CXXFLAGS\" | perl -pe 's/-O\d+//g;'` -fprofile-arcs -ftest-coverage"
  GCOV_LDFLAGS="-XCClinker -fprofile-arcs -XCClinker -ftest-coverage"
fi

AC_SUBST(ENABLE_COVERAGE)
AC_SUBST(GCOV_CFLAGS)
AC_SUBST(GCOV_CXXFLAGS)
AC_SUBST(GCOV_LDFLAGS)

AC_CONFIG_HEADERS(config.h:config.hin)

AC_CONFIG_FILES([
  Makefile
  compiler/cpp/Makefile
  compiler/cpp/version.h
  lib/Makefile
  lib/cpp/Makefile
  lib/cpp/test/Makefile
  lib/cpp/thrift-nb.pc
  lib/cpp/thrift-z.pc
  lib/cpp/thrift.pc
  lib/c_glib/Makefile
  lib/c_glib/thrift_c_glib.pc
  lib/c_glib/test/Makefile
  lib/csharp/Makefile
  lib/erl/Makefile
  lib/erl/src/thrift.app.src
  lib/hs/Makefile
  lib/java/Makefile
  lib/js/test/Makefile
  lib/perl/Makefile
  lib/perl/test/Makefile
  lib/php/Makefile
  lib/py/Makefile
  lib/rb/Makefile
  test/Makefile
  test/cpp/Makefile
  test/hs/Makefile
  test/py/Makefile
  test/py.twisted/Makefile
  test/rb/Makefile
])

AC_OUTPUT


echo
echo "$PACKAGE $VERSION"
echo
echo "Building code generators ..... :$thrift_generators"
echo
echo "Building C++ Library ......... : $have_cpp"
echo "Building C (GLib) Library .... : $have_c_glib"
echo "Building Java Library ........ : $have_java"
echo "Building C# Library .......... : $have_csharp"
echo "Building Python Library ...... : $have_python"
echo "Building Ruby Library ........ : $have_ruby"
echo "Building Haskell Library ..... : $have_haskell"
echo "Building Perl Library ........ : $have_perl"
echo "Building PHP Library ......... : $have_php"
echo "Building Erlang Library ...... : $have_erlang"
echo "Building Go Library .......... : $have_go"
if test "$have_cpp" = "yes" ; then
  echo
  echo "Building TZlibTransport ...... : $have_zlib"
  echo "Building TNonblockingServer .. : $have_libevent"
fi
if test "$have_java" = "yes" ; then
  echo
  echo "Using javac .................. : $JAVAC"
  echo "Using java ................... : $JAVA"
  echo "Using ant .................... : $ANT"
fi
if test "$have_csharp" = "yes" ; then
  echo
  echo "Using .NET 3.5 ............... : $net_3_5"
fi
if test "$have_python" = "yes" ; then
  echo
  echo "Using Python ................. : $PYTHON"
fi
if test "$have_php" = "yes" ; then
  echo
  echo "Using php-config ............. : $PHP_CONFIG"
fi
if test "$have_ruby" = "yes" ; then
  echo
  echo "Using Ruby ................... : $RUBY"
  echo "Using rspec .................. : $RSPEC"
fi
if test "$have_haskell" = "yes" ; then
  echo
  echo "Using Haskell ................ : $RUNHASKELL"
  echo "Using Cabal .................. : $CABAL"
fi
if test "$have_perl" = "yes" ; then
  echo
  echo "Using Perl ................... : $PERL"
fi
if test "$have_erlang" = "yes" ; then
  echo
  echo "Using erlc ................... : $ERLC"
fi
if test "$have_go" = "yes" ; then
  echo
  echo "Using GOROOT.................. : $GOROOT"
  echo "Using GOBIN................... : $GOBIN"
  echo "Using GOARCH.................. : $GOARCH"
  echo "Using GO Compiler............. : $GO_C"
  echo "Using GO Linker............... : $GO_L"
fi
echo
echo "If something is missing that you think should be present,"
echo "please skim the output of configure to find the missing"
echo "component.  Details are present in config.log."

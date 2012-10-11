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

AC_INIT([thrift], [0.9.0])

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

AC_ARG_VAR([D_IMPORT_PREFIX], [Prefix for installing D modules.
                           [INCLUDEDIR/d2]])
AS_IF([test "x$D_IMPORT_PREFIX" = x], [D_IMPORT_PREFIX="${includedir}/d2"])

AC_ARG_VAR([DMD_LIBEVENT_FLAGS], [DMD flags for linking libevent (auto-detected if not set).])
AC_ARG_VAR([DMD_OPENSSL_FLAGS], [DMD flags for linking OpenSSL (auto-detected if not set).])

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
  AX_BOOST_BASE([1.40.0])
  if test "x$succeeded" = "xyes" ; then
    have_cpp="yes"
  fi

  AX_LIB_EVENT([1.0])
  have_libevent=$success

  AX_LIB_ZLIB([1.2.3])
  have_zlib=$success
  
  AX_THRIFT_LIB(qt4, [Qt], yes)
  have_qt=no
  if test "$with_qt4" = "yes";  then
    PKG_CHECK_MODULES([QT], [QtCore >= 4.3, QtNetwork >= 4.3], have_qt=yes, have_qt=no)
  fi
  if test "$have_qt" = "yes"; then
    AC_PATH_PROGS([QT_MOC], [moc-qt4 moc])
    have_qt=$success
  fi
fi
AM_CONDITIONAL([WITH_CPP], [test "$have_cpp" = "yes"])
AM_CONDITIONAL([AMX_HAVE_LIBEVENT], [test "$have_libevent" = "yes"])
AM_CONDITIONAL([AMX_HAVE_ZLIB], [test "$have_zlib" = "yes"])
AM_CONDITIONAL([AMX_HAVE_QT], [test "$have_qt" = "yes"])

AX_THRIFT_LIB(c_glib, [C (GLib)], yes)
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

AC_PATH_PROG([PHPUNIT], [phpunit])
AM_CONDITIONAL(HAVE_PHPUNIT, [test "x$PHPUNIT" != "x"])

AX_THRIFT_LIB(ruby, [Ruby], yes)
have_ruby=no
if test "$with_ruby" = "yes"; then
  AC_PATH_PROG([RUBY], [ruby])
  AC_PATH_PROG([RAKE], [rake])
  AC_PATH_PROG([BUNDLER], [bundle])
  if test "x$RUBY" != "x" -a "x$RAKE" != "x"; then
    have_ruby="yes"
  fi
fi
AM_CONDITIONAL(WITH_RUBY, [test "$have_ruby" = "yes"])
AM_CONDITIONAL(HAVE_BUNDLER, [test "x$BUNDLER" != "x"])

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


AX_THRIFT_LIB(d, [D], yes)
if test "$with_d" = "yes";  then
  AX_DMD
  AC_SUBST(DMD)
  if test "x$DMD" != "x"; then
    have_d="yes"
  fi
fi

# Determine actual name of the generated D library for use in the command line
# when compiling tests. This is needed because the -l<lib> syntax doesn't work
# with OPTLINK (Windows).
lib_prefix=lib
lib_suffix=a
case "$host_os" in
  cygwin* | mingw* | pw32* | cegcc*)
    lib_prefix=""
    lib_suffix=lib
    ;;
esac
D_LIB_NAME="${lib_prefix}thriftd.${lib_suffix}"
AC_SUBST(D_LIB_NAME)
D_EVENT_LIB_NAME="${lib_prefix}thriftd-event.${lib_suffix}"
AC_SUBST(D_EVENT_LIB_NAME)
D_SSL_LIB_NAME="${lib_prefix}thriftd-ssl.${lib_suffix}"
AC_SUBST(D_SSL_LIB_NAME)

if test "$have_d" = "yes"; then
  AX_CHECK_D_MODULE(deimos.event2.event)
  have_deimos_event2=$success

  with_d_event_tests="no"
  if test "$have_deimos_event2" = "yes"; then
    if test "x$DMD_LIBEVENT_FLAGS" = "x"; then
      if test "$dmd_optlink" = "yes"; then
        AC_MSG_WARN([D libevent interface found, but cannot auto-detect \
linker flags for OPTLINK. Please set DMD_LIBEVENT_FLAGS manually.])
      else
        AX_LIB_EVENT([2.0])
        if test "$success" = "yes"; then
          DMD_LIBEVENT_FLAGS=$(echo "$LIBEVENT_LDFLAGS $LIBEVENT_LIBS" | \
            sed -e 's/^ *//g;s/ *$//g;s/^\(.\)/-L\1/g;s/  */ -L/g')
          with_d_event_tests="yes"
        else
          AC_MSG_WARN([D libevent interface present, but libevent library not found.])
        fi
      fi
    else
      with_d_event_tests="yes"
    fi
  fi

  AX_CHECK_D_MODULE(deimos.openssl.ssl)
  have_deimos_openssl=$success

  with_d_ssl_tests="no"
  if test "$have_deimos_openssl" = "yes"; then
    if test "x$DMD_OPENSSL_FLAGS" = "x"; then
      if test "$dmd_optlink" = "yes"; then
        AC_MSG_WARN([D OpenSSL interface found, but cannot auto-detect \
linker flags for OPTLINK. Please set DMD_OPENSSL_FLAGS manually.])
      else
        AX_CHECK_OPENSSL([with_d_ssl_tests="yes"])
        if test "$with_d_ssl_tests" = "yes"; then
          DMD_OPENSSL_FLAGS=$(echo "$OPENSSL_LDFLAGS $OPENSSL_LIBS" | \
            sed -e 's/^ *//g;s/ *$//g;s/^\(.\)/-L\1/g;s/  */ -L/g')
        else
          AC_MSG_WARN([D OpenSSL interface present, but OpenSSL library not found.])
        fi
      fi
    else
      with_d_ssl_tests="yes"
    fi
  fi
fi

AM_CONDITIONAL(WITH_D, [test "$have_d" = "yes"])
AM_CONDITIONAL(DMD_OPTLINK, [test "$dmd_optlink" = "yes"])
AC_SUBST(DMD_OF_DIRSEP, "$dmd_of_dirsep")
AM_CONDITIONAL(HAVE_DEIMOS_EVENT2, [test "$have_deimos_event2" = "yes"])
AM_CONDITIONAL(WITH_D_EVENT_TESTS, [test "$with_d_event_tests" = "yes"])
AC_SUBST(DMD_LIBEVENT_FLAGS)
AM_CONDITIONAL(HAVE_DEIMOS_OPENSSL, [test "$have_deimos_openssl" = "yes"])
AM_CONDITIONAL(WITH_D_SSL_TESTS, [test "$with_d_ssl_tests" = "yes"])
AC_SUBST(DMD_OPENSSL_FLAGS)


have_tests=yes
if test "$with_tests" = "no"; then
  have_tests="no"
fi
AM_CONDITIONAL(WITH_TESTS, [test "$have_tests" = "yes"])

AM_CONDITIONAL(MINGW, false)
case "${host_os}" in
*mingw*)
  mingw32_support="yes"
  AC_CHECK_HEADER(windows.h)
  AM_CONDITIONAL(MINGW, true)
  ;;
*)
  AC_ISC_POSIX
  ;;
esac

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
AC_CHECK_HEADERS([sys/un.h])
AC_CHECK_HEADERS([sys/poll.h])
AC_CHECK_HEADERS([sys/resource.h])
AC_CHECK_HEADERS([unistd.h])
AC_CHECK_HEADERS([libintl.h])
AC_CHECK_HEADERS([malloc.h])
AC_CHECK_HEADERS([openssl/ssl.h])
AC_CHECK_HEADERS([openssl/rand.h])
AC_CHECK_HEADERS([openssl/x509v3.h])
AC_CHECK_HEADERS([sched.h])

AC_CHECK_LIB(pthread, pthread_create)
dnl NOTE(dreiss): I haven't been able to find any really solid docs
dnl on what librt is and how it fits into various Unix systems.
dnl My best guess is that it is where glibc stashes its implementation
dnl of the POSIX Real-Time Extensions.  This seems necessary on Linux,
dnl and we haven't yet found a system where this is a problem.
AC_CHECK_LIB(rt, clock_gettime)
AC_CHECK_LIB(socket, setsockopt)

if test "$have_cpp" = "yes" ; then
# mingw toolchain used to build "Thrift Compiler for Windows"
# does not support libcrypto, so we just check if we building the cpp library
AC_CHECK_LIB(crypto,
    BN_init,
    [AC_CHECK_LIB(ssl,
        SSL_ctrl,
        [LIBS="-lssl -lcrypto $LIBS"],
        [AC_MSG_ERROR(["Error: libssl required"])],
        -lcrypto
    )],
    [AC_MSG_ERROR(["Error: libcrypto required."])]
)
fi

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

AC_ARG_ENABLE(boostthreads,
              [  --enable-boostthreads      use boost threads, instead of POSIX pthread (experimental) ],
              [case "${enableval}" in
                yes) ENABLE_BOOSTTHREADS=1 ;;
                no) ENABLE_BOOSTTHREADS=0 ;;
                *) AC_MSG_ERROR(bad value ${enableval} for --enable-cov) ;;
              esac],
              [ENABLE_BOOSTTHREADS=2])


if test "x[$]ENABLE_BOOSTTHREADS" = "x1"; then
  AC_MSG_WARN(enable boostthreads)
  AC_DEFINE([USE_BOOST_THREAD], [1], [experimental --enable-boostthreads that replaces POSIX pthread by boost::thread])
  LIBS="-lboost_thread $LIBS"
fi

AM_CONDITIONAL([WITH_BOOSTTHREADS], [test "x[$]ENABLE_BOOSTTHREADS" = "x1"])

AC_CONFIG_HEADERS(config.h:config.hin)

AC_CONFIG_FILES([
  Makefile
  compiler/cpp/Makefile
  compiler/cpp/version.h
  compiler/cpp/src/windows/version.h
  lib/Makefile
  lib/cpp/Makefile
  lib/cpp/test/Makefile
  lib/cpp/thrift-nb.pc
  lib/cpp/thrift-z.pc
  lib/cpp/thrift-qt.pc
  lib/cpp/thrift.pc
  lib/c_glib/Makefile
  lib/c_glib/thrift_c_glib.pc
  lib/c_glib/test/Makefile
  lib/csharp/Makefile
  lib/d/Makefile
  lib/d/test/Makefile
  lib/erl/Makefile
  lib/hs/Makefile
  lib/java/Makefile
  lib/js/test/Makefile
  lib/perl/Makefile
  lib/perl/test/Makefile
  lib/php/Makefile
  lib/php/test/Makefile
  lib/py/Makefile
  lib/rb/Makefile
  test/Makefile
  test/cpp/Makefile
  test/hs/Makefile
  test/nodejs/Makefile
  test/php/Makefile
  test/perl/Makefile
  test/py/Makefile
  test/py.twisted/Makefile
  test/rb/Makefile
])

AC_OUTPUT


echo
echo "$PACKAGE $VERSION"
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
echo "Building D Library ........... : $have_d"

if test "$have_cpp" = "yes" ; then
  echo
  echo "C++ Library:"
  echo "   Build TZlibTransport ...... : $have_zlib"
  echo "   Build TNonblockingServer .. : $have_libevent"
  echo "   Build TQTcpServer (Qt) .... : $have_qt"
fi
if test "$have_java" = "yes" ; then
  echo
  echo "Java Library:"
  echo "   Using javac ............... : $JAVAC"
  echo "   Using java ................ : $JAVA"
  echo "   Using ant ................. : $ANT"
fi
if test "$have_csharp" = "yes" ; then
  echo
  echo "C# Library:"
  echo "   Using .NET 3.5 ............ : $net_3_5"
fi
if test "$have_python" = "yes" ; then
  echo
  echo "Python Library:"
  echo "   Using Python .............. : $PYTHON"
fi
if test "$have_php" = "yes" ; then
  echo
  echo "PHP Library:"
  echo "   Using php-config .......... : $PHP_CONFIG"
fi
if test "$have_ruby" = "yes" ; then
  echo
  echo "Ruby Library:"
  echo "   Using Ruby ................ : $RUBY"
fi
if test "$have_haskell" = "yes" ; then
  echo
  echo "Haskell Library:"
  echo "   Using Haskell ............. : $RUNHASKELL"
  echo "   Using Cabal ............... : $CABAL"
fi
if test "$have_perl" = "yes" ; then
  echo
  echo "Perl Library:"
  echo "   Using Perl ................ : $PERL"
fi
if test "$have_erlang" = "yes" ; then
  echo
  echo "Erlang Library:"
  echo "   Using erlc ................ : $ERLC"
fi
if test "$have_go" = "yes" ; then
  echo
  echo "Go Library:"
  echo "   Using GOROOT............... : $GOROOT"
  echo "   Using GOBIN................ : $GOBIN"
  echo "   Using GOARCH............... : $GOARCH"
  echo "   Using GO Compiler.......... : $GO_C"
  echo "   Using GO Linker............ : $GO_L"
fi
if test "$have_d" = "yes" ; then
  echo
  echo "Using D Compiler ............. : $DMD"
  echo "Building D libevent tests .... : $with_d_event_tests"
  echo "Building D SSL tests ......... : $with_d_ssl_tests"
fi
echo
echo "If something is missing that you think should be present,"
echo "please skim the output of configure to find the missing"
echo "component.  Details are present in config.log."

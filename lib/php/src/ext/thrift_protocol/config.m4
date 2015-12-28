dnl Copyright (C) 2009 Facebook
dnl Copying and distribution of this file, with or without modification,
dnl are permitted in any medium without royalty provided the copyright
dnl notice and this notice are preserved.

PHP_ARG_ENABLE(thrift_protocol, whether to enable the thrift_protocol extension,
[  --enable-thrift_protocol	Enable the fbthrift_protocol extension])

if test "$PHP_THRIFT_PROTOCOL" != "no"; then
  PHP_REQUIRE_CXX()
  PHP_ADD_LIBRARY_WITH_PATH(stdc++, "", THRIFT_PROTOCOL_SHARED_LIBADD)
  CXXFLAGS="$CXXFLAGS -std=c++11"

  AC_MSG_CHECKING([check for supported PHP versions])
  PHP_THRIFT_FOUND_VERSION=`${PHP_CONFIG} --version`
  PHP_THRIFT_FOUND_VERNUM=`echo "${PHP_THRIFT_FOUND_VERSION}" | $AWK 'BEGIN { FS = "."; } { printf "%d", ([$]1 * 100 + [$]2) * 100 + [$]3;}'`
  if test "$PHP_THRIFT_FOUND_VERNUM" -ge "50000"; then
    PHP_SUBST(THRIFT_PROTOCOL_SHARED_LIBADD)
    PHP_NEW_EXTENSION(thrift_protocol, php_thrift_protocol.cpp php_thrift_protocol7.cpp, $ext_shared)
    AC_MSG_RESULT([supported ($PHP_THRIFT_FOUND_VERSION)])
  else
    AC_MSG_ERROR([unsupported PHP version ($PHP_THRIFT_FOUND_VERSION)])
  fi
fi


dnl Copyright (C) 2009 Facebook
dnl Copying and distribution of this file, with or without modification,
dnl are permitted in any medium without royalty provided the copyright
dnl notice and this notice are preserved.

PHP_ARG_ENABLE(thrift_protocol, whether to enable the thrift_protocol extension,
[  --enable-thrift_protocol	Enable the fbthrift_protocol extension])

if test "$PHP_THRIFT_PROTOCOL" != "no"; then
  PHP_REQUIRE_CXX()
  PHP_ADD_LIBRARY_WITH_PATH(stdc++, "", THRIFT_PROTOCOL_SHARED_LIBADD)
  PHP_SUBST(THRIFT_PROTOCOL_SHARED_LIBADD)
  PHP_NEW_EXTENSION(thrift_protocol, php_thrift_protocol.cpp, $ext_shared)
fi


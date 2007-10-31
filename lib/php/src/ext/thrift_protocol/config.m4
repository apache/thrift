PHP_ARG_ENABLE(thrift_protocol, whether to enable the thrift_protocol extension,
[  --enable-thrift_protocol	Enable the fbthrift_protocol extension])

if test "$PHP_THRIFT_PROTOCOL" == "yes"; then
  PHP_REQUIRE_CXX()
  PHP_NEW_EXTENSION(thrift_protocol, php_thrift_protocol.cpp, $ext_shared)
fi


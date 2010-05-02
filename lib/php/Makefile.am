phpdir = ${PHP_PREFIX}
php_DATA =  \
  src/autoload.php \
  src/Thrift.php

phpextdir = ${phpdir}/ext/thrift_protocol
phpext_DATA = \
  src/ext/thrift_protocol/config.m4                \
  src/ext/thrift_protocol/php_thrift_protocol.cpp  \
  src/ext/thrift_protocol/php_thrift_protocol.h

phpprotocoldir = ${phpdir}/protocol
phpprotocol_DATA = \
  src/protocol/TBinaryProtocol.php         \
  src/protocol/TProtocol.php

phptransportdir = ${phpdir}/transport
phptransport_DATA = \
  src/transport/TBufferedTransport.php     \
  src/transport/TFramedTransport.php       \
  src/transport/THttpClient.php            \
  src/transport/TMemoryBuffer.php          \
  src/transport/TNullTransport.php         \
  src/transport/TPhpStream.php             \
  src/transport/TSocket.php                \
  src/transport/TSocketPool.php            \
  src/transport/TTransport.php

MAINTAINERCLEANFILES = Makefile Makefile.in

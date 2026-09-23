Thrift PHP Software Library

# License

Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements. See the NOTICE file
distributed with this work for additional information
regarding copyright ownership. The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied. See the License for the
specific language governing permissions and limitations
under the License.

# Using Thrift with PHP

Thrift requires PHP 8.2 Thrift makes as few assumptions about your PHP
environment as possible while trying to make some more advanced PHP
features (i.e. APCu cacheing using asbolute path URLs) as simple as possible.

To use Thrift in your PHP codebase, take the following steps:

1. Copy all of thrift/lib/php/lib into your PHP codebase
2. Configure Symfony Autoloader (or whatever you usually use)

After that, you have to manually include the Thrift package
created by the compiler:

```
require_once 'packages/Service/Service.php';
require_once 'packages/Service/Types.php';
```

# Dependencies

PHP_INT_SIZE

    This built-in signals whether your architecture is 32 or 64 bit and is
    used by the TBinaryProtocol to properly use pack() and unpack() to
    serialize data.

apcu_fetch(), apcu_store()

    APCu cache is used by the TSocketPool class. If you do not have APCu installed,
    Thrift will fill in null stub function definitions.

# Breaking Changes

## 0.26.0

1. `TJSONProtocol` now holds a string, binary or numeric value to the same maximum string size as `TBinaryProtocol` and `TCompactProtocol`, and reports a longer one with a `TProtocolException` of type `SIZE_LIMIT`. A JSON value carries no length in front of it, so the maximum applies to the bytes it takes on the wire while it is read. The maximum defaults to `TProtocol::DEFAULT_MAX_STRING_SIZE`, 16384000 bytes; it is an optional constructor argument of `TJSONProtocol` and of `TJSONProtocolFactory`; pass `0` to read values of any length, as before.

2. `TCurlClient` follows a redirect only within the origin of the URL it is configured with, that is to the same scheme, host and port. It sends the request again there, with its headers and body, and follows at most one redirect, as before. A redirect to another origin, including one from `http` to `https`, now fails the request with a `TTransportException`, as any redirect does with `THttpClient`. Configure the client with the scheme, host and port that serve the requests.

3. Each `TCurlClient` now owns its curl handle, so timeout settings and connection reuse are independent between client instances. `close()` releases that client's handle. Replace static calls to `TCurlClient::closeCurlHandle()` with `$client->closeCurlHandle()` to release a specific client's handle, or `$client->close()` to clear its buffers as well.

## 0.25.0

1. `TBinaryProtocol`, `TBinaryProtocolAccelerated` and `TCompactProtocol` now refuse a string or binary field longer than their maximum string size before reading it, with a `TProtocolException` of type `SIZE_LIMIT`. The maximum defaults to `TProtocol::DEFAULT_MAX_STRING_SIZE`, 16384000 bytes, the frame size limit the framed transports apply. It is an optional constructor argument of the three protocols and of their factories; pass `0` to read strings of any length, as before.

2. The legacy callable/string `$debugHandler` argument has been removed from `TSocket`, `TSSLSocket` and `TSocketPool`. The constructors now accept a PSR-3 logger via the `$logger` parameter instead. `TSocket::setDebug()`, `TSocket::DEFAULT_DEBUG_HANDLER` and `TSSLServerSocket::getSSLHost()` have also been removed. The `ssl://` prefix is still applied automatically by `TSSLServerSocket` and `TSSLSocket`. Additionally, `TSocket::open()` no longer falls back to the send timeout for the connect step; use `TSocket::setConnectTimeout()` to configure a dedicated connect timeout. The default connect timeout is now 1 second.

## 0.12.0

1. [PSR-4](https://www.php-fig.org/psr/psr-4/) loader is now the default. If you want to use class maps instead, use `-gen php:classmap`.

2. If using PSR-4, use `$thriftClassLoader->registerNamespace('namespace', '<path>')` instead of `$thriftClassLoader->registerDefinition('namespace', '<path>')`.

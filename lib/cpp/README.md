Thrift C++ Software Library

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

# Using Thrift with C++

The Thrift C++ libraries are built using the GNU tools. Follow the instructions
in the top-level README.md

In case you do not want to open another README.md file, do this thrift src:

    ./bootstrap.sh
    ./configure (--with-boost=/usr/local)
    make
    sudo make install

Thrift is divided into two libraries.

* libthrift - The core Thrift library contains all the core Thrift code.  This requires
  openssl, pthreads, and librt.

* libthriftnb - This library contains the Thrift nonblocking server, which uses libevent.
  To link this library you will also need to link libevent.

## Linking Against Thrift

After you build and install Thrift the libraries are installed to
/usr/local/lib by default. Make sure this is in your LDPATH.

On Linux, the best way to do this is to ensure that /usr/local/lib is in
your /etc/ld.so.conf and then run /sbin/ldconfig.

Depending upon whether you are linking dynamically or statically and how
your build environment it set up, you may need to include additional
libraries when linking against thrift, such as librt and/or libpthread. If
you are using libthriftnb you will also need libevent.

## Dependencies

C++11 is required at a minimum.  C++03/C++98 are not supported after version 0.12.0.

Boost is required to run the C++ unit tests.  It is not necessary to link against
the runtime library.

libevent (for libthriftnb only) - most linux distributions have dev packages for this:
http://monkey.org/~provos/libevent/

# Using Thrift with C++ on Windows

Both the autoconf and cmake build systems are able to automatically detect many
system configurations without the need to specify library locations, however if
you run into problems or want to redirect thrift to build and link against your
own provided third party libraries:

BOOST_ROOT : For boost, e.g. D:\boost_1_55_0
OPENSSL_ROOT_DIR : For OpenSSL, e.g. D:\OpenSSL-Win32

only required by libthriftnb:

LIBEVENT_ROOT_DIR : For Libevent e.g. D:\libevent-2.0.21-stable

See /3rdparty.user for more details.

The same linking guidelines described above for libthriftnb apply to windows as well.

## Linking Against Thrift

You need to link your project that uses thrift against all the thrift
dependencies; in the case of libthrift, openssl, pthreads, and librt and for
libthriftnb, libevent.

In the project properties you must also set HAVE_CONFIG_H as force include
the config header: "windows/config.h"

## Dependencies

libevent (for libthriftnb only)
http://monkey.org/~provos/libevent/

## Windows version compatibility

The Thrift library targets Windows 7 or latter versions. The supports for windows XP and Vista are avaiable until 0.12.0.

## Thrift and the VCPKG Package manager

You can download and install thrift using the 
[vcpkg](https://github.com/Microsoft/vcpkg) dependency manager:

    git clone https://github.com/Microsoft/vcpkg.git
    cd vcpkg
    ./bootstrap-vcpkg.sh
    ./vcpkg integrate install
    ./vcpkg install thrift

The thrift port in vcpkg is kept up to date by Microsoft team members 
and community contributors. The Apache Thrift project is *not* responsible
for the vcpkg port. Therefore, if the version is out of date, please 
[create an issue or pull request](https://github.com/Microsoft/vcpkg) 
on the vcpkg repository. 


## Named Pipes

Named Pipe transport has been added in the TPipe and TPipeServer classes. This
is currently Windows-only. Named pipe transport for *NIX has not been
implemented. Domain sockets are a better choice for local IPC under non-Windows
OS's. *NIX named pipes only support 1:1 client-server connection.

# Thrift/SSL

## Scope

This SSL only supports blocking mode socket I/O. It can only be used with
TSimpleServer, TThreadedServer, and TThreadPoolServer.

## Implementation

There are two main classes TSSLSocketFactory and TSSLSocket. Instances of
TSSLSocket are always created from TSSLSocketFactory.

## How to use SSL APIs

See the TestClient.cpp and TestServer.cpp files for examples.

### AccessManager (certificate validation)

An example of certificate validation can be found in TestServer.cpp.

AccessManager defines a callback interface. It has three callback methods:

(a) Decision verify(const sockaddr_storage& sa);

(b) Decision verify(const string& host, const char* name, int size);

(c) Decision verify(const sockaddr_storage& sa, const char* data, int size);

After SSL handshake completes, additional checks are conducted. Application
is given the chance to decide whether or not to continue the conversation
with the remote. Application is queried through the above three "verify"
method. They are called at different points of the verification process.

Decisions can be one of ALLOW, DENY, and SKIP. ALLOW and DENY means the
conversation should be continued or disconnected, respectively. ALLOW and
DENY decision stops the verification process. SKIP means there's no decision
based on the given input, continue the verification process.

First, (a) is called with the remote IP. It is called once at the beginning.
"sa" is the IP address of the remote peer.

Then, the certificate of remote peer is loaded. SubjectAltName extensions
are extracted and sent to application for verification. When a DNS
subjectAltName field is extracted, (b) is called. When an IP subjectAltName
field is extracted, (c) is called.

The "host" in (b) is the value from TSocket::getHost() if this is a client
side socket, or TSocket::getPeerHost() if this is a server side socket. The
reason is client side socket initiates the connection. TSocket::getHost()
is the remote host name. On server side, the remote host name is unknown
unless it's retrieved through TSocket::getPeerHost(). Either way, "host"
should be the remote host name. Keep in mind, if TSocket::getPeerHost()
failed, it would return the remote host name in numeric format.

If all subjectAltName extensions were "skipped", the common name field would
be checked. It is sent to application through (c), where "sa" is the remote
IP address. "data" is the IP address extracted from subjectAltName IP
extension, and "size" is the length of the extension data.

If any of the above "verify" methods returned a decision ALLOW or DENY, the
verification process would be stopped.

If any of the above "verify" methods returned SKIP, that decision would be
ignored and the verification process would move on till the last item is
examined. At that point, if there's still no decision, the connection is
terminated.

Thread safety, an access manager should not store state information if it's
to be used by many SSL sockets.

## SIGPIPE signal

Applications running OpenSSL over network connections may crash if SIGPIPE
is not ignored. This happens when they receive a connection reset by remote
peer exception, which somehow triggers a SIGPIPE signal. If not handled,
this signal would kill the application.

## How to run test client/server in SSL mode

The server and client expects the followings from the directory /test/

- keys/server.crt
- keys/server.key
- keys/CA.pem

The file names are hard coded in the source code. You need to create these
certificates before you can run the test code in SSL mode. Make sure at least
one of the followings is included in "keys/server.crt",

- subjectAltName, DNS localhost
- subjectAltName, IP  127.0.0.1
- common name,    localhost

Run within /test/ folder,

         ./cpp/TestServer --ssl &
         ./cpp/TestClient --ssl

If "-h <host>" is used to run client, the above "localhost" in the above
keys/server.crt has to be replaced with that host name.

## TSSLSocketFactory::randomize()

The default implementation of OpenSSLSocketFactory::randomize() simply calls
OpenSSL's RAND_poll() when OpenSSL library is first initialized.

The PRNG seed is key to the application security. This method should be
overridden if it's not strong enough for you.

# Deprecations

## 0.12.0

Support for C++03/C++98 was deprecated.
Support for Boost at runtime was deprecated.

# Breaking Changes

## 1.0.0

THRIFT-4720:
The classes Monitor and TimerManager now use std::chrono::milliseconds for timeout, the methods and functions involving THRIFT_TIMESPEC and timeval have been removed, the related tests have been modified.

Support for Windows XP/Vista has been dropped.

Support for C++03/C++98 has been dropped.  Use version 0.12.0 to support that
language level.  As a consequence, boost is no longer required as a runtime
library depenedency, but is is still required to build the runtime library
and to run the unit tests.  We will work towards removing boost as a
build dependency for folks who just want to build the runtime and not
run the tests.  This means the header thrift/stdcxx.h has been removed and
anything that relied on it has been changed to directly use C++11 concepts.

THRIFT-4730:
The classes BoostThreadFactory, PosixThreadFactory, StdThreadFactory, and
PlatformThreadFactory have been removed, and we will use a ThreadFactory
based on C++11 (essentially StdThreadFactory was renamed ThreadFactory).

THRIFT-4732:
The CMake build options WITH_SHARED_LIBS and WITH_STATIC_LIBS are deprecated.
The project no longer performs a side-by-side static and shared build; you
tell CMake through BUILD_SHARED_LIBS whether to make shared or static
libraries now.  This is CMake standard behavior.

THRIFT-4735:
Qt4 support was removed.

THRIFT-4762:
Added `const` specifier to `TTransport::getOrigin()`. This changes its function signature.
It's recommended to add the `override` specifier in implementations derived from `TTransport`.

## 0.11.0

Older versions of thrift depended on the <boost/smart_ptr.hpp> classes which
were used in thrift headers to define interfaces.  Thrift now detects C++11
at build time and will prefer to use <memory> classes from C++11 instead.
You can force the library to build with boost memory classes by defining the
preprocessor macro `FORCE_BOOST_SMART_PTR`.  (THRIFT-2221)

In the pthread mutex implementation, the contention profiling code was enabled
by default in all builds.  This changed to be disabled by default.  (THRIFT-4151)

In older releases, if a TSSLSocketFactory's lifetime was not at least as long
as the TSSLSockets it created, we silently reverted openssl to unsafe multithread behavior
and so the results were undefined.  Changes were made in 0.11.0 that cause either an
assertion or a core instead of undefined behavior.  The lifetime of a TSSLSocketFactory
*must* be longer than any TSSLSocket that it creates, otherwise openssl will be cleaned
up too early.  If the static boolean is set to disable openssl initialization and
cleanup and leave it up to the consuming application, this requirement is not needed.
(THRIFT-4164)


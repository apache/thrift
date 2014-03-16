Thrift C++ Software Library

License
=======

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

Using Thrift with C++
=====================

You need to define an enviroment variable called THIRD_PARTY. The project
assumes that you have extracted the dependancies into their default structure
into the path defined by THIRD_PARTY.

e.g. $(THIRD_PARTY)/boost/boost_1_47_0/

Thrift is divided into two libraries.

libthrift
  The core Thrift library contains all the core Thrift code. It requires
  boost shared pointers and boost thread.

libthriftnb
  This library contains the Thrift nonblocking server, which uses libevent.
  To link this library you will also need to link libevent.

Linking Against Thrift
======================

You need to link your project that uses thrift against all the thrift
dependancies; in the case of libthrift, boost and for
libthriftnb, libevent.

In the project properties you must also set HAVE_CONFIG_H as force include
the config header: "windows/confg.h"

Dependencies
============

boost shared pointers
http://www.boost.org/libs/smart_ptr/smart_ptr.htm

boost thread
http://www.boost.org/doc/libs/release/doc/html/thread.html

libevent (for libthriftnb only)
http://monkey.org/~provos/libevent/

Notes on boost thread (static vs shared):
=========================================

By default lib/cpp/windows/force_inc.h defines:

#define BOOST_ALL_NO_LIB 1
#define BOOST_THREAD_NO_LIB 1

This has for effect to have the host application linking against Thrift
to have to link with boost thread as a static library.

If you wanted instead to link with boost thread as a shared library,
you'll need to uncomment those two lines, and recompile.

Windows version compatibility
=============================
The Thrift library targets Windows XP for broadest compatbility. A notable
difference is in the Windows-specific implementation of the socket poll
function. To target Vista, Win7 or other versions, comment out the line
#define TARGET_WIN_XP.

Named Pipes
===========
- Named Pipe transport has been added in the TPipe and TPipeServer classes.
  This is currently Windows-only (see below).

Known issues
============

- Named pipe transport for *NIX has not been implemented. Domain sockets are
  a better choice for local IPC under non-Windows OS's. *NIX named pipes 
  only support 1:1 client-server connection.

TODO
====

- Port remaining classes in libthrift:
    - TSSLSocket

- Port test cases. (Not even started this. Run test cases in release mode?)

- Autolink libraries depending on debug\release build.

- Auto versioning.

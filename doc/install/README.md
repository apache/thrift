<!--
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
-->


## Basic requirements
* A relatively POSIX-compliant *NIX system
    * Cygwin or MinGW can be used on Windows (but there are better options, see below)
* g++ 4.2
* Runtime libraries for lex and yacc might be needed for the compiler.

## Requirements for building from source
* GNU build tools:
    * autoconf 2.65
    * automake 1.13
    * libtool 1.5.24
* pkg-config autoconf macros (pkg.m4)
* lex and yacc (developed primarily with flex and bison)
* libssl-dev

## Requirements for building the compiler from source on Windows
* Visual Studio C++
* Flex and Bison (e.g. the WinFlexBison package)

## Language requirements
These are only required if you choose to build the libraries for the given language

* C++
    * Boost 1.56.0 (for the unit tests, and for any build with autotools)
    * libevent (optional, to build the nonblocking server)
    * zlib (optional)
    * Qt (optional)
* Java
    * Java 17 (latest LTS)
    * Gradle 8.4
* C#: Mono 1.2.4 (and pkg-config to detect it) or Visual Studio 2005+
* Python 2.6 (including header files for extension modules)
* PHP 7.1 (optionally including header files for extension modules)
* Ruby 1.8
    * bundler gem
* Erlang R12 (R11 works but not recommended)
* Perl 5
    * Bit::Vector
    * Class::Accessor
* Haxe 3.1.3
* Go 1.4
* Delphi 2010

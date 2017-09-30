# Apache Thrift Docker containers

Docker containers used to build and test Apache Thrift for a variety of platforms.

## Supported Containers

The Travis CI (continuous integration) builds use the Ubuntu Trusty, Xenial, and Zesty images to maximize
language level coverage.

### Ubuntu
* trusty (legacy)
* xenial (stable)
* zesty  (latest)

## Unsupported Containers

These containers may be in various states, and may not build everything.

### CentOS
* 7.3
  * make check in lib/py may hang in test_sslsocket - root cause unknown

### Debian
* jessie
* stretch
  * make check in lib/cpp fails due to https://svn.boost.org/trac10/ticket/12507

## Usage
From the Apache Thrift code base root:

* Build

	docker build -t thrift build/docker/ubuntu-xenial

* Run

	docker run -v $(pwd):/thrift/src -it thrift /bin/bash

## Core Tool Versions per Dockerfile
| Tool      | ubuntu-trusty | ubuntu-xenial | ubuntu-artful | Notes |
| :-------- | :------------ | :------------ | :------------ | :---- |
| ant       | 1.9.3         | 1.9.6         | 1.9.9         |       |
| autoconf  | 2.69          | 2.69          | 2.69          |       |
| automake  | 1.14.1        | 1.15          | 1.15          |       |
| bison     | 3.0.2         | 3.0.4         | 3.0.4         |       |
| boost     | 1.54.0        | 1.58.0        | 1.63.0        | artful: stock boost 1.62.0 has problems running unit tests |
| cmake     | 3.2.2         | 3.5.1         | 3.9.1         |       |
| flex      | 2.5.35        | 2.6.0         | 2.6.1         |       |
| glibc     | 2.19          | 2.23          | 2.26          |       |
| libevent  | 2.0.21        | 2.0.21        | 2.1           |       |
| libstdc++ | 4.8.4         | 5.4.0         | 7.2.0         |       |
| make      | 3.81          | 4.1           | 4.1           |       |
| openssl   | 1.0.1f        | 1.0.2g        | 1.0.2g        |       |
| qt5       | 5.2.1         | 5.5.1         | 5.9.1         |       |

## Compiler/Language Versions per Dockerfile
| Language  | ubuntu-trusty | ubuntu-xenial | ubuntu-artful | Notes |
| :-------- | :------------ | :------------ | :------------ | :---- |
| as3       |               |               |               | Not in CI |
| C++ gcc   | 4.8.4         | 5.4.0         | 7.2.0         |       |
| C++ clang | 3.4           | 3.8           | 4.0           |       |
| C# (mono) | 3.2.8.0       | 4.2.1         | 4.6.2.7       |       |
| c_glib    | 2.40.2        | 2.48.2        | 2.54.0        |       |
| cocoa     |               |               |               | Not in CI |
| d         | 2.070.2       | 2.073.2       | 2.076.0       |       |
| dart      | 1.20.1        | 1.24.2        |               | artful: apt repo not compatible with apt 1.4? |
| delphi    |               |               |               | Not in CI |
| dotnet    |               |               |               | see THRIFT-4350 |
| erlang    | R16B03        | 18.3          | 20.0.4        |       |
| go        | 1.2.1         | 1.6.2         | 1.8.3         |       |
| haskell   | 7.6.3         | 7.10.3        | 8.0.2         |       |
| haxe      |               | 3.2.1         |               | disabled in trusty builds - cores on install v3.0.0, disabled in artful builds - see THRIFT-4352 |
| java      | 1.7.0_151     | 1.8.0_131     | 1.8.0_144     |       |
| js        |               |               |               | Unsure how to look for version info |
| lua       | 5.1.5         | 5.2.4         | 5.3.3         |       |
| nodejs    |               | 4.2.6         | 6.11.2        | trusty has node.js 0.10.0 which is too old |
| ocaml     |               | 4.02.3        | 4.04.0        |       |
| perl      | 5.18.2        | 5.22.1        | 5.26.0        |       |
| php       | 5.5.9         | 7.0.22        | 7.1.8         |       |
| python    | 2.7.6         | 2.7.12        | 2.7.14        |       |
| python3   | 3.4.3         | 3.5.2         | 3.6.3         |       |
| ruby      | 1.9.3p484     | 2.3.1p112     | 2.3.3p222     |       |
| rust      | 1.15.1        | 1.15.1        | 1.18.0        |       |
| smalltalk |               |               |               | Not in CI |
| swift     |               |               |               | Not in CI |


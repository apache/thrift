# Apache Thrift Docker containers
Docker containers used to build and test Apache Thrift for a variety of platforms.

## Available Containers

### CentOS
* 7.3 (current)

### Debian
* jessie
* stretch (current)

### Ubuntu
* trusty
* xenial (current)

## Dependencies
* A working Docker environment. A Vagrantfile is provided which will setup an Ubuntu host and working Docker environment as well as build the Apache Thrift Docker container for testing and development.

## Usage
From the Apache Thrift code base root:

* Build

	docker build -t thrift build/docker/ubuntu-xenial

	or

	docker build -t thrift build/docker/centos-7.3

* Run

	docker run -v $(pwd):/thrift/src -it thrift /bin/bash

## Core Tool Versions per Dockerfile
    | Tool      | centos-7.3 | debian-stretch | ubuntu-xenial |
    |-----------|------------|----------------|---------------|
    | ant       | 1.9.2      | 1.9.9          | 1.9.6         |
    | autoconf  | 2.69       | 2.69           | 2.69          |
    | automake  | 1.13.4     | 1.15           | 1.15          |
    | bison     | 2.7        | 3.0.4          | 3.0.4         |
    | boost     | 1.53.0     | 1.62.0         | 1.58.0        |
    | cmake     | 3.6.3      | 3.7.2          | 3.5.1         |
    | flex      | 2.5.37     | 2.6.1          | 2.6.0         |
    | glibc     | 2.17       | 2.24           | 2.23          |
    | libevent  | 2.0.21     | 2.0.21         | 2.0.21        |
    | libstdc++ | 4.8.5      | 6.3.0          | 5.4.0         |
    | make      | 3.82       | 4.1            | 4.1           |
    | openssl   | 1.0.1e     | 1.1.0f         | 1.0.2g        |

## Language Versions per Dockerfile
    | Language  | centos-7.3 | debian-stretch | ubuntu-xenial |
    |-----------|------------|----------------|---------------|
    | as3       |            |                |               |
    | C++-gcc   | 4.8.5      | 6.3.0          | 5.4.0         |
    | C++-clang | 3.4.2      | 3.8.1          | 3.8           |
    | C# (mono) | 4.6.2      | 4.6.2.7        | 5.2.0.215     |
    | c_glib    | 2.46.2     | 2.50.3         | 2.48.2        |
    | cocoa     |            |                |               |
    | d         | 2.076.0    | 2.075.1        | 2.075.1       |
    | dart      | 1.24.2     | 1.24.2         | 1.24.2        |
    | delphi    |            |                |               |
    | dotnet    |            |                |               |
    | erlang    | 20         | 19.2           | 18.3          |
    | go        | 1.9        | 1.7.4          | 1.6.2         |
    | haskell   | 7.6.3      | 8.0.1          | 7.10.3        |
    | haxe      |            | 3.2.1          | 3.2.1         |
    | java      | 1.8.0_141  | 1.8.0_141      | 1.8.0_131     |
    | js        |            |                |               |
    | lua       | 5.3.4      | 5.2.4          | 5.2.4         |
    | nodejs    | 6.11.1     | 8.4.0          | 7.10.1        |
    | ocaml     | 4.01.0     | 4.02.3         | 4.02.3        |
    | perl      | 5.16.3     | 5.24.1         | 5.22.1        |
    | php       | 5.4.16     | 7.0.19         | 7.0.22        |
    | python2   | 2.7.5      | 2.7.13         | 2.7.12        |
    | python3   | 3.4.5      | 3.5.3          | 3.5.2         |
    | ruby      | 2.0.0p648  | 2.3.3p222      | 2.3.1p112     |
    | rust      | 1.17.0     | 1.14.0         | 1.15.1        |
    | smalltalk |            |                |               |
    | swift     |            |                |               |


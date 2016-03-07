# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Apache Thrift Docker build environment for Centos
#
# Known missing client libraries:
#  - None

FROM ubuntu:trusty
MAINTAINER Apache Thrift <dev@thrift.apache.org>

ENV DEBIAN_FRONTEND noninteractive

# General dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
      apt-transport-https \
      autoconf \
      automake \
      bison \
      build-essential \
      clang \
      cmake \
      curl \
      debhelper \
      flex \
      g++ \
      git \
      libtool \
      make \
      ninja-build \
      pkg-config

# C++ dependencies
# libevent and OpenSSL are needed by D too
RUN apt-get update && apt-get install -y --no-install-recommends \
      libboost-dev \
      libboost-filesystem-dev \
      libboost-program-options-dev \
      libboost-system-dev \
      libboost-test-dev \
      libboost-thread-dev \
      libevent-dev \
      libssl-dev \
      qt5-default \
      qtbase5-dev \
      qtbase5-dev-tools

# Java dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
      ant \
      ant-optional \
      openjdk-7-jdk \
      maven \
    && update-java-alternatives -s java-1.7.0-openjdk-amd64

# Python dependencies
# TODO:
# Install twisted and zope.interface via pip. we need twisted at ./configure time, otherwise
# py.twisted tests are skipped.
RUN apt-get update && apt-get install -y --no-install-recommends \
      python-all \
      python-all-dbg \
      python-all-dev \
      python-pip \
      python-setuptools \
      python-support \
      python-twisted \
      python-zope.interface \
      python3-dev \
      python3-pip

# Ruby dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
      ruby \
      ruby-dev \
    && gem install bundler rake

# Perl dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
      libbit-vector-perl \
      libclass-accessor-class-perl \
      libcrypt-ssleay-perl \
      libio-socket-ssl-perl \
      libnet-ssleay-perl

# Php dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
      php5 \
      php5-dev \
      php5-cli \
      php-pear \
      re2c \
      phpunit

# GlibC dependencies
RUN apt-get update && apt-get install -y --no-install-recommends libglib2.0-dev

# Erlang dependencies
RUN echo 'deb http://packages.erlang-solutions.com/debian trusty contrib' > /etc/apt/sources.list.d/erlang_solutions.list && \
    curl -sSL http://packages.erlang-solutions.com/debian/erlang_solutions.asc | apt-key add - && \
    apt-get update && apt-get install -y --no-install-recommends \
      erlang-base \
      erlang-eunit \
      erlang-dev \
      erlang-tools \
      rebar

# Go dependencies
RUN curl -sSL https://storage.googleapis.com/golang/go1.4.3.linux-amd64.tar.gz | tar -C /usr/local/ -xz
ENV PATH /usr/local/go/bin:$PATH

# Haskell dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
      ghc \
      cabal-install
# Packages tend to be overwritten by build process since we're currently calling cabal update
#       libghc-binary-dev \
#       libghc-network-dev \
#       libghc-http-dev \
#       libghc-hashable-dev \
#       libghc-unordered-containers-dev \
#       libghc-vector-dev

# Haxe
RUN apt-get update && apt-get install -y --no-install-recommends \
      neko \
      neko-dev \
      libneko0 \
    && mkdir -p /usr/lib/haxe && \
    curl http://haxe.org/website-content/downloads/3.2.0/downloads/haxe-3.2.0-linux64.tar.gz | \
    tar -C /usr/lib/haxe --strip-components=1 -xz && \
    ln -s /usr/lib/haxe/haxe /usr/bin/haxe && \
    ln -s /usr/lib/haxe/haxelib /usr/bin/haxelib && \
    mkdir -p /usr/lib/haxe/lib  && \
    chmod -R 777 /usr/lib/haxe/lib && \
    haxelib setup /usr/lib/haxe/lib && \
    haxelib install hxcpp

# Node.js dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
      nodejs \
      nodejs-dev \
      nodejs-legacy \
      npm

# CSharp dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
      libmono-system-web2.0-cil \
      mono-complete \
      mono-devel \
      mono-gmcs \
      mono-xbuild

# D dependencies
RUN apt-get update && apt-get install -y --no-install-recommends xdg-utils && \
    curl -sSL http://downloads.dlang.org/releases/2.x/2.070.0/dmd_2.070.0-0_amd64.deb -o /tmp/dmd_2.070.0-0_amd64.deb && \
    dpkg -i /tmp/dmd_2.070.0-0_amd64.deb && \
    rm /tmp/dmd_2.070.0-0_amd64.deb && \
    curl -sSL https://github.com/D-Programming-Deimos/openssl/archive/master.tar.gz| tar xz && \
    curl -sSL https://github.com/D-Programming-Deimos/libevent/archive/master.tar.gz| tar xz && \
    mkdir -p /usr/include/dmd/druntime/import/deimos /usr/include/dmd/druntime/import/C && \
    mv libevent-master/deimos/* openssl-master/deimos/* /usr/include/dmd/druntime/import/deimos/ && \
    mv libevent-master/C/* openssl-master/C/* /usr/include/dmd/druntime/import/C/ && \
    rm -rf libevent-master openssl-master && \
    echo 'gcc -Wl,--no-as-needed $*' > /usr/local/bin/gcc-dmd && \
    chmod 755 /usr/local/bin/gcc-dmd && \
    echo 'CC=/usr/local/bin/gcc-dmd' >> /etc/dmd.conf

# Dart dependencies
RUN curl https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - && \
    curl https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list && \
    apt-get update && apt-get install -y --no-install-recommends dart
ENV PATH /usr/lib/dart/bin:$PATH

# Lua dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
      lua5.2 \
      lua5.2-dev

# MinGW dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
      mingw32 \
      mingw32-binutils \
      mingw32-runtime \
      nsis

# Clean up
RUN apt-get clean && \
    rm -rf /var/cache/apt/* && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /tmp/* && \
    rm -rf /var/tmp/*

ENV THRIFT_ROOT /thrift
RUN mkdir -p $THRIFT_ROOT/src
COPY scripts $THRIFT_ROOT
WORKDIR $THRIFT_ROOT/src

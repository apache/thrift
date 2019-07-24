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

#
# Apache Thrift Docker build environment for Ubuntu Disco
# with some updated packages.
#

FROM buildpack-deps:disco-scm
MAINTAINER Apache Thrift <dev@thrift.apache.org>
ENV DEBIAN_FRONTEND noninteractive

### Add apt repos

RUN apt-get update && \
    apt-get dist-upgrade -y && \
    apt-get install -y --no-install-recommends \
      apt \
      apt-transport-https \
      apt-utils \
      curl \
      dirmngr \
      software-properties-common \
      wget

# Dart
RUN curl https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - && \
    curl https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > \
      /etc/apt/sources.list.d/dart_stable.list

# dotnet (netcore)
RUN curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > /etc/apt/trusted.gpg.d/microsoft.gpg && \
    wget -q -O /etc/apt/sources.list.d/microsoft-prod.list https://packages.microsoft.com/config/ubuntu/18.04/prod.list && \
    chown root:root /etc/apt/trusted.gpg.d/microsoft.gpg && \
    chown root:root /etc/apt/sources.list.d/microsoft-prod.list

# erlang
RUN wget -O- https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc | apt-key add - && \
    echo "deb https://packages.erlang-solutions.com/ubuntu disco contrib" | tee /etc/apt/sources.list.d/erlang.list

# node.js
RUN curl -sL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add - && \
    echo "deb https://deb.nodesource.com/node_10.x disco main" | tee /etc/apt/sources.list.d/nodesource.list

### install general dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
`# General dependencies` \
      bash-completion \
      bison \
      build-essential \
      clang \
      cmake \
      debhelper \
      flex \
      gdb \
      libasound2 \
      libatk-bridge2.0-0 \
      libgtk-3-0 \
      llvm \
      ninja-build \
      pkg-config \
      unzip \
      valgrind \
      vim
ENV PATH /usr/lib/llvm-6.0/bin:$PATH

# lib/as3 (ActionScript)
RUN mkdir -p /usr/local/adobe/flex/4.6 && \
    cd /usr/local/adobe/flex/4.6 && \
    wget -q "http://download.macromedia.com/pub/flex/sdk/flex_sdk_4.6.zip" && \
    unzip flex_sdk_4.6.zip
ENV FLEX_HOME /usr/local/adobe/flex/4.6

RUN apt-get install -y --no-install-recommends \
`# C++ dependencies` \
      libboost-all-dev \
      libevent-dev \
      libssl-dev \
      qt5-default \
      qtbase5-dev \
      qtbase5-dev-tools

RUN apt-get install -y --no-install-recommends \
`# csharp (mono) dependencies` \
      mono-devel

ENV SBCL_VERSION 1.5.3
RUN \
`# Common Lisp (sbcl) dependencies` \
    curl --version && \
    curl -o sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2 -J -L https://sourceforge.net/projects/sbcl/files/sbcl/${SBCL_VERSION}/sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2/download?use_mirror=managedway# && \
    tar xjf sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2 && \
    cd sbcl-${SBCL_VERSION}-x86-64-linux && \
    ./install.sh && \
    sbcl --version && \
    cd .. && \
    rm -rf sbcl*

ENV D_VERSION     2.087.0
ENV DMD_DEB       dmd_2.087.0-0_amd64.deb
RUN \
`# D dependencies` \
    wget -q http://downloads.dlang.org/releases/2.x/${D_VERSION}/${DMD_DEB} && \
    dpkg --install ${DMD_DEB} && \
    rm -f ${DMD_DEB} && \
    mkdir -p /usr/include/dmd/druntime/import/deimos /usr/include/dmd/druntime/import/C && \
    git clone -b 'v2.0.2+2.0.16' https://github.com/D-Programming-Deimos/libevent.git deimos-libevent-2.0 && \
    mv deimos-libevent-2.0/deimos/* /usr/include/dmd/druntime/import/deimos/ && \
    mv deimos-libevent-2.0/C/* /usr/include/dmd/druntime/import/C/ && \
    rm -rf deimos-libevent-2.0 && \
    git clone -b 'v2.0.0+1.1.0h' https://github.com/D-Programming-Deimos/openssl.git deimos-openssl-1.1.0h && \
    mv deimos-openssl-1.1.0h/deimos/* /usr/include/dmd/druntime/import/deimos/ && \
    mv deimos-openssl-1.1.0h/C/* /usr/include/dmd/druntime/import/C/ && \
    rm -rf deimos-openssl-1.1.0h

ENV DART_VERSION 2.4.0-1
RUN apt-get install -y --no-install-recommends \
      `# Dart dependencies` \
      dart=$DART_VERSION
ENV PATH /usr/lib/dart/bin:$PATH

RUN apt-get install -y --no-install-recommends \
`# dotnet core dependencies` \
      dotnet-sdk-2.2

RUN apt-get install -y --no-install-recommends \
`# Erlang dependencies` \
      erlang && \
    wget https://s3.amazonaws.com/rebar3/rebar3 -O /usr/bin/rebar3 && \
    chmod 755 /usr/bin/rebar3 && \
    rebar3 --version

RUN apt-get install -y --no-install-recommends \
`# GlibC dependencies` \
      libglib2.0-dev

# golang
ENV GOLANG_VERSION 1.12.6
ENV GOLANG_DOWNLOAD_URL https://golang.org/dl/go$GOLANG_VERSION.linux-amd64.tar.gz
ENV GOLANG_DOWNLOAD_SHA256 dbcf71a3c1ea53b8d54ef1b48c85a39a6c9a935d01fc8291ff2b92028e59913c
RUN curl -fsSL "$GOLANG_DOWNLOAD_URL" -o golang.tar.gz && \
      echo "$GOLANG_DOWNLOAD_SHA256  golang.tar.gz" | sha256sum -c - && \
            tar -C /usr/local -xzf golang.tar.gz && \
                  ln -s /usr/local/go/bin/go /usr/local/bin && \
                        rm golang.tar.gz

RUN apt-get install -y --no-install-recommends \
`# Haskell dependencies` \
      ghc \
      cabal-install

RUN apt-get install -y --no-install-recommends \
`# Haxe dependencies` \
      haxe \
      neko \
      neko-dev && \
    haxelib setup --always /usr/share/haxe/lib && \
    haxelib install --always hxcpp 2>&1 > /dev/null

RUN apt-get install -y --no-install-recommends \
`# Java dependencies` \
      ant \
      ant-optional \
      maven \
      openjdk-11-jdk-headless

RUN apt-get install -y --no-install-recommends \
`# Lua dependencies` \
      lua5.2 \
      lua5.2-dev
# https://bugs.launchpad.net/ubuntu/+source/lua5.3/+bug/1707212
# lua5.3 does not install alternatives!
# need to update our luasocket code, lua doesn't have luaL_openlib any more

RUN apt-get install -y --no-install-recommends \
`# Node.js dependencies` \
      nodejs

# Test dependencies for running puppeteer
RUN apt-get install -y --no-install-recommends \
`# JS dependencies` \
      libxss1

# does not work on disco?
# RUN apt-get install -y --no-install-recommends \
# `# OCaml dependencies` \
#       ocaml \
#       opam && \
#     opam init --yes && \
#     opam install --yes oasis

RUN apt-get install -y --no-install-recommends \
`# Perl dependencies` \
      libbit-vector-perl \
      libclass-accessor-class-perl \
      libcrypt-ssleay-perl \
      libio-socket-ssl-perl \
      libnet-ssleay-perl

RUN apt-get install -y --no-install-recommends \
`# Php dependencies` \
      php \
      php-cli \
      php-dev \
      php-json \
      php-pear \
      re2c \
      composer

RUN apt-get install -y --no-install-recommends \
`# Python dependencies` \
      python-all \
      python-all-dbg \
      python-all-dev \
      python-ipaddress \
      python-pip \
      python-setuptools \
      python-six \
      python-tornado \
      python-twisted \
      python-wheel \
      python-zope.interface && \
   pip install --upgrade backports.ssl_match_hostname

RUN apt-get install -y --no-install-recommends \
`# Python3 dependencies` \
      python3-all \
      python3-all-dbg \
      python3-all-dev \
      python3-pip \
      python3-setuptools \
      python3-six \
      python3-tornado \
      python3-twisted \
      python3-wheel \
      python3-zope.interface

RUN apt-get install -y --no-install-recommends \
`# Ruby dependencies` \
      ruby \
      ruby-dev \
      ruby-bundler

# Rust dependencies
RUN curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain 1.35.0 -y
ENV PATH /root/.cargo/bin:$PATH

# Swift on Linux for cross tests
# does not work on disco
# RUN cd / && \
#     wget --quiet https://swift.org/builds/swift-4.2.1-release/ubuntu1804/swift-4.2.1-RELEASE/swift-4.2.1-RELEASE-ubuntu18.04.tar.gz && \
#     tar xf swift-4.2.1-RELEASE-ubuntu18.04.tar.gz --strip-components=1 && \
#     rm swift-4.2.1-RELEASE-ubuntu18.04.tar.gz && \
#     swift --version

# cppcheck-1.82 has a nasty cpp parser bug, so we're using something newer
# don't need this on disco, nobody uses it
# RUN apt-get install -y --no-install-recommends \
# `# Static Code Analysis dependencies` \
#       cppcheck \
#       sloccount && \
#     pip install flake8 && \
#     wget -q "https://launchpad.net/ubuntu/+source/cppcheck/1.83-2/+build/14874703/+files/cppcheck_1.83-2_amd64.deb" && \
#     dpkg -i cppcheck_1.83-2_amd64.deb && \
#     rm cppcheck_1.83-2_amd64.deb

# Clean up
RUN rm -rf /var/cache/apt/* && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /tmp/* && \
    rm -rf /var/tmp/*

ENV THRIFT_ROOT /thrift
RUN mkdir -p $THRIFT_ROOT/src
COPY Dockerfile $THRIFT_ROOT/
WORKDIR $THRIFT_ROOT/src

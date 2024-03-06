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
# Apache Thrift Docker build environment for Ubuntu Focal
# with some updated packages.
#

FROM buildpack-deps:focal-scm
LABEL MAINTAINER="Apache Thrift <dev@thrift.apache.org>"
ENV DEBIAN_FRONTEND=noninteractive

### Add apt repos

RUN apt-get update -yq && \
      apt-get dist-upgrade -y && \
      apt-get install -y --no-install-recommends --fix-missing \
      apt \
      apt-transport-https \
      apt-utils \
      curl \
      dirmngr \
      software-properties-common \
      wget

# Create a user
ARG user=build
ARG group=build
ARG uid=1000
ARG gid=1000

RUN apt-get install -y --no-install-recommends sudo && \
    echo "Running with: UID: ${uid}, User: ${user}, GID: ${gid}, Group: ${group}" && \
    if [ -z `cat /etc/group | grep "${group}:"` ] && [ -z `cat /etc/group | grep ":${gid}:"` ]; then addgroup --gid ${gid} ${group}; fi && \
    if [ -z `cat /etc/passwd | grep "${user}:"` ] && [ -z `cat /etc/passwd | grep ":${uid}:"` ]; then adduser --uid ${uid} --gid ${gid} --shell /bin/bash ${user} --disabled-password -q --gecos ""; fi && \
    echo "${user} ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers && \
    mkdir -p /home/${user} && \
    chown -R ${user}:${group} /home/${user}

# Dart
RUN curl https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - && \
      curl https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > \
      /etc/apt/sources.list.d/dart_stable.list

# dotnet (netcore)
RUN curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > /etc/apt/trusted.gpg.d/microsoft.gpg && \
      wget -q -O /etc/apt/sources.list.d/microsoft-prod.list https://packages.microsoft.com/config/ubuntu/20.04/prod.list && \
      chown root:root /etc/apt/trusted.gpg.d/microsoft.gpg && \
      chown root:root /etc/apt/sources.list.d/microsoft-prod.list

# node.js
RUN curl -sL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add - && \
      echo "deb https://deb.nodesource.com/node_16.x focal main" | tee /etc/apt/sources.list.d/nodesource.list

### install general dependencies
RUN apt-get update -yq && \
      apt-get install -y --no-install-recommends \
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

# TODO: "apt-get install" without "apt-get update" in the same "RUN" step can cause cache issues if modified later.
# See https://docs.docker.com/develop/develop-images/dockerfile_best-practices/#run
RUN apt-get install -y --no-install-recommends \
      `# C++ dependencies` \
      libboost-all-dev \
      libevent-dev \
      libssl-dev \
      qt5-default \
      qtbase5-dev \
      qtbase5-dev-tools

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

ENV DART_VERSION 2.7.2-1
RUN apt-get install -y --no-install-recommends \
      `# Dart dependencies` \
      dart=$DART_VERSION
ENV PATH /usr/lib/dart/bin:$PATH

RUN apt-get install -y --no-install-recommends \
      `# dotnet core dependencies` \
      dotnet-sdk-8.0 \
      dotnet-runtime-8.0 \
      aspnetcore-runtime-8.0 \
      dotnet-apphost-pack-8.0

# Erlang dependencies
ARG ERLANG_OTP_VERSION=23.3.4.11
ARG ERLANG_REBAR_VERSION=3.18.0
RUN apt-get update && apt-get install -y --no-install-recommends libncurses5-dev && \
      curl -ssLo /usr/local/bin/kerl https://raw.githubusercontent.com/kerl/kerl/master/kerl && chmod +x /usr/local/bin/kerl && \
      kerl build $ERLANG_OTP_VERSION && kerl install $ERLANG_OTP_VERSION /usr/local/lib/otp/ && . /usr/local/lib/otp/activate && \
      curl -ssLo /usr/local/bin/rebar3 https://github.com/erlang/rebar3/releases/download/${ERLANG_REBAR_VERSION}/rebar3 && chmod +x /usr/local/bin/rebar3 && \
      rebar3 --version
ENV PATH /usr/local/lib/otp/bin:$PATH

RUN apt-get install -y --no-install-recommends \
      `# GlibC dependencies` \
      libglib2.0-dev

# golang
ENV GOLANG_VERSION 1.21.7
ENV GOLANG_DOWNLOAD_URL https://go.dev/dl/go$GOLANG_VERSION.linux-amd64.tar.gz
ENV GOLANG_DOWNLOAD_SHA256 13b76a9b2a26823e53062fa841b07087d48ae2ef2936445dc34c4ae03293702c
RUN curl -fsSL "$GOLANG_DOWNLOAD_URL" -o golang.tar.gz && \
      echo "$GOLANG_DOWNLOAD_SHA256  golang.tar.gz" | sha256sum -c - && \
      tar -C /usr/local -xzf golang.tar.gz && \
      ln -s /usr/local/go/bin/go /usr/local/bin && \
      rm golang.tar.gz

# HAXE
ARG HAXE_VERSION=4.2.1
ARG NEKO_VERSION=2.3.0
RUN cd $HOME && \
    `# Haxe dependencies` && \
    wget https://github.com/HaxeFoundation/haxe/releases/download/${HAXE_VERSION}/haxe-${HAXE_VERSION}-linux64.tar.gz && \
    tar xvf haxe-${HAXE_VERSION}-linux64.tar.gz && \
    rm haxe-${HAXE_VERSION}-linux64.tar.gz && \
    mv haxe_* /opt/haxe && \
    wget https://github.com/HaxeFoundation/neko/releases/download/v`echo ${NEKO_VERSION} | sed "s/\./-/g"`/neko-${NEKO_VERSION}-linux64.tar.gz && \
    tar xvf neko-${NEKO_VERSION}-linux64.tar.gz && \
    rm neko-${NEKO_VERSION}-linux64.tar.gz && \
    mv neko-* /opt/neko
ENV PATH /opt/haxe:/opt/neko:$PATH
RUN echo "/opt/neko" > /etc/ld.so.conf.d/neko.conf && \
    ldconfig
USER ${user}
RUN mkdir -p $HOME/haxe/lib && \
    haxelib setup --always $HOME/haxe/lib && \
    haxelib install --always hxcpp 2>&1 > /dev/null && \
    haxelib install --always uuid 2>&1 > /dev/null
USER root

ENV GRADLE_VERSION="8.4"
RUN apt-get install -y --no-install-recommends \
      `# Java dependencies` \
      ant \
      ant-optional \
      maven \
      openjdk-17-jdk-headless && \
      `# Gradle` \
      wget https://services.gradle.org/distributions/gradle-$GRADLE_VERSION-bin.zip -q -O /tmp/gradle-$GRADLE_VERSION-bin.zip && \
      (echo "3e1af3ae886920c3ac87f7a91f816c0c7c436f276a6eefdb3da152100fef72ae  /tmp/gradle-$GRADLE_VERSION-bin.zip" | sha256sum -c -) && \
      unzip -d /tmp /tmp/gradle-$GRADLE_VERSION-bin.zip && \
      mv /tmp/gradle-$GRADLE_VERSION /usr/local/gradle && \
      ln -s /usr/local/gradle/bin/gradle /usr/local/bin

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
      libxss1 \
      libxtst6

RUN apt-get install -y --no-install-recommends \
      `# OCaml dependencies` \
      ocaml \
      opam && \
      `# disable sandboxing see https://github.com/ocaml/opam/issues/4327` \
      opam init --yes --disable-sandboxing && \
      opam install --yes oasis

RUN apt-get install -y --no-install-recommends \
      `# Perl dependencies` \
      libbit-vector-perl \
      libclass-accessor-class-perl \
      libcrypt-ssleay-perl \
      libio-socket-ssl-perl \
      libnet-ssleay-perl \
      libtest-exception-perl

RUN apt-get install -y --no-install-recommends \
      `# Php dependencies` \
      php7.4 \
      php7.4-cli \
      php7.4-dev \
      php7.4-mbstring \
      php7.4-xml \
      php7.4-curl \
      php7.4-xdebug \
      php-pear \
      re2c \
      composer

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

USER ${user}
RUN `# Rust dependencies` \
    curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain 1.65.0 -y
ENV PATH /home/${user}/.cargo/bin:$PATH
USER root

# Swift on Linux for cross tests
RUN apt-get install -yq \
      libedit-dev \
      libz3-dev \
      libpython2-dev \
      libxml2-dev && \
      cd / && \
      wget --quiet https://download.swift.org/swift-5.7-release/ubuntu2004/swift-5.7-RELEASE/swift-5.7-RELEASE-ubuntu20.04.tar.gz && \
      tar xf swift-5.7-RELEASE-ubuntu20.04.tar.gz && \
      mv swift-5.7-RELEASE-ubuntu20.04 /usr/share/swift && \
      rm swift-5.7-RELEASE-ubuntu20.04.tar.gz

ENV PATH /usr/share/swift/usr/bin:$PATH
RUN swift --version

# Locale(s) for cpp unit tests
RUN apt-get install -y --no-install-recommends \
      `# Locale dependencies` \
      locales && \
      locale-gen en_US.UTF-8 && \
      locale-gen de_DE.UTF-8 && \
      update-locale

RUN apt-get install -y --no-install-recommends \
      `# Static Code Analysis dependencies` \
      cppcheck \
      sloccount && \
      pip install flake8

# NOTE: this does not reduce the image size but adds an additional layer.
# # Clean up
# RUN rm -rf /var/cache/apt/* && \
#     rm -rf /var/lib/apt/lists/* && \
#     rm -rf /tmp/* && \
#     rm -rf /var/tmp/*

ENV THRIFT_ROOT /thrift
RUN mkdir -p $THRIFT_ROOT/src && \
    chown -R ${uid}:${uid} $THRIFT_ROOT/
COPY Dockerfile $THRIFT_ROOT/
WORKDIR $THRIFT_ROOT/src

USER ${user}

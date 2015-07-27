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
#  - D
#  - Haxe
#  - Lua
#

FROM centos:7
MAINTAINER Apache Thrift <dev@thrift.apache.org>

ENV HOME /root

# RUN yum -y update

# General dependencies
RUN yum -y install -y tar m4 perl gcc git libtool zlib-devel openssl-devel autoconf make bison bison-devel flex

RUN mkdir -p /tmp/epel && \
    curl -sSL "http://dl.fedoraproject.org/pub/epel/7/x86_64/e/epel-release-7-5.noarch.rpm" -o /tmp/epel/epel-release-7-5.noarch.rpm && \
    cd /tmp/epel && \
    rpm -ivh epel-release*.rpm && \
    cd $HOME

# Automake
RUN mkdir -p /tmp/automake && \
    curl -SL "http://ftp.gnu.org/gnu/automake/automake-1.14.tar.gz" | tar -xzC /tmp/automake && \
    cd /tmp/automake/automake-1.14 && \
    ./configure --prefix=/usr && \
    make && \
    make install && \
    cd $HOME

# C++ dependencies
RUN yum install -y libboost-dev libevent-devel

# Java Dependencies
RUN yum install -y ant junit ant-nodeps ant-junit java-1.7.0-openjdk-devel

# Python Dependencies
RUN yum install -y python-devel python-setuptools python-twisted

# Ruby Dependencies
RUN yum install -y ruby ruby-devel rubygems && \
    gem install bundler rake

# Perl Dependencies
RUN yum install -y perl-Bit-Vector perl-Class-Accessor perl-ExtUtils-MakeMaker perl-Test-Simple

# PHP Dependencies
RUN yum install -y php php-devel php-pear re2c

# GLibC Dependencies
RUN yum install -y glib2-devel

# Erlang Dependencies
RUN curl -sSL http://packages.erlang-solutions.com/rpm/centos/erlang_solutions.repo -o /etc/yum.repos.d/erlang_solutions.repo && \
    yum install -y erlang-kernel erlang-erts erlang-stdlib erlang-eunit erlang-rebar

# Go Dependencies
RUN curl -sSL https://storage.googleapis.com/golang/go1.4.2.linux-amd64.tar.gz | tar -C /usr/lib/ -xz && \
    mkdir -p /usr/share/go

ENV GOROOT /usr/lib/go
ENV GOPATH /usr/share/go
ENV PATH ${GOROOT}/bin:${GOPATH}/bin:$PATH

# Haskell Dependencies
RUN yum -y install cabal-dev && \
    cabal update && \
    cabal install cabal-install && \
    cd $HOME

# Node.js Dependencies
RUN yum install -y nodejs nodejs-devel npm

# C# Dependencies
RUN yum install -y mono-core mono-devel mono-web-devel mono-extras mingw32-binutils mingw32-runtime mingw32-nsis

# Clean up
RUN rm -rf /tmp/* && \
    yum clean all

WORKDIR $HOME

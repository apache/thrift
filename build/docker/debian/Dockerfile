# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

FROM bufferoverflow/debian:jessie
MAINTAINER Apache Thrift <dev@thrift.apache.org>

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y git

RUN git clone https://git-wip-us.apache.org/repos/asf/thrift
ADD . /thrift

RUN cd /thrift && git status

# General dependencies
RUN apt-get install -y $(cat /thrift/jessie.txt | grep -v ^#)

RUN cabal update
RUN gem install bundler rake

# D dependencies
#ENV FILE_NAME dmd_2.066.0-0_amd64.deb
#ENV SRC_URL http://downloads.dlang.org/releases/2014/$FILE_NAME
# Download the matching tarball from the remote site and unpack it inside the container.
#ADD $SRC_URL $FILE_NAME
#RUN dpkg -i $FILE_NAME


RUN cd /thrift/ && sh bootstrap.sh
# FIXME D build is broken
# FIXME lua build is broken
# FIXME php_extension build is broken
RUN cd /thrift/ && sh configure --without-php_extension --without-lua --without-d --without-haskell
RUN cd /thrift/ && make
RUN mkdir build_native && cd build_native && cmake /thrift/ && make
RUN mkdir build_mingw32 && cd build_mingw32 && cmake -DCMAKE_TOOLCHAIN_FILE=/thrift/contrib/mingw32-toolchain.cmake /thrift/ && make && cpack
RUN cd /thrift/ && make check
RUN cd /thrift/ && make install
RUN cd /thrift/ && make dist
RUN cd /thrift/ && dpkg-buildpackage -d
# FIXME run make cross
# RUN cd /thrift/ && make cross

ENTRYPOINT ["/thrift/compiler/cpp/thrift"]

# TODO export /thrift folder
# TODO provide /thrift/test/cpp/TestServer as reference implementation for testing purpose


#!/bin/sh

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


# Mainly aiming Travis CI's Ubuntu for now


# Install Dependencies
# ---
# General dependencies
sudo apt-add-repository "deb http://archive.ubuntu.com/ubuntu/ trusty main restricted" -y
sudo apt-get update -qq
sudo apt-get install -qq libboost-dev libboost-test-dev libboost-program-options-dev libevent-dev automake libtool flex bison pkg-config g++ libssl-dev make libqt4-dev git debhelper bc 
dpkg -S /usr/include/boost/version.hpp

# Java dependencies
sudo apt-get install -qq ant openjdk-7-jdk

# Python dependencies
sudo apt-get install -qq python-all python-all-dev python-all-dbg python-setuptools python-support

# Ruby dependencies
sudo apt-get install -qq ruby ruby-dev
sudo gem install bundler rake
#gem install bundler -v 1.3.5

# Perl dependencies
sudo apt-get install -qq libbit-vector-perl libclass-accessor-class-perl

# Php dependencies
sudo apt-get install -qq php5 php5-dev php5-cli php-pear re2c

# GlibC dependencies
sudo apt-get install -qq libglib2.0-dev

# Erlang dependencies
sudo apt-get install -qq erlang-base erlang-eunit erlang-dev

# GO dependencies
echo "golang-go golang-go/dashboard boolean false" | debconf-set-selections
sudo apt-get -y install -qq golang golang-go

# Haskell dependencies
#sudo apt-get install -qq ghc cabal-install libghc-binary-dev libghc-network-dev libghc-http-dev libghc-hashable-dev libghc-unordered-containers-dev libghc-vector-dev
#sudo cabal update

# Lua dependencies
sudo apt-get install -qq lua5.2 lua5.2-dev

# Node.js dependencies
sudo apt-get install -qq nodejs nodejs-dev npm
sudo update-alternatives --install /usr/bin/node node /usr/bin/nodejs 10

# CSharp
sudo apt-get install -qq mono-gmcs mono-devel libmono-system-web2.0-cil
sudo apt-get install -qq mingw32 mingw32-binutils mingw32-runtime

# Customize the system
# ---
# Default java to latest 1.7 version
update-java-alternatives -s java-1.7.0-openjdk-amd64 

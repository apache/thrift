# -*- mode: ruby -*-
# vi: set ft=ruby :

#
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

$build_and_test = <<SCRIPT
echo "Provisioning system to compile and test Apache Thrift."
sudo apt-get update -qq -y
sudo apt-get upgrade -qq -y

# Install Dependencies
# ---
# General dependencies
sudo apt-get install -qq libboost-dev libboost-test-dev libboost-program-options-dev libevent-dev automake libtool flex bison pkg-config g++ libssl-dev make libqt4-dev git debhelper

# Java dependencies
sudo apt-get install -qq ant openjdk-7-jdk libcommons-lang3-java

# Python dependencies
sudo apt-get install -qq python-all python-all-dev python-all-dbg python-setuptools

# Ruby dependencies
sudo apt-get install -qq ruby rubygems
sudo gem install bundler rake

# Perl dependencies
sudo apt-get install -qq libbit-vector-perl

# Php dependencies
sudo apt-get install -qq php5 php5-dev php5-cli php-pear

# GlibC dependencies
sudo apt-get install -qq libglib2.0-dev

# Erlang dependencies
sudo apt-get install -qq erlang-base erlang-eunit erlang-dev

# GO dependencies
echo "golang-go golang-go/dashboard boolean false" | debconf-set-selections
sudo apt-get -y install -qq golang golang-go

# Haskell dependencies
sudo apt-get install -qq ghc6 cabal-install libghc6-binary-dev libghc6-network-dev libghc6-http-dev libghc-hashable-dev libghc-unordered-containers-dev libghc-vector-dev

# Node.js dependencies
sudo apt-get install -qq npm

# CSharp
sudo apt-get install -qq mono-gmcs mono-devel libmono-system-web2.0-cil
sudo apt-get install -qq mingw32 mingw32-binutils mingw32-runtime

# Customize the system
# ---
# Default java to latest 1.7 version
update-java-alternatives -s java-1.7.0-openjdk-amd64 

# PHPUnit package broken in ubuntu. see https://bugs.launchpad.net/ubuntu/+source/phpunit/+bug/701544
sudo apt-get upgrade pear
sudo pear channel-discover pear.phpunit.de
sudo pear channel-discover pear.symfony.com
sudo pear channel-discover components.ez.no
sudo pear update-channels
sudo pear upgrade-all
sudo pear install --alldeps phpunit/PHPUnit

date > /etc/vagrant.provisioned

# Start the source build
# ---
echo "Starting Apache Thrift build..."
cd /thrift
sh bootstrap.sh
sh configure --without-erlang
make
make dist
make check
echo "Finished building Apache Thrift."

SCRIPT

Vagrant.configure("2") do |config|
  # Ubuntu 12.04 LTS (Precise Pangolin)
  config.vm.box = "precise64"
  config.vm.box_url = "http://cloud-images.ubuntu.com/precise/current/precise-server-cloudimg-vagrant-amd64-disk1.box"
  # config.vm.box = "precise32"
  # config.vm.box_url = "http://cloud-images.ubuntu.com/precise/current/precise-server-cloudimg-vagrant-i386-disk1.box"

  config.vm.synced_folder "../", "/thrift"

  config.vm.provider :virtualbox do |vbox|
    vbox.customize ["modifyvm", :id, "--memory", "1024"]
    vbox.customize ["modifyvm", :id, "--cpus", "2"]
  end

  # call the script
  config.vm.provision :shell, :inline => $build_and_test

end

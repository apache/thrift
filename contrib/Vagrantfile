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

$script = <<SCRIPT
echo I am provisioning for Apache Thrift ...
sudo apt-get update -qq -y
sudo apt-get upgrade -qq -y
sudo apt-get install -qq libboost-dev libboost-test-dev libboost-program-options-dev libevent-dev automake libtool flex bison pkg-config g++ libssl-dev make libqt4-dev
sudo apt-get install -qq ant openjdk-7-jdk
sudo apt-get install -qq python-all python-all-dev python-all-dbg
sudo apt-get install -qq libbit-vector-perl
sudo apt-get install -qq php5-dev php5-cli phpunit
sudo apt-get install -qq libglib2.0-dev
sudo apt-get install -qq git erlang-base erlang-eunit erlang-dev
sudo apt-get install -qq mono-gmcs mono-devel libmono-system-web2.0-cil
#sudo apt-get install -qq ghc6 cabal-install libghc6-binary-dev libghc6-network-dev libghc6-http-dev
sudo apt-get install -qq mingw32 mingw32-binutils mingw32-runtime
echo I am building Apache Thrift ...
cd /thrift
sh bootstrap.sh
sh configure --without-erlang
make
make dist
make check
#sh test/test.sh
date > /etc/vagrant_provisioned_at
SCRIPT

Vagrant.configure("2") do |config|
  # Ubuntu 12.04 LTS (Precise Pangolin)
  config.vm.box = "precise64"
  config.vm.box_url = "http://cloud-images.ubuntu.com/precise/current/precise-server-cloudimg-vagrant-amd64-disk1.box"
  # config.vm.box = "precise32"
  # config.vm.box_url = "http://cloud-images.ubuntu.com/precise/current/precise-server-cloudimg-vagrant-i386-disk1.box"

  config.vm.synced_folder "../", "/thrift"

  # call the script
  config.vm.provision :shell, :inline => $script
end

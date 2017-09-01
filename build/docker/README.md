# Apache Thrift Docker containers
A set of docker containers used to build and test Apache Thrift

### Available Containers

* centos - based on CentOS 7
* centos6 - based on CentOS 6
* ubuntu - based on Ubuntu Trusty (14.04 LTS)
* ubuntu1604 - based on Ubuntu Xenial (16.04 LTS)

## Dependencies

* A working Docker environment. A Vagrantfile is provided which will setup an Ubuntu host and working Docker environment as well as build the Apache Thrift Docker container for testing and development

## Usage
From the Apache Thrift code base root

* Build

	docker build -t thrift build/docker/ubuntu

	or

	docker build -t thrift build/docker/centos

* Run

	docker run -v $(pwd):/thrift/src -it thrift /bin/bash


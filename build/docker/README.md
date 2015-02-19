# Apache Thrift Docker containers
A set of docker containers used to build and test Apache Thrift

### Available Containers

* Ubuntu
* Centos

## Dependencies

* A working Docker environment. A Vagrantfile is provided which will setup an Ubuntu host and working Docker environment as well as build the Apache Thrift Docker container for testing and development

## Usage
From the Apache Thrift code base root

* Build

	docker build -t thrift contrib/docker/ubuntu

	or

	docker build -t thrift contrib/docker/centos

* Run

	docker run -v $(pwd):/thrift -it thrift /bin/bash


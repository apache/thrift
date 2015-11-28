# Apache Thrift Docker containers
A set of docker containers to use, build and test Apache Thrift

### Available Containers

* Ubuntu - based on ubuntu:trusty (14.04)
* Centos - based on centos:6.6
* Debian - based on debian:jessie

## Dependencies

* A working Docker environment. A Vagrantfile is provided which will setup an Ubuntu host and working Docker environment as well as build the Apache Thrift Docker container for testing and development

## Usage
From the Apache Thrift code base root

* Build

	docker build -t thrift build/docker/ubuntu

	or

	docker build -t thrift build/docker/centos

* Run

	docker run -v $(pwd):/thrift -it thrift /bin/bash

```
docker build -t thrift/ubuntu build/docker/ubuntu
docker run -it --rm -v $(pwd):/thrift thrift/ubuntu /bin/bash -c "cd /thrift && ./bootstrap.sh && ./configure && make check"
```

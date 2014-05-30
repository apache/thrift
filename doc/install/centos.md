## CentOS setup
The following command installs the required tools and libraries from the base repository needed to build and install the Apache Thrift compiler on a CentOS6/RHEL6 Linux based system. 

	sudo yum install automake libtool flex bison pkgconfig gcc-c++ 

The base version of autoconf installed is presently 2.63, however Apache Thrift requires 2.65. A newer version must be installed from a nonstandard repository. For example:

	sudo curl ftp://ftp.pbone.net/mirror/ftp5.gwdg.de/pub/opensuse/repositories/home:/monkeyiq:/centos6updates/CentOS_CentOS-6/noarch/autoconf-2.69-12.2.noarch.rpm > autoconf-2.69-12.2.noarch.rpm

	sudo yum install autoconf-2.69-12.2.noarch.rpm

To compile and install the Apache Thrift IDL compiler from the development source you will need to install git, clone the development master, then configure and build the IDL Compiler. For example:

	sudo yum install git
	git clone https://git-wip-us.apache.org/repos/asf/thrift.git
	cd thrift
	./bootstrap.sh
	./configure --enable-libs=no
	make
	sudo make install

This will build the compiler and install it on the path: /usr/local/bin/thrift

#### Additional reading

For more information on the requirements see: [Apache Thrift Requirements](/docs/install)

For more information on building and installing Thrift see: [Building from source](/docs/BuildingFromSource)

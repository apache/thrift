## Debian or Ubuntu setup
The following command install all the required tools and libraries to build and install the Apache Thrift compiler on a Debian/Ubuntu Linux based system.

	sudo apt-get install libboost-dev libboost-test-dev libboost-program-options-dev libboost-system-dev libboost-filesystem-dev libevent-dev automake libtool flex bison pkg-config g++ libssl-dev 

Then install the Java JDK of your choice. Type **javac** to see a list of available packages, pick the one you prefer and **apt-get install** it.

Debian stable users need to manually install a more recent automake version:

    wget http://ftp.debian.org/debian/pool/main/a/automake-1.14/automake_1.14.1-3_all.deb
    sudo dpkg -i automake_1.14.1-3_all.deb

## Optional packages

Some other packages depend on what languages you want Thrift to support.

 * Ruby 
	* ruby-full ruby-dev ruby-rspec rake rubygems libdaemons-ruby libgemplugin-ruby mongrel
 * Python
	* python-all python-all-dev python-all-dbg
 * Perl
	* libbit-vector-perl libclass-accessor-class-perl
 * Php, install
	* php5-dev php5-cli phpunit
 * C_glib
	* libglib2.0-dev
 * Erlang
	* erlang-base erlang-eunit erlang-dev
 * Csharp
	* mono-gmcs mono-devel libmono-system-web2.0-cil nunit nunit-console
 * Haskell
	* ghc6 cabal-install libghc6-binary-dev libghc6-network-dev libghc6-http-dev
 * Thrift Compiler for Windows
	* mingw32 mingw32-binutils mingw32-runtime nsis


## Additional reading

For more information on the requirements see: [Apache Thrift Requirements](/docs/install)

For more information on building and installing Thrift see: [Building from source](/docs/BuildingFromSource)

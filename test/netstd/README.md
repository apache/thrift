# Apache Thrift net-core-lib tests

Tests for Thrift client library ported to Microsoft .NET Core 

# Content
- ThriftTest - tests for Thrift library 

# Reused components 
- NET Core SDK 3.1 (LTS)

# How to build on Windows
- Get Thrift IDL compiler executable, add to some folder and add path to this folder into PATH variable
- Open ThriftTest.sln in Visual Studio and build
or 
- Build with scripts

# How to build on Unix
- Ensure you have .NET Core 3.0 SDK installed or use the Ubuntu Xenial docker image
- Follow common build practice for Thrift: bootstrap, configure, and make precross


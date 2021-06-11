# Apache Thrift Changelog

## 0.14.2

### Java

- [THRIFT-5383](https://issues.apache.org/jira/browse/THRIFT-5383) - THRIFT-5383 TJSONProtocol Java readString throws on bounds check

### Go

- [THRIFT-5369](https://issues.apache.org/jira/browse/THRIFT-5369) - No longer pre-allocating the whole container (map/set/list) in compiled go code to avoid huge allocations on malformed messages


## 0.14.1

### Known Open Issues (Blocker or Critical)

- [THRIFT-3877](https://issues.apache.org/jira/browse/THRIFT-3877) - C++: library don't work with HTTP (csharp server, cpp client; need cross test enhancement)
- [THRIFT-5098](https://issues.apache.org/jira/browse/THRIFT-5098) - Deprecated: "The high level Network interface is no longer supported. Please use Network.Socket." and other Haskell issues
- [THRIFT-5245](https://issues.apache.org/jira/browse/THRIFT-5245) - NPE when the value of map's key is null

### Deprecated Languages

- [THRIFT-5347](https://issues.apache.org/jira/browse/THRIFT-5347) - Deprecate Haskell bindings

### Build Process

- [THRIFT-5334](https://issues.apache.org/jira/browse/THRIFT-5334) - version of thrift-maven-plugin is not sync with the main project

### Delphi

- [THRIFT-5350](https://issues.apache.org/jira/browse/THRIFT-5350) - 0.14.0 fails to build on non-x86

### Go

- [THRIFT-5353](https://issues.apache.org/jira/browse/THRIFT-5353) - Namespace from type is ignored in generated code

### Python

- [THRIFT-5352](https://issues.apache.org/jira/browse/THRIFT-5352) - Python: IDL exceptions with no fields can't be instantiated

### Rust

- [THRIFT-5299](https://issues.apache.org/jira/browse/THRIFT-5299) - rs implementation compact protocol seq_id should not use zigzag encoding.


## 0.14.0

### Deprecated Languages

- [THRIFT-5229](https://issues.apache.org/jira/browse/THRIFT-5229) - Deprecate ActionScript 3 support

### Removed Languages

- [THRIFT-4980](https://issues.apache.org/jira/browse/THRIFT-4980) - Remove deprecated C# and netcore bindings from the code base
- [THRIFT-4981](https://issues.apache.org/jira/browse/THRIFT-4981) - Remove deprecated netcore bindings from the code base
- [THRIFT-4982](https://issues.apache.org/jira/browse/THRIFT-4982) - Remove deprecated C# bindings from the code base

### Breaking Changes

- [THRIFT-4981](https://issues.apache.org/jira/browse/THRIFT-4981) - Remove deprecated netcore bindings from the code base
- [THRIFT-4982](https://issues.apache.org/jira/browse/THRIFT-4982) - Remove deprecated csharp bindings from the code base
- [THRIFT-4990](https://issues.apache.org/jira/browse/THRIFT-4990) - Upgrade to .NET Core 3.1 (LTS)
- [THRIFT-5006](https://issues.apache.org/jira/browse/THRIFT-5006) - Implement DEFAULT_MAX_LENGTH at TFramedTransport
- [THRIFT-5069](https://issues.apache.org/jira/browse/THRIFT-5069) - In Go library TDeserializer.Transport is now typed \*TMemoryBuffer instead of TTransport
- [THRIFT-5072](https://issues.apache.org/jira/browse/THRIFT-5072) - Haskell generator fails to distinguish between multiple enum types with conflicting enum identifiers
- [THRIFT-5116](https://issues.apache.org/jira/browse/THRIFT-5116) - Upgrade NodeJS to 10.x
- [THRIFT-5138](https://issues.apache.org/jira/browse/THRIFT-5138) - Swift generator does not escape keywords properly
- [THRIFT-5164](https://issues.apache.org/jira/browse/THRIFT-5164) - In Go library TProcessor interface now includes ProcessorMap and AddToProcessorMap functions.
- [THRIFT-5186](https://issues.apache.org/jira/browse/THRIFT-5186) - cpp: use all getaddrinfo() results when retrying failed bind() in T{Nonblocking,}ServerSocket
- [THRIFT-5233](https://issues.apache.org/jira/browse/THRIFT-5233) - go: Now all Read*, Write* and Skip functions in TProtocol accept context arg
- [THRIFT-5152](https://issues.apache.org/jira/browse/THRIFT-5152) - go: TSocket and TSSLSocket now have separated connect timeout and socket timeout
- c++: dropped support for Windows XP
- [THRIFT-5326](https://issues.apache.org/jira/browse/THRIFT-5326) - go: TException interface now has a new function: TExceptionType
- [THRIFT-4914](https://issues.apache.org/jira/browse/THRIFT-4914) - go: TClient.Call now returns ResponseMeta in addition to error

### Known Open Issues (Blocker or Critical)

- [THRIFT-3877](https://issues.apache.org/jira/browse/THRIFT-3877) - C++: library don't work with HTTP (csharp server, cpp client; need cross test enhancement)
- [THRIFT-5098](https://issues.apache.org/jira/browse/THRIFT-5098) - Deprecated: "The high level Network interface is no longer supported. Please use Network.Socket." and other Haskell issues
- [THRIFT-5245](https://issues.apache.org/jira/browse/THRIFT-5245) - NPE when the value of map's key is null
- [THRIFT-4687](https://issues.apache.org/jira/browse/THRIFT-4687) - Add thrift 0.12.0 to pypi and/or enable more maintainers

### Build Process

- [THRIFT-4976](https://issues.apache.org/jira/browse/THRIFT-4976) - Docker build: Test failure for `StalenessCheckTest` on MacOS
- [THRIFT-5087](https://issues.apache.org/jira/browse/THRIFT-5087) - test/test.py fails with "AssertionError: Python 3.3 or later is required for proper operation."
- [THRIFT-5097](https://issues.apache.org/jira/browse/THRIFT-5097) - Incorrect THRIFT_VERSION in ThriftConfig.cmake
- [THRIFT-5109](https://issues.apache.org/jira/browse/THRIFT-5109) - Misc CMake improvements
- [THRIFT-5147](https://issues.apache.org/jira/browse/THRIFT-5147) - Add uninstall function
- [THRIFT-5218](https://issues.apache.org/jira/browse/THRIFT-5218) - Automated Github release artifacts do not match checksums provided
- [THRIFT-5249](https://issues.apache.org/jira/browse/THRIFT-5249) - travis-ci : Failed to run FastbinaryTest.py

### C glib

- [THRIFT-4873](https://issues.apache.org/jira/browse/THRIFT-4873) - Memory leak in c_glib
- [THRIFT-5118](https://issues.apache.org/jira/browse/THRIFT-5118) - Fix memory leak when the handler method return a exception
- [THRIFT-5134](https://issues.apache.org/jira/browse/THRIFT-5134) - Fix memory leak when the handler method return FALSE
- [THRIFT-5144](https://issues.apache.org/jira/browse/THRIFT-5144) - Fix memory leak when generate deserialize list element
- [THRIFT-4272](https://issues.apache.org/jira/browse/THRIFT-4272) - warnings in glibc library
- [THRIFT-4952](https://issues.apache.org/jira/browse/THRIFT-4952) - Modified ssl_read feedback value break all the time error.
- [THRIFT-5076](https://issues.apache.org/jira/browse/THRIFT-5076) - Improve CMake OpenSSL usage
- [THRIFT-5094](https://issues.apache.org/jira/browse/THRIFT-5094) - Fix memory leak in thrift_server_set_property()
- [THRIFT-5101](https://issues.apache.org/jira/browse/THRIFT-5101) - Return NULL install of FALSE for thrift_server_socket_accept()
- [THRIFT-5102](https://issues.apache.org/jira/browse/THRIFT-5102) - Fix memory leak in thrift_simple_server_serve()
- [THRIFT-5136](https://issues.apache.org/jira/browse/THRIFT-5136) - Fix memory leak in thrift_multiplexed_processor_process_impl()
- [THRIFT-5221](https://issues.apache.org/jira/browse/THRIFT-5221) - Fix stack overflow when reading buffer
- [THRIFT-5237](https://issues.apache.org/jira/browse/THRIFT-5237) - Implement MAX_MESSAGE_SIZE and consolidate limits into a TConfiguration class
- [THRIFT-5255](https://issues.apache.org/jira/browse/THRIFT-5255) - Fix stack overflow in framed transport
- [THRIFT-5256](https://issues.apache.org/jira/browse/THRIFT-5256) - Fix some compile warnings
- [THRIFT-5268](https://issues.apache.org/jira/browse/THRIFT-5268) - Fix some file loss ')' in define

### C++

- [THRIFT-1513](https://issues.apache.org/jira/browse/THRIFT-1513) - Thrift compiler generates inconsistent code with some complex values (causing g++ to error: "has no member named '__isset')
- [THRIFT-5168](https://issues.apache.org/jira/browse/THRIFT-5168) - Useless generated code when .thrift file only has service type
- [THRIFT-5179](https://issues.apache.org/jira/browse/THRIFT-5179) - Thrift compiler will generate wrong code if IDL struct's name is 'a' or  'b'
- [THRIFT-5200](https://issues.apache.org/jira/browse/THRIFT-5200) - Thrift compiler will generate incorrect code when add 'cob_style' option.
- [THRIFT-4282](https://issues.apache.org/jira/browse/THRIFT-4282) - StressTestNonBlocking is disabled in Appveyor as it is unstable on Windows in general
- [THRIFT-4682](https://issues.apache.org/jira/browse/THRIFT-4682) - C++ TBinaryProtocol crashes on port scan
- [THRIFT-4963](https://issues.apache.org/jira/browse/THRIFT-4963) - TNonblockingServer blocked int addTask(IOThread) and notify(workerThread)
- [THRIFT-5047](https://issues.apache.org/jira/browse/THRIFT-5047) - fix cmake support to build cpp server without OPENSSL
- [THRIFT-5076](https://issues.apache.org/jira/browse/THRIFT-5076) - Improve CMake OpenSSL usage
- [THRIFT-5078](https://issues.apache.org/jira/browse/THRIFT-5078) - Handle named pipe clients quickly disconnecting
- [THRIFT-5086](https://issues.apache.org/jira/browse/THRIFT-5086) - CMake target thrift::thrift has no INTERFACE_INCLUDE_DIRECTORIES property
- [THRIFT-5110](https://issues.apache.org/jira/browse/THRIFT-5110) - Added a number of required libs for using static OpenSSL
- [THRIFT-5114](https://issues.apache.org/jira/browse/THRIFT-5114) - Simplify the computation of the size of TMemoryBuffer
- [THRIFT-5177](https://issues.apache.org/jira/browse/THRIFT-5177) - getaddrinfo() should not be used for Unix sockets
- [THRIFT-5178](https://issues.apache.org/jira/browse/THRIFT-5178) - THttpClient should work without specifying host
- [THRIFT-5185](https://issues.apache.org/jira/browse/THRIFT-5185) - C++: Add WebSocket Server Transport
- [THRIFT-5186](https://issues.apache.org/jira/browse/THRIFT-5186) - AI_ADDRCONFIG: Thrift libraries crash with localhost-only network.
- [THRIFT-5215](https://issues.apache.org/jira/browse/THRIFT-5215) - C++: Remove portable_endian.h
- [THRIFT-5217](https://issues.apache.org/jira/browse/THRIFT-5217) - Deprecated boost header
- [THRIFT-5237](https://issues.apache.org/jira/browse/THRIFT-5237) - Implement MAX_MESSAGE_SIZE and consolidate limits into a TConfiguration class
- [THRIFT-5290](https://issues.apache.org/jira/browse/THRIFT-5290) - Adjusting cpp *.cproj msvcrt options according to LEGAL-538 
- [THRIFT-5295](https://issues.apache.org/jira/browse/THRIFT-5295) - Thread and ThreadFactory should be extensible
- [THRIFT-5344](https://issues.apache.org/jira/browse/THRIFT-5344) - TTransport may throw raw pointer exceptions

### Compiler (General)

- [THRIFT-4173](https://issues.apache.org/jira/browse/THRIFT-4173) - Go: thrift compiler generates wrong code for list of aliased type 
- [THRIFT-4938](https://issues.apache.org/jira/browse/THRIFT-4938) - Issues with version.h treatment
- [THRIFT-4973](https://issues.apache.org/jira/browse/THRIFT-4973) - Add deprecation messages for csharp and netcore
- [THRIFT-4980](https://issues.apache.org/jira/browse/THRIFT-4980) - Remove deprecated C# and netcore bindings from the code base
- [THRIFT-4982](https://issues.apache.org/jira/browse/THRIFT-4982) - Remove deprecated C# bindings from the code baseï…‚
- [THRIFT-5153](https://issues.apache.org/jira/browse/THRIFT-5153) - Deprecate byte
- [THRIFT-5225](https://issues.apache.org/jira/browse/THRIFT-5225) - Use nullptr instead of NULL 
- [THRIFT-5302](https://issues.apache.org/jira/browse/THRIFT-5302) - Add recursive function name uniqueness check

### D

- [THRIFT-5059](https://issues.apache.org/jira/browse/THRIFT-5059) - Add cross tests for TZlibTransport in D
- [THRIFT-5156](https://issues.apache.org/jira/browse/THRIFT-5156) - D: Fix library compilation on Windows and compiler warnings
- [THRIFT-5166](https://issues.apache.org/jira/browse/THRIFT-5166) - Add WebSocket Server Transport
- [THRIFT-5184](https://issues.apache.org/jira/browse/THRIFT-5184) - D: WebSocket Server Transport Fix for Firefox

### Delphi

- [THRIFT-5044](https://issues.apache.org/jira/browse/THRIFT-5044) - Improve serialization support for TApplicationExceptions and custom exceptions
- [THRIFT-5154](https://issues.apache.org/jira/browse/THRIFT-5154) - Generate interface IDs (IID) for Windows platforms
- [THRIFT-5235](https://issues.apache.org/jira/browse/THRIFT-5235) - Add property setter for isset flags
- [THRIFT-5261](https://issues.apache.org/jira/browse/THRIFT-5261) - Support for deprecated methods (via annotation)
- [THRIFT-5004](https://issues.apache.org/jira/browse/THRIFT-5004) - Make exception implementations more consistent 
- [THRIFT-5005](https://issues.apache.org/jira/browse/THRIFT-5005) - Refactoring of the Delphi libs
- [THRIFT-5006](https://issues.apache.org/jira/browse/THRIFT-5006) - Implement DEFAULT_MAX_LENGTH at TFramedTransport
- [THRIFT-5007](https://issues.apache.org/jira/browse/THRIFT-5007) - Implement MAX_MESSAGE_SIZE and remaining read bytes control
- [THRIFT-5009](https://issues.apache.org/jira/browse/THRIFT-5009) - Serializer implemtation lacks support for layered transports
- [THRIFT-5012](https://issues.apache.org/jira/browse/THRIFT-5012) - Centralize configuration aspects into a commonly used configuration object
- [THRIFT-5015](https://issues.apache.org/jira/browse/THRIFT-5015) - WinHTTP QueryDataAvailable cannot be used to retrieve total response size
- [THRIFT-5036](https://issues.apache.org/jira/browse/THRIFT-5036) - buffered transport over sockets may run into unexpected timeouts
- [THRIFT-5048](https://issues.apache.org/jira/browse/THRIFT-5048) - EnumUtils<T>.ToString() throws for elements not known to the receiving end
- [THRIFT-5088](https://issues.apache.org/jira/browse/THRIFT-5088) - Memory leak in TEndpointTransportBase
- [THRIFT-5123](https://issues.apache.org/jira/browse/THRIFT-5123) - add possibility to query HTTP status code with WinHTTP
- [THRIFT-5146](https://issues.apache.org/jira/browse/THRIFT-5146) - Align Delphi to the test suite arguments rules (its "--switch=value", not "--switch value")
- [THRIFT-5186](https://issues.apache.org/jira/browse/THRIFT-5186) - AI_ADDRCONFIG: Thrift libraries crash with localhost-only network.
- [THRIFT-5188](https://issues.apache.org/jira/browse/THRIFT-5188) - Occasional ERROR_INSUFFICIENT_BUFFER at WinHttpQueryHeaders()
- [THRIFT-5251](https://issues.apache.org/jira/browse/THRIFT-5251) - StringUtils<T>.ToString() raises an exception for enum values outside range
- [THRIFT-5304](https://issues.apache.org/jira/browse/THRIFT-5304) - TWinHTTPClientImpl may incorrectly report that the message size is reached

### Documentation

- [THRIFT-5037](https://issues.apache.org/jira/browse/THRIFT-5037) - Documentation for TConfiguration
- [THRIFT-5065](https://issues.apache.org/jira/browse/THRIFT-5065) - Fix broken links in the IDL document
- [THRIFT-5074](https://issues.apache.org/jira/browse/THRIFT-5074) - Cleanup test suite command line options

### Go

- [THRIFT-4914](https://issues.apache.org/jira/browse/THRIFT-4914) - Compiler generated service clients now provide a new function, LastResponseMeta_(), to get the response metadata (e.g. headers from THeader) from the last client call.
- [THRIFT-4984](https://issues.apache.org/jira/browse/THRIFT-4984) - Scary and spammy "error processing request: EOF" logs from TSimpleServer
- [THRIFT-4985](https://issues.apache.org/jira/browse/THRIFT-4985) - Clean up logging in go library
- [THRIFT-5002](https://issues.apache.org/jira/browse/THRIFT-5002) - remote client fails to compile when extending services
- [THRIFT-5019](https://issues.apache.org/jira/browse/THRIFT-5019) - Multiple import same namespace for go included files
- [THRIFT-5046](https://issues.apache.org/jira/browse/THRIFT-5046) - Custom tags remove db and json tags
- [THRIFT-5069](https://issues.apache.org/jira/browse/THRIFT-5069) - Add TSerializerPool and TDeserializerPool, which are thread-safe versions of TSerializer and TDeserializer.
- [THRIFT-5092](https://issues.apache.org/jira/browse/THRIFT-5092) - Panic on nil buffer writes
- [THRIFT-5152](https://issues.apache.org/jira/browse/THRIFT-5152) - Separate timeout in TSocket
- [THRIFT-5164](https://issues.apache.org/jira/browse/THRIFT-5164) - Go middleware support
- [THRIFT-5214](https://issues.apache.org/jira/browse/THRIFT-5214) - go: Implement connection check in TSocket
- [THRIFT-5233](https://issues.apache.org/jira/browse/THRIFT-5233) - I/O timeout handling in go library
- [THRIFT-5240](https://issues.apache.org/jira/browse/THRIFT-5240) - The context passed into server handler implementations will be canceled when we detected that the client closed the connection.
- [THRIFT-5257](https://issues.apache.org/jira/browse/THRIFT-5257) - Go THeader implementation doesn't handle endOfFrame correctly
- [THRIFT-5270](https://issues.apache.org/jira/browse/THRIFT-5270) - Go library unit test is broken in go 1.15
- [THRIFT-5278](https://issues.apache.org/jira/browse/THRIFT-5278) - Expose API to use THeader+TCompactProtocol in go library client code
- [THRIFT-5279](https://issues.apache.org/jira/browse/THRIFT-5279) - Cleanups/small optimizations for go's serializer/deserializer code
- [THRIFT-5294](https://issues.apache.org/jira/browse/THRIFT-5294) - Go: TSimpleJSONProtocol could panic on WriteMessageEnd without matching WriteMessageBegin
- [THRIFT-5322](https://issues.apache.org/jira/browse/THRIFT-5322) - Add support to TConfiguration, and also fix a bug that could cause excessive memory usage when reading malformed messages from TCompactProtocol.
- [THRIFT-5338](https://issues.apache.org/jira/browse/THRIFT-5338) - Proposal: Raise minimal supported Go version with upcoming 0.14.0 release

### Haskell

- [THRIFT-5072](https://issues.apache.org/jira/browse/THRIFT-5072) - Haskell generator fails to distinguish between multiple enum types with conflicting enum identifiers
- [THRIFT-4959](https://issues.apache.org/jira/browse/THRIFT-4959) - cabal.exe: --enable-tests was specified, but tests can't be enabled in a remote package
- [THRIFT-5211](https://issues.apache.org/jira/browse/THRIFT-5211) - Handle incomplete reads correctly

### Java

- [THRIFT-4252](https://issues.apache.org/jira/browse/THRIFT-4252) - Cannot shutdown Java server when clients are still connected
- [THRIFT-4889](https://issues.apache.org/jira/browse/THRIFT-4889) - Add SASL support for non-blocking server
- [THRIFT-4937](https://issues.apache.org/jira/browse/THRIFT-4937) - Apache HttpCore 4.4.1 reached EoS
- [THRIFT-4949](https://issues.apache.org/jira/browse/THRIFT-4949) - improve HTTP/1 server test case
- [THRIFT-5008](https://issues.apache.org/jira/browse/THRIFT-5008) - Add a logger line in case of failing to dispose sasl
- [THRIFT-5013](https://issues.apache.org/jira/browse/THRIFT-5013) - Use Java Objects RequireNonNull
- [THRIFT-5016](https://issues.apache.org/jira/browse/THRIFT-5016) - Do Not Check 'other' For Null in Equals
- [THRIFT-5022](https://issues.apache.org/jira/browse/THRIFT-5022) - TIOStreamTransport.isOpen returns true for one-sided transports (see THRIFT-2530).
- [THRIFT-5031](https://issues.apache.org/jira/browse/THRIFT-5031) - Fix javadoc of TIOStreamTransport
- [THRIFT-5115](https://issues.apache.org/jira/browse/THRIFT-5115) - PR #2022 Updated gradle to 6.2 broke CI
- [THRIFT-5190](https://issues.apache.org/jira/browse/THRIFT-5190) - StringUtils haven't take `(offset + length) > bytes.length` into account
- [THRIFT-5197](https://issues.apache.org/jira/browse/THRIFT-5197) - TSSLTransportFactory Do Not Wrap NOT_OPEN Exception Type for Client
- [THRIFT-5201](https://issues.apache.org/jira/browse/THRIFT-5201) - Use Apache Parent Pom for Thrift Maven Plugin
- [THRIFT-5202](https://issues.apache.org/jira/browse/THRIFT-5202) - TNonblockingMultiFetchClient Use SLF4J Parameterized Logging
- [THRIFT-5203](https://issues.apache.org/jira/browse/THRIFT-5203) - Remove Unused toString Method in TSerializer
- [THRIFT-5237](https://issues.apache.org/jira/browse/THRIFT-5237) - Implement MAX_MESSAGE_SIZE and consolidate limits into a TConfiguration class
- [THRIFT-5247](https://issues.apache.org/jira/browse/THRIFT-5247) - Avoiding meaningless System.copy
- [THRIFT-5274](https://issues.apache.org/jira/browse/THRIFT-5274) - Thrift 0.13.0 does not work with JDK8
- [THRIFT-5287](https://issues.apache.org/jira/browse/THRIFT-5287) - Log When Client Connections are Dropped
- [THRIFT-5288](https://issues.apache.org/jira/browse/THRIFT-5288) - Move Support for ByteBuffer into TTransport

### JavaScript

- [THRIFT-5234](https://issues.apache.org/jira/browse/THRIFT-5234) - Fix a number of js/ts generation issues

### Lua

- [THRIFT-5106](https://issues.apache.org/jira/browse/THRIFT-5106) - Fix various Lua library and compiler issues
- [THRIFT-5260](https://issues.apache.org/jira/browse/THRIFT-5260) - Fix the thrift compiler generate problematic lua code for the oneway method
- [THRIFT-4992](https://issues.apache.org/jira/browse/THRIFT-4992) - thrift lua TcompactProtocol bug fix  #1881
- [THRIFT-5262](https://issues.apache.org/jira/browse/THRIFT-5262) - Fix a encoding struct bug in the compact protocol implementation to lua
- [THRIFT-5282](https://issues.apache.org/jira/browse/THRIFT-5282) - Add IPv6 client support to Lua library
- [THRIFT-5286](https://issues.apache.org/jira/browse/THRIFT-5286) - Fix Lua library readBool() in TCompactProtocol
- [THRIFT-5325](https://issues.apache.org/jira/browse/THRIFT-5325) - Fix Lua library writeStructEnd() in TCompactProtocol

### Markdown

- [THRIFT-5289](https://issues.apache.org/jira/browse/THRIFT-5289) - Add markdown compiler

### netstd

- [THRIFT-5032](https://issues.apache.org/jira/browse/THRIFT-5032) - Allows PascalCase properties for netstd
- [THRIFT-5091](https://issues.apache.org/jira/browse/THRIFT-5091) - Netstd generator produces uncompileable code for struct names ending with "_result" or "_args"
- [THRIFT-5095](https://issues.apache.org/jira/browse/THRIFT-5095) - ToString() should print entire structure, not just the top-level data
- [THRIFT-5198](https://issues.apache.org/jira/browse/THRIFT-5198) - Fix certain Visual Studio hints in generated netstd code
- [THRIFT-5216](https://issues.apache.org/jira/browse/THRIFT-5216) - generate DeepCopy methods
- [THRIFT-5220](https://issues.apache.org/jira/browse/THRIFT-5220) - DeepCopy() extension methods not generated when the IDL contains no service
- [THRIFT-5238](https://issues.apache.org/jira/browse/THRIFT-5238) - GetHashCode can throw NullReferenceException
- [THRIFT-5253](https://issues.apache.org/jira/browse/THRIFT-5253) - using Result in result name generates wrong IAsync interface
- [THRIFT-5254](https://issues.apache.org/jira/browse/THRIFT-5254) - Member name cannot be Isset (unless it is an "required" member)
- [THRIFT-5316](https://issues.apache.org/jira/browse/THRIFT-5316) - Netstd compiler generates wrong ToString() method: .ToString(sb) 
- [THRIFT-5317](https://issues.apache.org/jira/browse/THRIFT-5317) - netstd compiler does not escape keywords
- [THRIFT-5320](https://issues.apache.org/jira/browse/THRIFT-5320) - Usage of "Task" as IDL identifier generates uncompileable code
- [THRIFT-4990](https://issues.apache.org/jira/browse/THRIFT-4990) - Upgrade to .NET Core 3.1 (LTS)
- [THRIFT-5010](https://issues.apache.org/jira/browse/THRIFT-5010) - BinaryPrimitives.Read/WriteInt32BigEndian should be used to convert to/from network byte order
- [THRIFT-5020](https://issues.apache.org/jira/browse/THRIFT-5020) - Refactoring & minor fixes for netstd library
- [THRIFT-5021](https://issues.apache.org/jira/browse/THRIFT-5021) - Implement MAX_MESSAGE_SIZE and consolidate limits into a TConfiguration class
- [THRIFT-5026](https://issues.apache.org/jira/browse/THRIFT-5026) - TestClient/Server ignores first cmdline argument
- [THRIFT-5027](https://issues.apache.org/jira/browse/THRIFT-5027) - Implement remaining read bytes checks
- [THRIFT-5053](https://issues.apache.org/jira/browse/THRIFT-5053) - Fix the netstd tutorial console logging and README
- [THRIFT-5083](https://issues.apache.org/jira/browse/THRIFT-5083) - NetStd JSON Protocol left in incorrect state 
- [THRIFT-5133](https://issues.apache.org/jira/browse/THRIFT-5133) - TCompactProtocol string allocation improvement
- [THRIFT-5172](https://issues.apache.org/jira/browse/THRIFT-5172) - NetStd TBaseClient open output transport multiple times
- [THRIFT-5210](https://issues.apache.org/jira/browse/THRIFT-5210) - further performance optimizations
- [THRIFT-5239](https://issues.apache.org/jira/browse/THRIFT-5239) - THttpTransport should support passing in an HttpClient
- [THRIFT-5252](https://issues.apache.org/jira/browse/THRIFT-5252) - Make CreateHttpClientHandler() method virtual
- [THRIFT-5275](https://issues.apache.org/jira/browse/THRIFT-5275) - Compilation error with Thrift when used in .Net Framework 4.6.1 or above
- [THRIFT-5343](https://issues.apache.org/jira/browse/THRIFT-5343) - TTlsSocketTransport does not resolve IPv4 addresses or validate hostnames correctly

### Node.js

- [THRIFT-3356](https://issues.apache.org/jira/browse/THRIFT-3356) - TypeError: 'undefined' is not a function (evaluating 'Error.captureStackTrace(this, this.constructor)')
- [THRIFT-4994](https://issues.apache.org/jira/browse/THRIFT-4994) - TWebSocketTransport false scope in forEach in browser
- [THRIFT-5003](https://issues.apache.org/jira/browse/THRIFT-5003) - Websocket Connection in Browsers with nodejs code
- [THRIFT-5116](https://issues.apache.org/jira/browse/THRIFT-5116) - Ubuntu xenial NodeJS 6.x is too old, 10.x required
- [THRIFT-5163](https://issues.apache.org/jira/browse/THRIFT-5163) - adds Q to exports for browserify

### Perl

- [THRIFT-5050](https://issues.apache.org/jira/browse/THRIFT-5050) - Fix MemoryBuffer.pm to raise a proper exception if no data is available
- [THRIFT-5066](https://issues.apache.org/jira/browse/THRIFT-5066) - Implement testBinary invocation in TestClient.pl

### PHP

- [THRIFT-4942](https://issues.apache.org/jira/browse/THRIFT-4942) - Set PHP struct generated field values as private with getters and setters
- [THRIFT-5082](https://issues.apache.org/jira/browse/THRIFT-5082) - Add a Class reference for PHP enum $_TSPEC
- [THRIFT-5103](https://issues.apache.org/jira/browse/THRIFT-5103) - PHP 7.4 THttpClient deprecated error
- [THRIFT-5130](https://issues.apache.org/jira/browse/THRIFT-5130) - Use Apcu instead of APC
- [THRIFT-5132](https://issues.apache.org/jira/browse/THRIFT-5132) - Warning in TSocket when using ssl connection
- [THRIFT-5199](https://issues.apache.org/jira/browse/THRIFT-5199) - Infinite loop in PHP TSocket::write when peer closes connection 
- [THRIFT-5336](https://issues.apache.org/jira/browse/THRIFT-5336) - Add possibility to setup connection timeout in TCurlClient

### Python

- [THRIFT-2087](https://issues.apache.org/jira/browse/THRIFT-2087) - unicode decode errors
- [THRIFT-4002](https://issues.apache.org/jira/browse/THRIFT-4002) - Thrift exceptions are not hashable in Python 3
- [THRIFT-5107](https://issues.apache.org/jira/browse/THRIFT-5107) - Travis build fails with missing Python 3.3 or newer?
- [THRIFT-5165](https://issues.apache.org/jira/browse/THRIFT-5165) - Python THttpClient saves cookie when Set-Cookie response header is present
- [THRIFT-5186](https://issues.apache.org/jira/browse/THRIFT-5186) - AI_ADDRCONFIG: Thrift libraries crash with localhost-only network.
- [THRIFT-5248](https://issues.apache.org/jira/browse/THRIFT-5248) - Python: Make TSocket.isOpen check if the other end is still connected
- [THRIFT-5303](https://issues.apache.org/jira/browse/THRIFT-5303) - Unicode decode errors in _fast_decode
- [THRIFT-5331](https://issues.apache.org/jira/browse/THRIFT-5331) - Python: allow THeaderProtocol to choose which subprotocol to use for outbound connections

### Ruby

- [THRIFT-5281](https://issues.apache.org/jira/browse/THRIFT-5281) -  Some warning messages need to be fixed
- [THRIFT-4707](https://issues.apache.org/jira/browse/THRIFT-4707) - Enable maintainers to upload newer versions of Ruby Gem of Thrift
- [THRIFT-5061](https://issues.apache.org/jira/browse/THRIFT-5061) - Pin Ruby's rack version to 2.0.8
- [THRIFT-5100](https://issues.apache.org/jira/browse/THRIFT-5100) - Gem::InstallError: byebug requires Ruby version >= 2.4.0.
- [THRIFT-5266](https://issues.apache.org/jira/browse/THRIFT-5266) - release ruby library thrift 0.13.0

### Rust

- [THRIFT-4764](https://issues.apache.org/jira/browse/THRIFT-4764) - Rust frontend emits deprecated clippy suppression attributes
- [THRIFT-5071](https://issues.apache.org/jira/browse/THRIFT-5071) - Rust: rust tutorial can not be compiled with rust edition 2018
- [THRIFT-5158](https://issues.apache.org/jira/browse/THRIFT-5158) - Update Rust Compiler to generate 2018 edition code only
- [THRIFT-5307](https://issues.apache.org/jira/browse/THRIFT-5307) - Rust generated code should compile cleanly with clippy
- [THRIFT-4915](https://issues.apache.org/jira/browse/THRIFT-4915) - Deserializing double into OrderedFloat always returns zero when using TCompactProtocol
- [THRIFT-4995](https://issues.apache.org/jira/browse/THRIFT-4995) - [Rust] Use `ToSocketAddrs` for expressing network addresses
- [THRIFT-5042](https://issues.apache.org/jira/browse/THRIFT-5042) - Fix failing cargo tests
- [THRIFT-5043](https://issues.apache.org/jira/browse/THRIFT-5043) - Make TBufferChannel clonable
- [THRIFT-5111](https://issues.apache.org/jira/browse/THRIFT-5111) - CI fails with error[E0721]: `await` is a keyword in the 2018 edition
- [THRIFT-5131](https://issues.apache.org/jira/browse/THRIFT-5131) - i64 maxint decoding panics with integer-encoding >= 1.1.0
- [THRIFT-5306](https://issues.apache.org/jira/browse/THRIFT-5306) - Rust library, tutorial, test, cross-test code should not throw any clippy errors

### Swift

- [THRIFT-4989](https://issues.apache.org/jira/browse/THRIFT-4989) - Run time exception when using TCompactProtocol
- [THRIFT-5128](https://issues.apache.org/jira/browse/THRIFT-5128) - Swift TFramedTransport does not work using present code
- [THRIFT-5138](https://issues.apache.org/jira/browse/THRIFT-5138) - Swift generator does not escape keywords properly
- [THRIFT-5155](https://issues.apache.org/jira/browse/THRIFT-5155) - Swift 5.1 support
- [THRIFT-5070](https://issues.apache.org/jira/browse/THRIFT-5070) - Swift: Hashable.hashValue is deprecated as a protocol requirement
- [THRIFT-5084](https://issues.apache.org/jira/browse/THRIFT-5084) - Swift: Server-side support for Multiplexing Services
- [THRIFT-5121](https://issues.apache.org/jira/browse/THRIFT-5121) - Logic bug in TMultiplexedProcessor â€“ Swift
- [THRIFT-5125](https://issues.apache.org/jira/browse/THRIFT-5125) - Swift server does not work using present code.
- [THRIFT-5129](https://issues.apache.org/jira/browse/THRIFT-5129) - Swift TSocketTransport cannot be used to connect to client
- [THRIFT-5150](https://issues.apache.org/jira/browse/THRIFT-5150) - TSet does not compile with Swift 5.2

### Test Suite

- [THRIFT-4974](https://issues.apache.org/jira/browse/THRIFT-4974) - Add cross test for Python's Unix domain socket transport
- [THRIFT-5145](https://issues.apache.org/jira/browse/THRIFT-5145) - Streamline  --pipe and --named-pipe options in the code base
- [THRIFT-5171](https://issues.apache.org/jira/browse/THRIFT-5171) - Fix maven-ant-tasks to use HTTPS instead of HTTP

### TypeScript - Library

- [THRIFT-5003](https://issues.apache.org/jira/browse/THRIFT-5003) - Websocket Connection in Browsers with nodejs code

### Tutorial

- [THRIFT-4972](https://issues.apache.org/jira/browse/THRIFT-4972) - Add Makefile.am to the Perl tutorial
- [THRIFT-4975](https://issues.apache.org/jira/browse/THRIFT-4975) - Add Makefile.am to the PHP tutorial
- [THRIFT-5051](https://issues.apache.org/jira/browse/THRIFT-5051) - Fix Python tutorials to address THRIFT-4002
- [THRIFT-5052](https://issues.apache.org/jira/browse/THRIFT-5052) - Make the Go tutorial executable to the end
- [THRIFT-5122](https://issues.apache.org/jira/browse/THRIFT-5122) - Fix memory leak in c_glib tutorial server


## 0.13.0

### New Languages

- (none)

### Deprecated Languages

- [THRIFT-4723](https://issues.apache.org/jira/browse/THRIFT-4723) - CSharp and Netcore targets are deprecated and will be removed with the next release - use NetStd instead.
	
### Removed Languages

- [THRIFT-4719](https://issues.apache.org/jira/browse/THRIFT-4719) - Cocoa language was removed - use swift instead.

### Breaking Changes

- [THRIFT-4743](https://issues.apache.org/jira/browse/THRIFT-4743) - compiler: removed the plug-in mechanism
- [THRIFT-4720](https://issues.apache.org/jira/browse/THRIFT-4720) - cpp: C++03/C++98 support has been removed; also removed boost as a runtime dependency
- [THRIFT-4730](https://issues.apache.org/jira/browse/THRIFT-4730) - cpp: BoostThreadFactory, PosixThreadFactory, StdThreadFactory removed
- [THRIFT-4732](https://issues.apache.org/jira/browse/THRIFT-4732) - cpp: CMake build changed to use BUILD_SHARED_LIBS
- [THRIFT-4735](https://issues.apache.org/jira/browse/THRIFT-4735) - cpp: Removed Qt4 support
- [THRIFT-4740](https://issues.apache.org/jira/browse/THRIFT-4740) - cpp: Use std::chrono::duration for timeouts
- [THRIFT-4762](https://issues.apache.org/jira/browse/THRIFT-4762) - cpp: TTransport::getOrigin() is now const
- [THRIFT-4702](https://issues.apache.org/jira/browse/THRIFT-4702) - java: class org.apache.thrift.AutoExpandingBuffer is no longer public
- [THRIFT-4709](https://issues.apache.org/jira/browse/THRIFT-4709) - java: changes to UTF-8 handling require JDK 1.7 at a minimum
- [THRIFT-4712](https://issues.apache.org/jira/browse/THRIFT-4712) - java: class org.apache.thrift.ShortStack is no longer public
- [THRIFT-4725](https://issues.apache.org/jira/browse/THRIFT-4725) - java: change return type signature of 'process' methods
- [THRIFT-4805](https://issues.apache.org/jira/browse/THRIFT-4805) - java: replaced TSaslTransportException with TTransportException
- [THRIFT-2530](https://issues.apache.org/jira/browse/THRIFT-2530) - java: TIOStreamTransport's "isOpen" now returns false after "close" is called
- [THRIFT-4675](https://issues.apache.org/jira/browse/THRIFT-4675) - js: now uses node-int64 for 64 bit integer constants
- [THRIFT-4841](https://issues.apache.org/jira/browse/THRIFT-4841) - delphi: old THTTPTransport is now TMsxmlHTTPTransport
- [THRIFT-4536](https://issues.apache.org/jira/browse/THRIFT-4536) - rust: convert from try-from crate to rust stable (1.34+), re-export ordered-float

### Known Issues (Blocker or Critical)

- [THRIFT-3877](https://issues.apache.org/jira/browse/THRIFT-3877) - C++: library don't work with HTTP (csharp server, cpp client; need cross test enhancement)

### As3

- [THRIFT-4784](https://issues.apache.org/jira/browse/THRIFT-4784) - Thrift should throw when skipping over unexpected data

### Build Process

- [THRIFT-2333](https://issues.apache.org/jira/browse/THRIFT-2333) - RPMBUILD: Abort build if user did not disable ruby but ruby build will fail later on
- [THRIFT-4689](https://issues.apache.org/jira/browse/THRIFT-4689) - Pull changes from 0.12.0 release branch into master
- [THRIFT-4690](https://issues.apache.org/jira/browse/THRIFT-4690) - Update dlang deimos for OpenSSL 1.1 (use 1.1.0h tagged release instead of master)
- [THRIFT-4694](https://issues.apache.org/jira/browse/THRIFT-4694) - Upgrade Java to Java 1.8
- [THRIFT-4716](https://issues.apache.org/jira/browse/THRIFT-4716) - Create a version alignment tool to make releases easier
- [THRIFT-4760](https://issues.apache.org/jira/browse/THRIFT-4760) - Install pkgconfig when using cmake
- [THRIFT-4769](https://issues.apache.org/jira/browse/THRIFT-4769) - Change NuGet package to use netstd artifact
- [THRIFT-4811](https://issues.apache.org/jira/browse/THRIFT-4811) - Add cmake config module
- [THRIFT-4855](https://issues.apache.org/jira/browse/THRIFT-4855) - go CI fails with "cannot find package "golang.org/x/tools/go/packages" in any of ..."
- [THRIFT-4864](https://issues.apache.org/jira/browse/THRIFT-4864) - CI fails at netstd
- [THRIFT-4874](https://issues.apache.org/jira/browse/THRIFT-4874) - Thrift 0.12.0 Source Distribution (.tar.gz) Contains Hardlinks - Extract Fails
- [THRIFT-4896](https://issues.apache.org/jira/browse/THRIFT-4896) - cpp and c_glib include paths are added to source files when building
- [THRIFT-4966](https://issues.apache.org/jira/browse/THRIFT-4966) - Git ignore files generated by the build

### C glib

- [THRIFT-4842](https://issues.apache.org/jira/browse/THRIFT-4842) - Multiplexed protocol has a memory leak in set c_glib
- [THRIFT-4878](https://issues.apache.org/jira/browse/THRIFT-4878) - c_glib ThriftSocket support for unix domain sockets
- [THRIFT-4950](https://issues.apache.org/jira/browse/THRIFT-4950) - fix bind print  error and Macro call errors thrift_server_socket

### C#

- [THRIFT-3587](https://issues.apache.org/jira/browse/THRIFT-3587) - C# TTLSSocket does not use timeout for opening the socket
- [THRIFT-4024](https://issues.apache.org/jira/browse/THRIFT-4024) - Skip() should throw on unknown data types
- [THRIFT-4684](https://issues.apache.org/jira/browse/THRIFT-4684) - Missing namespace and un-used private fields in WCF fault classes when enable WCF in C# code generation
- [THRIFT-4715](https://issues.apache.org/jira/browse/THRIFT-4715) - C# union "data" should be strongly-typed
- [THRIFT-4723](https://issues.apache.org/jira/browse/THRIFT-4723) - Consolidate C# and netcore into new netstd language target (and finally deprecate both C# and netcore bindings)
- [THRIFT-4741](https://issues.apache.org/jira/browse/THRIFT-4741) - Missing "inner" argument from one CTOR
- [THRIFT-4769](https://issues.apache.org/jira/browse/THRIFT-4769) - Change NuGet package to use netstd artifact
- [THRIFT-4859](https://issues.apache.org/jira/browse/THRIFT-4859) - Enables changing 'UserAgent' 
- [THRIFT-4907](https://issues.apache.org/jira/browse/THRIFT-4907) - strong named assemblies wanted

### C++

- [THRIFT-4384](https://issues.apache.org/jira/browse/THRIFT-4384) - Using a concurrent client with cpp async is not safe.
- [THRIFT-4441](https://issues.apache.org/jira/browse/THRIFT-4441) - C++: support building lib without Boost
- [THRIFT-4487](https://issues.apache.org/jira/browse/THRIFT-4487) - gettimeofday: windows implementation not quoting source, applying license to foreign code
- [THRIFT-4593](https://issues.apache.org/jira/browse/THRIFT-4593) - Unit Tests failing on Alpine Linux due to non-portable mutex initializers
- [THRIFT-4678](https://issues.apache.org/jira/browse/THRIFT-4678) - add noexcept cpp generator option
- [THRIFT-4720](https://issues.apache.org/jira/browse/THRIFT-4720) - Drop support for C++03/C++98 and begin refactoring
- [THRIFT-4730](https://issues.apache.org/jira/browse/THRIFT-4730) - Remove pthread and boost::thread library support and use std::thread for C++11
- [THRIFT-4735](https://issues.apache.org/jira/browse/THRIFT-4735) - Remove C++ Qt4 support (leave Qt5) - Qt4 LTS ended in 2014
- [THRIFT-4739](https://issues.apache.org/jira/browse/THRIFT-4739) - Good old concurrency_test failing on windows builds again with some regularity
- [THRIFT-4740](https://issues.apache.org/jira/browse/THRIFT-4740) - Use std::chrono for timeout and remove old structures.
- [THRIFT-4762](https://issues.apache.org/jira/browse/THRIFT-4762) - C++: Applied some C++11 refactorings to the runtime library and compiler
- [THRIFT-4776](https://issues.apache.org/jira/browse/THRIFT-4776) - Modernize c++11 code by clang-tidy
- [THRIFT-4830](https://issues.apache.org/jira/browse/THRIFT-4830) - Add to_string function for enum in C++ file generate
- [THRIFT-4861](https://issues.apache.org/jira/browse/THRIFT-4861) - Fix use of deprecated boost endian header; move to minimum boost 1.56.0
- [THRIFT-4936](https://issues.apache.org/jira/browse/THRIFT-4936) - add depth limit type exception description
- [THRIFT-4962](https://issues.apache.org/jira/browse/THRIFT-4962) - Deadlock in TimerManager::stop

### cocoa

- [THRIFT-4719](https://issues.apache.org/jira/browse/THRIFT-4719) - Remove cocoa language support

## Compiler (General)

- [THRIFT-4743](https://issues.apache.org/jira/browse/THRIFT-4743) - Remove the compiler plug-in mode

### contributed

- [THRIFT-4897](https://issues.apache.org/jira/browse/THRIFT-4897) - UT of thrift-maven-plugin failed

## D language

- [THRIFT-4690](https://issues.apache.org/jira/browse/THRIFT-4690) - Update dlang deimos for OpenSSL 1.1 (use 1.1.0h tagged release instead of master)
- [THRIFT-4724](https://issues.apache.org/jira/browse/THRIFT-4724) - dlang dub.json dependency for openssl is too restrictive
- [THRIFT-4918](https://issues.apache.org/jira/browse/THRIFT-4918) - dlang name conflict

### dart

- [THRIFT-4654](https://issues.apache.org/jira/browse/THRIFT-4654) - Thrift Dart port is not compatible with Dart 2

### Delphi

- [THRIFT-4024](https://issues.apache.org/jira/browse/THRIFT-4024) - Skip() should throw on unknown data types
- [THRIFT-4841](https://issues.apache.org/jira/browse/THRIFT-4841) - THTTPTransport relies on activeX component
- [THRIFT-4843](https://issues.apache.org/jira/browse/THRIFT-4843) - http:// and https:// schemes are switched in test client
- [THRIFT-4862](https://issues.apache.org/jira/browse/THRIFT-4862) - better ToString() support for enums and container types
- [THRIFT-4863](https://issues.apache.org/jira/browse/THRIFT-4863) - better indication of WinHTTP errors
- [THRIFT-4881](https://issues.apache.org/jira/browse/THRIFT-4881) - Allow TLS1.1 and TLS1.2 over WinHTTP even when not configured as systemwide default
- [THRIFT-4882](https://issues.apache.org/jira/browse/THRIFT-4882) - Autodetect proxy settings with WinHTTP
- [THRIFT-4884](https://issues.apache.org/jira/browse/THRIFT-4884) - Add serialisation performance test for Delphi
- [THRIFT-4886](https://issues.apache.org/jira/browse/THRIFT-4886) - More detailed error information for WinHTTP transport
- [THRIFT-4894](https://issues.apache.org/jira/browse/THRIFT-4894) - Enable automatic content encoding handling for gzip,deflate in the WinHTTP client
- [THRIFT-4939](https://issues.apache.org/jira/browse/THRIFT-4939) - TThriftListImpl<T>.Sort() does not use comparer
- [THRIFT-4944](https://issues.apache.org/jira/browse/THRIFT-4944) - Field IDs > 255 fail with compact protocol

### Documentation

- [THRIFT-4697](https://issues.apache.org/jira/browse/THRIFT-4697) - Create updated release procedures
- [THRIFT-4808](https://issues.apache.org/jira/browse/THRIFT-4808) - Update LANGUAGES.md on master to reflect master
- [THRIFT-4933](https://issues.apache.org/jira/browse/THRIFT-4933) - Incorrect description in the 0.12.0 version of the documentation

### Erlang

- [THRIFT-4583](https://issues.apache.org/jira/browse/THRIFT-4583) - Support rebar3 for erlang builds
- [THRIFT-4744](https://issues.apache.org/jira/browse/THRIFT-4744) - Erlang help intendation not aligned

### Go

- [THRIFT-4024](https://issues.apache.org/jira/browse/THRIFT-4024) - Skip() should throw on unknown data types
- [THRIFT-4612](https://issues.apache.org/jira/browse/THRIFT-4612) - Add THeader for Go
- [THRIFT-4747](https://issues.apache.org/jira/browse/THRIFT-4747) - The 'omitempty' tag should not be appended to optional fields that have a default value
- [THRIFT-4797](https://issues.apache.org/jira/browse/THRIFT-4797) - Generated Go code produces name collisions on imports
- [THRIFT-4908](https://issues.apache.org/jira/browse/THRIFT-4908) - reader&writer in golang's TBinaryProtocol is not necessary and misleading

### haskell

- [THRIFT-4834](https://issues.apache.org/jira/browse/THRIFT-4834) - CI error at Haskell: Failed to load interface for `Network'
- [THRIFT-4955](https://issues.apache.org/jira/browse/THRIFT-4955) - Haskell test broken due to extension to CompactProtoTestStruct
- [THRIFT-4956](https://issues.apache.org/jira/browse/THRIFT-4956) - DebugProtoTest_Main.hs: Invalid ThriftType 128

### haxe

- [THRIFT-4024](https://issues.apache.org/jira/browse/THRIFT-4024) - Skip() should throw on unknown data types
- [THRIFT-4812](https://issues.apache.org/jira/browse/THRIFT-4812) - haxelib readme still points to old ASF git repo

### HTML

- [THRIFT-4763](https://issues.apache.org/jira/browse/THRIFT-4763) - HTML compiler produces invalid HTML document

### Java

- [THRIFT-2530](https://issues.apache.org/jira/browse/THRIFT-2530) - TIOStreamTransport's isOpen() always be true even if close() was called.
- [THRIFT-4368](https://issues.apache.org/jira/browse/THRIFT-4368) - Guaranteed NPE in TBaseAsyncProcessor.java
- [THRIFT-4469](https://issues.apache.org/jira/browse/THRIFT-4469) - isServing is not thread safe
- [THRIFT-4481](https://issues.apache.org/jira/browse/THRIFT-4481) - TBinaryProtocol.writeMessageEnd isn't throwable exception
- [THRIFT-4695](https://issues.apache.org/jira/browse/THRIFT-4695) - Pre-Size Java Collections in Union
- [THRIFT-4696](https://issues.apache.org/jira/browse/THRIFT-4696) - NonBlocking Server: Use case-switch Statement Instead of if-else Clauses
- [THRIFT-4702](https://issues.apache.org/jira/browse/THRIFT-4702) - Improve AutoExpandingBuffer
- [THRIFT-4704](https://issues.apache.org/jira/browse/THRIFT-4704) - Streamline TDeserializer Implementation
- [THRIFT-4709](https://issues.apache.org/jira/browse/THRIFT-4709) - Use StandardCharset UTF-8
- [THRIFT-4711](https://issues.apache.org/jira/browse/THRIFT-4711) - Improve Immutable None Type Instantiation
- [THRIFT-4712](https://issues.apache.org/jira/browse/THRIFT-4712) - Improve Performance of ShortStack
- [THRIFT-4713](https://issues.apache.org/jira/browse/THRIFT-4713) - Review of TBaseHelper.java
- [THRIFT-4714](https://issues.apache.org/jira/browse/THRIFT-4714) - Java TFramedTransport calls write twice for each flush
- [THRIFT-4725](https://issues.apache.org/jira/browse/THRIFT-4725) - Change Return Type Signature of Process Methods
- [THRIFT-4726](https://issues.apache.org/jira/browse/THRIFT-4726) - Remove SLF4J Logging Guards
- [THRIFT-4748](https://issues.apache.org/jira/browse/THRIFT-4748) - Add Jitpack support
- [THRIFT-4766](https://issues.apache.org/jira/browse/THRIFT-4766) - JDK9+ fails on missing annotations
- [THRIFT-4773](https://issues.apache.org/jira/browse/THRIFT-4773) - TSaslTransport should relay underlying TTransportException to TSaslTransportException 
- [THRIFT-4805](https://issues.apache.org/jira/browse/THRIFT-4805) - Suppress excessive logging of SASL TTransportExceptions in case of END_OF_FILE 
- [THRIFT-4849](https://issues.apache.org/jira/browse/THRIFT-4849) - Do not Ignore InterruptedException
- [THRIFT-4851](https://issues.apache.org/jira/browse/THRIFT-4851) - Remove All Calls To printStackTrace
- [THRIFT-4857](https://issues.apache.org/jira/browse/THRIFT-4857) - Java field hash code implementation inconsistent with equals.
- [THRIFT-4858](https://issues.apache.org/jira/browse/THRIFT-4858) - Java TThreadPoolServer: confusing error message on closed socket
- [THRIFT-4865](https://issues.apache.org/jira/browse/THRIFT-4865) - warning: [deprecation] UTF_8 in Charsets has been deprecated
- [THRIFT-4899](https://issues.apache.org/jira/browse/THRIFT-4899) - Generated TypeScript declarations incorrectly references types when there is more than 1 include
- [THRIFT-4945](https://issues.apache.org/jira/browse/THRIFT-4945) - Log output mode is not standardized
- [THRIFT-4957](https://issues.apache.org/jira/browse/THRIFT-4957) - testSanePartsOfCompactProtoTestStruct FAILED

### JavaScript

- [THRIFT-4675](https://issues.apache.org/jira/browse/THRIFT-4675) - JS code generators not handling int64 type properly for constants and for TypeScript type mappings
- [THRIFT-4728](https://issues.apache.org/jira/browse/THRIFT-4728) - Cleanup for the double rendering test in JS 
- [THRIFT-4737](https://issues.apache.org/jira/browse/THRIFT-4737) - thrift.js does not use customHeaders in jqRequest
- [THRIFT-4745](https://issues.apache.org/jira/browse/THRIFT-4745) - warning C4305: 'initializing' : truncation from '"__int64' to 'long'
- [THRIFT-4757](https://issues.apache.org/jira/browse/THRIFT-4757) - grunt-shell-spawn drags in sync-exec which has a security notice

### netcore

- [THRIFT-4024](https://issues.apache.org/jira/browse/THRIFT-4024) - Skip() should throw on unknown data types
- [THRIFT-4722](https://issues.apache.org/jira/browse/THRIFT-4722) - Netcore union "data" should be strongly-typed
- [THRIFT-4723](https://issues.apache.org/jira/browse/THRIFT-4723) - Consolidate C# and netcore into new netstd language target (and finally deprecate both C# and netcore bindings)
- [THRIFT-4742](https://issues.apache.org/jira/browse/THRIFT-4742) - Typo "cannot read from null input stream" on write
- [THRIFT-4769](https://issues.apache.org/jira/browse/THRIFT-4769) - Change NuGet package to use netstd artifact
- [THRIFT-4919](https://issues.apache.org/jira/browse/THRIFT-4919) - THttpTransport.cs (netstd) and THttpClientTransport (netcore) have bad timeout code

### netstd

- [THRIFT-4768](https://issues.apache.org/jira/browse/THRIFT-4768) - Remove "nullable" option from the code base (netstd ONLY)
- [THRIFT-4772](https://issues.apache.org/jira/browse/THRIFT-4772) - fully enable server-side usage of framed/buffered transports
- [THRIFT-4813](https://issues.apache.org/jira/browse/THRIFT-4813) - NamedPipes may not work in all cases
- [THRIFT-4816](https://issues.apache.org/jira/browse/THRIFT-4816) - JSONTransports Context.WriteAsync/ReadAsync are badly named
- [THRIFT-4817](https://issues.apache.org/jira/browse/THRIFT-4817) - Add string CTOR to TTlsSocketTransport
- [THRIFT-4818](https://issues.apache.org/jira/browse/THRIFT-4818) - Test client should use cancellation token
- [THRIFT-4821](https://issues.apache.org/jira/browse/THRIFT-4821) - Normalize TServerSocketTransport constructors in netstd
- [THRIFT-4822](https://issues.apache.org/jira/browse/THRIFT-4822) - Refactor bool CTOR flags into enum type
- [THRIFT-4824](https://issues.apache.org/jira/browse/THRIFT-4824) - Logger deprecation warnings in tutorial
- [THRIFT-4825](https://issues.apache.org/jira/browse/THRIFT-4825) - Align TTlsServerSocketTransport constructors with TServerSocketTransport - Breaking Change
- [THRIFT-4829](https://issues.apache.org/jira/browse/THRIFT-4829) - HTTP server transport lacks TransportFactory arguments
- [THRIFT-4831](https://issues.apache.org/jira/browse/THRIFT-4831) - interface ITProtocolFactory should be class TProtocolFactory again
- [THRIFT-4832](https://issues.apache.org/jira/browse/THRIFT-4832) - superfluous backing field causes CS0169 "field never used"
- [THRIFT-4839](https://issues.apache.org/jira/browse/THRIFT-4839) - Remove embedded buffering/framed options from TCP transports
- [THRIFT-4840](https://issues.apache.org/jira/browse/THRIFT-4840) - Update the README in the netstd tutorial to include references to the new buffering arguments
- [THRIFT-4848](https://issues.apache.org/jira/browse/THRIFT-4848) - Add ability to set Content-Type,Accept headers in HTTP client
- [THRIFT-4853](https://issues.apache.org/jira/browse/THRIFT-4853) - TServerFramedTransport is now obsolete and can be removed
- [THRIFT-4854](https://issues.apache.org/jira/browse/THRIFT-4854) - oneway calls do not work over HTTP
- [THRIFT-4860](https://issues.apache.org/jira/browse/THRIFT-4860) - Allow changing "User-Agent"
- [THRIFT-4879](https://issues.apache.org/jira/browse/THRIFT-4879) - general performance improvements for netstd library
- [THRIFT-4891](https://issues.apache.org/jira/browse/THRIFT-4891) - Align HTTP test client with all other variants
- [THRIFT-4893](https://issues.apache.org/jira/browse/THRIFT-4893) - Enable automatic content encoding handling for gzip,deflate in the HTTP client
- [THRIFT-4898](https://issues.apache.org/jira/browse/THRIFT-4898) - Pipe write operations across a network are limited to 65,535 bytes per write. 
- [THRIFT-4919](https://issues.apache.org/jira/browse/THRIFT-4919) - THttpTransport.cs (netstd) and THttpClientTransport (netcore) have bad timeout code

### node.js

- [THRIFT-3060](https://issues.apache.org/jira/browse/THRIFT-3060) - Node.js client retry logic doesn't flush offline queue on reconnect
- [THRIFT-4675](https://issues.apache.org/jira/browse/THRIFT-4675) - JS code generators not handling int64 type properly for constants and for TypeScript type mappings
- [THRIFT-4738](https://issues.apache.org/jira/browse/THRIFT-4738) - Generated typescript type definition files are incorrect
- [THRIFT-4771](https://issues.apache.org/jira/browse/THRIFT-4771) - THeader for node.js
- [THRIFT-4809](https://issues.apache.org/jira/browse/THRIFT-4809) - Javascript episodic code generation
- [THRIFT-4844](https://issues.apache.org/jira/browse/THRIFT-4844) - createConnection ignores connect_timeout option

### perl

- [THRIFT-4691](https://issues.apache.org/jira/browse/THRIFT-4691) - The perl CPAN module contains no tests

### PHP

- [THRIFT-4751](https://issues.apache.org/jira/browse/THRIFT-4751) - Missing imports in TProtocol (phpdoc related only)
- [THRIFT-4794](https://issues.apache.org/jira/browse/THRIFT-4794) - Finish adding json protocol to the php cross test
- [THRIFT-4807](https://issues.apache.org/jira/browse/THRIFT-4807) - PHP extension segfaults if reference is used in input
- [THRIFT-4845](https://issues.apache.org/jira/browse/THRIFT-4845) - PHP's TCurlClient ignores timeout values smaller that 1 second

### python

- [THRIFT-1549](https://issues.apache.org/jira/browse/THRIFT-1549) - Python TSSLSocket: Shutdown cleanly
- [THRIFT-4733](https://issues.apache.org/jira/browse/THRIFT-4733) - Address already in use with python unit test
- [THRIFT-4767](https://issues.apache.org/jira/browse/THRIFT-4767) - support tcp keepalive in python
- [THRIFT-4778](https://issues.apache.org/jira/browse/THRIFT-4778) - Python protocol factories do not derive from TProtocolFactory
- [THRIFT-4779](https://issues.apache.org/jira/browse/THRIFT-4779) - Python, Java TMultiplexedProcessor do not raise TProtocolException
- [THRIFT-4780](https://issues.apache.org/jira/browse/THRIFT-4780) - TMultiplexedProcessor is not fully tested or implemented in Python
- [THRIFT-4783](https://issues.apache.org/jira/browse/THRIFT-4783) - Thrift should throw when skipping over unexpected data
- [THRIFT-4798](https://issues.apache.org/jira/browse/THRIFT-4798) - Fix python THttpServer to honor correct oneway reply semantics
- [THRIFT-4892](https://issues.apache.org/jira/browse/THRIFT-4892) - SASL data type exception for PLAIN
- [THRIFT-4920](https://issues.apache.org/jira/browse/THRIFT-4920) - Binary constants emit non-binary Python literals

### ruby

- [THRIFT-4721](https://issues.apache.org/jira/browse/THRIFT-4721) - Installing the ruby gem on systems without make fails in the build_ext task.
- [THRIFT-4971](https://issues.apache.org/jira/browse/THRIFT-4971) - Fix lib/rb/spec/union_spec.rb so that CI succeeds

### rust

- [THRIFT-4953](https://issues.apache.org/jira/browse/THRIFT-4953) - Unspecified Field Identifier Creates Non Compiling Rust Code
- [THRIFT-4960](https://issues.apache.org/jira/browse/THRIFT-4960) - Bare Trait Warnings

### Swift

- [THRIFT-4902](https://issues.apache.org/jira/browse/THRIFT-4902) - Swift compatibility with Swift 4.2, 5.0 and 5.1

### Test suite

- [THRIFT-4301](https://issues.apache.org/jira/browse/THRIFT-4301) - configuring --without-python and --without-py3 still invokes py3 tests in make cross
- [THRIFT-4405](https://issues.apache.org/jira/browse/THRIFT-4405) - Incorrect handling of sequence numbers that wrap to negative
- [THRIFT-4794](https://issues.apache.org/jira/browse/THRIFT-4794) - Finish adding json protocol to the php cross test
- [THRIFT-4969](https://issues.apache.org/jira/browse/THRIFT-4969) - PHP test doesn't check the code generation with php:classmap

### Tutorial

- [THRIFT-4426](https://issues.apache.org/jira/browse/THRIFT-4426) - repository should not include symbolic links
- [THRIFT-4965](https://issues.apache.org/jira/browse/THRIFT-4965) - Perl tutorial server doesn't work due to the lack of use statement
- [THRIFT-4967](https://issues.apache.org/jira/browse/THRIFT-4967) - Node.js tutorial server fails if the zip function invoked
- [THRIFT-4968](https://issues.apache.org/jira/browse/THRIFT-4968) - Makefile.am in the Ruby tutorial refers to Python directory
- [THRIFT-4970](https://issues.apache.org/jira/browse/THRIFT-4970) - PHP tutorial doesn't work with Thrift v0.12.0+

### Typescript

- [THRIFT-4675](https://issues.apache.org/jira/browse/THRIFT-4675) - JS code generators not handling int64 type properly for constants and for TypeScript type mappings

## 0.12.0

Released 2019-JAN-04

### New Languages
- Common LISP (cl)
- Swift
- Typescript (nodets)

### Deprecated Languages
- C++03/C++98 (move to C++11)
- Cocoa (move to Swift)

### Breaking Changes (since 0.11.0)
- [THRIFT-4529](https://issues.apache.org/jira/browse/THRIFT-4529) - Rust enum variants are now camel-cased instead of uppercased to conform to Rust naming conventions
- [THRIFT-4448](https://issues.apache.org/jira/browse/THRIFT-4448) - Support for golang 1.6 and earlier has been dropped.
- [THRIFT-4474](https://issues.apache.org/jira/browse/THRIFT-4474) - PHP now uses the PSR-4 loader by default instead of class maps.
- [THRIFT-4532](https://issues.apache.org/jira/browse/THRIFT-4532) - method signatures changed in the compiler's t_oop_generator.
- [THRIFT-4648](https://issues.apache.org/jira/browse/THRIFT-4648) - The C (GLib) compiler's handling of namespaces has been improved.

### Known Issues (Blocker or Critical)
- [THRIFT-4037](https://issues.apache.org/jira/browse/THRIFT-4037) - build: use a single build system for thrift
- [THRIFT-4119](https://issues.apache.org/jira/browse/THRIFT-4119) - build: bootstrap.sh is missing from source tarball
- [THRIFT-3289](https://issues.apache.org/jira/browse/THRIFT-3289) - csharp: socket exhaustion in csharp implementation
- [THRIFT-3029](https://issues.apache.org/jira/browse/THRIFT-3029) - cocoa: Getters for fields defined with uppercase names do not work
- [THRIFT-3325](https://issues.apache.org/jira/browse/THRIFT-3325) - cocoa: Extended services aren't subclasses in generated Cocoa
- [THRIFT-4116](https://issues.apache.org/jira/browse/THRIFT-4116) - cocoa: Thrift de-capitalizes the name of IsSet property in Cocoa
- [THRIFT-3877](https://issues.apache.org/jira/browse/THRIFT-3877) - cpp: the http implementation is not standard; interop with other languages is spotty at best
- [THRIFT-4180](https://issues.apache.org/jira/browse/THRIFT-4180) - cpp: Impossible to build Thrift C++ library for Android (NDK)
- [THRIFT-4384](https://issues.apache.org/jira/browse/THRIFT-4384) - cpp: Using multiple async services simultaneously is not thread-safe
- [THRIFT-3108](https://issues.apache.org/jira/browse/THRIFT-3108) - haskell: Defaulted struct parameters on a service generates invalid Haskell
- [THRIFT-3990](https://issues.apache.org/jira/browse/THRIFT-3990) - nodejs: Exception swallowed by deserialization function
- [THRIFT-4214](https://issues.apache.org/jira/browse/THRIFT-4214) - nodejs: map<i64,value> key treated as hex value in JavaScript
- [THRIFT-4602](https://issues.apache.org/jira/browse/THRIFT-4602) - nodejs: ERROR in ./node_modules/thrift/lib/nodejs/lib/thrift/connection.js Module not found: Error: Can't resolve 'child_process'
- [THRIFT-4639](https://issues.apache.org/jira/browse/THRIFT-4639) - nodejs: Sequence numbering for multiplexed protocol broken
- [THRIFT-1310](https://issues.apache.org/jira/browse/THRIFT-1310) - php: sequence and reconnection management issues
- [THRIFT-1538](https://issues.apache.org/jira/browse/THRIFT-1538) - php: Error during deserialization int64 on 32-bit architecture
- [THRIFT-1580](https://issues.apache.org/jira/browse/THRIFT-1580) - php: thrift type i64 java to php serialize/deserealize not working
- [THRIFT-1950](https://issues.apache.org/jira/browse/THRIFT-1950) - php: PHP gets stuck in infinite loop
- [THRIFT-2954](https://issues.apache.org/jira/browse/THRIFT-2954) - python: sending int or float in a double field breaks the connection
- [THRIFT-4080](https://issues.apache.org/jira/browse/THRIFT-4080) - python: unix sockets can get stuck forever
- [THRIFT-4281](https://issues.apache.org/jira/browse/THRIFT-4281) - python: generated code is out of order and causes load issues
- [THRIFT-4677](https://issues.apache.org/jira/browse/THRIFT-4677) - py3: UnicodeDecideError in Python3

### Build Process
- [THRIFT-4067](https://issues.apache.org/jira/browse/THRIFT-4067) - Windows thrift compiler distributed on the apache web site has runtime dependencies
- [THRIFT-4308](https://issues.apache.org/jira/browse/THRIFT-4308) - D language docker images need demios for libevent and openssl fixed to re-enable make cross on dlang
- [THRIFT-4579](https://issues.apache.org/jira/browse/THRIFT-4579) - Use Ubuntu Bionic (18.04 LTS) for CI builds instead of Artful (17.10)
- [THRIFT-4508](https://issues.apache.org/jira/browse/THRIFT-4508) - Define CI operating system coverage rules for the project and (hopefully) simplify CI a little more
- [THRIFT-4397](https://issues.apache.org/jira/browse/THRIFT-4397) - ubuntu install instructions broken on 16.04
- [THRIFT-4545](https://issues.apache.org/jira/browse/THRIFT-4545) - Appveyor builds are failing due to a haskell / cabal update in chocolatey
- [THRIFT-4452](https://issues.apache.org/jira/browse/THRIFT-4452) - optimize Dockerfile (only onetime apt-get update)
- [THRIFT-4440](https://issues.apache.org/jira/browse/THRIFT-4440) - rm `build/docker/ubuntu-trusty/Dockerfile.orig`
- [THRIFT-4352](https://issues.apache.org/jira/browse/THRIFT-4352) - Ubuntu Artful doesn't appear to be compatible with Thrift and Haxe 3.4.2
- [THRIFT-4666](https://issues.apache.org/jira/browse/THRIFT-4666) - DLang Client Pool Test fails sporadically
- [THRIFT-4676](https://issues.apache.org/jira/browse/THRIFT-4676) - CL tutorial build fails sporadically
- [THRIFT-4456](https://issues.apache.org/jira/browse/THRIFT-4456) - Make haxelib download quiet so it doesn't blow up the build log
- [THRIFT-4605](https://issues.apache.org/jira/browse/THRIFT-4605) - bootstrap.sh fails if automake=1.16.1

### c_glib
- [THRIFT-4648](https://issues.apache.org/jira/browse/THRIFT-4648) - The C (GLib) compiler's handling of namespaces has been improved.
- [THRIFT-4622](https://issues.apache.org/jira/browse/THRIFT-4622) - glibC compilation issue
- [THRIFT-4671](https://issues.apache.org/jira/browse/THRIFT-4671) - c glib is unable to handle client close unexpectedly

### cl (new language support in 0.12.0)
- [THRIFT-82](https://issues.apache.org/jira/browse/THRIFT-82) - Common Lisp support

### csharp
- [THRIFT-4558](https://issues.apache.org/jira/browse/THRIFT-4558) - reserved Csharp keywords are not escaped in some cases
- [THRIFT-4637](https://issues.apache.org/jira/browse/THRIFT-4637) - C# async mode generates incorrect code with inherited services
- [THRIFT-4672](https://issues.apache.org/jira/browse/THRIFT-4672) - IAsyncResult style methods not being supported by certain transports leads to issues in mixed ISync/IAsync use cases
- [THRIFT-4539](https://issues.apache.org/jira/browse/THRIFT-4539) - Allow TBufferedTransport to be used as base class
- [THRIFT-4535](https://issues.apache.org/jira/browse/THRIFT-4535) - XML docs; code cleanup (tabs->spaces; String->string)
- [THRIFT-4492](https://issues.apache.org/jira/browse/THRIFT-4492) - protected ExceptionType type member of TApplicationException cannot be accessed
- [THRIFT-4446](https://issues.apache.org/jira/browse/THRIFT-4446) - JSONProtocol Base64 Encoding Trims Padding
- [THRIFT-4455](https://issues.apache.org/jira/browse/THRIFT-4455) - Missing dispose calls in ThreadedServer & ThreadpoolServer
- [THRIFT-4609](https://issues.apache.org/jira/browse/THRIFT-4609) - keep InnerException wherever appropriate
- [THRIFT-4673](https://issues.apache.org/jira/browse/THRIFT-4673) - IAsyncResult not supported by layered transports (buffered/framed)

### cpp
- [THRIFT-4476](https://issues.apache.org/jira/browse/THRIFT-4476) - Typecasting problem on list items
- [THRIFT-4465](https://issues.apache.org/jira/browse/THRIFT-4465) - TNonblockingServer throwing THRIFT LOGGER: TConnection::workSocket(): THRIFT_EAGAIN (unavailable resources)
- [THRIFT-4680](https://issues.apache.org/jira/browse/THRIFT-4680) - TBufferTransports.h does not compile under Visual Studio 2017
- [THRIFT-4618](https://issues.apache.org/jira/browse/THRIFT-4618) - TNonblockingServer crash because of limitation of select()
- [THRIFT-4620](https://issues.apache.org/jira/browse/THRIFT-4620) - TZlibTransport.cpp doesn't ensure that there is enough space for the zlib flush marker in the buffer.
- [THRIFT-4571](https://issues.apache.org/jira/browse/THRIFT-4571) - ZeroMQ contrib library needs a refresh
- [THRIFT-4559](https://issues.apache.org/jira/browse/THRIFT-4559) - TSSLServerSocket incorrectly prints errors
- [THRIFT-4578](https://issues.apache.org/jira/browse/THRIFT-4578) - Move `TAsyncProtocolProcessor` into main thrift library
- [THRIFT-4418](https://issues.apache.org/jira/browse/THRIFT-4418) - evhttp_connection_new is deprecated; use evhttp_connection_base_new

### compiler
- [THRIFT-4644](https://issues.apache.org/jira/browse/THRIFT-4644) - Compiler cannot be compiled on macOS(maybe also on other platforms with clang)
- [THRIFT-4531](https://issues.apache.org/jira/browse/THRIFT-4531) - Thrift generates wrong Python code for immutable structures with optional members
- [THRIFT-4513](https://issues.apache.org/jira/browse/THRIFT-4513) - thrift generated code is not stable for constants
- [THRIFT-4532](https://issues.apache.org/jira/browse/THRIFT-4532) - Avoid updating Thrift compiler generated code if the output has not changed
- [THRIFT-4400](https://issues.apache.org/jira/browse/THRIFT-4400) - Visual Studio Compiler project should link runtime statically in release builds
- [THRIFT-4399](https://issues.apache.org/jira/browse/THRIFT-4399) - plugin.thrift t_const_value is not used as a union in C++ code -- fix this
- [THRIFT-4496](https://issues.apache.org/jira/browse/THRIFT-4496) - Dealing with language keywords in Thrift (e.g. service method names)
- [THRIFT-4393](https://issues.apache.org/jira/browse/THRIFT-4393) - repeated runs of compiler produce different binary output at plugin interface

### dlang
- [THRIFT-4478](https://issues.apache.org/jira/browse/THRIFT-4478) - Thrift will not build with dlang 2.078 or later
- [THRIFT-4503](https://issues.apache.org/jira/browse/THRIFT-4503) - dlang servers logError on normal client disconnection
- [THRIFT-4308](https://issues.apache.org/jira/browse/THRIFT-4308) - D language docker images need demios for libevent and openssl fixed to re-enable make cross on dlang

### dart
- [THRIFT-4646](https://issues.apache.org/jira/browse/THRIFT-4646) - Effective Dart and Exceptions
- [THRIFT-4439](https://issues.apache.org/jira/browse/THRIFT-4439) - Shouldn't download dart.deb directly.

### delphi
- [THRIFT-4562](https://issues.apache.org/jira/browse/THRIFT-4562) - Calling wrong exception CTOR leads to "call failed: unknown result" instead of the real exception being thrown
- [THRIFT-4554](https://issues.apache.org/jira/browse/THRIFT-4554) - uncompileable code with member names that are also types under specific conditions
- [THRIFT-4422](https://issues.apache.org/jira/browse/THRIFT-4422) - Add Async implementation via IFuture
- [THRIFT-4485](https://issues.apache.org/jira/browse/THRIFT-4485) - Possible invalid ptr AV with overlapped read/write on pipes
- [THRIFT-4549](https://issues.apache.org/jira/browse/THRIFT-4549) - Thrift exceptions should derive from TException
- [THRIFT-4540](https://issues.apache.org/jira/browse/THRIFT-4540) - buffered transport broken when trying to re-open a formerly closed transport
- [THRIFT-4473](https://issues.apache.org/jira/browse/THRIFT-4473) - Move Thrift.Console.pas out of the Library
- [THRIFT-4490](https://issues.apache.org/jira/browse/THRIFT-4490) - Allow a default service as fallback for multiplex processors connected by old clients
- [THRIFT-4454](https://issues.apache.org/jira/browse/THRIFT-4454) - Large writes/reads may cause range check errors in debug mode
- [THRIFT-4461](https://issues.apache.org/jira/browse/THRIFT-4461) - Compiler directive should match Delphi XE4
- [THRIFT-4462](https://issues.apache.org/jira/browse/THRIFT-4462) - First line in Console duplicated
- [THRIFT-4642](https://issues.apache.org/jira/browse/THRIFT-4642) - FPU ctrl word settings may cause an unexpected "denormalized" error
- [THRIFT-4589](https://issues.apache.org/jira/browse/THRIFT-4589) - HTTP client timeouts are a) incomplete and b) not used at all
- [THRIFT-4590](https://issues.apache.org/jira/browse/THRIFT-4590) - running the test client using HTTP transport leads to "CoInitialize not called"

### erlang
- [THRIFT-4497](https://issues.apache.org/jira/browse/THRIFT-4497) - Erlang records should use map() for map type
- [THRIFT-4495](https://issues.apache.org/jira/browse/THRIFT-4495) - Erlang records should allow 'undefined' for non-required fields
- [THRIFT-4580](https://issues.apache.org/jira/browse/THRIFT-4580) - Fix erlang tutorial unpack on Windows
- [THRIFT-4582](https://issues.apache.org/jira/browse/THRIFT-4582) - Ubuntu Xenial erlang 18.3 "make check" fails

### golang
- [THRIFT-4448](https://issues.apache.org/jira/browse/THRIFT-4448) - Support for golang 1.6 and earlier has been dropped.
- [THRIFT-4253](https://issues.apache.org/jira/browse/THRIFT-4253) - Go generator assigns strings to field in const instead of pointers.
- [THRIFT-4573](https://issues.apache.org/jira/browse/THRIFT-4573) - Unions Field Count Does Not Consider Binary
- [THRIFT-4447](https://issues.apache.org/jira/browse/THRIFT-4447) - Golang: Panic on p.c.Call when using deprecated initializers
- [THRIFT-4650](https://issues.apache.org/jira/browse/THRIFT-4650) - Required field incorrectly marked as set when fieldType does not match
- [THRIFT-4486](https://issues.apache.org/jira/browse/THRIFT-4486) - Golang: -remote.go client cleanup
- [THRIFT-4537](https://issues.apache.org/jira/browse/THRIFT-4537) - TSimpleServer can exit Accept loop with lock still acquired
- [THRIFT-4516](https://issues.apache.org/jira/browse/THRIFT-4516) - Add support for go 1.10
- [THRIFT-4421](https://issues.apache.org/jira/browse/THRIFT-4421) - golang tests rely on gomock, which has change behaviour, causing tests to fail
- [THRIFT-4626](https://issues.apache.org/jira/browse/THRIFT-4626) - Communication crash when using binary/compact protocol and zlib transport
- [THRIFT-4659](https://issues.apache.org/jira/browse/THRIFT-4659) - golang race detected when closing listener socket

### haskell
- [THRIFT-4634](https://issues.apache.org/jira/browse/THRIFT-4634) - Haskell builds with older cabal cannot reconcile complex version requirements

### java
- [THRIFT-4259](https://issues.apache.org/jira/browse/THRIFT-4259) - Thrift does not compile due to Ant Maven task errors
- [THRIFT-1418](https://issues.apache.org/jira/browse/THRIFT-1418) - Compiling Thrift from source: Class org.apache.tools.ant.taskdefs.ConditionTask doesn't support the nested "typefound" element
- [THRIFT-4530](https://issues.apache.org/jira/browse/THRIFT-4530) - proposal: add nullability annotations to generated Java code
- [THRIFT-4614](https://issues.apache.org/jira/browse/THRIFT-4614) - Generate missing @Nullable annotations for Java iterator getters
- [THRIFT-4555](https://issues.apache.org/jira/browse/THRIFT-4555) - Getter of binary field in Java creates unnecessary copy
- [THRIFT-3983](https://issues.apache.org/jira/browse/THRIFT-3983) - libthrift is deployed on central with pom packaging instead of jar
- [THRIFT-4294](https://issues.apache.org/jira/browse/THRIFT-4294) - Java Configure Fails for Ant >= 1.10
- [THRIFT-4178](https://issues.apache.org/jira/browse/THRIFT-4178) - Java libraries missing from package when using cmake
- [THRIFT-4120](https://issues.apache.org/jira/browse/THRIFT-4120) - pom files are not generated or provided in the build
- [THRIFT-1507](https://issues.apache.org/jira/browse/THRIFT-1507) - Maven can't download resource from central when behind a proxy and won't use local repository
- [THRIFT-4556](https://issues.apache.org/jira/browse/THRIFT-4556) - Optional rethrow of unhandled exceptions in java processor
- [THRIFT-4337](https://issues.apache.org/jira/browse/THRIFT-4337) - Able to set keyStore and trustStore as InputStream in the TSSLTransportFactory.TSSLTransportParameters
- [THRIFT-4566](https://issues.apache.org/jira/browse/THRIFT-4566) - Pass message of unhandled exception to optional rethrow.
- [THRIFT-4506](https://issues.apache.org/jira/browse/THRIFT-4506) - Remove assertion in Java SASL code that would be ignored in release builds
- [THRIFT-4470](https://issues.apache.org/jira/browse/THRIFT-4470) - Include popular IDE file templates to gitignore
- [THRIFT-4429](https://issues.apache.org/jira/browse/THRIFT-4429) - Make TThreadPoolServer.executorService_ available in inherited classes and refactor methods to be able customization
- [THRIFT-3769](https://issues.apache.org/jira/browse/THRIFT-3769) - Fix logic of THRIFT-2268
- [THRIFT-4494](https://issues.apache.org/jira/browse/THRIFT-4494) - Increase Java Socket Buffer Size
- [THRIFT-4499](https://issues.apache.org/jira/browse/THRIFT-4499) - Remove Magic Number In TFIleTransport

### js
- [THRIFT-4406](https://issues.apache.org/jira/browse/THRIFT-4406) - JavaScript: Use modern Promise implementations
- [THRIFT-4625](https://issues.apache.org/jira/browse/THRIFT-4625) - let / const variable decorators for es6 compiler
- [THRIFT-4653](https://issues.apache.org/jira/browse/THRIFT-4653) - ES6 Classes
- [THRIFT-4592](https://issues.apache.org/jira/browse/THRIFT-4592) - JS: readI32 performance on large arrays is very poor in Chrome
- [THRIFT-4509](https://issues.apache.org/jira/browse/THRIFT-4509) - js and nodejs libraries need to be refreshed with current libraries
- [THRIFT-4403](https://issues.apache.org/jira/browse/THRIFT-4403) - thrift.js: Incorrect usage of 'this' in TWebSocketTransport.__onOpen
- [THRIFT-4436](https://issues.apache.org/jira/browse/THRIFT-4436) - Deserialization of nested list discards content
- [THRIFT-4437](https://issues.apache.org/jira/browse/THRIFT-4437) - JS WebSocket client callbacks invoked twice on parallel requests
- [THRIFT-4679](https://issues.apache.org/jira/browse/THRIFT-4679) - Duplicate declaration of InputBufferUnderrunError in lib/nodejs/lib/thrift/json_protocol.js
- [THRIFT-4551](https://issues.apache.org/jira/browse/THRIFT-4551) - Add prettier for consistent JS code formatting

### lua
- [THRIFT-4591](https://issues.apache.org/jira/browse/THRIFT-4591) - lua client uses two write() calls per framed message send
- [THRIFT-3863](https://issues.apache.org/jira/browse/THRIFT-3863) - Can't "make install" Lua Library

### netcore
- [THRIFT-4524](https://issues.apache.org/jira/browse/THRIFT-4524) - .NET Core Server doesn't close properly when cancelled
- [THRIFT-4434](https://issues.apache.org/jira/browse/THRIFT-4434) - Update .NET Core components, add tests for .Net Core library and .Net Core compiler, fix bugs and build process
- [THRIFT-4446](https://issues.apache.org/jira/browse/THRIFT-4446) - JSONProtocol Base64 Encoding Trims Padding

### node.js
- [THRIFT-4225](https://issues.apache.org/jira/browse/THRIFT-4225) - Error handling malformed arguments leaks memory, corrupts transport buffers causing next RPC to fail
- [THRIFT-3950](https://issues.apache.org/jira/browse/THRIFT-3950) - Memory leak while calling oneway method
- [THRIFT-3143](https://issues.apache.org/jira/browse/THRIFT-3143) - add typescript directory support
- [THRIFT-4564](https://issues.apache.org/jira/browse/THRIFT-4564) - TBufferedTransport can leave corrupt data in the buffer
- [THRIFT-4647](https://issues.apache.org/jira/browse/THRIFT-4647) - Node.js Fileserver webroot path
- [THRIFT-4489](https://issues.apache.org/jira/browse/THRIFT-4489) - Unix domain socket support for NodeJS client
- [THRIFT-4443](https://issues.apache.org/jira/browse/THRIFT-4443) - node.js json_protocol throws error in skip function
- [THRIFT-4604](https://issues.apache.org/jira/browse/THRIFT-4604) - NodeJS: Expose Int64 from browser.js for consumption by browser
- [THRIFT-4480](https://issues.apache.org/jira/browse/THRIFT-4480) - NodeJS warning on binary_protocol writeMessageEnd when seqid = 0

### perl
- [THRIFT-4382](https://issues.apache.org/jira/browse/THRIFT-4382) - Replace the use of Perl Indirect Object Syntax calls to new()
- [THRIFT-4471](https://issues.apache.org/jira/browse/THRIFT-4471) - Thrift CPAN release is missing Makefile.PL and the clients are unable to build the module
- [THRIFT-4416](https://issues.apache.org/jira/browse/THRIFT-4416) - Perl CPAN Packaging Improvements

### php
- [THRIFT-4474](https://issues.apache.org/jira/browse/THRIFT-4474) - PHP generator use PSR-4 default
- [THRIFT-4463](https://issues.apache.org/jira/browse/THRIFT-4463) - PHP generated code match PSR-2
- [THRIFT-4373](https://issues.apache.org/jira/browse/THRIFT-4373) - Extending Thrift class results in "Attempt serialize from non-Thrift object"
- [THRIFT-4354](https://issues.apache.org/jira/browse/THRIFT-4354) - TSocket block on read
- [THRIFT-4423](https://issues.apache.org/jira/browse/THRIFT-4423) - migrate php library to psr-4
- [THRIFT-4656](https://issues.apache.org/jira/browse/THRIFT-4656) - infinite loop in latest PHP library
- [THRIFT-4477](https://issues.apache.org/jira/browse/THRIFT-4477) - TBufferedTransport must have underlying transport
- [THRIFT-4475](https://issues.apache.org/jira/browse/THRIFT-4475) - lib/php/test should be checked for PSR-2
- [THRIFT-4498](https://issues.apache.org/jira/browse/THRIFT-4498) - add phpcs back
- [THRIFT-4460](https://issues.apache.org/jira/browse/THRIFT-4460) - php library use PSR-2
- [THRIFT-4641](https://issues.apache.org/jira/browse/THRIFT-4641) - TCurlClient doesn't check for HTTP status code
- [THRIFT-4645](https://issues.apache.org/jira/browse/THRIFT-4645) - TCurlClient: show actual error message when throwing TTransportException
- [THRIFT-4674](https://issues.apache.org/jira/browse/THRIFT-4674) - Add stream context support into PHP/THttpClient
- [THRIFT-4459](https://issues.apache.org/jira/browse/THRIFT-4459) - reduce php library directory depth

### python
- [THRIFT-4670](https://issues.apache.org/jira/browse/THRIFT-4670) - Twisted, slots, and void method fails with "object has no attribute 'success'"
- [THRIFT-4464](https://issues.apache.org/jira/browse/THRIFT-4464) - Potentially server-crashing typo in Python TNonblockingServer
- [THRIFT-4548](https://issues.apache.org/jira/browse/THRIFT-4548) - Supporting TBinaryProtocolAccelerated protocol when using TMultiplexedProcessor in Python
- [THRIFT-4577](https://issues.apache.org/jira/browse/THRIFT-4577) - Outdated cipher string in python unit test
- [THRIFT-4505](https://issues.apache.org/jira/browse/THRIFT-4505) - python build on Vagrant Windows boxes fails
- [THRIFT-4621](https://issues.apache.org/jira/browse/THRIFT-4621) - THeader for Python
- [THRIFT-4668](https://issues.apache.org/jira/browse/THRIFT-4668) - make socket backlog configurable for python
- [THRIFT-4561](https://issues.apache.org/jira/browse/THRIFT-4561) - Python: cleanup socket timeout settings

### ruby
- [THRIFT-4289](https://issues.apache.org/jira/browse/THRIFT-4289) - Thrift RSpec test suite fails with Ruby 2.4.x due to Fixnum deprecation
- [THRIFT-4342](https://issues.apache.org/jira/browse/THRIFT-4342) - Support ruby rspec 3
- [THRIFT-4525](https://issues.apache.org/jira/browse/THRIFT-4525) - Add ssl socket option to ruby cross tests
- [THRIFT-4450](https://issues.apache.org/jira/browse/THRIFT-4450) - Add seek support to TCompactInputProtocol in Rust
- [THRIFT-4631](https://issues.apache.org/jira/browse/THRIFT-4631) - Codegen Creates Invalid Ruby for Recursive Structs
- [THRIFT-4472](https://issues.apache.org/jira/browse/THRIFT-4472) - Fix the genspec for ruby so it does not complain about an invalid license

### rust
- [THRIFT-4662](https://issues.apache.org/jira/browse/THRIFT-4662) - Rust const string calls function at compile time
- [THRIFT-4661](https://issues.apache.org/jira/browse/THRIFT-4661) - Rust enum name wrong case in generated structs
- [THRIFT-4617](https://issues.apache.org/jira/browse/THRIFT-4617) - Avoid generating conflicting struct names in Rust code
- [THRIFT-4529](https://issues.apache.org/jira/browse/THRIFT-4529) - Rust generation should include #![allow(non_snake_case)] or force conform to Rust style guidelines
- [THRIFT-4390](https://issues.apache.org/jira/browse/THRIFT-4390) - Rust binary protocol and buffered transport cannot handle writes above 4096 bytes
- [THRIFT-4419](https://issues.apache.org/jira/browse/THRIFT-4419) - Rust framed transport cannot handle writes above 4096 bytes
- [THRIFT-4658](https://issues.apache.org/jira/browse/THRIFT-4658) - Rust's TBinaryInputProtocol fails when strict is false
- [THRIFT-4187](https://issues.apache.org/jira/browse/THRIFT-4187) - Dart -> Rust Framed cross tests fail
- [THRIFT-4664](https://issues.apache.org/jira/browse/THRIFT-4664) - Rust cannot create ReadHalf/WriteHalf to implement custom tranports
- [THRIFT-4665](https://issues.apache.org/jira/browse/THRIFT-4665) - Keep Rust library up-to-date on crates.io

### swift (new language support in 0.12.0)
- [THRIFT-3773](https://issues.apache.org/jira/browse/THRIFT-3773) - Swift Library

### test suite
- [THRIFT-4515](https://issues.apache.org/jira/browse/THRIFT-4515) - Gracefully shutdown cross-test servers to fully test teardown
- [THRIFT-4085](https://issues.apache.org/jira/browse/THRIFT-4085) - Add .NET Core to the make cross standard test suite
- [THRIFT-4358](https://issues.apache.org/jira/browse/THRIFT-4358) - Add unix domain sockets in ruby to cross test - code exists

### typescript (new language support in 0.12.0)
- [THRIFT-3143](https://issues.apache.org/jira/browse/THRIFT-3143) - add typescript directory support

## 0.11.0

Released 2017-DEC-27

### Sub-task
- [THRIFT-2733](https://issues.apache.org/jira/browse/THRIFT-2733) - Erlang coding standards
- [THRIFT-2740](https://issues.apache.org/jira/browse/THRIFT-2740) - Perl coding standards
- [THRIFT-3610](https://issues.apache.org/jira/browse/THRIFT-3610) - Streamline exception handling in Python server handler
- [THRIFT-3686](https://issues.apache.org/jira/browse/THRIFT-3686) - Java processor should report internal error on uncaught exception
- [THRIFT-4049](https://issues.apache.org/jira/browse/THRIFT-4049) - Skip() should throw TProtocolException.INVALID_DATA on unknown data types
- [THRIFT-4053](https://issues.apache.org/jira/browse/THRIFT-4053) - Skip() should throw TProtocolException.INVALID_DATA on unknown data types
- [THRIFT-4136](https://issues.apache.org/jira/browse/THRIFT-4136) - Align is_binary() method with is_string() to simplify those checks
- [THRIFT-4137](https://issues.apache.org/jira/browse/THRIFT-4137) - Fix remaining undefined behavior invalid vptr casts in Thrift Compiler
- [THRIFT-4138](https://issues.apache.org/jira/browse/THRIFT-4138) - Fix remaining undefined behavior invalid vptr casts in C++ library
- [THRIFT-4296](https://issues.apache.org/jira/browse/THRIFT-4296) - Fix Ubuntu Xenial build environment for the python language
- [THRIFT-4298](https://issues.apache.org/jira/browse/THRIFT-4298) - Fix Ubuntu Xenial build environment for the go 1.6 language
- [THRIFT-4299](https://issues.apache.org/jira/browse/THRIFT-4299) - Fix Ubuntu Xenial build environment for the D language
- [THRIFT-4300](https://issues.apache.org/jira/browse/THRIFT-4300) - Fix make cross in Ubuntu Xenial docker environment, once all language support issues are fixed
- [THRIFT-4302](https://issues.apache.org/jira/browse/THRIFT-4302) - Fix Ubuntu Xenial make cross testing for lua and php7
- [THRIFT-4398](https://issues.apache.org/jira/browse/THRIFT-4398) - Update EXTRA_DIST for "make dist"

### Bug
- [THRIFT-381](https://issues.apache.org/jira/browse/THRIFT-381) - Fail fast if configure detects C++ problems
- [THRIFT-1677](https://issues.apache.org/jira/browse/THRIFT-1677) - MinGW support broken
- [THRIFT-1805](https://issues.apache.org/jira/browse/THRIFT-1805) - Thrift should not swallow ALL exceptions
- [THRIFT-2026](https://issues.apache.org/jira/browse/THRIFT-2026) - Fix TCompactProtocol 64 bit builds
- [THRIFT-2642](https://issues.apache.org/jira/browse/THRIFT-2642) - Recursive structs don't work in python
- [THRIFT-2889](https://issues.apache.org/jira/browse/THRIFT-2889) - stable release 0.9.2, erlang tutorial broken
- [THRIFT-2913](https://issues.apache.org/jira/browse/THRIFT-2913) - Ruby Server Thrift::ThreadPoolServer should serve inside a thread
- [THRIFT-2998](https://issues.apache.org/jira/browse/THRIFT-2998) - Node.js: Missing header from http request
- [THRIFT-3000](https://issues.apache.org/jira/browse/THRIFT-3000) - .NET implementation has trouble with mixed IP modes
- [THRIFT-3281](https://issues.apache.org/jira/browse/THRIFT-3281) - Travis CI build passed but the log says BUILD FAILED
- [THRIFT-3358](https://issues.apache.org/jira/browse/THRIFT-3358) - Makefile:1362: *** missing separator. Stop.
- [THRIFT-3600](https://issues.apache.org/jira/browse/THRIFT-3600) - Make TTwisted server send exception on unexpected handler error
- [THRIFT-3602](https://issues.apache.org/jira/browse/THRIFT-3602) - Make Tornado server send exception on unexpected handler error
- [THRIFT-3657](https://issues.apache.org/jira/browse/THRIFT-3657) - D TFileWriterTransport close should use non-priority send
- [THRIFT-3700](https://issues.apache.org/jira/browse/THRIFT-3700) - Go Map has wrong default value when optional
- [THRIFT-3703](https://issues.apache.org/jira/browse/THRIFT-3703) - Unions Field Count Does Not Consider Map/Set/List Fields
- [THRIFT-3730](https://issues.apache.org/jira/browse/THRIFT-3730) - server log error twice
- [THRIFT-3778](https://issues.apache.org/jira/browse/THRIFT-3778) - go client can not pass method parameter to server of other language if no field_id is given
- [THRIFT-3784](https://issues.apache.org/jira/browse/THRIFT-3784) - thrift-maven-plugin generates invalid include directories for IDL in dependency JARs
- [THRIFT-3801](https://issues.apache.org/jira/browse/THRIFT-3801) - Node Thrift client throws exception with multiplexer and responses that are bigger than a single buffer
- [THRIFT-3821](https://issues.apache.org/jira/browse/THRIFT-3821) - TMemoryBuffer buffer may overflow when resizing
- [THRIFT-3832](https://issues.apache.org/jira/browse/THRIFT-3832) - Thrift version 0.9.3 example on Windows, Visual Studio, linking errors during compiling
- [THRIFT-3847](https://issues.apache.org/jira/browse/THRIFT-3847) - thrift/config.h includes a #define for VERSION which will likely conflict with existing user environment or code
- [THRIFT-3873](https://issues.apache.org/jira/browse/THRIFT-3873) - Fix various build warnings when using Visual Studio
- [THRIFT-3891](https://issues.apache.org/jira/browse/THRIFT-3891) - TNonblockingServer configured with more than one IO threads does not always return from serve() upon stop()
- [THRIFT-3892](https://issues.apache.org/jira/browse/THRIFT-3892) - Thrift uses TLS SNI extension provided by OpenSSL library. Older version of OpenSSL(< 0.9.8f) may create problem because they do not support 'SSL_set_tlsext_host_name()'.
- [THRIFT-3895](https://issues.apache.org/jira/browse/THRIFT-3895) - Build fails using Java 1.8 with Ant < 1.9
- [THRIFT-3896](https://issues.apache.org/jira/browse/THRIFT-3896) - map<string,string> data with number string key cannot access that deserialized by php extension
- [THRIFT-3938](https://issues.apache.org/jira/browse/THRIFT-3938) - Python TNonblockingServer does not work with SSL
- [THRIFT-3944](https://issues.apache.org/jira/browse/THRIFT-3944) - TSSLSocket has dead code in checkHandshake
- [THRIFT-3946](https://issues.apache.org/jira/browse/THRIFT-3946) - Java 1.5 compatibility broken for binary fields (java5 option)
- [THRIFT-3960](https://issues.apache.org/jira/browse/THRIFT-3960) - Inherited services in Lua generator are not named correctly
- [THRIFT-3962](https://issues.apache.org/jira/browse/THRIFT-3962) - Ant build.xml broken on Windows for Java library
- [THRIFT-3963](https://issues.apache.org/jira/browse/THRIFT-3963) - Thrift.cabal filename does not match module name
- [THRIFT-3967](https://issues.apache.org/jira/browse/THRIFT-3967) - gobject/gparam.h:166:33: warning: enumerator value for ‘G_PARAM_DEPRECATED’ is not an integer constant expression
- [THRIFT-3968](https://issues.apache.org/jira/browse/THRIFT-3968) - Deserializing empty string/binary fields
- [THRIFT-3974](https://issues.apache.org/jira/browse/THRIFT-3974) - Using clang-3.8 and ThreadSanitizer on the concurrency_test claims bad PThread behavior
- [THRIFT-3984](https://issues.apache.org/jira/browse/THRIFT-3984) - PHP7 extension causes segfault
- [THRIFT-4008](https://issues.apache.org/jira/browse/THRIFT-4008) - broken ci due to upstream dependency versioning break
- [THRIFT-4009](https://issues.apache.org/jira/browse/THRIFT-4009) - Use @implementer instead of implements in TTwisted.py
- [THRIFT-4010](https://issues.apache.org/jira/browse/THRIFT-4010) - Q.fcall messing up with *this* pointer inside called function
- [THRIFT-4011](https://issues.apache.org/jira/browse/THRIFT-4011) - Sets of Thrift structs generate Go code that can't be serialized to JSON
- [THRIFT-4012](https://issues.apache.org/jira/browse/THRIFT-4012) - Python Twisted implementation uses implements, not compatible with Py3
- [THRIFT-4014](https://issues.apache.org/jira/browse/THRIFT-4014) - align C# meta data in AssemblyInfo.cs
- [THRIFT-4015](https://issues.apache.org/jira/browse/THRIFT-4015) - Fix wrongly spelled "Thirft"s
- [THRIFT-4016](https://issues.apache.org/jira/browse/THRIFT-4016) - testInsanity() impl does not conform to test spec in ThriftTest.thrift
- [THRIFT-4023](https://issues.apache.org/jira/browse/THRIFT-4023) - Skip unexpected field types on read/write
- [THRIFT-4024](https://issues.apache.org/jira/browse/THRIFT-4024) - Skip() should throw on unknown data types
- [THRIFT-4026](https://issues.apache.org/jira/browse/THRIFT-4026) - TSSLSocket doesn't work with Python < 2.7.9
- [THRIFT-4029](https://issues.apache.org/jira/browse/THRIFT-4029) - Accelerated protocols do not build from thrift-py 0.10.0 on PyPI
- [THRIFT-4031](https://issues.apache.org/jira/browse/THRIFT-4031) - Go plugin generates invalid code for lists of typedef'ed built-in types
- [THRIFT-4033](https://issues.apache.org/jira/browse/THRIFT-4033) - Default build WITH_PLUGIN=ON for all builds results in packaging errors
- [THRIFT-4034](https://issues.apache.org/jira/browse/THRIFT-4034) - CMake doesn't work to build compiler on MacOS
- [THRIFT-4036](https://issues.apache.org/jira/browse/THRIFT-4036) - Add .NET Core environment/build support to the docker image
- [THRIFT-4038](https://issues.apache.org/jira/browse/THRIFT-4038) - socket check: checking an unsigned number against >= 0 never fails
- [THRIFT-4042](https://issues.apache.org/jira/browse/THRIFT-4042) - ExtractionError when using accelerated thrift in a multiprocess test
- [THRIFT-4043](https://issues.apache.org/jira/browse/THRIFT-4043) - thrift perl debian package is placing files in the wrong place
- [THRIFT-4044](https://issues.apache.org/jira/browse/THRIFT-4044) - Build job 17 failing on every pull request; hspec core (haskell) 2.4 issue
- [THRIFT-4046](https://issues.apache.org/jira/browse/THRIFT-4046) - MinGW with gcc 6.2 does not compile on Windows
- [THRIFT-4060](https://issues.apache.org/jira/browse/THRIFT-4060) - Thrift printTo ostream overload mechanism breaks down when types are nested
- [THRIFT-4062](https://issues.apache.org/jira/browse/THRIFT-4062) - Remove debug print from TServiceClient
- [THRIFT-4065](https://issues.apache.org/jira/browse/THRIFT-4065) - Document Perl ForkingServer signal restriction imposed by THRIFT-3848 and remove unnecessary code
- [THRIFT-4068](https://issues.apache.org/jira/browse/THRIFT-4068) - A code comment in Java ServerSocket is wrong around accept()
- [THRIFT-4073](https://issues.apache.org/jira/browse/THRIFT-4073) - enum files are still being generated with unused imports
- [THRIFT-4076](https://issues.apache.org/jira/browse/THRIFT-4076) - Appveyor builds failing because ant 1.9.8 was removed from apache servers
- [THRIFT-4077](https://issues.apache.org/jira/browse/THRIFT-4077) - AI_ADDRCONFIG redefined after recent change to PlatformSocket header
- [THRIFT-4079](https://issues.apache.org/jira/browse/THRIFT-4079) - Generated perl code that returns structures from included thrift files is missing a necessary use clause
- [THRIFT-4087](https://issues.apache.org/jira/browse/THRIFT-4087) - Spurious exception destroying TThreadedServer because of incorrect join() call
- [THRIFT-4102](https://issues.apache.org/jira/browse/THRIFT-4102) - TBufferedTransport performance issue since 0.10.0
- [THRIFT-4106](https://issues.apache.org/jira/browse/THRIFT-4106) - concurrency_test fails randomly
- [THRIFT-4108](https://issues.apache.org/jira/browse/THRIFT-4108) - c_glib thrift ssl has multiple bugs and deprecated functions
- [THRIFT-4109](https://issues.apache.org/jira/browse/THRIFT-4109) - Configure Script uses string comparison for versions
- [THRIFT-4129](https://issues.apache.org/jira/browse/THRIFT-4129) - C++ TNonblockingServer fd leak when failing to dispatch new connections
- [THRIFT-4131](https://issues.apache.org/jira/browse/THRIFT-4131) - Javascript with WebSocket handles oneway methods wrong
- [THRIFT-4134](https://issues.apache.org/jira/browse/THRIFT-4134) - Fix remaining undefined behavior invalid vptr casts
- [THRIFT-4140](https://issues.apache.org/jira/browse/THRIFT-4140) - Use of non-thread-safe function gmtime()
- [THRIFT-4141](https://issues.apache.org/jira/browse/THRIFT-4141) - Installation of haxe in docker files refers to a redirect link and fails
- [THRIFT-4147](https://issues.apache.org/jira/browse/THRIFT-4147) - Rust: protocol should accept transports with non-static lifetime
- [THRIFT-4148](https://issues.apache.org/jira/browse/THRIFT-4148) - [maven-thrift-plugin] compile error while import a thrift in dependency jar file.
- [THRIFT-4149](https://issues.apache.org/jira/browse/THRIFT-4149) - System.out pollutes log files 
- [THRIFT-4154](https://issues.apache.org/jira/browse/THRIFT-4154) - PHP close() of a TSocket needs to close any type of socket
- [THRIFT-4158](https://issues.apache.org/jira/browse/THRIFT-4158) - minor issue in README-MSYS2.md
- [THRIFT-4159](https://issues.apache.org/jira/browse/THRIFT-4159) - Building tests fails on MSYS2 (MinGW64) due to a (small?) linker error
- [THRIFT-4160](https://issues.apache.org/jira/browse/THRIFT-4160) - TNonblocking server fix use of closed/freed connections
- [THRIFT-4161](https://issues.apache.org/jira/browse/THRIFT-4161) - TNonBlocking server using uninitialized event in error paths
- [THRIFT-4162](https://issues.apache.org/jira/browse/THRIFT-4162) - TNonBlocking handling of TSockets in error state is incorrect after fd is closed
- [THRIFT-4164](https://issues.apache.org/jira/browse/THRIFT-4164) - Core in TSSLSocket cleanupOpenSSL when destroying a mutex used by openssl
- [THRIFT-4165](https://issues.apache.org/jira/browse/THRIFT-4165) - C++ build has many warnings under c++03 due to recent changes, cmake needs better platform-independent language level control
- [THRIFT-4166](https://issues.apache.org/jira/browse/THRIFT-4166) - Recent fix to remove boost::lexical_cast usage broke VS2010
- [THRIFT-4167](https://issues.apache.org/jira/browse/THRIFT-4167) - Missing compile flag
- [THRIFT-4170](https://issues.apache.org/jira/browse/THRIFT-4170) - Support lua 5.1 or earlier properly for object length determination
- [THRIFT-4172](https://issues.apache.org/jira/browse/THRIFT-4172) - node.js tutorial client does not import assert, connection issues are not handled properly
- [THRIFT-4177](https://issues.apache.org/jira/browse/THRIFT-4177) - Java compiler produces deep copy constructor that could make shallow copy instead
- [THRIFT-4184](https://issues.apache.org/jira/browse/THRIFT-4184) - Building on Appveyor: invalid escape sequence \L
- [THRIFT-4185](https://issues.apache.org/jira/browse/THRIFT-4185) - fb303 counter encoding fix
- [THRIFT-4189](https://issues.apache.org/jira/browse/THRIFT-4189) - Framed/buffered transport Dispose() does not dispose the nested transport
- [THRIFT-4193](https://issues.apache.org/jira/browse/THRIFT-4193) - Lower the default maxReadBufferBytes for non-blocking servers
- [THRIFT-4195](https://issues.apache.org/jira/browse/THRIFT-4195) - Compilation to GO produces broken code
- [THRIFT-4196](https://issues.apache.org/jira/browse/THRIFT-4196) - Cannot generate recursive Rust types
- [THRIFT-4204](https://issues.apache.org/jira/browse/THRIFT-4204) - typo in compact spec
- [THRIFT-4206](https://issues.apache.org/jira/browse/THRIFT-4206) - Strings in container fields are not decoded properly with py:dynamic and py:utf8strings
- [THRIFT-4208](https://issues.apache.org/jira/browse/THRIFT-4208) - C# NamedPipesServer not really working in some scenarios
- [THRIFT-4211](https://issues.apache.org/jira/browse/THRIFT-4211) - Fix GError glib management under Thrift
- [THRIFT-4212](https://issues.apache.org/jira/browse/THRIFT-4212) - c_glib flush tries to close SSL even if socket is invalid
- [THRIFT-4213](https://issues.apache.org/jira/browse/THRIFT-4213) - Travis build fails at curl -sSL https://www.npmjs.com/install.sh | sh
- [THRIFT-4215](https://issues.apache.org/jira/browse/THRIFT-4215) - Golang TTransportFactory Pattern Squelches Errors
- [THRIFT-4216](https://issues.apache.org/jira/browse/THRIFT-4216) - Golang Http Clients Do Not Respect User Options
- [THRIFT-4218](https://issues.apache.org/jira/browse/THRIFT-4218) - Set TCP_NODELAY for PHP client socket
- [THRIFT-4219](https://issues.apache.org/jira/browse/THRIFT-4219) - Golang HTTP clients created with Nil buffer
- [THRIFT-4231](https://issues.apache.org/jira/browse/THRIFT-4231) - TJSONProtocol throws unexpected non-Thrift-exception on null strings
- [THRIFT-4232](https://issues.apache.org/jira/browse/THRIFT-4232) - ./configure does bad ant version check
- [THRIFT-4234](https://issues.apache.org/jira/browse/THRIFT-4234) - Travis build fails cross language tests with "Unsupported security protocol type"
- [THRIFT-4237](https://issues.apache.org/jira/browse/THRIFT-4237) - Go TServerSocket Race Conditions
- [THRIFT-4240](https://issues.apache.org/jira/browse/THRIFT-4240) - Go TSimpleServer does not close properly
- [THRIFT-4243](https://issues.apache.org/jira/browse/THRIFT-4243) - Go TSimpleServer race on wait in Stop() method
- [THRIFT-4245](https://issues.apache.org/jira/browse/THRIFT-4245) - Golang TFramedTransport's writeBuffer increases if writes to transport failed
- [THRIFT-4246](https://issues.apache.org/jira/browse/THRIFT-4246) - Sequence number mismatch on multiplexed clients
- [THRIFT-4247](https://issues.apache.org/jira/browse/THRIFT-4247) - Compile fails with openssl 1.1
- [THRIFT-4248](https://issues.apache.org/jira/browse/THRIFT-4248) - Compile fails - strncpy, memcmp, memset not declared in src/thrift/transport/TSSLSocket.cpp
- [THRIFT-4251](https://issues.apache.org/jira/browse/THRIFT-4251) - Java Epoll Selector Bug
- [THRIFT-4257](https://issues.apache.org/jira/browse/THRIFT-4257) - Typescript async callbacks do not provide the correct types
- [THRIFT-4258](https://issues.apache.org/jira/browse/THRIFT-4258) - Boost/std thread wrapping faultiness
- [THRIFT-4260](https://issues.apache.org/jira/browse/THRIFT-4260) - Go context generation issue. Context is parameter in Interface not in implementation
- [THRIFT-4261](https://issues.apache.org/jira/browse/THRIFT-4261) - Go context generation issue: breaking change in generated code regarding thrift.TProcessorFunction interface
- [THRIFT-4262](https://issues.apache.org/jira/browse/THRIFT-4262) - Invalid binding to InterlockedCompareExchange64() with 64-bit targets
- [THRIFT-4263](https://issues.apache.org/jira/browse/THRIFT-4263) - Fix use after free bug for thrown exceptions
- [THRIFT-4266](https://issues.apache.org/jira/browse/THRIFT-4266) - Erlang library throws during skipping fields of composite type (maps, lists, structs, sets)
- [THRIFT-4268](https://issues.apache.org/jira/browse/THRIFT-4268) - Erlang library emits debugging output in transport layer
- [THRIFT-4273](https://issues.apache.org/jira/browse/THRIFT-4273) - erlang:now/0: Deprecated BIF. 
- [THRIFT-4274](https://issues.apache.org/jira/browse/THRIFT-4274) - Python feature tests for SSL/TLS failing
- [THRIFT-4279](https://issues.apache.org/jira/browse/THRIFT-4279) - Wrong path in include directive in generated Thrift sources
- [THRIFT-4283](https://issues.apache.org/jira/browse/THRIFT-4283) - TNamedPipeServer race condition in interrupt
- [THRIFT-4284](https://issues.apache.org/jira/browse/THRIFT-4284) - File contains a NBSP: lib/nodejs/lib/thrift/web_server.js
- [THRIFT-4290](https://issues.apache.org/jira/browse/THRIFT-4290) - C# nullable option generates invalid code for non-required enum field with default value
- [THRIFT-4292](https://issues.apache.org/jira/browse/THRIFT-4292) - TimerManager::remove() is not implemented
- [THRIFT-4307](https://issues.apache.org/jira/browse/THRIFT-4307) - Make ssl-open timeout effective in golang client
- [THRIFT-4312](https://issues.apache.org/jira/browse/THRIFT-4312) - Erlang client cannot connect to Python server: exception error: econnrefused
- [THRIFT-4313](https://issues.apache.org/jira/browse/THRIFT-4313) - Program code of the Erlang tutorial files contain syntax errors
- [THRIFT-4316](https://issues.apache.org/jira/browse/THRIFT-4316) - TByteBuffer.java will read too much data if a previous read returns fewer bytes than requested
- [THRIFT-4319](https://issues.apache.org/jira/browse/THRIFT-4319) - command line switch for "evhttp" incorrectly resolved to anon pipes
- [THRIFT-4323](https://issues.apache.org/jira/browse/THRIFT-4323) - range check errors or NPE in edge cases
- [THRIFT-4324](https://issues.apache.org/jira/browse/THRIFT-4324) - field names can conflict with local vars in generated code
- [THRIFT-4328](https://issues.apache.org/jira/browse/THRIFT-4328) - Travis CI builds are timing out (job 1) and haxe builds are failing since 9/11
- [THRIFT-4329](https://issues.apache.org/jira/browse/THRIFT-4329) - c_glib Doesn't have a multiplexed processor
- [THRIFT-4331](https://issues.apache.org/jira/browse/THRIFT-4331) - C++: TSSLSockets bug in handling huge messages, bug in handling polling
- [THRIFT-4332](https://issues.apache.org/jira/browse/THRIFT-4332) - Binary protocol has memory leaks
- [THRIFT-4334](https://issues.apache.org/jira/browse/THRIFT-4334) - Perl indentation incorrect when defaulting field attribute to a struct
- [THRIFT-4339](https://issues.apache.org/jira/browse/THRIFT-4339) - Thrift Framed Transport in Erlang crashes server when client disconnects
- [THRIFT-4340](https://issues.apache.org/jira/browse/THRIFT-4340) - Erlang fix a crash on client close
- [THRIFT-4355](https://issues.apache.org/jira/browse/THRIFT-4355) - Javascript indentation incorrect when defaulting field attribute to a struct
- [THRIFT-4356](https://issues.apache.org/jira/browse/THRIFT-4356) - thrift_protocol call Transport cause Segmentation fault
- [THRIFT-4359](https://issues.apache.org/jira/browse/THRIFT-4359) - Haxe compiler looks like it is producing incorrect code for map or set key that is binary type
- [THRIFT-4362](https://issues.apache.org/jira/browse/THRIFT-4362) - Missing size-check can lead to huge memory allocation
- [THRIFT-4364](https://issues.apache.org/jira/browse/THRIFT-4364) - Website contributing guide erroneously recommends submitting patches in JIRA
- [THRIFT-4365](https://issues.apache.org/jira/browse/THRIFT-4365) - Perl generated code uses indirect object syntax, which occasionally causes compilation errors.
- [THRIFT-4367](https://issues.apache.org/jira/browse/THRIFT-4367) - python TProcessor.process is missing "self"
- [THRIFT-4370](https://issues.apache.org/jira/browse/THRIFT-4370) - Ubuntu Artful cppcheck and flake8 are more stringent and causing SCA build job failures
- [THRIFT-4372](https://issues.apache.org/jira/browse/THRIFT-4372) - Pipe write operations across a network are limited to 65,535 bytes per write. 
- [THRIFT-4374](https://issues.apache.org/jira/browse/THRIFT-4374) - cannot load thrift_protocol due to undefined symbol: _ZTVN10__cxxabiv120__si_class_type_infoE
- [THRIFT-4375](https://issues.apache.org/jira/browse/THRIFT-4375) - TMemory throw bad_alloc due to counter overflow
- [THRIFT-4376](https://issues.apache.org/jira/browse/THRIFT-4376) - Coverity high impact issue resolution
- [THRIFT-4377](https://issues.apache.org/jira/browse/THRIFT-4377) - haxe. socket handles leak in TSimpleServer
- [THRIFT-4381](https://issues.apache.org/jira/browse/THRIFT-4381) - Wrong isset bitfield value after transmission
- [THRIFT-4385](https://issues.apache.org/jira/browse/THRIFT-4385) - Go remote client -u flag is broken
- [THRIFT-4392](https://issues.apache.org/jira/browse/THRIFT-4392) - compiler/..../plugin.thrift structs mis-ordered blows up ocaml generator
- [THRIFT-4395](https://issues.apache.org/jira/browse/THRIFT-4395) - Unable to build in the ubuntu-xenial docker image: clap 2.28 requires Rust 1.20
- [THRIFT-4396](https://issues.apache.org/jira/browse/THRIFT-4396) - inconsistent (or plain wrong) version numbers in master/trunk 

### Documentation
- [THRIFT-4157](https://issues.apache.org/jira/browse/THRIFT-4157) - outdated readme about Haxe installation on Linux

### Improvement
- [THRIFT-105](https://issues.apache.org/jira/browse/THRIFT-105) - make a thrift_spec for a structures with negative tags
- [THRIFT-281](https://issues.apache.org/jira/browse/THRIFT-281) - Cocoa library code needs comments, badly
- [THRIFT-775](https://issues.apache.org/jira/browse/THRIFT-775) - performance improvements for Perl
- [THRIFT-2221](https://issues.apache.org/jira/browse/THRIFT-2221) - Generate c++ code with std::shared_ptr instead of boost::shared_ptr.
- [THRIFT-2364](https://issues.apache.org/jira/browse/THRIFT-2364) - OCaml: Use Oasis exclusively for build process
- [THRIFT-2504](https://issues.apache.org/jira/browse/THRIFT-2504) - TMultiplexedProcessor should allow registering default processor called if no service name is present
- [THRIFT-3207](https://issues.apache.org/jira/browse/THRIFT-3207) - Enable build with OpenSSL 1.1.0 series
- [THRIFT-3272](https://issues.apache.org/jira/browse/THRIFT-3272) - Perl SSL Authentication Support
- [THRIFT-3357](https://issues.apache.org/jira/browse/THRIFT-3357) - Generate EnumSet/EnumMap where elements/keys are enums
- [THRIFT-3369](https://issues.apache.org/jira/browse/THRIFT-3369) - Implement SSL/TLS support on C with c_glib
- [THRIFT-3467](https://issues.apache.org/jira/browse/THRIFT-3467) - Go Maps for Thrift Sets Should Have Values of Type struct{} 
- [THRIFT-3580](https://issues.apache.org/jira/browse/THRIFT-3580) - THeader for Haskell
- [THRIFT-3627](https://issues.apache.org/jira/browse/THRIFT-3627) - Missing basic code style consistency of JavaScript.
- [THRIFT-3706](https://issues.apache.org/jira/browse/THRIFT-3706) - There's no support for Multiplexed protocol on c_glib library
- [THRIFT-3766](https://issues.apache.org/jira/browse/THRIFT-3766) - Add getUnderlyingTransport() to TZlibTransport
- [THRIFT-3776](https://issues.apache.org/jira/browse/THRIFT-3776) - Go code from multiple thrift files with the same namespace
- [THRIFT-3823](https://issues.apache.org/jira/browse/THRIFT-3823) - Escape documentation while generating non escaped documetation
- [THRIFT-3854](https://issues.apache.org/jira/browse/THRIFT-3854) - allow users to clear read buffers
- [THRIFT-3859](https://issues.apache.org/jira/browse/THRIFT-3859) - Unix Domain Socket Support in Objective-C
- [THRIFT-3921](https://issues.apache.org/jira/browse/THRIFT-3921) - C++ code should print enums as strings
- [THRIFT-3926](https://issues.apache.org/jira/browse/THRIFT-3926) - There should be an error emitted when http status code is not 200 
- [THRIFT-4007](https://issues.apache.org/jira/browse/THRIFT-4007) - Micro-optimization of TTransport.py
- [THRIFT-4040](https://issues.apache.org/jira/browse/THRIFT-4040) - Add real cause of TNonblockingServerSocket error to exception
- [THRIFT-4064](https://issues.apache.org/jira/browse/THRIFT-4064) - Update node library dependencies
- [THRIFT-4069](https://issues.apache.org/jira/browse/THRIFT-4069) - All perl packages should have proper namespace, version syntax, and use proper thrift exceptions
- [THRIFT-4071](https://issues.apache.org/jira/browse/THRIFT-4071) - Consolidate the Travis CI jobs where possible to put less stress on the Apache Foundation's allocation of CI build slaves
- [THRIFT-4072](https://issues.apache.org/jira/browse/THRIFT-4072) - Add the possibility to send custom headers in TCurlClient
- [THRIFT-4075](https://issues.apache.org/jira/browse/THRIFT-4075) - Better MinGW support for headers-only boost (without thread library)
- [THRIFT-4081](https://issues.apache.org/jira/browse/THRIFT-4081) - Provide a MinGW 64-bit Appveyor CI build for better pull request validation
- [THRIFT-4084](https://issues.apache.org/jira/browse/THRIFT-4084) - Improve SSL security in thrift by adding a make cross client that checks to make sure SSLv3 protocol cannot be negotiated
- [THRIFT-4095](https://issues.apache.org/jira/browse/THRIFT-4095) - Add multiplexed protocol to Travis CI for make cross
- [THRIFT-4099](https://issues.apache.org/jira/browse/THRIFT-4099) - Auto-derive Hash for generated Rust structs
- [THRIFT-4110](https://issues.apache.org/jira/browse/THRIFT-4110) - The debian build files do not produce a "-dbg" package for debug symbols of libthrift0
- [THRIFT-4114](https://issues.apache.org/jira/browse/THRIFT-4114) - Space after '///' in doc comments
- [THRIFT-4126](https://issues.apache.org/jira/browse/THRIFT-4126) - Validate objects in php extension
- [THRIFT-4130](https://issues.apache.org/jira/browse/THRIFT-4130) - Ensure Apache Http connection is released back to pool after use
- [THRIFT-4151](https://issues.apache.org/jira/browse/THRIFT-4151) - Thrift Mutex Contention Profiling (pthreads) should be disabled by default
- [THRIFT-4176](https://issues.apache.org/jira/browse/THRIFT-4176) - Implement a threaded and threadpool server type for Rust
- [THRIFT-4183](https://issues.apache.org/jira/browse/THRIFT-4183) - Named pipe client blocks forever on Open() when there is no server at the other end
- [THRIFT-4190](https://issues.apache.org/jira/browse/THRIFT-4190) - improve C# TThreadPoolServer defaults
- [THRIFT-4197](https://issues.apache.org/jira/browse/THRIFT-4197) - Implement transparent gzip compression for HTTP transport
- [THRIFT-4198](https://issues.apache.org/jira/browse/THRIFT-4198) - Ruby should log Thrift internal errors to global logger
- [THRIFT-4203](https://issues.apache.org/jira/browse/THRIFT-4203) - thrift server stop gracefully
- [THRIFT-4205](https://issues.apache.org/jira/browse/THRIFT-4205) - c_glib is not linking against glib + gobject
- [THRIFT-4209](https://issues.apache.org/jira/browse/THRIFT-4209) - warning CS0414 in T[TLS]ServerSocket.cs
- [THRIFT-4210](https://issues.apache.org/jira/browse/THRIFT-4210) - include Thrift.45.csproj into CI runs 
- [THRIFT-4217](https://issues.apache.org/jira/browse/THRIFT-4217) - HttpClient should support gzip and deflate
- [THRIFT-4222](https://issues.apache.org/jira/browse/THRIFT-4222) - Support Unix Domain Sockets in Golang TServerSocket
- [THRIFT-4233](https://issues.apache.org/jira/browse/THRIFT-4233) - Make THsHaServer.invoker available (get method only) in inherited classes
- [THRIFT-4236](https://issues.apache.org/jira/browse/THRIFT-4236) - Support context in go generated code.
- [THRIFT-4238](https://issues.apache.org/jira/browse/THRIFT-4238) - JSON generator: make annotation-aware
- [THRIFT-4269](https://issues.apache.org/jira/browse/THRIFT-4269) - Don't append '.' to Erlang namespace if it ends in '_'.
- [THRIFT-4270](https://issues.apache.org/jira/browse/THRIFT-4270) - Generate Erlang mapping functions for const maps and lists
- [THRIFT-4275](https://issues.apache.org/jira/browse/THRIFT-4275) - Add support for zope.interface only, apart from twisted support.
- [THRIFT-4285](https://issues.apache.org/jira/browse/THRIFT-4285) - Pull generated send/recv into library to allow behaviour to be customised
- [THRIFT-4287](https://issues.apache.org/jira/browse/THRIFT-4287) - Add c++ compiler "no_skeleton" flag option
- [THRIFT-4288](https://issues.apache.org/jira/browse/THRIFT-4288) - Implement logging levels properly for node.js
- [THRIFT-4295](https://issues.apache.org/jira/browse/THRIFT-4295) - Refresh the Docker image file suite for Ubuntu, Debian, and CentOS
- [THRIFT-4305](https://issues.apache.org/jira/browse/THRIFT-4305) - Emit ddoc for generated items
- [THRIFT-4306](https://issues.apache.org/jira/browse/THRIFT-4306) - Thrift imports not replicated to D service output
- [THRIFT-4315](https://issues.apache.org/jira/browse/THRIFT-4315) - Add default message for TApplicationException
- [THRIFT-4318](https://issues.apache.org/jira/browse/THRIFT-4318) - Delphi performance improvements
- [THRIFT-4325](https://issues.apache.org/jira/browse/THRIFT-4325) - Simplify automake cross compilation by relying on one global THRIFT compiler path
- [THRIFT-4327](https://issues.apache.org/jira/browse/THRIFT-4327) - Improve TimerManager API to allow removing specific task
- [THRIFT-4330](https://issues.apache.org/jira/browse/THRIFT-4330) - Allow unused crates in Rust files
- [THRIFT-4333](https://issues.apache.org/jira/browse/THRIFT-4333) - Erlang tutorial examples are using a different port (9999)
- [THRIFT-4343](https://issues.apache.org/jira/browse/THRIFT-4343) - Change CI builds to use node.js 8.x LTS once available
- [THRIFT-4345](https://issues.apache.org/jira/browse/THRIFT-4345) - Create a docker build environment that uses the minimum supported language levels
- [THRIFT-4346](https://issues.apache.org/jira/browse/THRIFT-4346) - Allow Zlib transport factory to wrap other transports
- [THRIFT-4348](https://issues.apache.org/jira/browse/THRIFT-4348) - Perl HTTP Client custom HTTP headers
- [THRIFT-4350](https://issues.apache.org/jira/browse/THRIFT-4350) - Update netcore build for dotnet 2.0 sdk and make cross validation
- [THRIFT-4351](https://issues.apache.org/jira/browse/THRIFT-4351) - Use Travis CI Build Stages to optimize the CI build
- [THRIFT-4353](https://issues.apache.org/jira/browse/THRIFT-4353) - cannot read via thrift_protocol at server side
- [THRIFT-4378](https://issues.apache.org/jira/browse/THRIFT-4378) - add set stopTimeoutUnit method to TThreadPoolServer

### New Feature
- [THRIFT-750](https://issues.apache.org/jira/browse/THRIFT-750) - C++ Compiler Virtual Function Option
- [THRIFT-2945](https://issues.apache.org/jira/browse/THRIFT-2945) - Implement support for Rust language
- [THRIFT-3857](https://issues.apache.org/jira/browse/THRIFT-3857) - thrift js:node compiler support an object as parameter not an instance of struct
- [THRIFT-3933](https://issues.apache.org/jira/browse/THRIFT-3933) - Port official C# .NET library for Thrift to C# .NET Core library
- [THRIFT-4039](https://issues.apache.org/jira/browse/THRIFT-4039) - Update of Apache Thrift .Net Core lib 
- [THRIFT-4113](https://issues.apache.org/jira/browse/THRIFT-4113) - Provide a buffer transport for reading/writing in memory byte stream

### Question
- [THRIFT-2956](https://issues.apache.org/jira/browse/THRIFT-2956) - autoconf - possibly undefined macro - AC_PROG_BISON
- [THRIFT-4223](https://issues.apache.org/jira/browse/THRIFT-4223) - Add support to the isServing() method for the C++ library

### Task
- [THRIFT-3622](https://issues.apache.org/jira/browse/THRIFT-3622) - Fix deprecated uses of std::auto_ptr
- [THRIFT-4028](https://issues.apache.org/jira/browse/THRIFT-4028) - Please remove System.out.format from the source code
- [THRIFT-4186](https://issues.apache.org/jira/browse/THRIFT-4186) - Build and test rust client in Travis

### Test
- [THRIFT-4264](https://issues.apache.org/jira/browse/THRIFT-4264) - PHP - Support both shared & static linking of sockets library

### Wish
- [THRIFT-4344](https://issues.apache.org/jira/browse/THRIFT-4344) - Define and maintain the minimum language level for all languages in one place

## 0.10.0

### Bug
- [THRIFT-1840](https://issues.apache.org/jira/browse/THRIFT-1840) - Thrift Generated Code Causes Global Variable Leaks
- [THRIFT-1828](https://issues.apache.org/jira/browse/THRIFT-1828) - moc_TQTcpServer.cpp was removed from source tree but is in thrift-0.9.0.tar.gz
- [THRIFT-1790](https://issues.apache.org/jira/browse/THRIFT-1790) - cocoa: Duplicate interface definition error
- [THRIFT-1776](https://issues.apache.org/jira/browse/THRIFT-1776) - TPipeServer should implement "listen", so that TServerEventHandler preServe will work right
- [THRIFT-1351](https://issues.apache.org/jira/browse/THRIFT-1351) - Compiler does not care about binary strings
- [THRIFT-1229](https://issues.apache.org/jira/browse/THRIFT-1229) - Python fastbinary.c can not handle unicode as generated python code
- [THRIFT-749](https://issues.apache.org/jira/browse/THRIFT-749) - C++ TBufferedTransports do not flush their buffers on delete
- [THRIFT-747](https://issues.apache.org/jira/browse/THRIFT-747) - C++ TSocket->close calls shutdown breaking forked parent process
- [THRIFT-732](https://issues.apache.org/jira/browse/THRIFT-732) - server exits abnormally when client calls send_xxx function without calling recv_xxx function
- [THRIFT-3942](https://issues.apache.org/jira/browse/THRIFT-3942) - TSSLSocket does not honor send and receive timeouts
- [THRIFT-3941](https://issues.apache.org/jira/browse/THRIFT-3941) - WinXP version of thrift_poll() relies on undefined behavior by passing a destructed variable to select()
- [THRIFT-3940](https://issues.apache.org/jira/browse/THRIFT-3940) - Visual Studio project file for compiler is broken
- [THRIFT-3943](https://issues.apache.org/jira/browse/THRIFT-3943) - Coverity Scan identified some high severity defects
- [THRIFT-3929](https://issues.apache.org/jira/browse/THRIFT-3929) - PHP "nsglobal" Option Results in Syntax Error in Generated Code (Trailing Backslash)
- [THRIFT-3936](https://issues.apache.org/jira/browse/THRIFT-3936) - Cannot compile 0.10.0 development tip with VS2013 and earlier (snprintf, uint32_t)
- [THRIFT-3935](https://issues.apache.org/jira/browse/THRIFT-3935) - Incorrect skipping of map and set
- [THRIFT-3920](https://issues.apache.org/jira/browse/THRIFT-3920) - Ruby: Ensuring that HTTP failures will clear the http transport outbuf var
- [THRIFT-3919](https://issues.apache.org/jira/browse/THRIFT-3919) - C# TTLSServerSocket does not use clientTimeout
- [THRIFT-3917](https://issues.apache.org/jira/browse/THRIFT-3917) - Check backports.ssl_match_hostname module version
- [THRIFT-3909](https://issues.apache.org/jira/browse/THRIFT-3909) - Fix c_glib static lib CMake build
- [THRIFT-3904](https://issues.apache.org/jira/browse/THRIFT-3904) - Typo in node tutorial leads to wrong transport being used
- [THRIFT-3848](https://issues.apache.org/jira/browse/THRIFT-3848) - As an implementer of a perl socket server, I do not want to have to remember to ignore SIGCHLD for it to work properly
- [THRIFT-3844](https://issues.apache.org/jira/browse/THRIFT-3844) - thrift_protocol cannot compile in 7.0.7
- [THRIFT-3843](https://issues.apache.org/jira/browse/THRIFT-3843) - integer issues with Haxe PHP targets cause ZigZag encoding to fail
- [THRIFT-3842](https://issues.apache.org/jira/browse/THRIFT-3842) - Dart generates incorrect code for a const struct
- [THRIFT-3841](https://issues.apache.org/jira/browse/THRIFT-3841) - dart compact protocol incorrectly serializes/deserialized doubles
- [THRIFT-3708](https://issues.apache.org/jira/browse/THRIFT-3708) - NameError: global name 'TProtocol' is not defined
- [THRIFT-3704](https://issues.apache.org/jira/browse/THRIFT-3704) - "TConnectedClient died: Could not refill buffer" message shown when using HTTP Server
- [THRIFT-3678](https://issues.apache.org/jira/browse/THRIFT-3678) - Fix javadoc errors on JDK 8
- [THRIFT-3014](https://issues.apache.org/jira/browse/THRIFT-3014) - AppVeyor support
- [THRIFT-2994](https://issues.apache.org/jira/browse/THRIFT-2994) - Node.js TJSONProtocol cannot be used for object serialization.
- [THRIFT-2974](https://issues.apache.org/jira/browse/THRIFT-2974) - writeToParcel throws NPE for optional enum fields
- [THRIFT-2948](https://issues.apache.org/jira/browse/THRIFT-2948) - Python TJSONProtocol doesn't handle structs with binary fields containing invalid unicode.
- [THRIFT-2845](https://issues.apache.org/jira/browse/THRIFT-2845) - ChildService.Plo: No such file or directory
- [THRIFT-3276](https://issues.apache.org/jira/browse/THRIFT-3276) - Binary data does not decode correctly using the TJSONProtocol when the base64 encoded data is padded.
- [THRIFT-3253](https://issues.apache.org/jira/browse/THRIFT-3253) - Using latest version of D gives deprecation notices
- [THRIFT-2883](https://issues.apache.org/jira/browse/THRIFT-2883) - TTwisted.py, during ConnectionLost processing: exceptions.RuntimeError: dictionary changed size during iteration
- [THRIFT-2019](https://issues.apache.org/jira/browse/THRIFT-2019) - Writing on a disconnected socket on Mac causes SIG PIPE
- [THRIFT-2020](https://issues.apache.org/jira/browse/THRIFT-2020) - Thrift library has some empty files that haven't really been deleted
- [THRIFT-2049](https://issues.apache.org/jira/browse/THRIFT-2049) - Go compiler doesn't build on native Windows
- [THRIFT-2024](https://issues.apache.org/jira/browse/THRIFT-2024) - TServer.cpp warns on 64-bit platforms about truncating an rlim_t into an int
- [THRIFT-2023](https://issues.apache.org/jira/browse/THRIFT-2023) - gettimeofday implementation on Windows errors when no time zone is passed in.
- [THRIFT-2022](https://issues.apache.org/jira/browse/THRIFT-2022) - CoB and dense code generation still uses TR1 bind, even though that doesn't work with clang
- [THRIFT-2027](https://issues.apache.org/jira/browse/THRIFT-2027) - Minor 64-bit and NOMINMAX issues in C++ library
- [THRIFT-2156](https://issues.apache.org/jira/browse/THRIFT-2156) - TServerSocket::listen() is throwing exceptions with misleading information
- [THRIFT-2154](https://issues.apache.org/jira/browse/THRIFT-2154) - Missing <operator body
- [THRIFT-2148](https://issues.apache.org/jira/browse/THRIFT-2148) - TNonblockingMultiFetchClient imports log4j
- [THRIFT-2103](https://issues.apache.org/jira/browse/THRIFT-2103) - [python] Support for SSL certificates with Subject Alternative Names
- [THRIFT-1931](https://issues.apache.org/jira/browse/THRIFT-1931) - Sending a frame size of zero to a TNonblockingServer causes an assertion failure
- [THRIFT-1751](https://issues.apache.org/jira/browse/THRIFT-1751) - definition of increase_max_fds doesn't compile when HAVE_SYS_RESOURCE_H is not defined
- [THRIFT-1522](https://issues.apache.org/jira/browse/THRIFT-1522) - TServerSocket potential memory leak with addrinfo *res0
- [THRIFT-1547](https://issues.apache.org/jira/browse/THRIFT-1547) - Problems building against static libevent
- [THRIFT-1545](https://issues.apache.org/jira/browse/THRIFT-1545) - Generated javascript code uses "for in" for looping over arrays
- [THRIFT-1487](https://issues.apache.org/jira/browse/THRIFT-1487) - Namespace problem, compile fails on generated code
- [THRIFT-1472](https://issues.apache.org/jira/browse/THRIFT-1472) - Configuration conflicts with boost platform include header
- [THRIFT-6](https://issues.apache.org/jira/browse/THRIFT-6) - Thrift libraries and compiler lack version number
- [THRIFT-1680](https://issues.apache.org/jira/browse/THRIFT-1680) - make install requires GNU make
- [THRIFT-3869](https://issues.apache.org/jira/browse/THRIFT-3869) - Dart Tutorial build fails with Error 65 at "pub get"
- [THRIFT-3861](https://issues.apache.org/jira/browse/THRIFT-3861) - Travis CI builds are timing out - C++TServerIntegrationTest appears to be hanging
- [THRIFT-3855](https://issues.apache.org/jira/browse/THRIFT-3855) - In the go simple server, if Stop() is called multiple times it hangs
- [THRIFT-3885](https://issues.apache.org/jira/browse/THRIFT-3885) - PHP: Error when readI64 in TCompactProtocol
- [THRIFT-3883](https://issues.apache.org/jira/browse/THRIFT-3883) - Go TestAllConnection can fail with port 9090 collision
- [THRIFT-3884](https://issues.apache.org/jira/browse/THRIFT-3884) - Fix Erlang compact protocol double endianess and boolean list
- [THRIFT-3880](https://issues.apache.org/jira/browse/THRIFT-3880) - Erlang Compact protocol - boolean values inverted
- [THRIFT-3879](https://issues.apache.org/jira/browse/THRIFT-3879) - Undefined evaluation order causes incorrect processing in the C++ library JSON protocol
- [THRIFT-3851](https://issues.apache.org/jira/browse/THRIFT-3851) - Golang thrift continually adds the x/thrift content type 
- [THRIFT-3850](https://issues.apache.org/jira/browse/THRIFT-3850) - All apache builds are failing when initiated from a github pull request
- [THRIFT-3837](https://issues.apache.org/jira/browse/THRIFT-3837) - Thift 0.9.3 can't be build with QuickCheck 2.8.2 and unordered-containers 0.2.6
- [THRIFT-3831](https://issues.apache.org/jira/browse/THRIFT-3831) - build of test/cpp/src/TestClient.cpp fails with newer gcc on platforms with unsigned char due to narrowing conversions
- [THRIFT-3827](https://issues.apache.org/jira/browse/THRIFT-3827) - php CompactProtocol readI64 function has bug, when value has 32bit ~64bit, Example：value=1461563457000 
- [THRIFT-3825](https://issues.apache.org/jira/browse/THRIFT-3825) - Javascript test dependency is no longer available
- [THRIFT-3814](https://issues.apache.org/jira/browse/THRIFT-3814) - Fix contention in TNonblockingServerTest
- [THRIFT-3793](https://issues.apache.org/jira/browse/THRIFT-3793) - Appveyor builds reference an ant version that is no longer there
- [THRIFT-3786](https://issues.apache.org/jira/browse/THRIFT-3786) - Node.js TLS emits 'connect' before connection is ready
- [THRIFT-3780](https://issues.apache.org/jira/browse/THRIFT-3780) - Fix dart int64 usage when compiled to js
- [THRIFT-3789](https://issues.apache.org/jira/browse/THRIFT-3789) - Node.js lacks ability to destroy connection
- [THRIFT-3796](https://issues.apache.org/jira/browse/THRIFT-3796) - There's no --dbg for dh_strip, maybe someone has mistaken this for --dbg-package.
- [THRIFT-3795](https://issues.apache.org/jira/browse/THRIFT-3795) - Generated hashValue method in Swift will overflow
- [THRIFT-3790](https://issues.apache.org/jira/browse/THRIFT-3790) - Fix Delphi named pipe client to use timeout even when pipe doesn't yet exist
- [THRIFT-3787](https://issues.apache.org/jira/browse/THRIFT-3787) - Node.js Connection object doesn't handle errors correctly
- [THRIFT-3791](https://issues.apache.org/jira/browse/THRIFT-3791) - Delphi pipe client may fail even in a non-error condition
- [THRIFT-3771](https://issues.apache.org/jira/browse/THRIFT-3771) - TBufferedTransport gets in invalid state on read/write errors
- [THRIFT-3764](https://issues.apache.org/jira/browse/THRIFT-3764) - PHP "make install" does not install TMultiplexedProtocol.php nor TSimpleJSONProtocol.php
- [THRIFT-3768](https://issues.apache.org/jira/browse/THRIFT-3768) - TThreadedServer may crash if it is destroyed immediately after it returns from serve(); TThreadedServer disconnects clients
- [THRIFT-3765](https://issues.apache.org/jira/browse/THRIFT-3765) - memory leak in python compact protocol extension
- [THRIFT-3758](https://issues.apache.org/jira/browse/THRIFT-3758) - TApplicationException::getType and TProtocolException::getType should be const
- [THRIFT-3763](https://issues.apache.org/jira/browse/THRIFT-3763) - Fix serialization of i64 larger than 2^53 for browserify
- [THRIFT-3759](https://issues.apache.org/jira/browse/THRIFT-3759) - required fields that are nil are silently ignored on write
- [THRIFT-3753](https://issues.apache.org/jira/browse/THRIFT-3753) - TServerFramework::stop may fail to interrupt connected clients
- [THRIFT-3755](https://issues.apache.org/jira/browse/THRIFT-3755) - TDebugProtocol::writeString hits assert in isprint on Windows with debug CRT
- [THRIFT-3751](https://issues.apache.org/jira/browse/THRIFT-3751) - Compiler allows field ids that are too large for generated code
- [THRIFT-3748](https://issues.apache.org/jira/browse/THRIFT-3748) - Node.js Deserialization of lists of lists is broken
- [THRIFT-3760](https://issues.apache.org/jira/browse/THRIFT-3760) - Fix install paths etc of debian packages for py and perl
- [THRIFT-3757](https://issues.apache.org/jira/browse/THRIFT-3757) - Fix various build warnings on Windows with VS2015 compiler
- [THRIFT-3750](https://issues.apache.org/jira/browse/THRIFT-3750) - NSCopying copyWithZone: implementation does not check isSet
- [THRIFT-3747](https://issues.apache.org/jira/browse/THRIFT-3747) - Duplicate node.js build on Travis-CI
- [THRIFT-3744](https://issues.apache.org/jira/browse/THRIFT-3744) - The precision should be 17 (16 bits need after dot) after dot for double type.
- [THRIFT-3741](https://issues.apache.org/jira/browse/THRIFT-3741) - haxe test is broken
- [THRIFT-3739](https://issues.apache.org/jira/browse/THRIFT-3739) - Deprecation warning in codegen/base.d
- [THRIFT-3735](https://issues.apache.org/jira/browse/THRIFT-3735) - JSON protocol left in incorrect state when an exception is thrown during read or write operations
- [THRIFT-3734](https://issues.apache.org/jira/browse/THRIFT-3734) - To compare two string as lowercase.
- [THRIFT-3743](https://issues.apache.org/jira/browse/THRIFT-3743) - Java JSON protocol left in incorrect state when an exception is thrown during read or write operations
- [THRIFT-3731](https://issues.apache.org/jira/browse/THRIFT-3731) - Perl multiplex test is flaky
- [THRIFT-3729](https://issues.apache.org/jira/browse/THRIFT-3729) - Restrict rake version
- [THRIFT-3727](https://issues.apache.org/jira/browse/THRIFT-3727) - Incorrect require paths in Node.js tutorial
- [THRIFT-3723](https://issues.apache.org/jira/browse/THRIFT-3723) - Fix Lua include path
- [THRIFT-3722](https://issues.apache.org/jira/browse/THRIFT-3722) - Fix cert path in C++ cross tests for non-Linux platform
- [THRIFT-3726](https://issues.apache.org/jira/browse/THRIFT-3726) - Fix incorrect conditional in TMultiplexedProcessor.py
- [THRIFT-3725](https://issues.apache.org/jira/browse/THRIFT-3725) - Skip a flaky cross test entry (d-dart compact framed-ip)
- [THRIFT-3724](https://issues.apache.org/jira/browse/THRIFT-3724) - Fix incorrect timeval conversion in libevent.d
- [THRIFT-3721](https://issues.apache.org/jira/browse/THRIFT-3721) - CLONE - why not add unicode strings support to python directly?
- [THRIFT-3720](https://issues.apache.org/jira/browse/THRIFT-3720) - TTcpSocketStreamImpl.Read() returns 0 if not all requested bytes could be read
- [THRIFT-3719](https://issues.apache.org/jira/browse/THRIFT-3719) - Dart generator should use lowerCamelCase for service names
- [THRIFT-3902](https://issues.apache.org/jira/browse/THRIFT-3902) - TSocket.open throws NullPointerException
- [THRIFT-3901](https://issues.apache.org/jira/browse/THRIFT-3901) - TFramedTransport.open throws NullPointerException
- [THRIFT-3893](https://issues.apache.org/jira/browse/THRIFT-3893) - Command injection in format_go_output
- [THRIFT-3807](https://issues.apache.org/jira/browse/THRIFT-3807) - Swift compiler does not escape reserved words
- [THRIFT-3798](https://issues.apache.org/jira/browse/THRIFT-3798) - THttpClient does not use proxy from http_proxy, https_proxy environment variables
- [THRIFT-3809](https://issues.apache.org/jira/browse/THRIFT-3809) - wrong/unused BINARY type code
- [THRIFT-3806](https://issues.apache.org/jira/browse/THRIFT-3806) - Swift generator does not handle self-referring structs
- [THRIFT-3805](https://issues.apache.org/jira/browse/THRIFT-3805) - Golang server susceptible to memory spike from malformed message
- [THRIFT-3797](https://issues.apache.org/jira/browse/THRIFT-3797) - Generated Delphi processor shouldn't error out on timed out exceptions
- [THRIFT-3813](https://issues.apache.org/jira/browse/THRIFT-3813) - Appveyor builds reference an openssl version that is no longer there
- [THRIFT-3658](https://issues.apache.org/jira/browse/THRIFT-3658) - Missing file in THRIFT-3599
- [THRIFT-3649](https://issues.apache.org/jira/browse/THRIFT-3649) - Python TSaslClientTransport initializes TTransportException incorrectly
- [THRIFT-3650](https://issues.apache.org/jira/browse/THRIFT-3650) - incorrect union serialization 
- [THRIFT-3713](https://issues.apache.org/jira/browse/THRIFT-3713) - lib/d/test/thrift_test_runner.sh is flaky on Jenkins
- [THRIFT-3668](https://issues.apache.org/jira/browse/THRIFT-3668) - range check error in compact protocol
- [THRIFT-3663](https://issues.apache.org/jira/browse/THRIFT-3663) - CMake cpp test fails to build on system without zlib
- [THRIFT-3712](https://issues.apache.org/jira/browse/THRIFT-3712) - TTornadoServer cannot handle IPv6 address
- [THRIFT-3710](https://issues.apache.org/jira/browse/THRIFT-3710) - Dart generator does not camel case Constants class names
- [THRIFT-3697](https://issues.apache.org/jira/browse/THRIFT-3697) - Dart generator does not name imports
- [THRIFT-3690](https://issues.apache.org/jira/browse/THRIFT-3690) - Work around docker image build failures on Travis-CI
- [THRIFT-3689](https://issues.apache.org/jira/browse/THRIFT-3689) - thrift_reconnecting_client start failed when server is not available
- [THRIFT-3695](https://issues.apache.org/jira/browse/THRIFT-3695) - Fix D test scripts
- [THRIFT-3675](https://issues.apache.org/jira/browse/THRIFT-3675) - Union is not serialized correctly by Thrift C Glib
- [THRIFT-3673](https://issues.apache.org/jira/browse/THRIFT-3673) - API fails with std::exception after a timeout occurred in earlier any API call
- [THRIFT-3709](https://issues.apache.org/jira/browse/THRIFT-3709) - Comment syntax can produce broken code
- [THRIFT-3705](https://issues.apache.org/jira/browse/THRIFT-3705) - Go map has incorrect types when used with forward-defined types
- [THRIFT-3702](https://issues.apache.org/jira/browse/THRIFT-3702) - Fix cross tests for Dart compact protocol (3 failing)
- [THRIFT-3683](https://issues.apache.org/jira/browse/THRIFT-3683) - BadYieldError in thrift py:tornado server
- [THRIFT-3682](https://issues.apache.org/jira/browse/THRIFT-3682) - Do not reuse refused sockets in test scripts
- [THRIFT-3681](https://issues.apache.org/jira/browse/THRIFT-3681) - Fix Dart tutorial build
- [THRIFT-3680](https://issues.apache.org/jira/browse/THRIFT-3680) - Java async processor fails to notify errors to clients
- [THRIFT-3714](https://issues.apache.org/jira/browse/THRIFT-3714) - Thrift.TProtocolException is not defined in js/src/thrift.js
- [THRIFT-3688](https://issues.apache.org/jira/browse/THRIFT-3688) - Fix socket bind failure detection of cross test
- [THRIFT-3641](https://issues.apache.org/jira/browse/THRIFT-3641) - Ruby client should try to connect to every result of getaddrinfo
- [THRIFT-3635](https://issues.apache.org/jira/browse/THRIFT-3635) - D transport_test is flaky on Jenkins and Travis
- [THRIFT-3618](https://issues.apache.org/jira/browse/THRIFT-3618) - Python TSSLSocket deprecation message should print caller's location
- [THRIFT-3145](https://issues.apache.org/jira/browse/THRIFT-3145) - JSON protocol does not handle bool and empty containers correctly
- [THRIFT-3158](https://issues.apache.org/jira/browse/THRIFT-3158) - TBase<T,F>#deepCopy should return T
- [THRIFT-3157](https://issues.apache.org/jira/browse/THRIFT-3157) - TBase signature should be TBase<T extends TBase<T,F>, F extends TFieldIdEnum>
- [THRIFT-3156](https://issues.apache.org/jira/browse/THRIFT-3156) - Node TLS: server executes processing logic two full times
- [THRIFT-3154](https://issues.apache.org/jira/browse/THRIFT-3154) - tutorial/py.tornado throw EOF exception
- [THRIFT-3063](https://issues.apache.org/jira/browse/THRIFT-3063) - C++ build -Wunused-parameter warnings on processor_test, TransportTest
- [THRIFT-3056](https://issues.apache.org/jira/browse/THRIFT-3056) - Add string/collection length limits for Python protocol readers
- [THRIFT-3237](https://issues.apache.org/jira/browse/THRIFT-3237) - Fix TNamedPipeServer::createNamedPipe memory leak
- [THRIFT-3233](https://issues.apache.org/jira/browse/THRIFT-3233) - Fix C++ ThreadManager::Impl::removeWorker worker join
- [THRIFT-3232](https://issues.apache.org/jira/browse/THRIFT-3232) - Cannot deserialize json messages created with fieldNamesAsString 
- [THRIFT-3206](https://issues.apache.org/jira/browse/THRIFT-3206) - Fix Visual Studio build failure due 'pthread_self': identifier not found
- [THRIFT-3200](https://issues.apache.org/jira/browse/THRIFT-3200) - JS and nodejs do not encode JSON protocol binary fields as base64
- [THRIFT-3199](https://issues.apache.org/jira/browse/THRIFT-3199) - Exception field has basic metadata
- [THRIFT-3182](https://issues.apache.org/jira/browse/THRIFT-3182) - TFramedTransport is in an invalid state after frame size exception
- [THRIFT-2536](https://issues.apache.org/jira/browse/THRIFT-2536) - new TSocket, uninitialised value reported by valgrind
- [THRIFT-2527](https://issues.apache.org/jira/browse/THRIFT-2527) - Apache Thrift IDL Compiler code generated for Node.js should be jshint clean
- [THRIFT-2519](https://issues.apache.org/jira/browse/THRIFT-2519) - "processor" class is not being generated
- [THRIFT-2431](https://issues.apache.org/jira/browse/THRIFT-2431) - TFileTransportTest fails with "check delta < XXX failed"
- [THRIFT-2708](https://issues.apache.org/jira/browse/THRIFT-2708) - Erlang library does not support "oneway" message type
- [THRIFT-3377](https://issues.apache.org/jira/browse/THRIFT-3377) - Deep copy is actually shallow when using typedef members
- [THRIFT-3376](https://issues.apache.org/jira/browse/THRIFT-3376) - C# and Python JSON protocol double values lose precision
- [THRIFT-3373](https://issues.apache.org/jira/browse/THRIFT-3373) - Various fixes for cross test servers and clients
- [THRIFT-3370](https://issues.apache.org/jira/browse/THRIFT-3370) - errno extern variable redefined. Not compiling for Android
- [THRIFT-3379](https://issues.apache.org/jira/browse/THRIFT-3379) -  Potential out of range panic in Go JSON protocols
- [THRIFT-3371](https://issues.apache.org/jira/browse/THRIFT-3371) - Abstract namespace Unix domain sockets broken in C++
- [THRIFT-3380](https://issues.apache.org/jira/browse/THRIFT-3380) - nodejs: 0.9.2 -> 0.9.3 upgrade breaks Protocol and Transport requires
- [THRIFT-3367](https://issues.apache.org/jira/browse/THRIFT-3367) - Fix bad links to coding_standards.md #634 
- [THRIFT-3401](https://issues.apache.org/jira/browse/THRIFT-3401) - Nested collections emit Objective-C code that cannot compile
- [THRIFT-3403](https://issues.apache.org/jira/browse/THRIFT-3403) - JSON String reader doesn't recognize UTF-16 surrogate pairs
- [THRIFT-3362](https://issues.apache.org/jira/browse/THRIFT-3362) - make check fails for C++ at the SecurityTest
- [THRIFT-3395](https://issues.apache.org/jira/browse/THRIFT-3395) - Cocoa compiler produces corrupt code when boxing enums inside map.
- [THRIFT-3394](https://issues.apache.org/jira/browse/THRIFT-3394) - compiler generates uncompilable code
- [THRIFT-3388](https://issues.apache.org/jira/browse/THRIFT-3388) - hash doesn't work on set/list
- [THRIFT-3391](https://issues.apache.org/jira/browse/THRIFT-3391) - Wrong bool formatting in test server
- [THRIFT-3390](https://issues.apache.org/jira/browse/THRIFT-3390) - TTornado server doesn't handle closed connections properly
- [THRIFT-3382](https://issues.apache.org/jira/browse/THRIFT-3382) - TBase class for C++ Library
- [THRIFT-3392](https://issues.apache.org/jira/browse/THRIFT-3392) - Java TZlibTransport does not close its wrapper streams upon close()
- [THRIFT-3383](https://issues.apache.org/jira/browse/THRIFT-3383) - i64 related warnings 
- [THRIFT-3386](https://issues.apache.org/jira/browse/THRIFT-3386) - misc. warnings with make check
- [THRIFT-3385](https://issues.apache.org/jira/browse/THRIFT-3385) - warning: format ‘%lu’ expects ‘long unsigned int’, but has type ‘std::basic_string<char>::size_type {aka unsigned int}
- [THRIFT-3355](https://issues.apache.org/jira/browse/THRIFT-3355) - npm WARN package.json thrift@1.0.0-dev No license field.
- [THRIFT-3360](https://issues.apache.org/jira/browse/THRIFT-3360) - Improve cross test servers and clients further
- [THRIFT-3359](https://issues.apache.org/jira/browse/THRIFT-3359) - Binary field incompatibilities
- [THRIFT-3354](https://issues.apache.org/jira/browse/THRIFT-3354) - Fix word-extraction substr bug in initialism code
- [THRIFT-3350](https://issues.apache.org/jira/browse/THRIFT-3350) - Python JSON protocol does not encode binary as Base64
- [THRIFT-3577](https://issues.apache.org/jira/browse/THRIFT-3577) - assertion failed at line 512 of testcontainertest.c
- [THRIFT-3576](https://issues.apache.org/jira/browse/THRIFT-3576) - Boost test --log_format arg does not accept lowercase
- [THRIFT-3575](https://issues.apache.org/jira/browse/THRIFT-3575) - Go compiler tries to use unexported library methods when using read_write_private
- [THRIFT-3574](https://issues.apache.org/jira/browse/THRIFT-3574) - Cocoa generator makes uncompilable imports
- [THRIFT-3570](https://issues.apache.org/jira/browse/THRIFT-3570) - Remove duplicate instances that are added by upstream
- [THRIFT-3571](https://issues.apache.org/jira/browse/THRIFT-3571) - Make feature test result browsable
- [THRIFT-3569](https://issues.apache.org/jira/browse/THRIFT-3569) - c_glib protocols do not check number of bytes read by transport
- [THRIFT-3568](https://issues.apache.org/jira/browse/THRIFT-3568) - THeader server crashes on readSlow
- [THRIFT-3567](https://issues.apache.org/jira/browse/THRIFT-3567) - GLib-GObject-CRITICAL **: g_object_unref: assertion 'G_IS_OBJECT (object)' failed
- [THRIFT-3566](https://issues.apache.org/jira/browse/THRIFT-3566) - C++/Qt: TQTcpServerTest::test_communicate() is never executed
- [THRIFT-3564](https://issues.apache.org/jira/browse/THRIFT-3564) - C++/Qt: potential core dump in TQTcpServer in case an exception occurs in TAsyncProcessor::process()
- [THRIFT-3558](https://issues.apache.org/jira/browse/THRIFT-3558) - typos in c_glib tests
- [THRIFT-3559](https://issues.apache.org/jira/browse/THRIFT-3559) - Fix awkward extra semi-colons with Cocoa container literals
- [THRIFT-3555](https://issues.apache.org/jira/browse/THRIFT-3555) - 'configure' script does not honor --with-openssl=<path> for libcrypto for BN_init
- [THRIFT-3554](https://issues.apache.org/jira/browse/THRIFT-3554) - Constant decls may lead to "Error: internal error: prepare_member_name_mapping() already active for different struct"
- [THRIFT-3552](https://issues.apache.org/jira/browse/THRIFT-3552) - glib_c Memory Leak
- [THRIFT-3551](https://issues.apache.org/jira/browse/THRIFT-3551) - Thrift perl library missing package declaration
- [THRIFT-3549](https://issues.apache.org/jira/browse/THRIFT-3549) - Exceptions are not properly stringified in Perl library
- [THRIFT-3546](https://issues.apache.org/jira/browse/THRIFT-3546) - NodeJS code should not be namespaced (and is currently not strict-mode compliant)
- [THRIFT-3545](https://issues.apache.org/jira/browse/THRIFT-3545) - Container type literals do not compile
- [THRIFT-3538](https://issues.apache.org/jira/browse/THRIFT-3538) - Remove UnboundMethodType in TProtocolDecorator
- [THRIFT-3536](https://issues.apache.org/jira/browse/THRIFT-3536) - Error 'char' does not contain a definition for 'IsLowSurrogate' for WP7 target
- [THRIFT-3534](https://issues.apache.org/jira/browse/THRIFT-3534) - Link error when building with Qt5
- [THRIFT-3533](https://issues.apache.org/jira/browse/THRIFT-3533) - Can not send nil pointer as service method argument
- [THRIFT-3507](https://issues.apache.org/jira/browse/THRIFT-3507) - THttpClient does not use proxy from http_proxy, https_proxy environment variables
- [THRIFT-3502](https://issues.apache.org/jira/browse/THRIFT-3502) - C++ TServerSocket passes small buffer to getsockname
- [THRIFT-3501](https://issues.apache.org/jira/browse/THRIFT-3501) - Forward slash in comment causes compiler error
- [THRIFT-3498](https://issues.apache.org/jira/browse/THRIFT-3498) - C++ library assumes optional function pthread_attr_setschedpolicy is available
- [THRIFT-3497](https://issues.apache.org/jira/browse/THRIFT-3497) - Build fails with "invalid use of incomplete type"
- [THRIFT-3496](https://issues.apache.org/jira/browse/THRIFT-3496) - C++: Cob style client fails when sending a consecutive request
- [THRIFT-3493](https://issues.apache.org/jira/browse/THRIFT-3493) - libthrift does not compile on windows using visual studio
- [THRIFT-3488](https://issues.apache.org/jira/browse/THRIFT-3488) - warning: unused variable 'program'
- [THRIFT-3489](https://issues.apache.org/jira/browse/THRIFT-3489) - warning: deprecated conversion from string constant to 'char*' [-Wwrite-strings]
- [THRIFT-3487](https://issues.apache.org/jira/browse/THRIFT-3487) - Full support for newer Delphi versions
- [THRIFT-3528](https://issues.apache.org/jira/browse/THRIFT-3528) - Fix warnings in thrift.ll
- [THRIFT-3527](https://issues.apache.org/jira/browse/THRIFT-3527) - -gen py:dynamic,utf8strings ignores utf8strings option
- [THRIFT-3526](https://issues.apache.org/jira/browse/THRIFT-3526) - Code generated by py:utf8strings does not work for Python3
- [THRIFT-3524](https://issues.apache.org/jira/browse/THRIFT-3524) - dcc32 warning "W1000 Symbol 'IsLowSurrogate' is deprecated: 'Use TCharHelper'" in Thrift.Protocol.JSON.pas
- [THRIFT-3525](https://issues.apache.org/jira/browse/THRIFT-3525) - py:dynamic fails to handle binary list/set/map element
- [THRIFT-3521](https://issues.apache.org/jira/browse/THRIFT-3521) - TSimpleJSONProtocolTest is not deterministic (fails when run on JDK 8)
- [THRIFT-3520](https://issues.apache.org/jira/browse/THRIFT-3520) - Dart TSocket onError stream should be typed as Object
- [THRIFT-3519](https://issues.apache.org/jira/browse/THRIFT-3519) - fastbinary does not work with -gen py:utf8strings
- [THRIFT-3518](https://issues.apache.org/jira/browse/THRIFT-3518) - TConcurrentClientSyncInfo files were missing for Visual Studio
- [THRIFT-3512](https://issues.apache.org/jira/browse/THRIFT-3512) - c_glib: Build fails due to missing features.h
- [THRIFT-3483](https://issues.apache.org/jira/browse/THRIFT-3483) - Incorrect empty binary handling introduced by THRIFT-3359
- [THRIFT-3479](https://issues.apache.org/jira/browse/THRIFT-3479) - Oneway calls should not return exceptions to clients
- [THRIFT-3478](https://issues.apache.org/jira/browse/THRIFT-3478) - Restore dropped method to THsHaServer.java
- [THRIFT-3477](https://issues.apache.org/jira/browse/THRIFT-3477) - Parser fails on enum item that starts with 'E' letter and continues with number 
- [THRIFT-3476](https://issues.apache.org/jira/browse/THRIFT-3476) - Missing include in ./src/thrift/protocol/TJSONProtocol.cpp
- [THRIFT-3474](https://issues.apache.org/jira/browse/THRIFT-3474) - Docker: thrift-compiler
- [THRIFT-3473](https://issues.apache.org/jira/browse/THRIFT-3473) - When "optional' is used with a struct member, C++ server seems to not return it correctly
- [THRIFT-3468](https://issues.apache.org/jira/browse/THRIFT-3468) - Dart TSocketTransport onError handler is too restrictive
- [THRIFT-3451](https://issues.apache.org/jira/browse/THRIFT-3451) - thrift_protocol PHP extension missing config.m4 file
- [THRIFT-3456](https://issues.apache.org/jira/browse/THRIFT-3456) - rounding issue in static assert
- [THRIFT-3455](https://issues.apache.org/jira/browse/THRIFT-3455) - struct write method's return value is incorrect
- [THRIFT-3454](https://issues.apache.org/jira/browse/THRIFT-3454) - Python Tornado tutorial is broken
- [THRIFT-3463](https://issues.apache.org/jira/browse/THRIFT-3463) - Java can't be disabled in CMake build
- [THRIFT-3450](https://issues.apache.org/jira/browse/THRIFT-3450) - NPE when using SSL
- [THRIFT-3449](https://issues.apache.org/jira/browse/THRIFT-3449) - TBaseAsyncProcessor fb.responseReady() never called for oneway functions
- [THRIFT-3471](https://issues.apache.org/jira/browse/THRIFT-3471) - Dart generator does not handle uppercase argument names
- [THRIFT-3470](https://issues.apache.org/jira/browse/THRIFT-3470) - Sporadic timeouts with pipes
- [THRIFT-3465](https://issues.apache.org/jira/browse/THRIFT-3465) - Go Code With Complex Const Initializer Compilation Depends On Struct Order
- [THRIFT-3464](https://issues.apache.org/jira/browse/THRIFT-3464) - Fix several defects in c_glib code generator
- [THRIFT-3462](https://issues.apache.org/jira/browse/THRIFT-3462) - Cocoa generates Incorrect #import header names
- [THRIFT-3453](https://issues.apache.org/jira/browse/THRIFT-3453) - remove rat_exclude
- [THRIFT-3418](https://issues.apache.org/jira/browse/THRIFT-3418) - Use of ciphers in ssl.wrap_socket() breaks python 2.6 compatibility
- [THRIFT-3417](https://issues.apache.org/jira/browse/THRIFT-3417) - "namespace xsd" is not really working
- [THRIFT-3413](https://issues.apache.org/jira/browse/THRIFT-3413) - Thrift code generation bug in Go when extending service
- [THRIFT-3420](https://issues.apache.org/jira/browse/THRIFT-3420) - C++: TSSLSockets are not interruptable
- [THRIFT-3415](https://issues.apache.org/jira/browse/THRIFT-3415) - include unistd.h conditionally
- [THRIFT-3414](https://issues.apache.org/jira/browse/THRIFT-3414) - #include <pwd.h> in THeaderTransport.h breaks windows build
- [THRIFT-3411](https://issues.apache.org/jira/browse/THRIFT-3411) - Go generates remotes with wrong package qualifiers when including
- [THRIFT-3430](https://issues.apache.org/jira/browse/THRIFT-3430) - Go THttpClient does not read HTTP response body to completion when closing
- [THRIFT-3423](https://issues.apache.org/jira/browse/THRIFT-3423) - First call to thrift_transport:read_exact fails to dispatch correct function
- [THRIFT-3422](https://issues.apache.org/jira/browse/THRIFT-3422) - Go TServerSocket doesn't close on Interrupt
- [THRIFT-3421](https://issues.apache.org/jira/browse/THRIFT-3421) - rebar as dependency instead of bundling (was:  rebar fails if PWD contains Unicode)
- [THRIFT-3428](https://issues.apache.org/jira/browse/THRIFT-3428) - Go test fails when running make check
- [THRIFT-3445](https://issues.apache.org/jira/browse/THRIFT-3445) - Throwable messages are hidden from JVM stack trace output
- [THRIFT-3443](https://issues.apache.org/jira/browse/THRIFT-3443) - Thrift include can generate uncompilable code
- [THRIFT-3444](https://issues.apache.org/jira/browse/THRIFT-3444) - Large 64 bit Integer does not preserve value through Node.js JSONProtocol 
- [THRIFT-3436](https://issues.apache.org/jira/browse/THRIFT-3436) - misc. cross test issues with UTF-8 path names
- [THRIFT-3435](https://issues.apache.org/jira/browse/THRIFT-3435) - Put generated Java code for fullcamel tests in a separate package/namespace
- [THRIFT-3433](https://issues.apache.org/jira/browse/THRIFT-3433) - Doubles aren't interpreted correctly
- [THRIFT-3437](https://issues.apache.org/jira/browse/THRIFT-3437) - Mingw-w64 build fail
- [THRIFT-3434](https://issues.apache.org/jira/browse/THRIFT-3434) - Dart generator produces empty name in pubspec.yaml for includes without namespaces
- [THRIFT-3408](https://issues.apache.org/jira/browse/THRIFT-3408) - JSON generator emits incorrect types
- [THRIFT-3406](https://issues.apache.org/jira/browse/THRIFT-3406) - Cocoa client should not schedule streams on main runloop
- [THRIFT-3404](https://issues.apache.org/jira/browse/THRIFT-3404) - JSON String reader doesn't recognize UTF-16 surrogate pair
- [THRIFT-3636](https://issues.apache.org/jira/browse/THRIFT-3636) - Double precision is not fully preserved in C++ TJSONProtocol
- [THRIFT-3632](https://issues.apache.org/jira/browse/THRIFT-3632) - c_glib testserialization fails with glib assertion
- [THRIFT-3619](https://issues.apache.org/jira/browse/THRIFT-3619) - Using Thrift 0.9.3 with googletest on Linux gcc 4.9 / C++11
- [THRIFT-3617](https://issues.apache.org/jira/browse/THRIFT-3617) - CMake does not build gv/xml generators
- [THRIFT-3615](https://issues.apache.org/jira/browse/THRIFT-3615) - Fix Python SSL client resource leak on connection failure
- [THRIFT-3616](https://issues.apache.org/jira/browse/THRIFT-3616) - lib/py/test/test_sslsocket.py is flaky
- [THRIFT-3643](https://issues.apache.org/jira/browse/THRIFT-3643) - Perl SSL server crushes if a client disconnect without handshake
- [THRIFT-3639](https://issues.apache.org/jira/browse/THRIFT-3639) - C# Thrift library forces TLS 1.0, thwarting TLS 1.2 usage
- [THRIFT-3633](https://issues.apache.org/jira/browse/THRIFT-3633) - Travis "C C++ - GCC" build was using clang
- [THRIFT-3634](https://issues.apache.org/jira/browse/THRIFT-3634) - Fix Python TSocket resource leak on connection failure
- [THRIFT-3630](https://issues.apache.org/jira/browse/THRIFT-3630) - Debian/Ubuntu install docs need an update
- [THRIFT-3629](https://issues.apache.org/jira/browse/THRIFT-3629) - Parser sets exitcode on errors, but generator does not
- [THRIFT-3608](https://issues.apache.org/jira/browse/THRIFT-3608) - lib/cpp/test/SecurityTest is flaky in jenkins Thrift-precommit build.
- [THRIFT-3601](https://issues.apache.org/jira/browse/THRIFT-3601) - Better conformance to PEP8 for generated code
- [THRIFT-3599](https://issues.apache.org/jira/browse/THRIFT-3599) - Validate client IP address against cert's SubjectAltName
- [THRIFT-3598](https://issues.apache.org/jira/browse/THRIFT-3598) - TBufferedTransport doesn't instantiate client connection
- [THRIFT-3597](https://issues.apache.org/jira/browse/THRIFT-3597) - `make check` hangs in go tests
- [THRIFT-3589](https://issues.apache.org/jira/browse/THRIFT-3589) - Dart generator uses wrong name in constructor for uppercase arguments with defaults
- [THRIFT-3588](https://issues.apache.org/jira/browse/THRIFT-3588) - Using TypeScript with --noImplicitAny fails
- [THRIFT-3584](https://issues.apache.org/jira/browse/THRIFT-3584) - boolean false value cannot be transferred
- [THRIFT-3578](https://issues.apache.org/jira/browse/THRIFT-3578) - Make THeaderTransport detect TCompact framed and unframed
- [THRIFT-3323](https://issues.apache.org/jira/browse/THRIFT-3323) - Python library does not handle escaped forward slash ("/") in JSON
- [THRIFT-3322](https://issues.apache.org/jira/browse/THRIFT-3322) - CMake generated "make check" failes on python_test
- [THRIFT-3321](https://issues.apache.org/jira/browse/THRIFT-3321) - Thrift can't be added as a subdirectory of another CMake-based project
- [THRIFT-3314](https://issues.apache.org/jira/browse/THRIFT-3314) - Dots in file names of includes causes dots in javascript variable names
- [THRIFT-3307](https://issues.apache.org/jira/browse/THRIFT-3307) - Segfault in Ruby serializer
- [THRIFT-3309](https://issues.apache.org/jira/browse/THRIFT-3309) - Missing TConstant.php in /lib/php/Makefile.am
- [THRIFT-3810](https://issues.apache.org/jira/browse/THRIFT-3810) - unresolved external symbol public: virtual void __cdecl apache::thrift::server::TServerFramework::serve(void)
- [THRIFT-3736](https://issues.apache.org/jira/browse/THRIFT-3736) - C++ library build fails if OpenSSL does not surrpot SSLv3
- [THRIFT-3878](https://issues.apache.org/jira/browse/THRIFT-3878) - Compile error in TSSLSocket.cpp with new OpenSSL [CRYPTO_num_locks]
- [THRIFT-3949](https://issues.apache.org/jira/browse/THRIFT-3949) - missing make dist entry for compiler/cpp/test
- [THRIFT-449](https://issues.apache.org/jira/browse/THRIFT-449) - The wire format of the JSON Protocol may not always be valid JSON if it contains non-UTF8 encoded strings
- [THRIFT-162](https://issues.apache.org/jira/browse/THRIFT-162) - Thrift structures are unhashable, preventing them from being used as set elements
- [THRIFT-3961](https://issues.apache.org/jira/browse/THRIFT-3961) - TConnectedClient does not terminate the connection to the client if an exception while processing the received message occures.
- [THRIFT-3881](https://issues.apache.org/jira/browse/THRIFT-3881) - Travis CI builds are failing due to docker failures (three retries, and gives up)
- [THRIFT-3937](https://issues.apache.org/jira/browse/THRIFT-3937) - Cannot compile 0.10.0 development tip with gcc-4.6.x
- [THRIFT-3964](https://issues.apache.org/jira/browse/THRIFT-3964) - Unsupported mechanism type ????? due to dependency on default OS-dependent charset
- [THRIFT-3038](https://issues.apache.org/jira/browse/THRIFT-3038) - Use of volatile in cpp library
- [THRIFT-3301](https://issues.apache.org/jira/browse/THRIFT-3301) - Java generated code uses imports that can lead to class name collisions with IDL defined types
- [THRIFT-3348](https://issues.apache.org/jira/browse/THRIFT-3348) - PHP TCompactProtocol bool&int64 readvalue bug
- [THRIFT-3955](https://issues.apache.org/jira/browse/THRIFT-3955) - TThreadedServer Memory Leak
- [THRIFT-3829](https://issues.apache.org/jira/browse/THRIFT-3829) - Thrift does not install Python Libraries if Twisted is not installed
- [THRIFT-3932](https://issues.apache.org/jira/browse/THRIFT-3932) - C++ ThreadManager has a rare termination race
- [THRIFT-3828](https://issues.apache.org/jira/browse/THRIFT-3828) - cmake fails when Boost_INCLUDE_DIRS (and other variables passed to include_directories()) is empty 
- [THRIFT-3958](https://issues.apache.org/jira/browse/THRIFT-3958) - CMake WITH_MT option for windows static runtime linking does not support the cmake build type RelWithDebInfo
- [THRIFT-3957](https://issues.apache.org/jira/browse/THRIFT-3957) - TConnectedClient does not disconnect from clients when their timeout is reached.
- [THRIFT-3953](https://issues.apache.org/jira/browse/THRIFT-3953) - TSSLSocket::close should handle exceptions from waitForEvent because it is called by the destructor.
- [THRIFT-3977](https://issues.apache.org/jira/browse/THRIFT-3977) - PHP extension creates undefined values when deserializing sets
- [THRIFT-3947](https://issues.apache.org/jira/browse/THRIFT-3947) - sockaddr type isn't always large enough for the return of getsockname
- [THRIFT-2755](https://issues.apache.org/jira/browse/THRIFT-2755) - ThreadSanitizer reports data race in ThreadManager::Impl::addWorker
- [THRIFT-3948](https://issues.apache.org/jira/browse/THRIFT-3948) - errno is not the correct method of getting the error in windows
- [THRIFT-4008](https://issues.apache.org/jira/browse/THRIFT-4008) - broken ci due to upstream dependency versioning break
- [THRIFT-3999](https://issues.apache.org/jira/browse/THRIFT-3999) - Fix Debian & Ubuntu package dependencies
- [THRIFT-3886](https://issues.apache.org/jira/browse/THRIFT-3886) - PHP cross test client returns 0 even when failing
- [THRIFT-3997](https://issues.apache.org/jira/browse/THRIFT-3997) - building thrift libs does not support new openssl

### Documentation
- [THRIFT-3867](https://issues.apache.org/jira/browse/THRIFT-3867) - Specify BinaryProtocol and CompactProtocol

### Epic
- [THRIFT-3049](https://issues.apache.org/jira/browse/THRIFT-3049) - As an iOS developer, I want a generator and library that produces Swift code
- [THRIFT-2336](https://issues.apache.org/jira/browse/THRIFT-2336) - UTF-8 sent by PHP as JSON is not understood by TJsonProtocol

### Improvement
- [THRIFT-1867](https://issues.apache.org/jira/browse/THRIFT-1867) - Python client/server should support client-side certificates.
- [THRIFT-1313](https://issues.apache.org/jira/browse/THRIFT-1313) - c_glib compact support
- [THRIFT-1385](https://issues.apache.org/jira/browse/THRIFT-1385) - make install doesn't install java library in the setted folder
- [THRIFT-1437](https://issues.apache.org/jira/browse/THRIFT-1437) - Update RPM spec
- [THRIFT-847](https://issues.apache.org/jira/browse/THRIFT-847) - Test Framework harmonization across all languages
- [THRIFT-819](https://issues.apache.org/jira/browse/THRIFT-819) - add Enumeration for protocol, transport and server types
- [THRIFT-3927](https://issues.apache.org/jira/browse/THRIFT-3927) - Emit an error instead of throw an error in the async callback
- [THRIFT-3931](https://issues.apache.org/jira/browse/THRIFT-3931) - TSimpleServer: If process request encounter UNKNOWN_METHOD, don't close transport.
- [THRIFT-3934](https://issues.apache.org/jira/browse/THRIFT-3934) - Automatically resolve OpenSSL binary version on Windows CI
- [THRIFT-3918](https://issues.apache.org/jira/browse/THRIFT-3918) - Run subset of make cross
- [THRIFT-3908](https://issues.apache.org/jira/browse/THRIFT-3908) - Remove redundant dependencies from Dockerfile
- [THRIFT-3907](https://issues.apache.org/jira/browse/THRIFT-3907) - Skip Docker image build on CI when unchanged
- [THRIFT-3868](https://issues.apache.org/jira/browse/THRIFT-3868) - Java struct equals should do identity check before field comparison
- [THRIFT-3849](https://issues.apache.org/jira/browse/THRIFT-3849) - Port Go serializer and deserializer to dart
- [THRIFT-2989](https://issues.apache.org/jira/browse/THRIFT-2989) - Complete CMake build for Apache Thrift
- [THRIFT-2980](https://issues.apache.org/jira/browse/THRIFT-2980) - ThriftMemoryBuffer doesn't have a constructor option to take an existing buffer
- [THRIFT-2856](https://issues.apache.org/jira/browse/THRIFT-2856) - refactor erlang basic transports and unify interfaces
- [THRIFT-2877](https://issues.apache.org/jira/browse/THRIFT-2877) - Optimize generated hashCode
- [THRIFT-2869](https://issues.apache.org/jira/browse/THRIFT-2869) - JSON: run schema validation from tests
- [THRIFT-3112](https://issues.apache.org/jira/browse/THRIFT-3112) - [Java] AsyncMethodCallback should be typed in generated AsyncIface
- [THRIFT-3263](https://issues.apache.org/jira/browse/THRIFT-3263) - PHP jsonSerialize() should cast scalar types
- [THRIFT-2905](https://issues.apache.org/jira/browse/THRIFT-2905) - Cocoa compiler should have option to produce "modern" Objective-C
- [THRIFT-2821](https://issues.apache.org/jira/browse/THRIFT-2821) - Enable the use of custom HTTP-Header in the Transport
- [THRIFT-2093](https://issues.apache.org/jira/browse/THRIFT-2093) - added the ability to set compression level in C++ zlib transport
- [THRIFT-2089](https://issues.apache.org/jira/browse/THRIFT-2089) - Compiler ignores duplicate typenames
- [THRIFT-2056](https://issues.apache.org/jira/browse/THRIFT-2056) - Moved all #include config.h statements to #include <thrift/config.h>
- [THRIFT-2031](https://issues.apache.org/jira/browse/THRIFT-2031) - Make SO_KEEPALIVE configurable for C++ lib
- [THRIFT-2021](https://issues.apache.org/jira/browse/THRIFT-2021) - Improve large binary protocol string performance
- [THRIFT-2028](https://issues.apache.org/jira/browse/THRIFT-2028) - Cleanup threading headers / libraries
- [THRIFT-2014](https://issues.apache.org/jira/browse/THRIFT-2014) - Change C++ lib includes to use <namespace/> style throughout
- [THRIFT-2312](https://issues.apache.org/jira/browse/THRIFT-2312) - travis.yml: build everything
- [THRIFT-1915](https://issues.apache.org/jira/browse/THRIFT-1915) - Multiplexing Services
- [THRIFT-1736](https://issues.apache.org/jira/browse/THRIFT-1736) - Visual Studio top level project files within msvc
- [THRIFT-1735](https://issues.apache.org/jira/browse/THRIFT-1735) - integrate tutorial into regular build
- [THRIFT-1533](https://issues.apache.org/jira/browse/THRIFT-1533) - Make TTransport should be Closeable
- [THRIFT-35](https://issues.apache.org/jira/browse/THRIFT-35) - Move language tests into their appropriate library directory
- [THRIFT-1079](https://issues.apache.org/jira/browse/THRIFT-1079) - Support i64 in AS3
- [THRIFT-1108](https://issues.apache.org/jira/browse/THRIFT-1108) - SSL support for the Ruby library 
- [THRIFT-3856](https://issues.apache.org/jira/browse/THRIFT-3856) - update debian package deependencies
- [THRIFT-3833](https://issues.apache.org/jira/browse/THRIFT-3833) - haxe http server implementation (by embeding into php web server)
- [THRIFT-3839](https://issues.apache.org/jira/browse/THRIFT-3839) - Performance issue with big message deserialization using php extension
- [THRIFT-3820](https://issues.apache.org/jira/browse/THRIFT-3820) - Erlang: Detect OTP >= 18 to use new time correction
- [THRIFT-3816](https://issues.apache.org/jira/browse/THRIFT-3816) - Reduce docker build duration on Travis-CI
- [THRIFT-3815](https://issues.apache.org/jira/browse/THRIFT-3815) - Put appveyor dependency versions to one place
- [THRIFT-3788](https://issues.apache.org/jira/browse/THRIFT-3788) - Compatibility improvements and Win64 support
- [THRIFT-3792](https://issues.apache.org/jira/browse/THRIFT-3792) - Timeouts for anonymous pipes should be configurable
- [THRIFT-3794](https://issues.apache.org/jira/browse/THRIFT-3794) - Split Delphi application, protocol and transport exception subtypes into separate exceptions
- [THRIFT-3774](https://issues.apache.org/jira/browse/THRIFT-3774) - The generated code should have exception_names meta info
- [THRIFT-3762](https://issues.apache.org/jira/browse/THRIFT-3762) - Fix build warnings for deprecated Thrift "byte" fields
- [THRIFT-3756](https://issues.apache.org/jira/browse/THRIFT-3756) - Improve requiredness documentation
- [THRIFT-3761](https://issues.apache.org/jira/browse/THRIFT-3761) - Add debian package for Python3
- [THRIFT-3742](https://issues.apache.org/jira/browse/THRIFT-3742) - haxe php cli support
- [THRIFT-3733](https://issues.apache.org/jira/browse/THRIFT-3733) - Socket timeout improvements
- [THRIFT-3728](https://issues.apache.org/jira/browse/THRIFT-3728) - http transport for thrift-lua
- [THRIFT-3905](https://issues.apache.org/jira/browse/THRIFT-3905) - Dart compiler does not initialize bool, int, and double properties
- [THRIFT-3911](https://issues.apache.org/jira/browse/THRIFT-3911) - Loosen Ruby dev dependency version requirements
- [THRIFT-3906](https://issues.apache.org/jira/browse/THRIFT-3906) - Run C# tests with make check
- [THRIFT-3900](https://issues.apache.org/jira/browse/THRIFT-3900) - Add Python SSL flags
- [THRIFT-3897](https://issues.apache.org/jira/browse/THRIFT-3897) - Provide meaningful exception type based on WebExceptionStatus in case of timeout
- [THRIFT-3808](https://issues.apache.org/jira/browse/THRIFT-3808) - Missing `DOUBLE` in thrift type enumeration
- [THRIFT-3803](https://issues.apache.org/jira/browse/THRIFT-3803) - Remove "file" attribute from XML generator
- [THRIFT-3660](https://issues.apache.org/jira/browse/THRIFT-3660) - Add V4 mapped address to test client cert's altname
- [THRIFT-3661](https://issues.apache.org/jira/browse/THRIFT-3661) - Use https to download meck in erlang test build
- [THRIFT-3659](https://issues.apache.org/jira/browse/THRIFT-3659) - Check configure result of CMake on CI
- [THRIFT-3667](https://issues.apache.org/jira/browse/THRIFT-3667) - Add TLS SNI support to clients
- [THRIFT-3651](https://issues.apache.org/jira/browse/THRIFT-3651) - Make backports.match_hostname and ipaddress optional
- [THRIFT-3666](https://issues.apache.org/jira/browse/THRIFT-3666) - Build D tutorial as part of Autotools build
- [THRIFT-3665](https://issues.apache.org/jira/browse/THRIFT-3665) - Add D libevent and OpenSSL to docker images
- [THRIFT-3664](https://issues.apache.org/jira/browse/THRIFT-3664) - Remove md5.c
- [THRIFT-3662](https://issues.apache.org/jira/browse/THRIFT-3662) - Add Haskell to debian docker image
- [THRIFT-3711](https://issues.apache.org/jira/browse/THRIFT-3711) - Add D to cross language test
- [THRIFT-3691](https://issues.apache.org/jira/browse/THRIFT-3691) - Run flake8 Python style check on Travis-CI
- [THRIFT-3692](https://issues.apache.org/jira/browse/THRIFT-3692) - (Re)enable Appveyor C++ and Python build
- [THRIFT-3677](https://issues.apache.org/jira/browse/THRIFT-3677) - Improve CMake Java build
- [THRIFT-3679](https://issues.apache.org/jira/browse/THRIFT-3679) - Add stdout log to testBinary in Java test server
- [THRIFT-3718](https://issues.apache.org/jira/browse/THRIFT-3718) - Reduce size of docker image for build environment
- [THRIFT-3698](https://issues.apache.org/jira/browse/THRIFT-3698) - [Travis-CI] Introduce retry to apt commands
- [THRIFT-3127](https://issues.apache.org/jira/browse/THRIFT-3127) - switch -recurse to --recurse and reserve -r
- [THRIFT-3087](https://issues.apache.org/jira/browse/THRIFT-3087) - Pass on errors like "connection closed"
- [THRIFT-3240](https://issues.apache.org/jira/browse/THRIFT-3240) - Thrift Python client should support subjectAltName and wildcard certs in TSSLSocket
- [THRIFT-3213](https://issues.apache.org/jira/browse/THRIFT-3213) - make cross should indicate when it skips a known failing test
- [THRIFT-3208](https://issues.apache.org/jira/browse/THRIFT-3208) - Fix Visual Studio solution build failure due to missing source
- [THRIFT-3186](https://issues.apache.org/jira/browse/THRIFT-3186) - Add TServerHTTP to Go library
- [THRIFT-2342](https://issues.apache.org/jira/browse/THRIFT-2342) - Add __FILE__ and __LINE__ to Thrift C++ excpetions
- [THRIFT-3372](https://issues.apache.org/jira/browse/THRIFT-3372) - Add dart generator to Visual Studio project
- [THRIFT-3366](https://issues.apache.org/jira/browse/THRIFT-3366) - ThriftTest to implement standard return values 
- [THRIFT-3402](https://issues.apache.org/jira/browse/THRIFT-3402) - Provide a perl Unix Socket implementation
- [THRIFT-3361](https://issues.apache.org/jira/browse/THRIFT-3361) - Improve C# library
- [THRIFT-3393](https://issues.apache.org/jira/browse/THRIFT-3393) - Introduce i8 to provide consistent set of Thrift IDL integer types
- [THRIFT-3339](https://issues.apache.org/jira/browse/THRIFT-3339) - Support for database/sql
- [THRIFT-3565](https://issues.apache.org/jira/browse/THRIFT-3565) - C++: T[Async]Processor::getEventHandler() should be declared as const member functions
- [THRIFT-3563](https://issues.apache.org/jira/browse/THRIFT-3563) - C++/Qt: removed usage of macro QT_PREPEND_NAMESPACE as it isn't consequently used for all references to Qt types.
- [THRIFT-3562](https://issues.apache.org/jira/browse/THRIFT-3562) - Removed unused TAsyncProcessor::getAsyncServer()
- [THRIFT-3561](https://issues.apache.org/jira/browse/THRIFT-3561) - C++/Qt: make use of Q_DISABLE_COPY() to get rid of copy ctor and assignment operator
- [THRIFT-3556](https://issues.apache.org/jira/browse/THRIFT-3556) - c_glib file descriptor transport
- [THRIFT-3544](https://issues.apache.org/jira/browse/THRIFT-3544) - Make cross test fail when server process died unexpectedly
- [THRIFT-3540](https://issues.apache.org/jira/browse/THRIFT-3540) - Make python tutorial more in line with PEP8
- [THRIFT-3535](https://issues.apache.org/jira/browse/THRIFT-3535) - Dart generator argument to produce a file structure usable in parent library
- [THRIFT-3505](https://issues.apache.org/jira/browse/THRIFT-3505) - Enhance Python TSSLSocket
- [THRIFT-3506](https://issues.apache.org/jira/browse/THRIFT-3506) - Eliminate old style classes from library code
- [THRIFT-3503](https://issues.apache.org/jira/browse/THRIFT-3503) - Enable py:utf8string by default
- [THRIFT-3499](https://issues.apache.org/jira/browse/THRIFT-3499) - Add package_prefix to python generator
- [THRIFT-3495](https://issues.apache.org/jira/browse/THRIFT-3495) - Minor enhancements and fixes for cross test
- [THRIFT-3486](https://issues.apache.org/jira/browse/THRIFT-3486) - Java generated `getFieldValue` is incompatible with `setFieldValue` for binary values.
- [THRIFT-3484](https://issues.apache.org/jira/browse/THRIFT-3484) - Consolidate temporary buffers in Java's TCompactProtocol
- [THRIFT-3516](https://issues.apache.org/jira/browse/THRIFT-3516) - Add feature test for THeader TBinaryProtocol interop
- [THRIFT-3515](https://issues.apache.org/jira/browse/THRIFT-3515) - Python 2.6 compatibility and test on CI
- [THRIFT-3514](https://issues.apache.org/jira/browse/THRIFT-3514) - PHP 7 compatible version of binary protocol
- [THRIFT-3469](https://issues.apache.org/jira/browse/THRIFT-3469) - Docker: Debian support
- [THRIFT-3416](https://issues.apache.org/jira/browse/THRIFT-3416) - Retire old "xxx_namespace" declarations from the IDL
- [THRIFT-3426](https://issues.apache.org/jira/browse/THRIFT-3426) - Align autogen comment in XSD
- [THRIFT-3424](https://issues.apache.org/jira/browse/THRIFT-3424) - Add CMake android build option
- [THRIFT-3439](https://issues.apache.org/jira/browse/THRIFT-3439) - Run make cross using Python3 when available
- [THRIFT-3440](https://issues.apache.org/jira/browse/THRIFT-3440) - Python make check takes too much time
- [THRIFT-3441](https://issues.apache.org/jira/browse/THRIFT-3441) - Stabilize Travis-CI builds
- [THRIFT-3431](https://issues.apache.org/jira/browse/THRIFT-3431) - Avoid "schemes" HashMap lookups during struct reads/writes
- [THRIFT-3432](https://issues.apache.org/jira/browse/THRIFT-3432) - Add a TByteBuffer transport to the Java library
- [THRIFT-3438](https://issues.apache.org/jira/browse/THRIFT-3438) - Enable py:new_style by default
- [THRIFT-3405](https://issues.apache.org/jira/browse/THRIFT-3405) - Go THttpClient misuses http.Client objects
- [THRIFT-3614](https://issues.apache.org/jira/browse/THRIFT-3614) - Improve logging of test_sslsocket.py
- [THRIFT-3647](https://issues.apache.org/jira/browse/THRIFT-3647) - Fix php extension build warnings
- [THRIFT-3642](https://issues.apache.org/jira/browse/THRIFT-3642) - Speed up cross test runner
- [THRIFT-3637](https://issues.apache.org/jira/browse/THRIFT-3637) - Implement compact protocol for dart
- [THRIFT-3613](https://issues.apache.org/jira/browse/THRIFT-3613) - Port Python C extension to Python 3
- [THRIFT-3612](https://issues.apache.org/jira/browse/THRIFT-3612) - Add Python C extension for compact protocol
- [THRIFT-3611](https://issues.apache.org/jira/browse/THRIFT-3611) - Add --regex filter to cross test runner
- [THRIFT-3631](https://issues.apache.org/jira/browse/THRIFT-3631) - JSON protocol implementation for Lua
- [THRIFT-3609](https://issues.apache.org/jira/browse/THRIFT-3609) - Remove or replace TestPortFixture.h
- [THRIFT-3605](https://issues.apache.org/jira/browse/THRIFT-3605) - Have the compiler complain about invalid arguments and options
- [THRIFT-3596](https://issues.apache.org/jira/browse/THRIFT-3596) - Better conformance to PEP8
- [THRIFT-3585](https://issues.apache.org/jira/browse/THRIFT-3585) - Compact protocol implementation for Lua
- [THRIFT-3582](https://issues.apache.org/jira/browse/THRIFT-3582) - Erlang libraries should have service metadata
- [THRIFT-3579](https://issues.apache.org/jira/browse/THRIFT-3579) - Introduce retry to make cross
- [THRIFT-3306](https://issues.apache.org/jira/browse/THRIFT-3306) - Java: TBinaryProtocol: Use 1 temp buffer instead of allocating 8
- [THRIFT-3910](https://issues.apache.org/jira/browse/THRIFT-3910) - Do not invoke pip as part of build process
- [THRIFT-1857](https://issues.apache.org/jira/browse/THRIFT-1857) - Python 3.X Support
- [THRIFT-1944](https://issues.apache.org/jira/browse/THRIFT-1944) - Binding to zero port
- [THRIFT-3954](https://issues.apache.org/jira/browse/THRIFT-3954) - Enable the usage of structs called "Object" in Java
- [THRIFT-3981](https://issues.apache.org/jira/browse/THRIFT-3981) - Enable analyzer strong mode in Dart library
- [THRIFT-3998](https://issues.apache.org/jira/browse/THRIFT-3998) - Document ability to add custom tags to thrift structs
- [THRIFT-4006](https://issues.apache.org/jira/browse/THRIFT-4006) - Add a removeEventListener method on TSocket

### New Feature
- [THRIFT-640](https://issues.apache.org/jira/browse/THRIFT-640) - Support deprecation
- [THRIFT-948](https://issues.apache.org/jira/browse/THRIFT-948) - SSL socket support for PHP
- [THRIFT-764](https://issues.apache.org/jira/browse/THRIFT-764) - add Support for Vala language
- [THRIFT-3046](https://issues.apache.org/jira/browse/THRIFT-3046) - Allow PSR4 class loading for generated classes (PHP)
- [THRIFT-2113](https://issues.apache.org/jira/browse/THRIFT-2113) - Erlang SSL Socket Support
- [THRIFT-1482](https://issues.apache.org/jira/browse/THRIFT-1482) - Unix domain socket support under PHP
- [THRIFT-519](https://issues.apache.org/jira/browse/THRIFT-519) - Support collections of types without having to explicitly define it
- [THRIFT-468](https://issues.apache.org/jira/browse/THRIFT-468) - Rack Middleware Application for Rails
- [THRIFT-1708](https://issues.apache.org/jira/browse/THRIFT-1708) - Add event handlers for processor events
- [THRIFT-3834](https://issues.apache.org/jira/browse/THRIFT-3834) - Erlang namespacing and exception metadata
- [THRIFT-2510](https://issues.apache.org/jira/browse/THRIFT-2510) - Implement TNonblockingServer's ability to listen on unix domain sockets
- [THRIFT-3397](https://issues.apache.org/jira/browse/THRIFT-3397) - Implement TProcessorFactory in C# to enable per-client processors
- [THRIFT-3523](https://issues.apache.org/jira/browse/THRIFT-3523) - XML Generator
- [THRIFT-3510](https://issues.apache.org/jira/browse/THRIFT-3510) - Add HttpTaskAsyncHandler implementation
- [THRIFT-3318](https://issues.apache.org/jira/browse/THRIFT-3318) - PHP: SimpleJSONProtocol Implementation
- [THRIFT-3299](https://issues.apache.org/jira/browse/THRIFT-3299) - Dart language bindings in Thrift
- [THRIFT-2835](https://issues.apache.org/jira/browse/THRIFT-2835) - Add possibility to distribute generators separately from thrift core, and load them dynamically
- [THRIFT-184](https://issues.apache.org/jira/browse/THRIFT-184) - Add OSGi Manifest headers to the libthrift java library to be able to use Thrift in the OSGi runtime
- [THRIFT-141](https://issues.apache.org/jira/browse/THRIFT-141) - If a required field is not present on serialization, throw an exception
- [THRIFT-1891](https://issues.apache.org/jira/browse/THRIFT-1891) - Add Windows ALPC transport which is right counterpart of Unix domain sockets

### Question
- [THRIFT-1808](https://issues.apache.org/jira/browse/THRIFT-1808) - The Thrift struct should be considered self-contained?
- [THRIFT-2895](https://issues.apache.org/jira/browse/THRIFT-2895) - Tutorial cpp
- [THRIFT-3860](https://issues.apache.org/jira/browse/THRIFT-3860) - Elephant-bird application Test fails for Thrift
- [THRIFT-3811](https://issues.apache.org/jira/browse/THRIFT-3811) - HTTPS Support for C++ applications
- [THRIFT-3509](https://issues.apache.org/jira/browse/THRIFT-3509) - "make check" error

### Story
- [THRIFT-3452](https://issues.apache.org/jira/browse/THRIFT-3452) - .travis.yml: Migrating from legacy to container-based infrastructure

### Sub-task
- [THRIFT-1811](https://issues.apache.org/jira/browse/THRIFT-1811) - ruby tutorial as part of the regular build
- [THRIFT-2779](https://issues.apache.org/jira/browse/THRIFT-2779) - PHP TJSONProtocol encode unicode into UCS-4LE which can't be parsed by other language bindings
- [THRIFT-2110](https://issues.apache.org/jira/browse/THRIFT-2110) - Erlang: Support for Multiplexing Services on any Transport, Protocol and Server
- [THRIFT-3852](https://issues.apache.org/jira/browse/THRIFT-3852) - A Travis-CI job fails with "write error"
- [THRIFT-3740](https://issues.apache.org/jira/browse/THRIFT-3740) - Fix haxelib.json classpath 
- [THRIFT-3653](https://issues.apache.org/jira/browse/THRIFT-3653) - incorrect union serialization
- [THRIFT-3652](https://issues.apache.org/jira/browse/THRIFT-3652) - incorrect serialization of optionals
- [THRIFT-3655](https://issues.apache.org/jira/browse/THRIFT-3655) - incorrect union serialization
- [THRIFT-3654](https://issues.apache.org/jira/browse/THRIFT-3654) - incorrect serialization of optionals
- [THRIFT-3656](https://issues.apache.org/jira/browse/THRIFT-3656) - incorrect serialization of optionals
- [THRIFT-3699](https://issues.apache.org/jira/browse/THRIFT-3699) - Fix integer limit symbol includes in Python C extension
- [THRIFT-3693](https://issues.apache.org/jira/browse/THRIFT-3693) - Fix include issue in C++ TSSLSocketInterruptTest on Windows
- [THRIFT-3694](https://issues.apache.org/jira/browse/THRIFT-3694) - [Windows] Disable tests of a few servers that are not supported
- [THRIFT-3696](https://issues.apache.org/jira/browse/THRIFT-3696) - Install pip to CentOS Docker images to fix Python builds
- [THRIFT-3638](https://issues.apache.org/jira/browse/THRIFT-3638) - Fix haxelib.json
- [THRIFT-3251](https://issues.apache.org/jira/browse/THRIFT-3251) - Add http transport for server to Go lib
- [THRIFT-2424](https://issues.apache.org/jira/browse/THRIFT-2424) - Recursive Types
- [THRIFT-2423](https://issues.apache.org/jira/browse/THRIFT-2423) - THeader
- [THRIFT-2413](https://issues.apache.org/jira/browse/THRIFT-2413) - Python: UTF-8 sent by PHP as JSON is not understood by TJsonProtocol
- [THRIFT-2409](https://issues.apache.org/jira/browse/THRIFT-2409) - Java: UTF-8 sent by PHP as JSON is not understood by TJsonProtocol
- [THRIFT-2412](https://issues.apache.org/jira/browse/THRIFT-2412) - D: UTF-8 sent by PHP as JSON is not understood by TJsonProtocol
- [THRIFT-2411](https://issues.apache.org/jira/browse/THRIFT-2411) - C++: UTF-8 sent by PHP as JSON is not understood by TJsonProtocol
- [THRIFT-2410](https://issues.apache.org/jira/browse/THRIFT-2410) - JavaMe: UTF-8 sent by PHP as JSON is not understood by TJsonProtocol
- [THRIFT-2668](https://issues.apache.org/jira/browse/THRIFT-2668) - TestSuite: detailed result on passed tests by feature
- [THRIFT-2659](https://issues.apache.org/jira/browse/THRIFT-2659) - python Test Server fails when throwing TException
- [THRIFT-3398](https://issues.apache.org/jira/browse/THRIFT-3398) - Add CMake build  for Haskell library and tests
- [THRIFT-3396](https://issues.apache.org/jira/browse/THRIFT-3396) - DART: UTF-8 sent by PHP as JSON is not understood by TJsonProtocol
- [THRIFT-3364](https://issues.apache.org/jira/browse/THRIFT-3364) -   Fix ruby binary field encoding in TJSONProtocol
- [THRIFT-3381](https://issues.apache.org/jira/browse/THRIFT-3381) - Fix for misc. codegen issues with THRIFT-2905
- [THRIFT-3573](https://issues.apache.org/jira/browse/THRIFT-3573) - No rule to make target `../../../test/c_glib/src/.deps/testthrifttest-thrift_test_handler.Po'.
- [THRIFT-3572](https://issues.apache.org/jira/browse/THRIFT-3572) - "Unable to determine the behavior of a signed right shift"
- [THRIFT-3542](https://issues.apache.org/jira/browse/THRIFT-3542) - Add length limit support to Java test server
- [THRIFT-3537](https://issues.apache.org/jira/browse/THRIFT-3537) - Remove the (now obsolete) csharp:asyncctp flag 
- [THRIFT-3532](https://issues.apache.org/jira/browse/THRIFT-3532) - Add configurable string and container read size limit to Python protocols
- [THRIFT-3531](https://issues.apache.org/jira/browse/THRIFT-3531) - Create cross lang feature test for string and container read length limit
- [THRIFT-3482](https://issues.apache.org/jira/browse/THRIFT-3482) - Haskell JSON protocol does not encode binary field as Base64
- [THRIFT-3425](https://issues.apache.org/jira/browse/THRIFT-3425) - Minor fixes + simplification for CentOS Dockerfile
- [THRIFT-3442](https://issues.apache.org/jira/browse/THRIFT-3442) - Run CMake tests on Appveyor
- [THRIFT-3409](https://issues.apache.org/jira/browse/THRIFT-3409) - NodeJS binary field issues
- [THRIFT-3621](https://issues.apache.org/jira/browse/THRIFT-3621) - Fix lib/cpp/test/SecurityTest.cpp to use ephemeral ports
- [THRIFT-3628](https://issues.apache.org/jira/browse/THRIFT-3628) - Fix lib/cpp/test/TServerIntegrationTest.cpp to use ephemeral ports
- [THRIFT-3625](https://issues.apache.org/jira/browse/THRIFT-3625) - Kill unused #include "TestPortFixture.h" in lib/cpp/test/TServerTransportTest.cpp.
- [THRIFT-3646](https://issues.apache.org/jira/browse/THRIFT-3646) - Fix Python extension build warnings
- [THRIFT-3626](https://issues.apache.org/jira/browse/THRIFT-3626) - Fix lib/cpp/test/TSocketInterruptTest.cpp to use ephemeral ports.
- [THRIFT-3624](https://issues.apache.org/jira/browse/THRIFT-3624) - Fix lib/cpp/test/TServerSocketTest.cpp to use ephemeral ports
- [THRIFT-3623](https://issues.apache.org/jira/browse/THRIFT-3623) - Fix Fix cpp/lib/test/TSSLSocketInterruptTest.cpp to use ephemeral ports
- [THRIFT-3592](https://issues.apache.org/jira/browse/THRIFT-3592) - Add basic test client
- [THRIFT-3980](https://issues.apache.org/jira/browse/THRIFT-3980) - add TExtendedBinaryProtocol.java

### Task
- [THRIFT-1801](https://issues.apache.org/jira/browse/THRIFT-1801) - Sync up TApplicationException codes across languages and thrift implementations
- [THRIFT-1259](https://issues.apache.org/jira/browse/THRIFT-1259) - Automate versioning 

### Test
- [THRIFT-3400](https://issues.apache.org/jira/browse/THRIFT-3400) - Add Erlang to cross test
- [THRIFT-3504](https://issues.apache.org/jira/browse/THRIFT-3504) - Fix FastbinaryTest.py

### Wish
- [THRIFT-3923](https://issues.apache.org/jira/browse/THRIFT-3923) - Maybe remove Aereo from the "Powered by" list
- [THRIFT-2149](https://issues.apache.org/jira/browse/THRIFT-2149) - Add an option to disable the generation of default operators

## 0.9.3.1

Released March 13, 2019 to backport a CVE fix to the popular 0.9.3 release.

### Bug
- [THRIFT-4506](https://issues.apache.org/jira/browse/THRIFT-4506) - CVE-2018-1320 for Java SASL backported from 0.12.0

## 0.9.3

### Bug
- [THRIFT-2441](https://issues.apache.org/jira/browse/THRIFT-2441) - Cannot shutdown TThreadedServer when clients are still connected
- [THRIFT-2465](https://issues.apache.org/jira/browse/THRIFT-2465) - TBinaryProtocolT breaks if copied/moved
- [THRIFT-2474](https://issues.apache.org/jira/browse/THRIFT-2474) - thrift.h causes a compile failure
- [THRIFT-2540](https://issues.apache.org/jira/browse/THRIFT-2540) - Running configure from outside the source directory fails
- [THRIFT-2598](https://issues.apache.org/jira/browse/THRIFT-2598) - Add check for minimum Go version to configure.ac
- [THRIFT-2647](https://issues.apache.org/jira/browse/THRIFT-2647) - compiler-hs: don't decapitalize field names, do decapitalize argument bindings
- [THRIFT-2773](https://issues.apache.org/jira/browse/THRIFT-2773) - Generated Java code for 'oneway' methods is incorrect.
- [THRIFT-2789](https://issues.apache.org/jira/browse/THRIFT-2789) - TNonblockingServer leaks socket FD's under load
- [THRIFT-2682](https://issues.apache.org/jira/browse/THRIFT-2682) - TThreadedServer leaks per-thread memory
- [THRIFT-2674](https://issues.apache.org/jira/browse/THRIFT-2674) - JavaScript: declare Accept: and Content-Type: in request
- [THRIFT-3078](https://issues.apache.org/jira/browse/THRIFT-3078) - TNonblockingServerSocket's logger is not named after TNonblockingServerSocket
- [THRIFT-3077](https://issues.apache.org/jira/browse/THRIFT-3077) - C++ TFileTransport ignores return code from ftruncate
- [THRIFT-3067](https://issues.apache.org/jira/browse/THRIFT-3067) - C++ cppcheck performance related warnings
- [THRIFT-3066](https://issues.apache.org/jira/browse/THRIFT-3066) - C++ TDenseProtocol assert modifies instead of checks
- [THRIFT-3071](https://issues.apache.org/jira/browse/THRIFT-3071) - bootstrap.sh on Ubuntu 12.04 (Precise) automake error
- [THRIFT-3069](https://issues.apache.org/jira/browse/THRIFT-3069) - C++ TServerSocket leaks socket on fcntl get or set flags error
- [THRIFT-3079](https://issues.apache.org/jira/browse/THRIFT-3079) - TNonblockingServerSocket's logger is not named after TNonblockingServerSocket
- [THRIFT-3080](https://issues.apache.org/jira/browse/THRIFT-3080) - C++ TNonblockingServer connection leak while accept huge number connections.
- [THRIFT-3086](https://issues.apache.org/jira/browse/THRIFT-3086) - C++ Valgrind Error Cleanup
- [THRIFT-3085](https://issues.apache.org/jira/browse/THRIFT-3085) - thrift_reconnecting_client never try to reconnect
- [THRIFT-3123](https://issues.apache.org/jira/browse/THRIFT-3123) - Missing include in compiler/cpp/src/main.h breaks build in some environments
- [THRIFT-3125](https://issues.apache.org/jira/browse/THRIFT-3125) - Fix the list of exported headers in automake input
- [THRIFT-3126](https://issues.apache.org/jira/browse/THRIFT-3126) - PHP JSON serializer converts empty or int-indexed maps to lists
- [THRIFT-3132](https://issues.apache.org/jira/browse/THRIFT-3132) - Properly format date in Java @Generated annotations
- [THRIFT-3137](https://issues.apache.org/jira/browse/THRIFT-3137) - Travis build hangs after failure
- [THRIFT-3138](https://issues.apache.org/jira/browse/THRIFT-3138) - "make check" parallel execution is underministic
- [THRIFT-3139](https://issues.apache.org/jira/browse/THRIFT-3139) - JS library test is flaky
- [THRIFT-3140](https://issues.apache.org/jira/browse/THRIFT-3140) - ConcurrentModificationException is thrown by JavaScript test server
- [THRIFT-3124](https://issues.apache.org/jira/browse/THRIFT-3124) - Some signed/unsigned warnings while building compiler
- [THRIFT-3128](https://issues.apache.org/jira/browse/THRIFT-3128) - Go generated code produces name collisions between services
- [THRIFT-3146](https://issues.apache.org/jira/browse/THRIFT-3146) - Graphviz generates function name collisions between services
- [THRIFT-3147](https://issues.apache.org/jira/browse/THRIFT-3147) - Segfault while receiving data
- [THRIFT-3148](https://issues.apache.org/jira/browse/THRIFT-3148) - Markdown links to coding_standards are dead
- [THRIFT-3090](https://issues.apache.org/jira/browse/THRIFT-3090) - cmake build is broken on MacOSX
- [THRIFT-3097](https://issues.apache.org/jira/browse/THRIFT-3097) - cmake targets unconditionally depend on optional libraries
- [THRIFT-3094](https://issues.apache.org/jira/browse/THRIFT-3094) - master as of 2015-APR-13 fails -DBOOST_THREADS cmake build
- [THRIFT-3099](https://issues.apache.org/jira/browse/THRIFT-3099) - cmake build is broken on FreeBSD
- [THRIFT-3089](https://issues.apache.org/jira/browse/THRIFT-3089) - Assigning default ENUM values results in non-compilable java code if java namespace is not defined
- [THRIFT-3093](https://issues.apache.org/jira/browse/THRIFT-3093) - mingw compile fixes for c++ library 0.9.2
- [THRIFT-3098](https://issues.apache.org/jira/browse/THRIFT-3098) - Thrift does not pretty print binary typedefs the way it does binary fields
- [THRIFT-3091](https://issues.apache.org/jira/browse/THRIFT-3091) - c_glib service method should return result from handler method
- [THRIFT-3088](https://issues.apache.org/jira/browse/THRIFT-3088) - TThreadPoolServer with Sasl auth may leak CLOSE_WAIT socket
- [THRIFT-3109](https://issues.apache.org/jira/browse/THRIFT-3109) - Cross test log file cannot be browsed when served in HTTP server
- [THRIFT-3113](https://issues.apache.org/jira/browse/THRIFT-3113) - m4 C++11 macro issue
- [THRIFT-3105](https://issues.apache.org/jira/browse/THRIFT-3105) - C++ libthriftnb library on Windows build failure
- [THRIFT-3115](https://issues.apache.org/jira/browse/THRIFT-3115) - Uncompileable code due to name collision with predefined used types
- [THRIFT-3117](https://issues.apache.org/jira/browse/THRIFT-3117) - Java TSSLTransportFactory can't load certificates within JAR archive
- [THRIFT-3102](https://issues.apache.org/jira/browse/THRIFT-3102) - could not make check for Go Library
- [THRIFT-3120](https://issues.apache.org/jira/browse/THRIFT-3120) - Minor spelling errors and an outdated URL
- [THRIFT-3121](https://issues.apache.org/jira/browse/THRIFT-3121) - Librt does not exist on OS X
- [THRIFT-3152](https://issues.apache.org/jira/browse/THRIFT-3152) - Compiler error on Mac OSX (missing #include <cstdlib>)
- [THRIFT-3162](https://issues.apache.org/jira/browse/THRIFT-3162) - make fails for dmd 2.067
- [THRIFT-3164](https://issues.apache.org/jira/browse/THRIFT-3164) - Thrift C++ library SSL socket by default allows for unsecure SSLv3 negotiation
- [THRIFT-3168](https://issues.apache.org/jira/browse/THRIFT-3168) - Fix Maven POM
- [THRIFT-3170](https://issues.apache.org/jira/browse/THRIFT-3170) - Initialism code in the Go compiler causes chaos
- [THRIFT-3169](https://issues.apache.org/jira/browse/THRIFT-3169) - Do not export thrift.TestStruct and thrift.TestEnum in thrift Go library 
- [THRIFT-3191](https://issues.apache.org/jira/browse/THRIFT-3191) - Perl compiler does not add support for unexpected exception handling
- [THRIFT-3178](https://issues.apache.org/jira/browse/THRIFT-3178) - glib C does not compile
- [THRIFT-3189](https://issues.apache.org/jira/browse/THRIFT-3189) - Perl ServerSocket should allow a specific interface to be listened to
- [THRIFT-3252](https://issues.apache.org/jira/browse/THRIFT-3252) - Missing TConcurrentClientSyncInfo.h in cpp Makefile, so doesn't install
- [THRIFT-3255](https://issues.apache.org/jira/browse/THRIFT-3255) - Thrift generator doesn't exclude 'package' keyword for thrift property names breaking java builds
- [THRIFT-3260](https://issues.apache.org/jira/browse/THRIFT-3260) - multiple warnings in c_glib tutorial
- [THRIFT-3256](https://issues.apache.org/jira/browse/THRIFT-3256) - Some D test timings are too aggressive for slow machines
- [THRIFT-3257](https://issues.apache.org/jira/browse/THRIFT-3257) - warning: extra tokens at end of #endif directive
- [THRIFT-3184](https://issues.apache.org/jira/browse/THRIFT-3184) - Thrift Go leaves file descriptors open
- [THRIFT-3203](https://issues.apache.org/jira/browse/THRIFT-3203) - DOAP - please fix "Ocaml" => "OCaml"
- [THRIFT-3210](https://issues.apache.org/jira/browse/THRIFT-3210) - (uncompileable) code generated for server events while are events not enabled
- [THRIFT-3215](https://issues.apache.org/jira/browse/THRIFT-3215) - TJSONProtocol '(c++) uses "throw new" to throw exceptions instead of "throw"
- [THRIFT-3202](https://issues.apache.org/jira/browse/THRIFT-3202) - Allow HSHAServer to configure min and max worker threads separately.
- [THRIFT-3205](https://issues.apache.org/jira/browse/THRIFT-3205) - TCompactProtocol return a wrong error when the io.EOF happens
- [THRIFT-3209](https://issues.apache.org/jira/browse/THRIFT-3209) - LGPL mentioned in license file
- [THRIFT-3197](https://issues.apache.org/jira/browse/THRIFT-3197) - keepAliveTime is hard coded as 60 sec in TThreadPoolServer
- [THRIFT-3196](https://issues.apache.org/jira/browse/THRIFT-3196) - Misspelling in lua TBinaryProtocol (stirctWrite => strictWrite)
- [THRIFT-3198](https://issues.apache.org/jira/browse/THRIFT-3198) - Allow construction of TTransportFactory with a specified maxLength
- [THRIFT-3192](https://issues.apache.org/jira/browse/THRIFT-3192) - Go import paths changed in 1.4, and expired June 1
- [THRIFT-3271](https://issues.apache.org/jira/browse/THRIFT-3271) - Could not find or load main class configtest_ax_javac_and_java on some non-english systems
- [THRIFT-3273](https://issues.apache.org/jira/browse/THRIFT-3273) - c_glib: Generated code tries to convert between function and void pointers
- [THRIFT-3264](https://issues.apache.org/jira/browse/THRIFT-3264) - Fix Erlang 16 namespaced types
- [THRIFT-3270](https://issues.apache.org/jira/browse/THRIFT-3270) - reusing TNonblockingServer::TConnection cause dirty TSocket
- [THRIFT-3267](https://issues.apache.org/jira/browse/THRIFT-3267) - c_glib: "Critical" failure during unit tests
- [THRIFT-3277](https://issues.apache.org/jira/browse/THRIFT-3277) - THttpClient leaks connections if it's used for multiple requests
- [THRIFT-3278](https://issues.apache.org/jira/browse/THRIFT-3278) - NodeJS: Fix exception stack traces and names
- [THRIFT-3279](https://issues.apache.org/jira/browse/THRIFT-3279) - Fix a bug in retry_max_delay (NodeJS)
- [THRIFT-3280](https://issues.apache.org/jira/browse/THRIFT-3280) - Initialize retry variables on construction
- [THRIFT-3283](https://issues.apache.org/jira/browse/THRIFT-3283) - c_glib: Tutorial server always exits with warning
- [THRIFT-3284](https://issues.apache.org/jira/browse/THRIFT-3284) - c_glib: Empty service produces unused-variable warning
- [THRIFT-1925](https://issues.apache.org/jira/browse/THRIFT-1925) - c_glib generated code does not compile
- [THRIFT-1849](https://issues.apache.org/jira/browse/THRIFT-1849) - after transport->open() opens isOpen returns true and next open() goes thru when it shall not
- [THRIFT-1866](https://issues.apache.org/jira/browse/THRIFT-1866) - java compiler generates non-compiling code with const's defined in a thrift when name includes non-identifier chars
- [THRIFT-1938](https://issues.apache.org/jira/browse/THRIFT-1938) - FunctionRunner.h -- uses wrong path for Thread.h when installed
- [THRIFT-1844](https://issues.apache.org/jira/browse/THRIFT-1844) - Password string not cleared
- [THRIFT-2004](https://issues.apache.org/jira/browse/THRIFT-2004) - Thrift::Union violates :== method contract and crashes
- [THRIFT-2073](https://issues.apache.org/jira/browse/THRIFT-2073) - Thrift C++ THttpClient error: cannot refill buffer
- [THRIFT-2127](https://issues.apache.org/jira/browse/THRIFT-2127) - Autoconf scripting does not properly account for cross-compile
- [THRIFT-2180](https://issues.apache.org/jira/browse/THRIFT-2180) - Integer types issues in Cocoa lib on ARM64
- [THRIFT-2189](https://issues.apache.org/jira/browse/THRIFT-2189) - Go needs "isset" to fully support "union" type (and optionals)
- [THRIFT-2192](https://issues.apache.org/jira/browse/THRIFT-2192) - autotools on Redhat based systems
- [THRIFT-2546](https://issues.apache.org/jira/browse/THRIFT-2546) - cross language tests fails at 'TestMultiException' when using nodejs server
- [THRIFT-2547](https://issues.apache.org/jira/browse/THRIFT-2547) - nodejs servers and clients fails to connect with cpp using compact protocol
- [THRIFT-2548](https://issues.apache.org/jira/browse/THRIFT-2548) - Nodejs servers and clients does not work properly with  -ssl
- [THRIFT-1471](https://issues.apache.org/jira/browse/THRIFT-1471) - toString() does not print ByteBuffer values when nested in a List
- [THRIFT-1201](https://issues.apache.org/jira/browse/THRIFT-1201) - getaddrinfo resource leak
- [THRIFT-615](https://issues.apache.org/jira/browse/THRIFT-615) - TThreadPoolServer doesn't call task_done after pulling tasks from it's clients queue
- [THRIFT-162](https://issues.apache.org/jira/browse/THRIFT-162) - Thrift structures are unhashable, preventing them from being used as set elements
- [THRIFT-810](https://issues.apache.org/jira/browse/THRIFT-810) - Crashed client on TSocket::close under loads
- [THRIFT-557](https://issues.apache.org/jira/browse/THRIFT-557) - charset problem with file Autogenerated by Thrift
- [THRIFT-233](https://issues.apache.org/jira/browse/THRIFT-233) - IDL doesn't support negative hex literals
- [THRIFT-1649](https://issues.apache.org/jira/browse/THRIFT-1649) - contrib/zeromq does not build in 0.8.0
- [THRIFT-1642](https://issues.apache.org/jira/browse/THRIFT-1642) - Miscalculation lead to throw unexpected "TTransportException::TIMED_OUT"(or called "EAGAIN (timed out)") exception
- [THRIFT-1587](https://issues.apache.org/jira/browse/THRIFT-1587) - TSocket::setRecvTimeout error
- [THRIFT-1248](https://issues.apache.org/jira/browse/THRIFT-1248) - pointer subtraction in TMemoryBuffer relies on undefined behavior
- [THRIFT-1774](https://issues.apache.org/jira/browse/THRIFT-1774) - Sasl Transport client would hang when trying to connect non-sasl transport server
- [THRIFT-1754](https://issues.apache.org/jira/browse/THRIFT-1754) - RangeError in buffer handling
- [THRIFT-1618](https://issues.apache.org/jira/browse/THRIFT-1618) - static structMap in FieldMetaData is not thread safe and can lead to deadlocks
- [THRIFT-2335](https://issues.apache.org/jira/browse/THRIFT-2335) - thrift incompatibility with py:tornado as server, java as client
- [THRIFT-2803](https://issues.apache.org/jira/browse/THRIFT-2803) - TCP_DEFER_ACCEPT not supported with domain sockets
- [THRIFT-2799](https://issues.apache.org/jira/browse/THRIFT-2799) - Build Problem(s): ld: library not found for -l:libboost_unit_test_framework.a
- [THRIFT-2801](https://issues.apache.org/jira/browse/THRIFT-2801) - C++ test suite compilation warnings
- [THRIFT-2802](https://issues.apache.org/jira/browse/THRIFT-2802) - C++ tutorial compilation warnings
- [THRIFT-2795](https://issues.apache.org/jira/browse/THRIFT-2795) - thrift_binary_protocol.c: 'dereferencing type-punned pointer will break strict-aliasing rules'
- [THRIFT-2817](https://issues.apache.org/jira/browse/THRIFT-2817) - TSimpleJSONProtocol reads beyond end of message
- [THRIFT-2826](https://issues.apache.org/jira/browse/THRIFT-2826) - html:standalone sometimes ignored
- [THRIFT-2829](https://issues.apache.org/jira/browse/THRIFT-2829) - Support haxelib installation via github
- [THRIFT-2828](https://issues.apache.org/jira/browse/THRIFT-2828) - slightly wrong help screen indent
- [THRIFT-2831](https://issues.apache.org/jira/browse/THRIFT-2831) - Removes dead code in web_server.js introduced in THRIFT-2819
- [THRIFT-2823](https://issues.apache.org/jira/browse/THRIFT-2823) - All JS-tests are failing when run with grunt test
- [THRIFT-2827](https://issues.apache.org/jira/browse/THRIFT-2827) - Thrift 0.9.2 fails to compile on Yosemite due to tr1/functional include in ProcessorTest.cpp
- [THRIFT-2843](https://issues.apache.org/jira/browse/THRIFT-2843) - Automake configure.ac has possible typo related to Java
- [THRIFT-2813](https://issues.apache.org/jira/browse/THRIFT-2813) - multiple haxe library fixes/improvements
- [THRIFT-2825](https://issues.apache.org/jira/browse/THRIFT-2825) - Supplying unicode to python Thrift client can cause next request arguments to get overwritten
- [THRIFT-2840](https://issues.apache.org/jira/browse/THRIFT-2840) - Cabal file points to LICENSE file outside the path of the Haskell project.
- [THRIFT-2818](https://issues.apache.org/jira/browse/THRIFT-2818) - Trailing commas in array
- [THRIFT-2830](https://issues.apache.org/jira/browse/THRIFT-2830) - Clean up ant warnings in tutorial dir
- [THRIFT-2842](https://issues.apache.org/jira/browse/THRIFT-2842) - Erlang thrift client has infinite timeout
- [THRIFT-2810](https://issues.apache.org/jira/browse/THRIFT-2810) - Do not leave the underlying ServerSocket open if construction of TServerSocket fails
- [THRIFT-2812](https://issues.apache.org/jira/browse/THRIFT-2812) - Go server adding redundant buffering layer
- [THRIFT-2839](https://issues.apache.org/jira/browse/THRIFT-2839) - TFramedTransport read bug
- [THRIFT-2844](https://issues.apache.org/jira/browse/THRIFT-2844) - Nodejs support broken when running under Browserify
- [THRIFT-2814](https://issues.apache.org/jira/browse/THRIFT-2814) - args/result classes not found when no namespace is set
- [THRIFT-2847](https://issues.apache.org/jira/browse/THRIFT-2847) - function IfValue() is a duplicate of System.StrUtils.IfThen
- [THRIFT-2848](https://issues.apache.org/jira/browse/THRIFT-2848) - certain Delphi tests do not build if TypeRegistry is used
- [THRIFT-2854](https://issues.apache.org/jira/browse/THRIFT-2854) - Go Struct writer and reader looses important error information
- [THRIFT-2858](https://issues.apache.org/jira/browse/THRIFT-2858) - Enable header field case insensitive match in THttpServer
- [THRIFT-2857](https://issues.apache.org/jira/browse/THRIFT-2857) - C# generator creates uncompilable code for struct constants
- [THRIFT-2860](https://issues.apache.org/jira/browse/THRIFT-2860) - Delphi server closes connection on unexpected exceptions
- [THRIFT-2868](https://issues.apache.org/jira/browse/THRIFT-2868) - Enhance error handling in the Go client 
- [THRIFT-2879](https://issues.apache.org/jira/browse/THRIFT-2879) - TMemoryBuffer: using lua string in wrong way
- [THRIFT-2851](https://issues.apache.org/jira/browse/THRIFT-2851) - Remove strange public Peek() from Go transports
- [THRIFT-2852](https://issues.apache.org/jira/browse/THRIFT-2852) - Better Open/IsOpen/Close behavior for StreamTransport.
- [THRIFT-2871](https://issues.apache.org/jira/browse/THRIFT-2871) - Missing semicolon in thrift.js
- [THRIFT-2872](https://issues.apache.org/jira/browse/THRIFT-2872) - ThreadManager deadlock for task expiration
- [THRIFT-2881](https://issues.apache.org/jira/browse/THRIFT-2881) - Handle errors from Accept() correctly
- [THRIFT-2849](https://issues.apache.org/jira/browse/THRIFT-2849) - Spell errors reported by codespell tool
- [THRIFT-2870](https://issues.apache.org/jira/browse/THRIFT-2870) - C++ TJSONProtocol using locale dependent formatting
- [THRIFT-2882](https://issues.apache.org/jira/browse/THRIFT-2882) - Lua Generator: using string.len function to get struct(map,list,set) size
- [THRIFT-2864](https://issues.apache.org/jira/browse/THRIFT-2864) - JSON generator missing from Visual Studio build project 
- [THRIFT-2878](https://issues.apache.org/jira/browse/THRIFT-2878) - Go validation support of required fields
- [THRIFT-2873](https://issues.apache.org/jira/browse/THRIFT-2873) - TPipe and TPipeServer don't compile on Windows with UNICODE enabled
- [THRIFT-2888](https://issues.apache.org/jira/browse/THRIFT-2888) - import of <limits> is missing in JSON generator
- [THRIFT-2900](https://issues.apache.org/jira/browse/THRIFT-2900) - Python THttpClient does not reset socket timeout on exception
- [THRIFT-2907](https://issues.apache.org/jira/browse/THRIFT-2907) - 'ntohll' macro redefined
- [THRIFT-2884](https://issues.apache.org/jira/browse/THRIFT-2884) - Map does not serialize correctly for JSON protocol in Go library
- [THRIFT-2887](https://issues.apache.org/jira/browse/THRIFT-2887) - --with-openssl configure flag is ignored
- [THRIFT-2894](https://issues.apache.org/jira/browse/THRIFT-2894) - PHP json serializer skips maps with int/bool keys
- [THRIFT-2904](https://issues.apache.org/jira/browse/THRIFT-2904) - json_protocol_test.go fails
- [THRIFT-2906](https://issues.apache.org/jira/browse/THRIFT-2906) - library not found for -l:libboost_unit_test_framework.a
- [THRIFT-2890](https://issues.apache.org/jira/browse/THRIFT-2890) - binary data may lose bytes with JSON transport under specific circumstances
- [THRIFT-2891](https://issues.apache.org/jira/browse/THRIFT-2891) - binary data may cause a failure with JSON transport under specific circumstances
- [THRIFT-2901](https://issues.apache.org/jira/browse/THRIFT-2901) - Fix for generated TypeScript functions + indentation of JavaScript maps
- [THRIFT-2916](https://issues.apache.org/jira/browse/THRIFT-2916) - make check fails for D language
- [THRIFT-2918](https://issues.apache.org/jira/browse/THRIFT-2918) - Race condition in Python TProcessPoolServer test
- [THRIFT-2920](https://issues.apache.org/jira/browse/THRIFT-2920) - Erlang Thrift test uses wrong IDL file
- [THRIFT-2922](https://issues.apache.org/jira/browse/THRIFT-2922) - $TRIAL is used with Python tests but not tested accordingly
- [THRIFT-2912](https://issues.apache.org/jira/browse/THRIFT-2912) - Autotool build for C++ Qt library is invalid
- [THRIFT-2914](https://issues.apache.org/jira/browse/THRIFT-2914) - explicit dependency to Lua5.2 fails on some systems
- [THRIFT-2910](https://issues.apache.org/jira/browse/THRIFT-2910) - libevent is not really optional
- [THRIFT-2911](https://issues.apache.org/jira/browse/THRIFT-2911) - fix c++ version zeromq transport, the old version cannot work
- [THRIFT-2915](https://issues.apache.org/jira/browse/THRIFT-2915) - Lua generator missing from Visual Studio build project
- [THRIFT-2917](https://issues.apache.org/jira/browse/THRIFT-2917) - "make clean" breaks test/c_glib
- [THRIFT-2919](https://issues.apache.org/jira/browse/THRIFT-2919) - Haxe test server timeout too large
- [THRIFT-2923](https://issues.apache.org/jira/browse/THRIFT-2923) - JavaScript client assumes a message being written
- [THRIFT-2924](https://issues.apache.org/jira/browse/THRIFT-2924) - TNonblockingServer crashes when user-provided event_base is used
- [THRIFT-2925](https://issues.apache.org/jira/browse/THRIFT-2925) - CMake build does not work with OpenSSL nor anything installed in non-system location
- [THRIFT-2931](https://issues.apache.org/jira/browse/THRIFT-2931) - Access to undeclared static property: Thrift\Protocol\TProtocol::$TBINARYPROTOCOLACCELERATED
- [THRIFT-2893](https://issues.apache.org/jira/browse/THRIFT-2893) - CMake build fails with boost thread or std thread
- [THRIFT-2902](https://issues.apache.org/jira/browse/THRIFT-2902) - Generated c_glib code does not compile with clang
- [THRIFT-2903](https://issues.apache.org/jira/browse/THRIFT-2903) - Qt4 library built with CMake does not work
- [THRIFT-2942](https://issues.apache.org/jira/browse/THRIFT-2942) - CSharp generate invalid code for property named read or write
- [THRIFT-2932](https://issues.apache.org/jira/browse/THRIFT-2932) - Node.js Thrift connection libraries throw Exceptions into event emitter
- [THRIFT-2933](https://issues.apache.org/jira/browse/THRIFT-2933) - v0.9.2: doubles encoded in node with compact protocol cannot be decoded by python
- [THRIFT-2934](https://issues.apache.org/jira/browse/THRIFT-2934) - createServer signature mismatch
- [THRIFT-2981](https://issues.apache.org/jira/browse/THRIFT-2981) - IDL with no namespace produces unparsable PHP
- [THRIFT-2999](https://issues.apache.org/jira/browse/THRIFT-2999) - Addition of .gitattributes text auto in THRIFT-2724 causes modified files on checkout
- [THRIFT-2949](https://issues.apache.org/jira/browse/THRIFT-2949) - typo in compiler/cpp/README.md
- [THRIFT-2957](https://issues.apache.org/jira/browse/THRIFT-2957) - warning: source file %s is in a subdirectory, but option 'subdir-objects' is disabled
- [THRIFT-2953](https://issues.apache.org/jira/browse/THRIFT-2953) - TNamedPipeServerTransport is not Stop()able
- [THRIFT-2962](https://issues.apache.org/jira/browse/THRIFT-2962) - Docker Thrift env for development and testing
- [THRIFT-2971](https://issues.apache.org/jira/browse/THRIFT-2971) - C++ test and tutorial parallel build is unstable
- [THRIFT-2972](https://issues.apache.org/jira/browse/THRIFT-2972) - Missing backslash in lib/cpp/test/Makefile.am
- [THRIFT-2951](https://issues.apache.org/jira/browse/THRIFT-2951) - Fix Erlang name conflict test
- [THRIFT-2955](https://issues.apache.org/jira/browse/THRIFT-2955) - Using list of typedefs does not compile on Go
- [THRIFT-2960](https://issues.apache.org/jira/browse/THRIFT-2960) - namespace regression for Ruby
- [THRIFT-2959](https://issues.apache.org/jira/browse/THRIFT-2959) - nodejs: fix binary unit tests
- [THRIFT-2966](https://issues.apache.org/jira/browse/THRIFT-2966) - nodejs: Fix bad references to TProtocolException and TProtocolExceptionType
- [THRIFT-2970](https://issues.apache.org/jira/browse/THRIFT-2970) - grunt-jsdoc fails due to dependency issues
- [THRIFT-3001](https://issues.apache.org/jira/browse/THRIFT-3001) - C# Equals fails for binary fields (byte[])
- [THRIFT-3003](https://issues.apache.org/jira/browse/THRIFT-3003) - Missing LICENSE file prevents package from being installed
- [THRIFT-3008](https://issues.apache.org/jira/browse/THRIFT-3008) - Node.js server does not fully support exception
- [THRIFT-3007](https://issues.apache.org/jira/browse/THRIFT-3007) - Travis build is broken because of directory conflict
- [THRIFT-3009](https://issues.apache.org/jira/browse/THRIFT-3009) - TSSLSocket does not use the correct hostname (breaks certificate checks)
- [THRIFT-3011](https://issues.apache.org/jira/browse/THRIFT-3011) - C# test server testException() not implemented according to specs
- [THRIFT-3012](https://issues.apache.org/jira/browse/THRIFT-3012) - Timing problems in NamedPipe implementation due to unnecessary open/close
- [THRIFT-3019](https://issues.apache.org/jira/browse/THRIFT-3019) - Golang generator missing docstring for structs
- [THRIFT-3021](https://issues.apache.org/jira/browse/THRIFT-3021) - Service remote tool does not import stub package with package prefix
- [THRIFT-3026](https://issues.apache.org/jira/browse/THRIFT-3026) - TMultiplexedProcessor does not have a constructor
- [THRIFT-3028](https://issues.apache.org/jira/browse/THRIFT-3028) - Regression caused by THRIFT-2180
- [THRIFT-3017](https://issues.apache.org/jira/browse/THRIFT-3017) - order of map key/value types incorrect for one CTOR
- [THRIFT-3020](https://issues.apache.org/jira/browse/THRIFT-3020) - Cannot compile thrift as C++03
- [THRIFT-3024](https://issues.apache.org/jira/browse/THRIFT-3024) - User-Agent "BattleNet" used in some Thrift library files
- [THRIFT-3047](https://issues.apache.org/jira/browse/THRIFT-3047) - Uneven calls to indent_up and indent_down in Cocoa generator
- [THRIFT-3048](https://issues.apache.org/jira/browse/THRIFT-3048) - NodeJS decoding of I64 is inconsistent across protocols
- [THRIFT-3043](https://issues.apache.org/jira/browse/THRIFT-3043) - go compiler generator uses non C++98 code
- [THRIFT-3044](https://issues.apache.org/jira/browse/THRIFT-3044) - Docker README.md paths to Dockerfiles are incorrect
- [THRIFT-3040](https://issues.apache.org/jira/browse/THRIFT-3040) - bower.json wrong "main" path
- [THRIFT-3051](https://issues.apache.org/jira/browse/THRIFT-3051) - Go Thrift generator creates bad go code
- [THRIFT-3057](https://issues.apache.org/jira/browse/THRIFT-3057) - Java compiler build is broken
- [THRIFT-3061](https://issues.apache.org/jira/browse/THRIFT-3061) - C++ TSSLSocket shutdown delay/vulnerability
- [THRIFT-3062](https://issues.apache.org/jira/browse/THRIFT-3062) - C++ TServerSocket invalid port number (over 999999) causes stack corruption
- [THRIFT-3065](https://issues.apache.org/jira/browse/THRIFT-3065) - Update libthrift dependencies (slf4j, httpcore, httpclient)
- [THRIFT-3244](https://issues.apache.org/jira/browse/THRIFT-3244) - TypeScript: fix namespace of included types
- [THRIFT-3246](https://issues.apache.org/jira/browse/THRIFT-3246) - Reduce the number of trivial warnings in Windows C++ CMake builds
- [THRIFT-3224](https://issues.apache.org/jira/browse/THRIFT-3224) - Fix TNamedPipeServer unpredictable behavior on accept
- [THRIFT-3230](https://issues.apache.org/jira/browse/THRIFT-3230) - Python compiler generates wrong code if there is function throwing a typedef of exception with another namespace
- [THRIFT-3236](https://issues.apache.org/jira/browse/THRIFT-3236) - MaxSkipDepth never checked
- [THRIFT-3239](https://issues.apache.org/jira/browse/THRIFT-3239) - Limit recursion depth
- [THRIFT-3241](https://issues.apache.org/jira/browse/THRIFT-3241) - fatal error: runtime: cannot map pages in arena address space
- [THRIFT-3242](https://issues.apache.org/jira/browse/THRIFT-3242) - OSGi Import-Package directive is missing the Apache HTTP packages
- [THRIFT-3234](https://issues.apache.org/jira/browse/THRIFT-3234) - Limit recursion depth
- [THRIFT-3222](https://issues.apache.org/jira/browse/THRIFT-3222) - TypeScript: Generated Enums are quoted
- [THRIFT-3229](https://issues.apache.org/jira/browse/THRIFT-3229) - unexpected Timeout exception when desired bytes are only partially available
- [THRIFT-3231](https://issues.apache.org/jira/browse/THRIFT-3231) - CPP: Limit recursion depth to 64
- [THRIFT-3235](https://issues.apache.org/jira/browse/THRIFT-3235) - Limit recursion depth
- [THRIFT-3175](https://issues.apache.org/jira/browse/THRIFT-3175) - fastbinary.c python deserialize can cause huge allocations from garbage
- [THRIFT-3176](https://issues.apache.org/jira/browse/THRIFT-3176) - Union incorrectly implements ==
- [THRIFT-3177](https://issues.apache.org/jira/browse/THRIFT-3177) - Fails to run rake test
- [THRIFT-3180](https://issues.apache.org/jira/browse/THRIFT-3180) - lua plugin: framed transport do not work
- [THRIFT-3179](https://issues.apache.org/jira/browse/THRIFT-3179) - lua plugin cant connect to remote server because function l_socket_create_and_connect always bind socket to localhost
- [THRIFT-3248](https://issues.apache.org/jira/browse/THRIFT-3248) - TypeScript: additional comma in method signature without parameters
- [THRIFT-3302](https://issues.apache.org/jira/browse/THRIFT-3302) - Go JSON protocol should encode Thrift byte type as signed integer string
- [THRIFT-3297](https://issues.apache.org/jira/browse/THRIFT-3297) - c_glib: an abstract base class is not generated
- [THRIFT-3294](https://issues.apache.org/jira/browse/THRIFT-3294) - TZlibTransport for Java does not write data correctly
- [THRIFT-3296](https://issues.apache.org/jira/browse/THRIFT-3296) - Go cross test does not conform to spec
- [THRIFT-3295](https://issues.apache.org/jira/browse/THRIFT-3295) - C# library does not build on Mono 4.0.2.5 or later
- [THRIFT-3293](https://issues.apache.org/jira/browse/THRIFT-3293) - JavaScript: null values turn into empty structs in constructor
- [THRIFT-3310](https://issues.apache.org/jira/browse/THRIFT-3310) - lib/erl/README.md has incorrect formatting
- [THRIFT-3319](https://issues.apache.org/jira/browse/THRIFT-3319) - CSharp tutorial will not build using the *.sln
- [THRIFT-3335](https://issues.apache.org/jira/browse/THRIFT-3335) - Ruby server does not handle processor exception
- [THRIFT-3338](https://issues.apache.org/jira/browse/THRIFT-3338) - Stray underscore in generated go when service name starts with "New"
- [THRIFT-3324](https://issues.apache.org/jira/browse/THRIFT-3324) - Update Go Docs for pulling all packages
- [THRIFT-3345](https://issues.apache.org/jira/browse/THRIFT-3345) - Clients blocked indefinitely when a java.lang.Error is thrown
- [THRIFT-3332](https://issues.apache.org/jira/browse/THRIFT-3332) - make dist fails on clean build
- [THRIFT-3326](https://issues.apache.org/jira/browse/THRIFT-3326) - Tests do not compile under *BSD
- [THRIFT-3334](https://issues.apache.org/jira/browse/THRIFT-3334) - Markdown notation of protocol spec is malformed
- [THRIFT-3331](https://issues.apache.org/jira/browse/THRIFT-3331) - warning: â€˜etypeâ€™ may be used uninitialized in this function
- [THRIFT-3349](https://issues.apache.org/jira/browse/THRIFT-3349) - Python server does not handle processor exception
- [THRIFT-3343](https://issues.apache.org/jira/browse/THRIFT-3343) - Fix haskell README
- [THRIFT-3340](https://issues.apache.org/jira/browse/THRIFT-3340) - Python: enable json tests again
- [THRIFT-3311](https://issues.apache.org/jira/browse/THRIFT-3311) - Top level README.md has incorrect formmating
- [THRIFT-2936](https://issues.apache.org/jira/browse/THRIFT-2936) - Minor memory leak in SSL
- [THRIFT-3290](https://issues.apache.org/jira/browse/THRIFT-3290) - Using from in variable names causes the generated Python code to have errors
- [THRIFT-3225](https://issues.apache.org/jira/browse/THRIFT-3225) - Fix TPipeServer unpredictable behavior on interrupt()
- [THRIFT-3354](https://issues.apache.org/jira/browse/THRIFT-3354) - Fix word-extraction substr bug in initialism code
- [THRIFT-2006](https://issues.apache.org/jira/browse/THRIFT-2006) - TBinaryProtocol message header call name length is not validated and can be used to core the server
- [THRIFT-3329](https://issues.apache.org/jira/browse/THRIFT-3329) - C++ library unit tests don't compile against the new boost-1.59 unit test framework
- [THRIFT-2630](https://issues.apache.org/jira/browse/THRIFT-2630) - windows7 64bit pc. ipv4 and ipv6 pc.can't use
- [THRIFT-3336](https://issues.apache.org/jira/browse/THRIFT-3336) - Thrift generated streaming operators added in 0.9.2 cannot be overridden
- [THRIFT-2681](https://issues.apache.org/jira/browse/THRIFT-2681) - Core of unwind_cleanup
- [THRIFT-3317](https://issues.apache.org/jira/browse/THRIFT-3317) - cpp namespace org.apache issue appears in 0.9

### Documentation
- [THRIFT-3286](https://issues.apache.org/jira/browse/THRIFT-3286) - Apache Ant is a necessary dependency

### Improvement
- [THRIFT-227](https://issues.apache.org/jira/browse/THRIFT-227) - Byte[] in collections aren't pretty printed like regular binary fields
- [THRIFT-2744](https://issues.apache.org/jira/browse/THRIFT-2744) - Vagrantfile for Centos 6.5
- [THRIFT-2644](https://issues.apache.org/jira/browse/THRIFT-2644) - Haxe support
- [THRIFT-2756](https://issues.apache.org/jira/browse/THRIFT-2756) - register Media Type @ IANA
- [THRIFT-3076](https://issues.apache.org/jira/browse/THRIFT-3076) - Compatibility with Haxe 3.2.0
- [THRIFT-3081](https://issues.apache.org/jira/browse/THRIFT-3081) - C++ Consolidate client processing loops in TServers
- [THRIFT-3083](https://issues.apache.org/jira/browse/THRIFT-3083) - C++ Consolidate server processing loops in TSimpleServer, TThreadedServer, TThreadPoolServer
- [THRIFT-3084](https://issues.apache.org/jira/browse/THRIFT-3084) - C++ add concurrent client limit to threaded servers
- [THRIFT-3074](https://issues.apache.org/jira/browse/THRIFT-3074) -     Add compiler/cpp/lex.yythriftl.cc to gitignore.
- [THRIFT-3134](https://issues.apache.org/jira/browse/THRIFT-3134) - Remove use of deprecated "phantom.args"
- [THRIFT-3133](https://issues.apache.org/jira/browse/THRIFT-3133) - Allow "make cross" and "make precross" to run without building all languages
- [THRIFT-3142](https://issues.apache.org/jira/browse/THRIFT-3142) - Make JavaScript use downloaded libraries
- [THRIFT-3141](https://issues.apache.org/jira/browse/THRIFT-3141) - Improve logging of JavaScript test
- [THRIFT-3144](https://issues.apache.org/jira/browse/THRIFT-3144) - Proposal: make String representation of enums in generated go code less verbose
- [THRIFT-3130](https://issues.apache.org/jira/browse/THRIFT-3130) - Remove the last vestiges of THRIFT_OVERLOAD_IF from THRIFT-1316
- [THRIFT-3131](https://issues.apache.org/jira/browse/THRIFT-3131) - Consolidate suggested import path for go thrift library to git.apache.org in docs and code
- [THRIFT-3092](https://issues.apache.org/jira/browse/THRIFT-3092) - Generated Haskell types should derive Generic
- [THRIFT-3110](https://issues.apache.org/jira/browse/THRIFT-3110) -  Print error log after cross test failures on Travis
- [THRIFT-3114](https://issues.apache.org/jira/browse/THRIFT-3114) - Using local temp variables to not pollute the global table
- [THRIFT-3106](https://issues.apache.org/jira/browse/THRIFT-3106) - CMake summary should give more information why a library is set to off
- [THRIFT-3119](https://issues.apache.org/jira/browse/THRIFT-3119) - Java's TThreadedSelectorServer has indistinguishable log messages in run()
- [THRIFT-3122](https://issues.apache.org/jira/browse/THRIFT-3122) - Javascript struct constructor should properly initialize struct and container members from plain js arguments
- [THRIFT-3151](https://issues.apache.org/jira/browse/THRIFT-3151) - Fix links to git-wip* - should be git.apache.org
- [THRIFT-3167](https://issues.apache.org/jira/browse/THRIFT-3167) - Windows build from source instructions need to be revised
- [THRIFT-3155](https://issues.apache.org/jira/browse/THRIFT-3155) - move contrib/mingw32-toolchain.cmake to build/cmake/
- [THRIFT-3160](https://issues.apache.org/jira/browse/THRIFT-3160) - Make generated go enums implement TextMarshaller and TextUnmarshaller interfaces
- [THRIFT-3150](https://issues.apache.org/jira/browse/THRIFT-3150) - Add an option to thrift go generator to make Read and Write methods private
- [THRIFT-3149](https://issues.apache.org/jira/browse/THRIFT-3149) - Make ReadFieldN methods in generated Go code private
- [THRIFT-3172](https://issues.apache.org/jira/browse/THRIFT-3172) - Add tutorial to Thrift web site
- [THRIFT-3214](https://issues.apache.org/jira/browse/THRIFT-3214) - Add Erlang option for using maps instead of dicts
- [THRIFT-3201](https://issues.apache.org/jira/browse/THRIFT-3201) - Capture github test artifacts for failed builds
- [THRIFT-3266](https://issues.apache.org/jira/browse/THRIFT-3266) - c_glib: Multiple compiler warnings building unit tests
- [THRIFT-3285](https://issues.apache.org/jira/browse/THRIFT-3285) - c_glib: Build library with all warnings enabled, no warnings generated
- [THRIFT-1954](https://issues.apache.org/jira/browse/THRIFT-1954) - Allow for a separate connection timeout value 
- [THRIFT-2098](https://issues.apache.org/jira/browse/THRIFT-2098) - Add support for Qt5+
- [THRIFT-2199](https://issues.apache.org/jira/browse/THRIFT-2199) - Remove Dense protocol (was: move to Contrib)
- [THRIFT-406](https://issues.apache.org/jira/browse/THRIFT-406) - C++ Test suite cleanup
- [THRIFT-902](https://issues.apache.org/jira/browse/THRIFT-902) - socket and connect timeout in TSocket should be distinguished
- [THRIFT-388](https://issues.apache.org/jira/browse/THRIFT-388) - Use a separate wire format for async calls
- [THRIFT-727](https://issues.apache.org/jira/browse/THRIFT-727) - support native C++ language specific exception message
- [THRIFT-1784](https://issues.apache.org/jira/browse/THRIFT-1784) - pep-3110 compliance for exception handling 
- [THRIFT-1025](https://issues.apache.org/jira/browse/THRIFT-1025) - C++ ServerSocket should inherit from Socket with the necessary Ctor to listen on connections from a specific host
- [THRIFT-2269](https://issues.apache.org/jira/browse/THRIFT-2269) - Can deploy libthrift-source.jar to maven center repository
- [THRIFT-2804](https://issues.apache.org/jira/browse/THRIFT-2804) - Pull an interface out of TBaseAsyncProcessor
- [THRIFT-2806](https://issues.apache.org/jira/browse/THRIFT-2806) - more whitespace fixups
- [THRIFT-2811](https://issues.apache.org/jira/browse/THRIFT-2811) - Make remote socket address accessible
- [THRIFT-2809](https://issues.apache.org/jira/browse/THRIFT-2809) - .gitignore update for compiler's visual project
- [THRIFT-2846](https://issues.apache.org/jira/browse/THRIFT-2846) - Expose ciphers parameter from ssl.wrap_socket()
- [THRIFT-2859](https://issues.apache.org/jira/browse/THRIFT-2859) - JSON generator: output complete descriptors
- [THRIFT-2861](https://issues.apache.org/jira/browse/THRIFT-2861) - add buffered transport
- [THRIFT-2865](https://issues.apache.org/jira/browse/THRIFT-2865) - Test case for Go: SeqId out of sequence
- [THRIFT-2866](https://issues.apache.org/jira/browse/THRIFT-2866) - Go generator source code is hard to read and maintain
- [THRIFT-2880](https://issues.apache.org/jira/browse/THRIFT-2880) - Read the network address from the listener if available.
- [THRIFT-2875](https://issues.apache.org/jira/browse/THRIFT-2875) - Typo in TDenseProtocol.h comment
- [THRIFT-2874](https://issues.apache.org/jira/browse/THRIFT-2874) - TBinaryProtocol  member variable "string_buf_" is never used.
- [THRIFT-2855](https://issues.apache.org/jira/browse/THRIFT-2855) - Move contributing.md to the root of the repository
- [THRIFT-2862](https://issues.apache.org/jira/browse/THRIFT-2862) - Enable RTTI and/or build macros for generated code
- [THRIFT-2876](https://issues.apache.org/jira/browse/THRIFT-2876) -  Add test for THRIFT-2526 Assignment operators and copy constructors in c++ don't copy the __isset struct
- [THRIFT-2897](https://issues.apache.org/jira/browse/THRIFT-2897) - Generate -isEqual: and -hash methods
- [THRIFT-2909](https://issues.apache.org/jira/browse/THRIFT-2909) - Improve travis build
- [THRIFT-2921](https://issues.apache.org/jira/browse/THRIFT-2921) - Make Erlang impl ready for OTP 18 release (dict/0 and set/0 are deprecated)
- [THRIFT-2928](https://issues.apache.org/jira/browse/THRIFT-2928) - Rename the erlang test_server module
- [THRIFT-2940](https://issues.apache.org/jira/browse/THRIFT-2940) - Allow installing Thrift from git as NPM module by providing package.json in top level directory
- [THRIFT-2937](https://issues.apache.org/jira/browse/THRIFT-2937) - Allow setting a maximum frame size in TFramedTransport
- [THRIFT-2976](https://issues.apache.org/jira/browse/THRIFT-2976) - nodejs: xhr and websocket support for browserify
- [THRIFT-2996](https://issues.apache.org/jira/browse/THRIFT-2996) - Test for Haxe 3.1.3 or better 
- [THRIFT-2969](https://issues.apache.org/jira/browse/THRIFT-2969) - nodejs: DRY up library tests
- [THRIFT-2973](https://issues.apache.org/jira/browse/THRIFT-2973) - Update Haxe lib readme regarding Haxe 3.1.3
- [THRIFT-2952](https://issues.apache.org/jira/browse/THRIFT-2952) - Improve handling of Server.Stop() 
- [THRIFT-2964](https://issues.apache.org/jira/browse/THRIFT-2964) - nodejs: move protocols and transports into separate files
- [THRIFT-2963](https://issues.apache.org/jira/browse/THRIFT-2963) - nodejs - add test coverage
- [THRIFT-3006](https://issues.apache.org/jira/browse/THRIFT-3006) - Attach 'omitempty' json tag for optional fields in Go
- [THRIFT-3027](https://issues.apache.org/jira/browse/THRIFT-3027) - Go compiler does not ensure common initialisms have consistent case
- [THRIFT-3030](https://issues.apache.org/jira/browse/THRIFT-3030) - TThreadedServer: Property for number of clientThreads
- [THRIFT-3023](https://issues.apache.org/jira/browse/THRIFT-3023) - Go compiler is a little overly conservative with names of attributes
- [THRIFT-3018](https://issues.apache.org/jira/browse/THRIFT-3018) - Compact protocol for Delphi
- [THRIFT-3025](https://issues.apache.org/jira/browse/THRIFT-3025) - Change pure Int constants into @enums (where possible)
- [THRIFT-3031](https://issues.apache.org/jira/browse/THRIFT-3031) - migrate "shouldStop" flag to TServer
- [THRIFT-3022](https://issues.apache.org/jira/browse/THRIFT-3022) - Compact protocol for Haxe
- [THRIFT-3041](https://issues.apache.org/jira/browse/THRIFT-3041) - Generate asynchronous clients for Cocoa
- [THRIFT-3053](https://issues.apache.org/jira/browse/THRIFT-3053) - Perl SSL Socket Support (Encryption)
- [THRIFT-3247](https://issues.apache.org/jira/browse/THRIFT-3247) - Generate a C++ thread-safe client
- [THRIFT-3217](https://issues.apache.org/jira/browse/THRIFT-3217) - Provide a little endian variant of the binary protocol in C++
- [THRIFT-3223](https://issues.apache.org/jira/browse/THRIFT-3223) - TypeScript: Add initial support for Enum Maps
- [THRIFT-3220](https://issues.apache.org/jira/browse/THRIFT-3220) - Option to suppress @Generated Annotation entirely
- [THRIFT-3300](https://issues.apache.org/jira/browse/THRIFT-3300) - Reimplement TZlibTransport in Java using streams
- [THRIFT-3288](https://issues.apache.org/jira/browse/THRIFT-3288) - c_glib: Build unit tests with all warnings enabled, no warnings generated
- [THRIFT-3347](https://issues.apache.org/jira/browse/THRIFT-3347) - Improve cross test servers and clients
- [THRIFT-3342](https://issues.apache.org/jira/browse/THRIFT-3342) - Improve ruby cross test client and server compatibility
- [THRIFT-2296](https://issues.apache.org/jira/browse/THRIFT-2296) - Add C++ Base class for service
- [THRIFT-3337](https://issues.apache.org/jira/browse/THRIFT-3337) - Add testBool method to cross tests
- [THRIFT-3303](https://issues.apache.org/jira/browse/THRIFT-3303) - Disable concurrent cabal jobs on Travis to avoid GHC crash
- [THRIFT-2623](https://issues.apache.org/jira/browse/THRIFT-2623) - Docker container for Thrift
- [THRIFT-3298](https://issues.apache.org/jira/browse/THRIFT-3298) - thrift endian converters may conflict with other libraries
- [THRIFT-1559](https://issues.apache.org/jira/browse/THRIFT-1559) - Provide memory pool for TBinaryProtocol to eliminate memory fragmentation
- [THRIFT-424](https://issues.apache.org/jira/browse/THRIFT-424) - Steal ProtocolBuffers' VarInt implementation for C++

### New Feature
- [THRIFT-3070](https://issues.apache.org/jira/browse/THRIFT-3070) - Add ability to set the LocalCertificateSelectionCallback
- [THRIFT-1909](https://issues.apache.org/jira/browse/THRIFT-1909) - Java: Add compiler flag to use the "option pattern" for optional fields
- [THRIFT-2099](https://issues.apache.org/jira/browse/THRIFT-2099) - Stop TThreadPoolServer with alive connections.
- [THRIFT-123](https://issues.apache.org/jira/browse/THRIFT-123) - implement TZlibTransport in Java
- [THRIFT-2368](https://issues.apache.org/jira/browse/THRIFT-2368) - New option: reuse-objects for Java generator
- [THRIFT-2836](https://issues.apache.org/jira/browse/THRIFT-2836) - Optionally generate C++11 MoveConstructible types
- [THRIFT-2824](https://issues.apache.org/jira/browse/THRIFT-2824) - Flag to disable html escaping doctext
- [THRIFT-2819](https://issues.apache.org/jira/browse/THRIFT-2819) - Add WebsSocket client to node.js
- [THRIFT-3050](https://issues.apache.org/jira/browse/THRIFT-3050) - Client certificate authentication for non-http TLS in C#
- [THRIFT-3292](https://issues.apache.org/jira/browse/THRIFT-3292) - Implement TZlibTransport in Go

### Question
- [THRIFT-2583](https://issues.apache.org/jira/browse/THRIFT-2583) - Thrift on xPC target (SpeedGoat)
- [THRIFT-2592](https://issues.apache.org/jira/browse/THRIFT-2592) - thrift server using c_glib
- [THRIFT-2832](https://issues.apache.org/jira/browse/THRIFT-2832) - c_glib: Handle string lists correctly
- [THRIFT-3136](https://issues.apache.org/jira/browse/THRIFT-3136) - thrift installation problem on mac
- [THRIFT-3346](https://issues.apache.org/jira/browse/THRIFT-3346) - c_glib: Tutorials example crashes saying Calculator.ping implementation returned FALSE but did not set an error

### Sub-task
- [THRIFT-2578](https://issues.apache.org/jira/browse/THRIFT-2578) - Moving 'make cross' from test.sh to test.py
- [THRIFT-2734](https://issues.apache.org/jira/browse/THRIFT-2734) - Go coding standards
- [THRIFT-2748](https://issues.apache.org/jira/browse/THRIFT-2748) - Add Vagrantfile for Centos 6.5
- [THRIFT-2753](https://issues.apache.org/jira/browse/THRIFT-2753) - Misc. Haxe improvements
- [THRIFT-2640](https://issues.apache.org/jira/browse/THRIFT-2640) - Compact Protocol in Cocoa
- [THRIFT-3262](https://issues.apache.org/jira/browse/THRIFT-3262) - warning: overflow in implicit constant conversion in DenseProtoTest.cpp
- [THRIFT-3194](https://issues.apache.org/jira/browse/THRIFT-3194) - Can't build with go enabled.  gomock SCC path incorrect.
- [THRIFT-3275](https://issues.apache.org/jira/browse/THRIFT-3275) - c_glib tutorial warnings in generated code
- [THRIFT-1125](https://issues.apache.org/jira/browse/THRIFT-1125) - Multiplexing support for the Ruby Library
- [THRIFT-2807](https://issues.apache.org/jira/browse/THRIFT-2807) - PHP Code Style
- [THRIFT-2841](https://issues.apache.org/jira/browse/THRIFT-2841) - Add comprehensive integration tests for the whole Go stack
- [THRIFT-2815](https://issues.apache.org/jira/browse/THRIFT-2815) - Haxe: Support for Multiplexing Services on any Transport, Protocol and Server
- [THRIFT-2886](https://issues.apache.org/jira/browse/THRIFT-2886) - Integrate binary type in standard Thrift cross test
- [THRIFT-2946](https://issues.apache.org/jira/browse/THRIFT-2946) - Enhance usability of cross test framework
- [THRIFT-2967](https://issues.apache.org/jira/browse/THRIFT-2967) - Add .editorconfig to root
- [THRIFT-3033](https://issues.apache.org/jira/browse/THRIFT-3033) - Perl: Support for Multiplexing Services on any Transport, Protocol and Server
- [THRIFT-3174](https://issues.apache.org/jira/browse/THRIFT-3174) - Initialism code in the Go compiler doesn't check first word
- [THRIFT-3193](https://issues.apache.org/jira/browse/THRIFT-3193) - Option to suppress date value in @Generated annotation
- [THRIFT-3305](https://issues.apache.org/jira/browse/THRIFT-3305) - Missing dist files for 0.9.3 release candidate
- [THRIFT-3341](https://issues.apache.org/jira/browse/THRIFT-3341) - Add testBool methods
- [THRIFT-3308](https://issues.apache.org/jira/browse/THRIFT-3308) - Fix broken test cases for 0.9.3 release candidate

### Task
- [THRIFT-2834](https://issues.apache.org/jira/browse/THRIFT-2834) - Remove semi-colons from python code generator
- [THRIFT-2853](https://issues.apache.org/jira/browse/THRIFT-2853) - Adjust comments not applying anymore after THRIFT-2852

### Test
- [THRIFT-3211](https://issues.apache.org/jira/browse/THRIFT-3211) - Add make cross support for php TCompactProtocol

### Wish
- [THRIFT-2838](https://issues.apache.org/jira/browse/THRIFT-2838) - TNonblockingServer can bind to port 0 (i.e., get an OS-assigned port) but there is no way to get the port number

## 0.9.2

### Bug
- [THRIFT-2793](https://issues.apache.org/jira/browse/THRIFT-2793) - Go compiler produces uncompilable code
- [THRIFT-1481](https://issues.apache.org/jira/browse/THRIFT-1481) - Unix domain sockets in C++ do not support the abstract namespace
- [THRIFT-1455](https://issues.apache.org/jira/browse/THRIFT-1455) - TBinaryProtocolT<Transport_>::writeString casts from size_t to uint32_t, which is not safe on 64-bit platforms
- [THRIFT-1579](https://issues.apache.org/jira/browse/THRIFT-1579) - PHP Extension - function thrift_protocol_read_binary not working from TBinarySerializer::deserialize
- [THRIFT-1584](https://issues.apache.org/jira/browse/THRIFT-1584) - Error: could not SetMinThreads in ThreadPool on single-core machines
- [THRIFT-1614](https://issues.apache.org/jira/browse/THRIFT-1614) - Thrift build from svn repo sources fails with automake-1.12
- [THRIFT-1047](https://issues.apache.org/jira/browse/THRIFT-1047) - rb_thrift_memory_buffer_write treats arg as string without check, segfaults if you pass non-string
- [THRIFT-1639](https://issues.apache.org/jira/browse/THRIFT-1639) - Java/Python: Serialization/Deserialization of double type using CompactProtocol
- [THRIFT-1647](https://issues.apache.org/jira/browse/THRIFT-1647) - NodeJS BufferedTransport does not work beyond the hello-world example
- [THRIFT-2130](https://issues.apache.org/jira/browse/THRIFT-2130) - Thrift's D library/test: parts of "make check" code do not compile with recent dmd-2.062 through dmd-2.064alpha
- [THRIFT-2140](https://issues.apache.org/jira/browse/THRIFT-2140) - Error compiling cpp tutorials
- [THRIFT-2139](https://issues.apache.org/jira/browse/THRIFT-2139) - MSVC 2012 Error - Cannot compile due to BoostThreadFactory
- [THRIFT-2138](https://issues.apache.org/jira/browse/THRIFT-2138) - pkgconfig file created with wrong include path
- [THRIFT-2160](https://issues.apache.org/jira/browse/THRIFT-2160) - Warning in thrift.h when compiling with -Wunused and NDEBUG
- [THRIFT-2158](https://issues.apache.org/jira/browse/THRIFT-2158) - Compact, JSON, and SimpleJSON protocols are not working correctly
- [THRIFT-2167](https://issues.apache.org/jira/browse/THRIFT-2167) - nodejs lib throws error if options argument isn't passed
- [THRIFT-2288](https://issues.apache.org/jira/browse/THRIFT-2288) - Go impl of Thrift JSON protocol wrongly writes/expects true/false for bools
- [THRIFT-2147](https://issues.apache.org/jira/browse/THRIFT-2147) - Thrift IDL grammar allows for dotted identifier names
- [THRIFT-2145](https://issues.apache.org/jira/browse/THRIFT-2145) - Rack and Thin are not just development dependencies
- [THRIFT-2267](https://issues.apache.org/jira/browse/THRIFT-2267) - Should be able to choose socket family in Python TSocket
- [THRIFT-2276](https://issues.apache.org/jira/browse/THRIFT-2276) - java path in spec file needs updating
- [THRIFT-2281](https://issues.apache.org/jira/browse/THRIFT-2281) - Generated send/recv code ignores errors returned by the underlying protocol
- [THRIFT-2280](https://issues.apache.org/jira/browse/THRIFT-2280) - TJSONProtocol.Flush() does not really flush the transport
- [THRIFT-2274](https://issues.apache.org/jira/browse/THRIFT-2274) - TNonblockingServer and TThreadedSelectorServer do not close their channel selectors on exit and leak file descriptors
- [THRIFT-2265](https://issues.apache.org/jira/browse/THRIFT-2265) - php library doesn't build
- [THRIFT-2232](https://issues.apache.org/jira/browse/THRIFT-2232) - IsSet* broken in Go
- [THRIFT-2246](https://issues.apache.org/jira/browse/THRIFT-2246) - Unset enum value is printed by ToString()
- [THRIFT-2240](https://issues.apache.org/jira/browse/THRIFT-2240) - thrift.vim (contrib) does not correctly handle 'union'
- [THRIFT-2243](https://issues.apache.org/jira/browse/THRIFT-2243) - TNonblockingServer in thrift crashes when TFramedTransport opens
- [THRIFT-2230](https://issues.apache.org/jira/browse/THRIFT-2230) - Cannot Build on RHEL/Centos/Amazon Linux 6.x
- [THRIFT-2247](https://issues.apache.org/jira/browse/THRIFT-2247) - Go generator doesn't deal well with map keys of type binary
- [THRIFT-2253](https://issues.apache.org/jira/browse/THRIFT-2253) - Python Tornado TTornadoServer base class change
- [THRIFT-2261](https://issues.apache.org/jira/browse/THRIFT-2261) - java: error: unmappable character for encoding ASCII
- [THRIFT-2259](https://issues.apache.org/jira/browse/THRIFT-2259) - C#: unexpected null logDelegate() pointer causes AV in TServer.serve()
- [THRIFT-2225](https://issues.apache.org/jira/browse/THRIFT-2225) - SSLContext destroy before cleanupOpenSSL
- [THRIFT-2224](https://issues.apache.org/jira/browse/THRIFT-2224) - TSSLSocket.h and TSSLServerSocket.h should use the platfromsocket too
- [THRIFT-2229](https://issues.apache.org/jira/browse/THRIFT-2229) - thrift failed to build on OSX 10.9 GM
- [THRIFT-2227](https://issues.apache.org/jira/browse/THRIFT-2227) - Thrift compiler generates spurious warnings with Xlint
- [THRIFT-2219](https://issues.apache.org/jira/browse/THRIFT-2219) - Thrift gem fails to build on OS X Mavericks with 1.9.3 rubies
- [THRIFT-2226](https://issues.apache.org/jira/browse/THRIFT-2226) - TServerSocket - keepAlive wrong initialization order
- [THRIFT-2285](https://issues.apache.org/jira/browse/THRIFT-2285) - TJsonProtocol implementation for Java doesn't allow a slash (/) to be escaped (\/)
- [THRIFT-2216](https://issues.apache.org/jira/browse/THRIFT-2216) - Extraneous semicolon in TProtocolUtil.h makes clang mad
- [THRIFT-2215](https://issues.apache.org/jira/browse/THRIFT-2215) - Generated HTML/Graphviz lists referenced enum identifiers as UNKNOWN.
- [THRIFT-2211](https://issues.apache.org/jira/browse/THRIFT-2211) - Exception constructor does not contain namespace prefix.
- [THRIFT-2210](https://issues.apache.org/jira/browse/THRIFT-2210) - lib/java TSimpleJSONProtocol can emit invalid JSON
- [THRIFT-2209](https://issues.apache.org/jira/browse/THRIFT-2209) - Ruby generator -- please namespace classes
- [THRIFT-2202](https://issues.apache.org/jira/browse/THRIFT-2202) - Delphi TServerImpl.DefaultLogDelegate may stop the server with I/O-Error 105
- [THRIFT-2201](https://issues.apache.org/jira/browse/THRIFT-2201) - Ternary operator returns different types (build error for some compilers)
- [THRIFT-2200](https://issues.apache.org/jira/browse/THRIFT-2200) - nested structs cause generate_fingerprint() to slow down at excessive CPU load
- [THRIFT-2197](https://issues.apache.org/jira/browse/THRIFT-2197) - fix jar output directory in rpm spec file
- [THRIFT-2196](https://issues.apache.org/jira/browse/THRIFT-2196) - Fix invalid dependency in Makefile.am
- [THRIFT-2194](https://issues.apache.org/jira/browse/THRIFT-2194) - Node: Not actually prepending residual data in TFramedTransport.receiver
- [THRIFT-2193](https://issues.apache.org/jira/browse/THRIFT-2193) - Java code generator emits spurious semicolon when deep copying binary data
- [THRIFT-2191](https://issues.apache.org/jira/browse/THRIFT-2191) - Fix charp JSONProtocol.ReadJSONDouble (specify InvariantCulture)
- [THRIFT-2214](https://issues.apache.org/jira/browse/THRIFT-2214) - System header sys/param.h is included inside the Thrift namespace
- [THRIFT-2178](https://issues.apache.org/jira/browse/THRIFT-2178) - Thrift generator returns error exit code on --version
- [THRIFT-2171](https://issues.apache.org/jira/browse/THRIFT-2171) - NodeJS implementation has extremely low test coverage
- [THRIFT-2183](https://issues.apache.org/jira/browse/THRIFT-2183) - gem install fails on zsh
- [THRIFT-2182](https://issues.apache.org/jira/browse/THRIFT-2182) - segfault in regression tests (GC bug in rb_thrift_memory_buffer_write)
- [THRIFT-2181](https://issues.apache.org/jira/browse/THRIFT-2181) - oneway calls don't work in NodeJS
- [THRIFT-2169](https://issues.apache.org/jira/browse/THRIFT-2169) - JavaME Thrift Library causes "java.io.IOException: No Response Entries Available" after using the Thrift client for some time
- [THRIFT-2168](https://issues.apache.org/jira/browse/THRIFT-2168) - Node.js appears broken (at least, examples don't work as intended)
- [THRIFT-2293](https://issues.apache.org/jira/browse/THRIFT-2293) - TSSLTransportFactory.createSSLContext() leaves files open
- [THRIFT-2279](https://issues.apache.org/jira/browse/THRIFT-2279) - TSerializer only returns the first 1024 bytes serialized
- [THRIFT-2278](https://issues.apache.org/jira/browse/THRIFT-2278) - Buffered transport doesn't support writes > buffer size
- [THRIFT-2275](https://issues.apache.org/jira/browse/THRIFT-2275) - Fix memory leak in golang compact_protocol.
- [THRIFT-2282](https://issues.apache.org/jira/browse/THRIFT-2282) - Incorect code generated for some typedefs
- [THRIFT-2009](https://issues.apache.org/jira/browse/THRIFT-2009) - Go redeclaration error
- [THRIFT-1964](https://issues.apache.org/jira/browse/THRIFT-1964) - 'Isset' causes problems with C#/.NET serializers
- [THRIFT-2026](https://issues.apache.org/jira/browse/THRIFT-2026) - Fix TCompactProtocol 64 bit builds
- [THRIFT-2108](https://issues.apache.org/jira/browse/THRIFT-2108) - Fix TAsyncClientManager timeout race
- [THRIFT-2068](https://issues.apache.org/jira/browse/THRIFT-2068) - Multiple calls from same connection are not processed in node
- [THRIFT-1750](https://issues.apache.org/jira/browse/THRIFT-1750) - Make compiler build cleanly under visual studio 10
- [THRIFT-1755](https://issues.apache.org/jira/browse/THRIFT-1755) - Comment parsing bug
- [THRIFT-1771](https://issues.apache.org/jira/browse/THRIFT-1771) - "make check" fails on x64 for libboost_unit_test_framework.a
- [THRIFT-1841](https://issues.apache.org/jira/browse/THRIFT-1841) - NodeJS Thrift incorrectly parses non-UTF8-string types
- [THRIFT-1908](https://issues.apache.org/jira/browse/THRIFT-1908) - Using php thrift_protocol accelerated transfer causes core dump
- [THRIFT-1892](https://issues.apache.org/jira/browse/THRIFT-1892) - Socket timeouts are declared in milli-seconds, but are actually set in micro-seconds
- [THRIFT-2303](https://issues.apache.org/jira/browse/THRIFT-2303) - TBufferredTransport not properly closing underlying transport
- [THRIFT-2313](https://issues.apache.org/jira/browse/THRIFT-2313) - nodejs server crash after processing the first request when using MultiplexedProcessor/FramedBuffer/BinaryProtocol
- [THRIFT-2311](https://issues.apache.org/jira/browse/THRIFT-2311) - Go: invalid code generated when exception name is a go keyword
- [THRIFT-2308](https://issues.apache.org/jira/browse/THRIFT-2308) - node: TJSONProtocol parse error when reading from buffered message
- [THRIFT-2316](https://issues.apache.org/jira/browse/THRIFT-2316) - ccp: TFileTransportTest
- [THRIFT-2352](https://issues.apache.org/jira/browse/THRIFT-2352) - msvc failed to compile thrift tests
- [THRIFT-2337](https://issues.apache.org/jira/browse/THRIFT-2337) - Golang does not report TIMED_OUT exceptions
- [THRIFT-2340](https://issues.apache.org/jira/browse/THRIFT-2340) - Generated server implementation does not send response type EXCEPTION on the Thrift.TApplicationExceptionType.UNKNOWN_METHOD exception
- [THRIFT-2354](https://issues.apache.org/jira/browse/THRIFT-2354) - Connection errors can lead to case_clause exceptions
- [THRIFT-2339](https://issues.apache.org/jira/browse/THRIFT-2339) - Uncaught exception in thrift c# driver
- [THRIFT-2356](https://issues.apache.org/jira/browse/THRIFT-2356) - c++ thrift client not working with ssl (SSL_connect hangs)
- [THRIFT-2331](https://issues.apache.org/jira/browse/THRIFT-2331) - Missing call to ReadStructBegin() in TApplicationException.Read()
- [THRIFT-2323](https://issues.apache.org/jira/browse/THRIFT-2323) - Uncompileable Delphi code generated for typedef'd structs
- [THRIFT-2322](https://issues.apache.org/jira/browse/THRIFT-2322) - Correctly show the number of times ExecutorService (java) has rejected the client.
- [THRIFT-2389](https://issues.apache.org/jira/browse/THRIFT-2389) - namespaces handled wrongly in acrionscript 3.0 implementation
- [THRIFT-2388](https://issues.apache.org/jira/browse/THRIFT-2388) - GoLang - Fix data races in simple_server and server_socket
- [THRIFT-2386](https://issues.apache.org/jira/browse/THRIFT-2386) - Thrift refuses to link yylex
- [THRIFT-2375](https://issues.apache.org/jira/browse/THRIFT-2375) - Excessive <br>'s in generated HTML
- [THRIFT-2373](https://issues.apache.org/jira/browse/THRIFT-2373) - warning CS0414 in THttpClient.cs: private field 'Thrift.Transport.THttpClient.connection' assigned but never used
- [THRIFT-2372](https://issues.apache.org/jira/browse/THRIFT-2372) - thrift/json_protocol.go:160: function ends without a return statement
- [THRIFT-2371](https://issues.apache.org/jira/browse/THRIFT-2371) - ruby bundler version fails on ~1.3.1, remove and take latest avail
- [THRIFT-2370](https://issues.apache.org/jira/browse/THRIFT-2370) - Compiler SEGFAULTs generating HTML documentation for complex strucre
- [THRIFT-2384](https://issues.apache.org/jira/browse/THRIFT-2384) - Binary map keys produce uncompilable code in go
- [THRIFT-2380](https://issues.apache.org/jira/browse/THRIFT-2380) - unreachable code (CID 1174546, CID 1174679)
- [THRIFT-2378](https://issues.apache.org/jira/browse/THRIFT-2378) - service method arguments of binary type lead to uncompileable Go code
- [THRIFT-2363](https://issues.apache.org/jira/browse/THRIFT-2363) - Issue with character encoding of Success returned from Login using Thrift Proxy and NodeJS
- [THRIFT-2359](https://issues.apache.org/jira/browse/THRIFT-2359) - TBufferedTransport doesn't clear it's buffer on a failed flush call
- [THRIFT-2428](https://issues.apache.org/jira/browse/THRIFT-2428) - Python 3 setup.py support
- [THRIFT-2367](https://issues.apache.org/jira/browse/THRIFT-2367) - Build failure: stdlib and boost both define uint64_t
- [THRIFT-2365](https://issues.apache.org/jira/browse/THRIFT-2365) - C# decodes too many binary bytes from JSON
- [THRIFT-2402](https://issues.apache.org/jira/browse/THRIFT-2402) - byte count of FrameBuffer in AWAITING_CLOSE state is not subtracted from readBufferBytesAllocated
- [THRIFT-2396](https://issues.apache.org/jira/browse/THRIFT-2396) - Build Error on MacOSX
- [THRIFT-2395](https://issues.apache.org/jira/browse/THRIFT-2395) - thrift Ruby gem requires development dependency 'thin' regardless of environment
- [THRIFT-2414](https://issues.apache.org/jira/browse/THRIFT-2414) - c_glib fix several bug.
- [THRIFT-2420](https://issues.apache.org/jira/browse/THRIFT-2420) - Go argument parser for methods without arguments does not skip fields
- [THRIFT-2439](https://issues.apache.org/jira/browse/THRIFT-2439) - Bug in TProtocolDecorator Class causes parsing errors
- [THRIFT-2419](https://issues.apache.org/jira/browse/THRIFT-2419) - golang - Fix fmt.Errorf in generated code
- [THRIFT-2418](https://issues.apache.org/jira/browse/THRIFT-2418) - Go handler function panics on internal error
- [THRIFT-2405](https://issues.apache.org/jira/browse/THRIFT-2405) - Node.js Multiplexer tests fail (silently)
- [THRIFT-2581](https://issues.apache.org/jira/browse/THRIFT-2581) - TFDTransport destructor should not throw
- [THRIFT-2575](https://issues.apache.org/jira/browse/THRIFT-2575) - Thrift includes siginfo_t within apache::thrift::protocol namespace
- [THRIFT-2577](https://issues.apache.org/jira/browse/THRIFT-2577) - TFileTransport  missuse of closesocket on windows platform
- [THRIFT-2576](https://issues.apache.org/jira/browse/THRIFT-2576) - Implement Thrift.Protocol.prototype.skip method in JavaScript library
- [THRIFT-2588](https://issues.apache.org/jira/browse/THRIFT-2588) - Thrift compiler is not buildable in Visual Studio 2010
- [THRIFT-2594](https://issues.apache.org/jira/browse/THRIFT-2594) - JS Compiler: Single quotes are not being escaped in constants.
- [THRIFT-2591](https://issues.apache.org/jira/browse/THRIFT-2591) - TFramedTransport does not handle payloads split across packets correctly
- [THRIFT-2599](https://issues.apache.org/jira/browse/THRIFT-2599) - Uncompileable Delphi code due to naming conflicts with IDL
- [THRIFT-2590](https://issues.apache.org/jira/browse/THRIFT-2590) - C++ Visual Studio solution doesn't include Multiplexing support
- [THRIFT-2595](https://issues.apache.org/jira/browse/THRIFT-2595) - Node.js: Fix global leaks and copy-paste errors
- [THRIFT-2565](https://issues.apache.org/jira/browse/THRIFT-2565) - autoconf fails to find mingw-g++ cross compiler on travis CI
- [THRIFT-2555](https://issues.apache.org/jira/browse/THRIFT-2555) - excessive "unused field" comments
- [THRIFT-2554](https://issues.apache.org/jira/browse/THRIFT-2554) - double initialization in generated Read() method
- [THRIFT-2551](https://issues.apache.org/jira/browse/THRIFT-2551) - OutOfMemoryError "unable to create new native thread" kills serve thread
- [THRIFT-2543](https://issues.apache.org/jira/browse/THRIFT-2543) - Generated enum type in haskell should be qualified
- [THRIFT-2560](https://issues.apache.org/jira/browse/THRIFT-2560) - Thrift compiler generator tries to concat ints with strings using +
- [THRIFT-2559](https://issues.apache.org/jira/browse/THRIFT-2559) - Centos 6.5 unable to "make" with Thrift 0.9.1
- [THRIFT-2526](https://issues.apache.org/jira/browse/THRIFT-2526) - Assignment operators and copy constructors in c++ don't copy the __isset struct
- [THRIFT-2454](https://issues.apache.org/jira/browse/THRIFT-2454) - c_glib: There is no gethostbyname_r() in some OS.
- [THRIFT-2451](https://issues.apache.org/jira/browse/THRIFT-2451) - Do not use pointers for optional fields with defaults. Do not write such fields if its value set to default. Also, do not use pointers for any optional fields mapped to go map or slice. generate Get accessors
- [THRIFT-2450](https://issues.apache.org/jira/browse/THRIFT-2450) - include HowToContribute in the src repo
- [THRIFT-2448](https://issues.apache.org/jira/browse/THRIFT-2448) - thrift/test/test.sh has incorrect Node.js test path
- [THRIFT-2460](https://issues.apache.org/jira/browse/THRIFT-2460) - unopened socket fd must be less than zero.
- [THRIFT-2459](https://issues.apache.org/jira/browse/THRIFT-2459) - --version should not exit 1
- [THRIFT-2468](https://issues.apache.org/jira/browse/THRIFT-2468) - Timestamp handling
- [THRIFT-2467](https://issues.apache.org/jira/browse/THRIFT-2467) - Unable to build contrib/fb303 on OSX 10.9.2
- [THRIFT-2466](https://issues.apache.org/jira/browse/THRIFT-2466) - Improper error handling for SSL/TLS connections that don't complete a handshake
- [THRIFT-2463](https://issues.apache.org/jira/browse/THRIFT-2463) - test/py/RunClientServer.py fails sometimes
- [THRIFT-2458](https://issues.apache.org/jira/browse/THRIFT-2458) - Generated golang server code for "oneway" methods is incorrect
- [THRIFT-2456](https://issues.apache.org/jira/browse/THRIFT-2456) - THttpClient fails when using async support outside Silverlight
- [THRIFT-2524](https://issues.apache.org/jira/browse/THRIFT-2524) - Visual Studio project is missing TThreadedServer files
- [THRIFT-2523](https://issues.apache.org/jira/browse/THRIFT-2523) - Visual Studio project is missing OverlappedSubmissionThread files
- [THRIFT-2520](https://issues.apache.org/jira/browse/THRIFT-2520) - cpp:cob_style generates incorrect .tcc file
- [THRIFT-2508](https://issues.apache.org/jira/browse/THRIFT-2508) - Uncompileable C# code due to language keywords in IDL
- [THRIFT-2506](https://issues.apache.org/jira/browse/THRIFT-2506) - Update TProtocolException error codes to be used consistently throughout the library
- [THRIFT-2505](https://issues.apache.org/jira/browse/THRIFT-2505) - go: struct should always be a pointer to avoid copying of potentially size-unbounded structs
- [THRIFT-2515](https://issues.apache.org/jira/browse/THRIFT-2515) - TLS Method error during make
- [THRIFT-2503](https://issues.apache.org/jira/browse/THRIFT-2503) - C++: Fix name collision when a struct has a member named "val"
- [THRIFT-2477](https://issues.apache.org/jira/browse/THRIFT-2477) - thrift --help text with misplaced comma
- [THRIFT-2492](https://issues.apache.org/jira/browse/THRIFT-2492) - test/cpp does not compile on mac
- [THRIFT-2500](https://issues.apache.org/jira/browse/THRIFT-2500) - sending random data crashes thrift(golang) service
- [THRIFT-2475](https://issues.apache.org/jira/browse/THRIFT-2475) - c_glib: buffered_transport_write function return always TRUE.
- [THRIFT-2495](https://issues.apache.org/jira/browse/THRIFT-2495) - JavaScript/Node string constants lack proper escaping
- [THRIFT-2491](https://issues.apache.org/jira/browse/THRIFT-2491) - unable to import generated ThriftTest service
- [THRIFT-2490](https://issues.apache.org/jira/browse/THRIFT-2490) - c_glib: if fail to read a exception from server, client may be occurred double free
- [THRIFT-2470](https://issues.apache.org/jira/browse/THRIFT-2470) - THttpHandler swallows exceptions from processor
- [THRIFT-2533](https://issues.apache.org/jira/browse/THRIFT-2533) - Boost version in requirements should be updated
- [THRIFT-2532](https://issues.apache.org/jira/browse/THRIFT-2532) - Java version in installation requirements should be updated
- [THRIFT-2529](https://issues.apache.org/jira/browse/THRIFT-2529) - TBufferedTransport split  Tcp data bug in nodeJs
- [THRIFT-2537](https://issues.apache.org/jira/browse/THRIFT-2537) - Path for "go get" does not work (pull request 115)
- [THRIFT-2443](https://issues.apache.org/jira/browse/THRIFT-2443) - Node fails cross lang tests
- [THRIFT-2437](https://issues.apache.org/jira/browse/THRIFT-2437) - Author fields in Python setup.py must be strings not lists.
- [THRIFT-2435](https://issues.apache.org/jira/browse/THRIFT-2435) - Java compiler doesn't like struct member names that are identical to an existing enum or struct type
- [THRIFT-2434](https://issues.apache.org/jira/browse/THRIFT-2434) - Missing namespace import for php TMultiplexedProcessor implementation
- [THRIFT-2432](https://issues.apache.org/jira/browse/THRIFT-2432) - Flaky parallel build
- [THRIFT-2430](https://issues.apache.org/jira/browse/THRIFT-2430) - Crash during TThreadPoolServer shutdown
- [THRIFT-667](https://issues.apache.org/jira/browse/THRIFT-667) - Period should not be allowed in identifier names
- [THRIFT-1212](https://issues.apache.org/jira/browse/THRIFT-1212) - Members capital case conflict
- [THRIFT-2584](https://issues.apache.org/jira/browse/THRIFT-2584) - Error handler not listened on javascript client
- [THRIFT-2294](https://issues.apache.org/jira/browse/THRIFT-2294) - Incorrect Makefile generation
- [THRIFT-2601](https://issues.apache.org/jira/browse/THRIFT-2601) - Fix vagrant to work again for builds again
- [THRIFT-2092](https://issues.apache.org/jira/browse/THRIFT-2092) - TNonblocking server should release handler as soon as connection closes
- [THRIFT-2557](https://issues.apache.org/jira/browse/THRIFT-2557) - CS0542 member names cannot be the same as their enclosing type
- [THRIFT-2605](https://issues.apache.org/jira/browse/THRIFT-2605) - TSocket warning on gcc 4.8.3
- [THRIFT-2607](https://issues.apache.org/jira/browse/THRIFT-2607) - ThreadManager.cpp warning on clang++ 3.4
- [THRIFT-1998](https://issues.apache.org/jira/browse/THRIFT-1998) - TCompactProtocol.tcc - one more warning on Visual 2010
- [THRIFT-2610](https://issues.apache.org/jira/browse/THRIFT-2610) - MSVC warning in TSocket.cpp
- [THRIFT-2614](https://issues.apache.org/jira/browse/THRIFT-2614) - TNonblockingServer.cpp warnings on MSVC
- [THRIFT-2608](https://issues.apache.org/jira/browse/THRIFT-2608) - TNonblockingServer.cpp warnings on clang 3.4
- [THRIFT-2606](https://issues.apache.org/jira/browse/THRIFT-2606) - ThreadManager.h warning in clang++ 3.4
- [THRIFT-2609](https://issues.apache.org/jira/browse/THRIFT-2609) - TFileTransport.h unused field warning (clang 3.4)
- [THRIFT-2416](https://issues.apache.org/jira/browse/THRIFT-2416) - Cannot use TCompactProtocol with MSVC
- [THRIFT-1803](https://issues.apache.org/jira/browse/THRIFT-1803) - Ruby Thrift 0.9.0 tries to encode UUID to UTF8 and crashes
- [THRIFT-2385](https://issues.apache.org/jira/browse/THRIFT-2385) - Problem with gethostbyname2 during make check
- [THRIFT-2262](https://issues.apache.org/jira/browse/THRIFT-2262) - thrift server 'MutateRow' operation gives no indication of success / failure
- [THRIFT-2048](https://issues.apache.org/jira/browse/THRIFT-2048) - Prefer boolean context to nullptr_t conversion
- [THRIFT-2528](https://issues.apache.org/jira/browse/THRIFT-2528) - Thrift Erlang Library: Multiple thrift applications in one bundle
- [THRIFT-1999](https://issues.apache.org/jira/browse/THRIFT-1999) - warning on gcc 4.7 while compiling BoostMutex.cpp
- [THRIFT-2104](https://issues.apache.org/jira/browse/THRIFT-2104) - Structs lose binary data when transferred from server to client in Java
- [THRIFT-2184](https://issues.apache.org/jira/browse/THRIFT-2184) - undefined method rspec_verify for Thrift::MemoryBufferTransport
- [THRIFT-2351](https://issues.apache.org/jira/browse/THRIFT-2351) - PHP TCompactProtocol has fails to decode messages
- [THRIFT-2016](https://issues.apache.org/jira/browse/THRIFT-2016) - Resource Leak in thrift struct under compiler/cpp/src/parse/t_function.h
- [THRIFT-2273](https://issues.apache.org/jira/browse/THRIFT-2273) - Please delete old releases from mirroring system
- [THRIFT-2270](https://issues.apache.org/jira/browse/THRIFT-2270) - Faulty library version numbering at build or documentation
- [THRIFT-2203](https://issues.apache.org/jira/browse/THRIFT-2203) - Tests keeping failing on Jenkins and Travis CI
- [THRIFT-2399](https://issues.apache.org/jira/browse/THRIFT-2399) - thrift.el: recognize "//"-style comments in emacs thrift-mode
- [THRIFT-2582](https://issues.apache.org/jira/browse/THRIFT-2582) - "FileTransport error" exception is raised when trying to use Java's TFileTransport
- [THRIFT-1682](https://issues.apache.org/jira/browse/THRIFT-1682) - Multiple thread calling a Service function unsafely causes message corruption and terminates with Broken Pipe
- [THRIFT-2357](https://issues.apache.org/jira/browse/THRIFT-2357) - recurse option has no effect when generating php
- [THRIFT-2248](https://issues.apache.org/jira/browse/THRIFT-2248) - Go generator doesn't deal well with map keys of type binary
- [THRIFT-2426](https://issues.apache.org/jira/browse/THRIFT-2426) - clarify IP rights and contributions from fbthrift
- [THRIFT-2041](https://issues.apache.org/jira/browse/THRIFT-2041) - TNonblocking server compilation on windows (ARITHMETIC_RIGHT_SHIFT)
- [THRIFT-2400](https://issues.apache.org/jira/browse/THRIFT-2400) - thrift.el: recognize "//"-style comments in emacs thrift-mode
- [THRIFT-1717](https://issues.apache.org/jira/browse/THRIFT-1717) - Fix deb build in jenkins
- [THRIFT-2266](https://issues.apache.org/jira/browse/THRIFT-2266) - ThreadManager.h:24:10: fatal error: 'tr1/functional' file not found on Mac 10.9 (Mavericks)
- [THRIFT-1300](https://issues.apache.org/jira/browse/THRIFT-1300) - Test failures with parallel builds (make -j)
- [THRIFT-2487](https://issues.apache.org/jira/browse/THRIFT-2487) - Tutorial requires two IDL files but only one is linked from the Thrift web site
- [THRIFT-2329](https://issues.apache.org/jira/browse/THRIFT-2329) - missing release tags within git
- [THRIFT-2306](https://issues.apache.org/jira/browse/THRIFT-2306) - concurent client calls with nodejs
- [THRIFT-2222](https://issues.apache.org/jira/browse/THRIFT-2222) - ruby gem cannot be compiled on OS X mavericks
- [THRIFT-2381](https://issues.apache.org/jira/browse/THRIFT-2381) - code which generated by thrift2/hbase.thrift compile error
- [THRIFT-2390](https://issues.apache.org/jira/browse/THRIFT-2390) - no close event when connection lost
- [THRIFT-2146](https://issues.apache.org/jira/browse/THRIFT-2146) - Unable to pass multiple "--gen" options to the thrift compiler
- [THRIFT-2438](https://issues.apache.org/jira/browse/THRIFT-2438) - Unexpected readFieldEnd call causes JSON Parsing errors
- [THRIFT-2498](https://issues.apache.org/jira/browse/THRIFT-2498) - Error message "Invalid method name" while trying to call HBase Thrift API
- [THRIFT-841](https://issues.apache.org/jira/browse/THRIFT-841) - Build cruft
- [THRIFT-2570](https://issues.apache.org/jira/browse/THRIFT-2570) - Wrong URL given in http://thrift.apache.org/developers
- [THRIFT-2604](https://issues.apache.org/jira/browse/THRIFT-2604) - Fix debian packaging
- [THRIFT-2618](https://issues.apache.org/jira/browse/THRIFT-2618) - Unignore /aclocal files required for build
- [THRIFT-2562](https://issues.apache.org/jira/browse/THRIFT-2562) - ./configure create MakeFile in lib/d with errors
- [THRIFT-2593](https://issues.apache.org/jira/browse/THRIFT-2593) - Unable to build thrift on ubuntu-12.04 (Precise)
- [THRIFT-2461](https://issues.apache.org/jira/browse/THRIFT-2461) - Can't install thrift-0.8.0 on OS X 10.9.2
- [THRIFT-2602](https://issues.apache.org/jira/browse/THRIFT-2602) - Fix missing dist files
- [THRIFT-2620](https://issues.apache.org/jira/browse/THRIFT-2620) - Fix python packaging
- [THRIFT-2545](https://issues.apache.org/jira/browse/THRIFT-2545) - Test CPP fails to build (possibly typo)

## Documentation
- [THRIFT-2155](https://issues.apache.org/jira/browse/THRIFT-2155) - Adding one liner guide to rename the version.h.in and rename thrifty.cc.h
- [THRIFT-1991](https://issues.apache.org/jira/browse/THRIFT-1991) - Add exceptions to examples
- [THRIFT-2334](https://issues.apache.org/jira/browse/THRIFT-2334) - add a tutorial for node JS
- [THRIFT-2392](https://issues.apache.org/jira/browse/THRIFT-2392) - Actionscript tutorial
- [THRIFT-2383](https://issues.apache.org/jira/browse/THRIFT-2383) - contrib: sample for connecting Thrift with Rebus
- [THRIFT-2382](https://issues.apache.org/jira/browse/THRIFT-2382) - contrib: sample for connecting Thrift with STOMP

### Improvement
- [THRIFT-1457](https://issues.apache.org/jira/browse/THRIFT-1457) - Capacity of TframedTransport write buffer is never reset
- [THRIFT-1135](https://issues.apache.org/jira/browse/THRIFT-1135) - Node.js tutorial
- [THRIFT-1371](https://issues.apache.org/jira/browse/THRIFT-1371) - Socket timeouts (SO_RCVTIMEO and SO_SNDTIMEO) not supported on Solaris
- [THRIFT-2142](https://issues.apache.org/jira/browse/THRIFT-2142) - Minor tweaks to thrift.el for better emacs package compatibility
- [THRIFT-2268](https://issues.apache.org/jira/browse/THRIFT-2268) - Modify TSaslTransport to ignore TCP health checks from loadbalancers
- [THRIFT-2264](https://issues.apache.org/jira/browse/THRIFT-2264) - GitHub page incorrectly states that Thrift is still incubating
- [THRIFT-2263](https://issues.apache.org/jira/browse/THRIFT-2263) - Always generate good hashCode for Java
- [THRIFT-2233](https://issues.apache.org/jira/browse/THRIFT-2233) - Java compiler should defensively copy its binary inputs
- [THRIFT-2239](https://issues.apache.org/jira/browse/THRIFT-2239) - Address FindBugs errors
- [THRIFT-2249](https://issues.apache.org/jira/browse/THRIFT-2249) - Add SMP Build option to thrift.spec (and three config defines)
- [THRIFT-2254](https://issues.apache.org/jira/browse/THRIFT-2254) - Exceptions generated by Go compiler should implement error interface
- [THRIFT-2260](https://issues.apache.org/jira/browse/THRIFT-2260) - Thrift imposes unneeded dependency on commons-lang3
- [THRIFT-2258](https://issues.apache.org/jira/browse/THRIFT-2258) - Add TLS v1.1/1.2 support to TSSLSocket.cpp
- [THRIFT-2205](https://issues.apache.org/jira/browse/THRIFT-2205) - Node.js Test Server to support test.js JavaScript Browser test and sundry fixes
- [THRIFT-2204](https://issues.apache.org/jira/browse/THRIFT-2204) - SSL client for the cocoa client
- [THRIFT-2172](https://issues.apache.org/jira/browse/THRIFT-2172) - Java compiler allocates optionals array for every struct with an optional field
- [THRIFT-2185](https://issues.apache.org/jira/browse/THRIFT-2185) - use cabal instead of runhaskell in haskell library
- [THRIFT-1926](https://issues.apache.org/jira/browse/THRIFT-1926) - PHP Constant Generation Refactoring
- [THRIFT-2029](https://issues.apache.org/jira/browse/THRIFT-2029) - Port C++ tests to Windows
- [THRIFT-2054](https://issues.apache.org/jira/browse/THRIFT-2054) - TSimpleFileTransport - Java Lib has no straight forward TTransport based file transport
- [THRIFT-2040](https://issues.apache.org/jira/browse/THRIFT-2040) - "uninitialized variable" warnings on MSVC/windows
- [THRIFT-2034](https://issues.apache.org/jira/browse/THRIFT-2034) - Give developers' C++ code direct access to socket FDs on server side
- [THRIFT-2095](https://issues.apache.org/jira/browse/THRIFT-2095) - Use print function for Python 3 compatibility
- [THRIFT-1868](https://issues.apache.org/jira/browse/THRIFT-1868) - Make the TPC backlog configurable in the Java servers
- [THRIFT-1813](https://issues.apache.org/jira/browse/THRIFT-1813) - Add @Generated annotation to generated classes
- [THRIFT-1815](https://issues.apache.org/jira/browse/THRIFT-1815) - Code generators line buffer output
- [THRIFT-2305](https://issues.apache.org/jira/browse/THRIFT-2305) - TFramedTransport empty constructor should probably be private
- [THRIFT-2304](https://issues.apache.org/jira/browse/THRIFT-2304) - Move client assignments from construtor in method
- [THRIFT-2309](https://issues.apache.org/jira/browse/THRIFT-2309) - Ruby (gem) & PHP RPM subpackages
- [THRIFT-2318](https://issues.apache.org/jira/browse/THRIFT-2318) - perl: dependency Class::Accessor not checked
- [THRIFT-2317](https://issues.apache.org/jira/browse/THRIFT-2317) - exclude tutorial from build
- [THRIFT-2320](https://issues.apache.org/jira/browse/THRIFT-2320) - Program level doctext does not get attached by parser
- [THRIFT-2349](https://issues.apache.org/jira/browse/THRIFT-2349) - Golang - improve tutorial
- [THRIFT-2348](https://issues.apache.org/jira/browse/THRIFT-2348) - PHP Generator: add array typehint to functions
- [THRIFT-2344](https://issues.apache.org/jira/browse/THRIFT-2344) - configure.ac: compiler-only option
- [THRIFT-2343](https://issues.apache.org/jira/browse/THRIFT-2343) - Golang - Return a single error for all exceptions instead of multiple return values
- [THRIFT-2341](https://issues.apache.org/jira/browse/THRIFT-2341) - Enable generation of Delphi XMLDoc comments (a.k.a. "Help Insight")
- [THRIFT-2355](https://issues.apache.org/jira/browse/THRIFT-2355) - Add SSL and Web Socket Support to Node and JavaScript
- [THRIFT-2350](https://issues.apache.org/jira/browse/THRIFT-2350) - Add async calls to normal JavaScript
- [THRIFT-2330](https://issues.apache.org/jira/browse/THRIFT-2330) - Generate PHPDoc comments
- [THRIFT-2332](https://issues.apache.org/jira/browse/THRIFT-2332) - RPMBUILD: run bootstrap (if needed)
- [THRIFT-2391](https://issues.apache.org/jira/browse/THRIFT-2391) - simple socket transport for actionscript 3.0
- [THRIFT-2376](https://issues.apache.org/jira/browse/THRIFT-2376) - nodejs: allow Promise style calls for client and server
- [THRIFT-2369](https://issues.apache.org/jira/browse/THRIFT-2369) - Add ssl support for nodejs implementation
- [THRIFT-2401](https://issues.apache.org/jira/browse/THRIFT-2401) - Haskell tutorial compiles
- [THRIFT-2417](https://issues.apache.org/jira/browse/THRIFT-2417) - C# Union classes are not partial
- [THRIFT-2415](https://issues.apache.org/jira/browse/THRIFT-2415) - Named pipes server performance & message mode
- [THRIFT-2404](https://issues.apache.org/jira/browse/THRIFT-2404) - emit warning on (typically inefficient) list<byte>
- [THRIFT-2398](https://issues.apache.org/jira/browse/THRIFT-2398) - Improve Node Server Library
- [THRIFT-2397](https://issues.apache.org/jira/browse/THRIFT-2397) - Add CORS and CSP support for JavaScript and Node.js libraries
- [THRIFT-2407](https://issues.apache.org/jira/browse/THRIFT-2407) - use markdown (rename README => README.md)
- [THRIFT-2300](https://issues.apache.org/jira/browse/THRIFT-2300) - D configure info output should follow same format as other languages
- [THRIFT-2579](https://issues.apache.org/jira/browse/THRIFT-2579) - Windows CE support
- [THRIFT-2574](https://issues.apache.org/jira/browse/THRIFT-2574) - Compiler option to generate namespace directories for Ruby
- [THRIFT-2571](https://issues.apache.org/jira/browse/THRIFT-2571) - Simplify cross compilation using CMake
- [THRIFT-2569](https://issues.apache.org/jira/browse/THRIFT-2569) - Introduce file to specify third party library locations on Windows
- [THRIFT-2568](https://issues.apache.org/jira/browse/THRIFT-2568) - Implement own certificate handler
- [THRIFT-2552](https://issues.apache.org/jira/browse/THRIFT-2552) - eliminate warning from configure.ac
- [THRIFT-2549](https://issues.apache.org/jira/browse/THRIFT-2549) - Generate json tag for struct members. use go.tag annotation to override the default generated tag.
- [THRIFT-2544](https://issues.apache.org/jira/browse/THRIFT-2544) - Add support for socket transport for c# library when using Windows Phone projects
- [THRIFT-2453](https://issues.apache.org/jira/browse/THRIFT-2453) - haskell tutorial: fix up division by 0 example
- [THRIFT-2449](https://issues.apache.org/jira/browse/THRIFT-2449) - Enhance typedef structure to distinguish between forwards and real typedefs
- [THRIFT-2446](https://issues.apache.org/jira/browse/THRIFT-2446) - There is no way to handle server stream errors
- [THRIFT-2455](https://issues.apache.org/jira/browse/THRIFT-2455) - Allow client certificates to be used with THttpClient
- [THRIFT-2511](https://issues.apache.org/jira/browse/THRIFT-2511) - Node.js needs the compact protocol
- [THRIFT-2493](https://issues.apache.org/jira/browse/THRIFT-2493) - Node.js lib needs HTTP client
- [THRIFT-2502](https://issues.apache.org/jira/browse/THRIFT-2502) - Optimize go implementations of binary and compact protocols for speed
- [THRIFT-2494](https://issues.apache.org/jira/browse/THRIFT-2494) - Add enum toString helper function in c_glib
- [THRIFT-2471](https://issues.apache.org/jira/browse/THRIFT-2471) - Make cpp.ref annotation language agnostic
- [THRIFT-2497](https://issues.apache.org/jira/browse/THRIFT-2497) - server and client for test/go, also several fixes and improvements
- [THRIFT-2535](https://issues.apache.org/jira/browse/THRIFT-2535) - TJSONProtocol when serialized yields TField ids rather than names
- [THRIFT-2220](https://issues.apache.org/jira/browse/THRIFT-2220) - Add a new struct structv?
- [THRIFT-1352](https://issues.apache.org/jira/browse/THRIFT-1352) - Thrift server
- [THRIFT-989](https://issues.apache.org/jira/browse/THRIFT-989) - Push boost m4 macros upstream
- [THRIFT-1349](https://issues.apache.org/jira/browse/THRIFT-1349) - Remove unnecessary print outs
- [THRIFT-2496](https://issues.apache.org/jira/browse/THRIFT-2496) - server and client for test/go, also several fixes and improvements
- [THRIFT-1114](https://issues.apache.org/jira/browse/THRIFT-1114) - Maven publish shouldn't require passwords hardcoded in settings.xml
- [THRIFT-2043](https://issues.apache.org/jira/browse/THRIFT-2043) - visual 2010 warnings - unreachable code
- [THRIFT-1683](https://issues.apache.org/jira/browse/THRIFT-1683) - Implement alternatives to Javascript Client side Transport protocol, just as NPAPI and WebSocket.
- [THRIFT-1746](https://issues.apache.org/jira/browse/THRIFT-1746) - provide a SPDX file
- [THRIFT-1772](https://issues.apache.org/jira/browse/THRIFT-1772) - Serialization does not check types of embedded structures.
- [THRIFT-2387](https://issues.apache.org/jira/browse/THRIFT-2387) - nodejs: external imports should be centralized in index.js
- [THRIFT-2037](https://issues.apache.org/jira/browse/THRIFT-2037) - More general macro THRIFT_UNUSED_VARIABLE

### New Feature
- [THRIFT-1012](https://issues.apache.org/jira/browse/THRIFT-1012) - Transport for DataInput DataOutput interface
- [THRIFT-2256](https://issues.apache.org/jira/browse/THRIFT-2256) - Using c++11/c++0x std library  replace boost library
- [THRIFT-2250](https://issues.apache.org/jira/browse/THRIFT-2250) - JSON and MemoryBuffer for JavaME
- [THRIFT-2114](https://issues.apache.org/jira/browse/THRIFT-2114) - Python Service Remote SSL Option
- [THRIFT-1719](https://issues.apache.org/jira/browse/THRIFT-1719) - SASL client support for Python
- [THRIFT-1894](https://issues.apache.org/jira/browse/THRIFT-1894) - Thrift multi-threaded async Java Server using Java 7 AsynchronousChannelGroup
- [THRIFT-1893](https://issues.apache.org/jira/browse/THRIFT-1893) - HTTP/JSON server/client for node js
- [THRIFT-2347](https://issues.apache.org/jira/browse/THRIFT-2347) - C# TLS Transport based on THRIFT-181
- [THRIFT-2377](https://issues.apache.org/jira/browse/THRIFT-2377) - Allow addition of custom HTTP Headers to an HTTP Transport
- [THRIFT-2408](https://issues.apache.org/jira/browse/THRIFT-2408) - Named Pipe Transport Option for C#
- [THRIFT-2572](https://issues.apache.org/jira/browse/THRIFT-2572) - Add string/collection length limit checks (from C++) to java protocol readers
- [THRIFT-2469](https://issues.apache.org/jira/browse/THRIFT-2469) - "java:fullcamel" option to automatically camel-case underscored attribute names
- [THRIFT-795](https://issues.apache.org/jira/browse/THRIFT-795) - Importing service functions (simulation multiple inheritance)
- [THRIFT-2164](https://issues.apache.org/jira/browse/THRIFT-2164) - Add a Get/Post Http Server to Node along with examples
- [THRIFT-2255](https://issues.apache.org/jira/browse/THRIFT-2255) - add Parent Class for generated Struct class

### Question
- [THRIFT-2539](https://issues.apache.org/jira/browse/THRIFT-2539) - Tsocket.cpp addrinfo ai_flags = AI_ADDRCONFIG
- [THRIFT-2440](https://issues.apache.org/jira/browse/THRIFT-2440) - how to connect as3 to java by thrift ,
- [THRIFT-2379](https://issues.apache.org/jira/browse/THRIFT-2379) - Memmory leaking while using multithreading in C++ server.
- [THRIFT-2277](https://issues.apache.org/jira/browse/THRIFT-2277) - Thrift: installing fb303 error
- [THRIFT-2567](https://issues.apache.org/jira/browse/THRIFT-2567) - Csharp slow ?
- [THRIFT-2573](https://issues.apache.org/jira/browse/THRIFT-2573) - thrift 0.9.2 release

### Sub-task
- [THRIFT-981](https://issues.apache.org/jira/browse/THRIFT-981) - cocoa: add version Info to the library
- [THRIFT-2132](https://issues.apache.org/jira/browse/THRIFT-2132) - Go: Support for Multiplexing Services on any Transport, Protocol and Server
- [THRIFT-2299](https://issues.apache.org/jira/browse/THRIFT-2299) - TJsonProtocol implementation for Ruby does not allow for both possible slash (solidus) encodings
- [THRIFT-2298](https://issues.apache.org/jira/browse/THRIFT-2298) - TJsonProtocol implementation for C# does not allow for both possible slash (solidus) encodings
- [THRIFT-2297](https://issues.apache.org/jira/browse/THRIFT-2297) - TJsonProtocol implementation for Delphi does not allow for both possible slash (solidus) encodings
- [THRIFT-2271](https://issues.apache.org/jira/browse/THRIFT-2271) - JavaScript: Support for Multiplexing Services
- [THRIFT-2251](https://issues.apache.org/jira/browse/THRIFT-2251) - go test for compact protocol is not running
- [THRIFT-2195](https://issues.apache.org/jira/browse/THRIFT-2195) - Delphi: Add event handlers for server and processing events
- [THRIFT-2176](https://issues.apache.org/jira/browse/THRIFT-2176) - TSimpleJSONProtocol.ReadFieldBegin() does not return field type and ID
- [THRIFT-2175](https://issues.apache.org/jira/browse/THRIFT-2175) - Wrong field type set for binary
- [THRIFT-2174](https://issues.apache.org/jira/browse/THRIFT-2174) - Deserializing JSON fails in specific cases
- [THRIFT-2053](https://issues.apache.org/jira/browse/THRIFT-2053) - NodeJS: Support for Multiplexing Services
- [THRIFT-1914](https://issues.apache.org/jira/browse/THRIFT-1914) - Python: Support for Multiplexing Services on any Transport, Protocol and Server
- [THRIFT-1810](https://issues.apache.org/jira/browse/THRIFT-1810) - add ruby to test/test.sh
- [THRIFT-2310](https://issues.apache.org/jira/browse/THRIFT-2310) - PHP: Client-side support for Multiplexing Services
- [THRIFT-2346](https://issues.apache.org/jira/browse/THRIFT-2346) - C#: UTF-8 sent by PHP as JSON is not understood by TJsonProtocol
- [THRIFT-2345](https://issues.apache.org/jira/browse/THRIFT-2345) - Delphi: UTF-8 sent by PHP as JSON is not understood by TJsonProtocol
- [THRIFT-2338](https://issues.apache.org/jira/browse/THRIFT-2338) - First doctext wrongly interpreted as program doctext in some cases
- [THRIFT-2325](https://issues.apache.org/jira/browse/THRIFT-2325) - SSL test certificates
- [THRIFT-2358](https://issues.apache.org/jira/browse/THRIFT-2358) - C++: add compact protocol to cross language test suite
- [THRIFT-2425](https://issues.apache.org/jira/browse/THRIFT-2425) - PHP: Server-side support for Multiplexing Services
- [THRIFT-2421](https://issues.apache.org/jira/browse/THRIFT-2421) - Tree/Recursive struct support in thrift
- [THRIFT-2290](https://issues.apache.org/jira/browse/THRIFT-2290) - Update Go tutorial to align with THRIFT-2232
- [THRIFT-2558](https://issues.apache.org/jira/browse/THRIFT-2558) - CSharp compiler generator tries to concat ints with strings using +
- [THRIFT-2507](https://issues.apache.org/jira/browse/THRIFT-2507) - Additional LUA TProtocolException error code needed?
- [THRIFT-2499](https://issues.apache.org/jira/browse/THRIFT-2499) - Compiler: allow annotations without "= value"
- [THRIFT-2534](https://issues.apache.org/jira/browse/THRIFT-2534) - Cross language test results should recorded to a status.md or status.html file automatically
- [THRIFT-66](https://issues.apache.org/jira/browse/THRIFT-66) - Java: Allow multiplexing multiple services over a single TCP connection
- [THRIFT-1681](https://issues.apache.org/jira/browse/THRIFT-1681) - Add Lua Support
- [THRIFT-1727](https://issues.apache.org/jira/browse/THRIFT-1727) - Ruby-1.9: data loss: "binary" fields are re-encoded
- [THRIFT-1726](https://issues.apache.org/jira/browse/THRIFT-1726) - Ruby-1.9: "binary" fields are represented by string whose encoding is "UTF-8"
- [THRIFT-988](https://issues.apache.org/jira/browse/THRIFT-988) - perl: add version Info to the library via configure
- [THRIFT-334](https://issues.apache.org/jira/browse/THRIFT-334) - Compact Protocol for PHP
- [THRIFT-2444](https://issues.apache.org/jira/browse/THRIFT-2444) - pull request 88: thrift: clean up enum value assignment

### Task
- [THRIFT-2223](https://issues.apache.org/jira/browse/THRIFT-2223) - Spam links on wiki
- [THRIFT-2566](https://issues.apache.org/jira/browse/THRIFT-2566) - Please create a DOAP file for your TLP
- [THRIFT-2237](https://issues.apache.org/jira/browse/THRIFT-2237) - Update archive to contain all versions
- [THRIFT-962](https://issues.apache.org/jira/browse/THRIFT-962) - Tutorial page on our website is really unhelpful

### Test
- [THRIFT-2327](https://issues.apache.org/jira/browse/THRIFT-2327) - nodejs: nodejs test suite should be bundled with the library
- [THRIFT-2445](https://issues.apache.org/jira/browse/THRIFT-2445) - THRIFT-2384 (code generation for go maps with binary keys) should be tested
- [THRIFT-2501](https://issues.apache.org/jira/browse/THRIFT-2501) - C# The test parameters from the TestServer and TestClient are different from the http://thrift.apache.org/test/

### Wish
- [THRIFT-2190](https://issues.apache.org/jira/browse/THRIFT-2190) - Add the JavaScript thrift.js lib to the Bower registry
- [THRIFT-2076](https://issues.apache.org/jira/browse/THRIFT-2076) - boost::optional instead of __isset

## 0.9.1

### Bug
- [THRIFT-1440](https://issues.apache.org/jira/browse/THRIFT-1440) - debian packaging: minor-ish policy problems
- [THRIFT-1402](https://issues.apache.org/jira/browse/THRIFT-1402) - Generated Y_types.js does not require() X_types.js when an include in the IDL file was used
- [THRIFT-1551](https://issues.apache.org/jira/browse/THRIFT-1551) - 2 thrift file define only struct (no service), one include another, the gen nodejs file didn't have "requires" at the top
- [THRIFT-1264](https://issues.apache.org/jira/browse/THRIFT-1264) - TSocketClient is queried by run loop after deallocation in Cocoa
- [THRIFT-1600](https://issues.apache.org/jira/browse/THRIFT-1600) - Thrift Go Compiler and Library out of date with Go 1 Release.
- [THRIFT-1603](https://issues.apache.org/jira/browse/THRIFT-1603) - Thrift IDL allows for multiple exceptions, args or struct member names to be the same
- [THRIFT-1062](https://issues.apache.org/jira/browse/THRIFT-1062) - Problems with python tutorials
- [THRIFT-864](https://issues.apache.org/jira/browse/THRIFT-864) - default value fails if identifier is a struct
- [THRIFT-930](https://issues.apache.org/jira/browse/THRIFT-930) - Ruby and Haskell bindings don't properly support DESTDIR (makes packaging painful)
- [THRIFT-820](https://issues.apache.org/jira/browse/THRIFT-820) - The readLength attribute of TBinaryProtocol is used as an instance variable and is decremented on each call of checkReadLength
- [THRIFT-1640](https://issues.apache.org/jira/browse/THRIFT-1640) - None of the tutorials linked on the website contain content
- [THRIFT-1637](https://issues.apache.org/jira/browse/THRIFT-1637) - NPM registry does not include version 0.8
- [THRIFT-1648](https://issues.apache.org/jira/browse/THRIFT-1648) - NodeJS clients always receive 0 for 'double' values.
- [THRIFT-1660](https://issues.apache.org/jira/browse/THRIFT-1660) - Python Thrift library can be installed with pip but not easy_install
- [THRIFT-1657](https://issues.apache.org/jira/browse/THRIFT-1657) - Chrome browser sending OPTIONS method before POST in xmlHttpRequest
- [THRIFT-2118](https://issues.apache.org/jira/browse/THRIFT-2118) - Certificate error handling still incorrect
- [THRIFT-2137](https://issues.apache.org/jira/browse/THRIFT-2137) - Ruby test lib fails jenkins build #864
- [THRIFT-2136](https://issues.apache.org/jira/browse/THRIFT-2136) - Vagrant build not compiling java, ruby, php, go libs due to missing dependencies
- [THRIFT-2135](https://issues.apache.org/jira/browse/THRIFT-2135) - GO lib leaves behind test files that are auto generated
- [THRIFT-2134](https://issues.apache.org/jira/browse/THRIFT-2134) - mingw-cross-compile script failing with strip errors
- [THRIFT-2133](https://issues.apache.org/jira/browse/THRIFT-2133) - java TestTBinaryProtocol.java test failing
- [THRIFT-2126](https://issues.apache.org/jira/browse/THRIFT-2126) - lib/cpp/src/thrift/concurrency/STD* files missing from DIST
- [THRIFT-2125](https://issues.apache.org/jira/browse/THRIFT-2125) - debian missing from DIST
- [THRIFT-2124](https://issues.apache.org/jira/browse/THRIFT-2124) - .o, .so, .la, .deps, .libs, gen-* files left tutorials, test and lib/cpp when making DIST
- [THRIFT-2123](https://issues.apache.org/jira/browse/THRIFT-2123) - GO lib missing files in DIST build
- [THRIFT-2121](https://issues.apache.org/jira/browse/THRIFT-2121) - Compilation bug for Node.js
- [THRIFT-2129](https://issues.apache.org/jira/browse/THRIFT-2129) - php ext missing from dist
- [THRIFT-2128](https://issues.apache.org/jira/browse/THRIFT-2128) - lib GO tests fail with funct ends without a return statement
- [THRIFT-2286](https://issues.apache.org/jira/browse/THRIFT-2286) - Failed to compile Thrift0.9.1 with boost1.55 by VS2010 if select Debug-mt&x64 mode.
- [THRIFT-1973](https://issues.apache.org/jira/browse/THRIFT-1973) - TCompactProtocol in C# lib does not serialize and deserialize negative int32 and int64 number correctly
- [THRIFT-1992](https://issues.apache.org/jira/browse/THRIFT-1992) - casts in TCompactProtocol.tcc causing "dereferencing type-punned pointer will break strict-aliasing rules" warnings from gcc
- [THRIFT-1930](https://issues.apache.org/jira/browse/THRIFT-1930) - C# generates unsigned byte for Thrift "byte" type
- [THRIFT-1929](https://issues.apache.org/jira/browse/THRIFT-1929) - Update website to use Mirrors for downloads
- [THRIFT-1928](https://issues.apache.org/jira/browse/THRIFT-1928) - Race may still exist in TFileTransport::flush()
- [THRIFT-1934](https://issues.apache.org/jira/browse/THRIFT-1934) - Tabs in Example section on main page are not working
- [THRIFT-1933](https://issues.apache.org/jira/browse/THRIFT-1933) - Delphi generator crashes when a typedef references another typedef from an included file
- [THRIFT-1942](https://issues.apache.org/jira/browse/THRIFT-1942) - Binary accelerated cpp extension does not use Thrift namespaces for Exceptions
- [THRIFT-1959](https://issues.apache.org/jira/browse/THRIFT-1959) - C#: Add Union TMemoryBuffer support
- [THRIFT-1958](https://issues.apache.org/jira/browse/THRIFT-1958) - C#: Use static Object.Equals instead of .Equals() calls in equals
- [THRIFT-1957](https://issues.apache.org/jira/browse/THRIFT-1957) - NodeJS TFramedTransport and TBufferedTransport read bytes as unsigned
- [THRIFT-1955](https://issues.apache.org/jira/browse/THRIFT-1955) - Union Type writer generated in C# does not WriteStructBegin
- [THRIFT-1952](https://issues.apache.org/jira/browse/THRIFT-1952) - Travis CI
- [THRIFT-1949](https://issues.apache.org/jira/browse/THRIFT-1949) - WP7 build broken
- [THRIFT-1943](https://issues.apache.org/jira/browse/THRIFT-1943) - docstrings for enum values are ignored
- [THRIFT-2070](https://issues.apache.org/jira/browse/THRIFT-2070) - Improper `HexChar' and 'HexVal' implementation in TJSONProtocol.cs
- [THRIFT-2017](https://issues.apache.org/jira/browse/THRIFT-2017) - Resource Leak in thrift struct under compiler/cpp/src/parse/t_program.h
- [THRIFT-2032](https://issues.apache.org/jira/browse/THRIFT-2032) - C# client leaks sockets/handles
- [THRIFT-1996](https://issues.apache.org/jira/browse/THRIFT-1996) - JavaME Constants generation is broken / inconsistent with regular Java generation
- [THRIFT-2002](https://issues.apache.org/jira/browse/THRIFT-2002) - Haskell: Test use Data.Maybe instead of Maybe
- [THRIFT-2051](https://issues.apache.org/jira/browse/THRIFT-2051) - Vagrant fails to build erlang
- [THRIFT-2050](https://issues.apache.org/jira/browse/THRIFT-2050) - Vagrant C# lib compile fails with TException missing
- [THRIFT-1978](https://issues.apache.org/jira/browse/THRIFT-1978) - Ruby: Thrift should allow for the SSL verify mode to be set
- [THRIFT-1984](https://issues.apache.org/jira/browse/THRIFT-1984) - namespace collision in python bindings
- [THRIFT-1988](https://issues.apache.org/jira/browse/THRIFT-1988) - When trying to build a debian package it fails as the file NEWS doesn't exist
- [THRIFT-1975](https://issues.apache.org/jira/browse/THRIFT-1975) - TBinaryProtocol CheckLength can't be used for a client
- [THRIFT-1995](https://issues.apache.org/jira/browse/THRIFT-1995) - '.' allowed at end of identifier generates non-compilable code
- [THRIFT-2112](https://issues.apache.org/jira/browse/THRIFT-2112) - Error in Go generator when using typedefs in map keys
- [THRIFT-2088](https://issues.apache.org/jira/browse/THRIFT-2088) - Typos in Thrift compiler help text
- [THRIFT-2080](https://issues.apache.org/jira/browse/THRIFT-2080) - C# multiplex processor does not catch IOException
- [THRIFT-2082](https://issues.apache.org/jira/browse/THRIFT-2082) - Executing "gmake clean" is broken
- [THRIFT-2102](https://issues.apache.org/jira/browse/THRIFT-2102) - constants are not referencing to correct type when included from another thrift file
- [THRIFT-2100](https://issues.apache.org/jira/browse/THRIFT-2100) - typedefs are not correctly referenced when including from other thrift files
- [THRIFT-2066](https://issues.apache.org/jira/browse/THRIFT-2066) - 'make install' does not install two headers required for C++ bindings
- [THRIFT-2065](https://issues.apache.org/jira/browse/THRIFT-2065) - Not valid constants filename in Java
- [THRIFT-2047](https://issues.apache.org/jira/browse/THRIFT-2047) - Thrift.Protocol.TCompactProtocol, intToZigZag data lost (TCompactProtocol.cs)
- [THRIFT-2036](https://issues.apache.org/jira/browse/THRIFT-2036) - Thrift gem warns about class variable access from top level
- [THRIFT-2057](https://issues.apache.org/jira/browse/THRIFT-2057) - Vagrant fails on php tests
- [THRIFT-2105](https://issues.apache.org/jira/browse/THRIFT-2105) - Generated code for default values of collections ignores t_field::T_REQUIRED
- [THRIFT-2091](https://issues.apache.org/jira/browse/THRIFT-2091) - Unnecessary 'friend' declaration causes warning in TWinsockSingleton
- [THRIFT-2090](https://issues.apache.org/jira/browse/THRIFT-2090) - Go generator, fix including of other thrift files
- [THRIFT-2106](https://issues.apache.org/jira/browse/THRIFT-2106) - Fix support for namespaces in GO generator
- [THRIFT-1783](https://issues.apache.org/jira/browse/THRIFT-1783) - C# doesn't handle required fields correctly
- [THRIFT-1782](https://issues.apache.org/jira/browse/THRIFT-1782) - async only defined in silverlight
- [THRIFT-1779](https://issues.apache.org/jira/browse/THRIFT-1779) - Missing process_XXXX method in generated TProcessor implementation for all 'oneway' service functions
- [THRIFT-1692](https://issues.apache.org/jira/browse/THRIFT-1692) - SO_REUSEADDR allows for socket hijacking on Windows
- [THRIFT-1720](https://issues.apache.org/jira/browse/THRIFT-1720) - JRuby times out on successful connection
- [THRIFT-1713](https://issues.apache.org/jira/browse/THRIFT-1713) - Named and Anonymous Pipe transport (Delphi)
- [THRIFT-1699](https://issues.apache.org/jira/browse/THRIFT-1699) - Native Union#read has extra read_field_end call
- [THRIFT-1749](https://issues.apache.org/jira/browse/THRIFT-1749) - Python TSSLSocket error handling obscures actual error
- [THRIFT-1748](https://issues.apache.org/jira/browse/THRIFT-1748) - Guard and RWGuard macros defined in global namespace
- [THRIFT-1734](https://issues.apache.org/jira/browse/THRIFT-1734) - Front webpage is still advertising v0.8 as current release
- [THRIFT-1729](https://issues.apache.org/jira/browse/THRIFT-1729) - C glib refactor left empty folders in svn
- [THRIFT-1767](https://issues.apache.org/jira/browse/THRIFT-1767) - unions can't have required fields (Delphi)
- [THRIFT-1765](https://issues.apache.org/jira/browse/THRIFT-1765) - Incorrect error message printed for null or negative keys
- [THRIFT-1778](https://issues.apache.org/jira/browse/THRIFT-1778) - Configure requires manual intervention due to tar failure
- [THRIFT-1777](https://issues.apache.org/jira/browse/THRIFT-1777) - TPipeServer is UNSTOPPABLE
- [THRIFT-1753](https://issues.apache.org/jira/browse/THRIFT-1753) - Multiple C++ Windows, OSX, and iOS portability issues
- [THRIFT-1756](https://issues.apache.org/jira/browse/THRIFT-1756) - 'make -j 8' fails with "unterminated #ifdef" error
- [THRIFT-1773](https://issues.apache.org/jira/browse/THRIFT-1773) - Python library should run on python 2.4
- [THRIFT-1769](https://issues.apache.org/jira/browse/THRIFT-1769) - unions can't have required fields (C++)
- [THRIFT-1768](https://issues.apache.org/jira/browse/THRIFT-1768) - unions can't have required fields (Compiler)
- [THRIFT-1666](https://issues.apache.org/jira/browse/THRIFT-1666) - htonll usage in TBinaryProtocol.tcc generates warning with MSVC2010
- [THRIFT-1919](https://issues.apache.org/jira/browse/THRIFT-1919) - libthrift depends on httpcore-4.1.3 (directly) and httpcore-4.1.4 (transitively)
- [THRIFT-1864](https://issues.apache.org/jira/browse/THRIFT-1864) - implement event handler for non-blocking server
- [THRIFT-1859](https://issues.apache.org/jira/browse/THRIFT-1859) - Generated error c++ code with -out and include_prefix param
- [THRIFT-1869](https://issues.apache.org/jira/browse/THRIFT-1869) - TThreadPoolServer (java) dies when threadpool is consumed
- [THRIFT-1842](https://issues.apache.org/jira/browse/THRIFT-1842) - Memory leak with Pipes
- [THRIFT-1838](https://issues.apache.org/jira/browse/THRIFT-1838) - Can't build compiler on OS X because of missing thrifty.h
- [THRIFT-1846](https://issues.apache.org/jira/browse/THRIFT-1846) - Restore socket.h header to support builds with Android NDK
- [THRIFT-1850](https://issues.apache.org/jira/browse/THRIFT-1850) - make check hangs on TSocket tests in TransportTest.cpp
- [THRIFT-1873](https://issues.apache.org/jira/browse/THRIFT-1873) - Binary protocol factory ignores struct read/write flags
- [THRIFT-1872](https://issues.apache.org/jira/browse/THRIFT-1872) - issues with TBufferedTransport buffer
- [THRIFT-1904](https://issues.apache.org/jira/browse/THRIFT-1904) - Incorrect code is generated for typedefs which use included types
- [THRIFT-1903](https://issues.apache.org/jira/browse/THRIFT-1903) - PHP namespaces cause binary protocols to not be used
- [THRIFT-1895](https://issues.apache.org/jira/browse/THRIFT-1895) - Delphi: reserved variable name "result" not detected properly
- [THRIFT-1881](https://issues.apache.org/jira/browse/THRIFT-1881) - TNonblockingServer does not release open connections or threads on shutdown
- [THRIFT-1888](https://issues.apache.org/jira/browse/THRIFT-1888) - Java Thrift client can't connect to Python Thrift server on same host
- [THRIFT-1831](https://issues.apache.org/jira/browse/THRIFT-1831) - Bug in list deserializer
- [THRIFT-1824](https://issues.apache.org/jira/browse/THRIFT-1824) - many compile warning, becase Thread.h includes config.h
- [THRIFT-1823](https://issues.apache.org/jira/browse/THRIFT-1823) - Missing parenthesis breaks "IS_..." macro in generated code
- [THRIFT-1806](https://issues.apache.org/jira/browse/THRIFT-1806) - Python generation always truncates __init__.py files
- [THRIFT-1795](https://issues.apache.org/jira/browse/THRIFT-1795) - Race condition in TThreadedServerPool java implementation
- [THRIFT-1794](https://issues.apache.org/jira/browse/THRIFT-1794) - C# asyncctp broken
- [THRIFT-1804](https://issues.apache.org/jira/browse/THRIFT-1804) - Binary+compact protocol single byte error in Ruby library (ARM architecture): caused by different char signedness
- [THRIFT-1800](https://issues.apache.org/jira/browse/THRIFT-1800) - Documentation text not always escaped correctly when rendered to HTML
- [THRIFT-1788](https://issues.apache.org/jira/browse/THRIFT-1788) - C#: Constants static constructor does not compile
- [THRIFT-1816](https://issues.apache.org/jira/browse/THRIFT-1816) - Need "require" included thrift files in "xxx_types.js"
- [THRIFT-1907](https://issues.apache.org/jira/browse/THRIFT-1907) - Compiling namespace and sub-namespace directives for unrecognized generators should only be a warning
- [THRIFT-1913](https://issues.apache.org/jira/browse/THRIFT-1913) - skipping unknown fields in java unions
- [THRIFT-2553](https://issues.apache.org/jira/browse/THRIFT-2553) - C++ linker error - transport/TSocket
- [THRIFT-274](https://issues.apache.org/jira/browse/THRIFT-274) - Towards a working release/versioning process

### Documentation
- [THRIFT-1971](https://issues.apache.org/jira/browse/THRIFT-1971) - [Graphviz] Adds tutorial/general description documentation
- [THRIFT-2001](https://issues.apache.org/jira/browse/THRIFT-2001) - http://thrift.apache.org/ Example "C++ Server" tab is broken

### Improvement
- [THRIFT-1574](https://issues.apache.org/jira/browse/THRIFT-1574) - Apache project branding requirements: DOAP file [PATCH]
- [THRIFT-1347](https://issues.apache.org/jira/browse/THRIFT-1347) - Unify the exceptions returned in generated Go code
- [THRIFT-1353](https://issues.apache.org/jira/browse/THRIFT-1353) - Switch to performance branch, get rid of BinaryParser
- [THRIFT-1629](https://issues.apache.org/jira/browse/THRIFT-1629) - Ruby 1.9 Compatibility during Thrift configure, make, install
- [THRIFT-991](https://issues.apache.org/jira/browse/THRIFT-991) - Refactor Haskell code and generator
- [THRIFT-990](https://issues.apache.org/jira/browse/THRIFT-990) - Sanify gettimeofday usage codebase-wide
- [THRIFT-791](https://issues.apache.org/jira/browse/THRIFT-791) - Let C++ TSimpleServer be driven by an external main loop
- [THRIFT-2117](https://issues.apache.org/jira/browse/THRIFT-2117) - Cocoa TBinaryProtocol strictWrite should be set to true by default
- [THRIFT-2014](https://issues.apache.org/jira/browse/THRIFT-2014) - Change C++ lib includes to use <namespace/> style throughout
- [THRIFT-1972](https://issues.apache.org/jira/browse/THRIFT-1972) - Add support for async processors
- [THRIFT-1970](https://issues.apache.org/jira/browse/THRIFT-1970) - [Graphviz] Adds option to render exceptions relationships
- [THRIFT-1966](https://issues.apache.org/jira/browse/THRIFT-1966) - Support different files for SSL certificates and keys
- [THRIFT-1965](https://issues.apache.org/jira/browse/THRIFT-1965) - Adds Graphviz (graph description language) generator
- [THRIFT-1956](https://issues.apache.org/jira/browse/THRIFT-1956) - Switch to Apache Commons Lang 3
- [THRIFT-1962](https://issues.apache.org/jira/browse/THRIFT-1962) - Multiplex processor should send any TApplicationException back to client
- [THRIFT-1960](https://issues.apache.org/jira/browse/THRIFT-1960) - main() declares 22 unused gen bools
- [THRIFT-1951](https://issues.apache.org/jira/browse/THRIFT-1951) - libthrift.jar has source files in it
- [THRIFT-1997](https://issues.apache.org/jira/browse/THRIFT-1997) - Add accept backlog configuration method to  TServerSocket
- [THRIFT-2003](https://issues.apache.org/jira/browse/THRIFT-2003) - Deprecate senum
- [THRIFT-2052](https://issues.apache.org/jira/browse/THRIFT-2052) - Vagrant machine image defaults to only 384MB of RAM
- [THRIFT-1980](https://issues.apache.org/jira/browse/THRIFT-1980) - Modernize Go tooling, fix go client library.
- [THRIFT-1977](https://issues.apache.org/jira/browse/THRIFT-1977) - C# compiler should generate constant files prefixed with thrift file name
- [THRIFT-1985](https://issues.apache.org/jira/browse/THRIFT-1985) - add a Vagrantfile to build and test Apache Thrift fully reproducible
- [THRIFT-1994](https://issues.apache.org/jira/browse/THRIFT-1994) - Deprecate slist
- [THRIFT-1993](https://issues.apache.org/jira/browse/THRIFT-1993) - Factory to create instances from known (generated) interface types with Delphi
- [THRIFT-2081](https://issues.apache.org/jira/browse/THRIFT-2081) - Specified timeout should be used in TSocket.Open()
- [THRIFT-2084](https://issues.apache.org/jira/browse/THRIFT-2084) - Delphi: Ability to create entity Thrift-generated instances based on TypeInfo
- [THRIFT-2083](https://issues.apache.org/jira/browse/THRIFT-2083) - Improve the go lib: buffered Transport, save memory allocation, handle concurrent request
- [THRIFT-2109](https://issues.apache.org/jira/browse/THRIFT-2109) - Secure connections should be supported in Go
- [THRIFT-2107](https://issues.apache.org/jira/browse/THRIFT-2107) - minor Go generator fixes
- [THRIFT-1695](https://issues.apache.org/jira/browse/THRIFT-1695) - allow warning-free compilation in VS 2012 and GNU 4.6
- [THRIFT-1735](https://issues.apache.org/jira/browse/THRIFT-1735) - integrate tutorial into regular build
- [THRIFT-1716](https://issues.apache.org/jira/browse/THRIFT-1716) - max allowed connections should be PIPE_UNLIMITED_INSTANCES
- [THRIFT-1715](https://issues.apache.org/jira/browse/THRIFT-1715) - Allow excluding python parts when building contrib/fb303
- [THRIFT-1733](https://issues.apache.org/jira/browse/THRIFT-1733) - Fix RPM build issues on RHEL6/OL6 systems
- [THRIFT-1728](https://issues.apache.org/jira/browse/THRIFT-1728) - Upgradation of httpcomponents
- [THRIFT-1876](https://issues.apache.org/jira/browse/THRIFT-1876) - Use enum names instead of casted integers in assignments
- [THRIFT-1874](https://issues.apache.org/jira/browse/THRIFT-1874) - timeout for the server-side end of a named pipe
- [THRIFT-1897](https://issues.apache.org/jira/browse/THRIFT-1897) - Support validation of required fields
- [THRIFT-1896](https://issues.apache.org/jira/browse/THRIFT-1896) - Add TBase protocol for Cocoa
- [THRIFT-1880](https://issues.apache.org/jira/browse/THRIFT-1880) - Make named pipes server work asynchronously (overlapped) to allow for clean server stops
- [THRIFT-1878](https://issues.apache.org/jira/browse/THRIFT-1878) - Add the possibility to send custom headers
- [THRIFT-1882](https://issues.apache.org/jira/browse/THRIFT-1882) - Use single include
- [THRIFT-1793](https://issues.apache.org/jira/browse/THRIFT-1793) - C#: Use static read instead of instance read
- [THRIFT-1799](https://issues.apache.org/jira/browse/THRIFT-1799) - Option to generate HTML in "standalone mode"
- [THRIFT-1815](https://issues.apache.org/jira/browse/THRIFT-1815) - Code generators line buffer output
- [THRIFT-1890](https://issues.apache.org/jira/browse/THRIFT-1890) - C++: Make named pipes server work asynchronously
- [THRIFT-474](https://issues.apache.org/jira/browse/THRIFT-474) - Generating Ruby on Rails friendly code

### New Feature
- [THRIFT-801](https://issues.apache.org/jira/browse/THRIFT-801) - Provide an interactive shell (irb) when generating ruby bindings
- [THRIFT-2292](https://issues.apache.org/jira/browse/THRIFT-2292) - Android Library Project
- [THRIFT-2012](https://issues.apache.org/jira/browse/THRIFT-2012) - Modernizing Go
- [THRIFT-1969](https://issues.apache.org/jira/browse/THRIFT-1969) - C#: Tests not properly linked from the solution
- [THRIFT-1785](https://issues.apache.org/jira/browse/THRIFT-1785) - C#: Add TMemoryBuffer serializer/deserializer
- [THRIFT-1780](https://issues.apache.org/jira/browse/THRIFT-1780) - Add option to generate nullable values
- [THRIFT-1786](https://issues.apache.org/jira/browse/THRIFT-1786) - C# Union Typing
- [THRIFT-591](https://issues.apache.org/jira/browse/THRIFT-591) - Make the C++ runtime library be compatible with Windows and Visual Studio
- [THRIFT-514](https://issues.apache.org/jira/browse/THRIFT-514) - Add option to configure compiler output directory

### Question
- [THRIFT-1764](https://issues.apache.org/jira/browse/THRIFT-1764) - how to get the context of client when on a rpc call in server side?
- [THRIFT-1791](https://issues.apache.org/jira/browse/THRIFT-1791) - thrift's namespace directive when generating haskell code

### Sub-task
- [THRIFT-1594](https://issues.apache.org/jira/browse/THRIFT-1594) - Java test clients should have a return codes that reflect whether it succeeds or not.
- [THRIFT-1595](https://issues.apache.org/jira/browse/THRIFT-1595) - Java test server should follow the documented behavior as of THRIFT-1590
- [THRIFT-986](https://issues.apache.org/jira/browse/THRIFT-986) - st: add version Info to the library
- [THRIFT-985](https://issues.apache.org/jira/browse/THRIFT-985) - php: add version Info to the library
- [THRIFT-984](https://issues.apache.org/jira/browse/THRIFT-984) - ocaml: add version Info to the library
- [THRIFT-1924](https://issues.apache.org/jira/browse/THRIFT-1924) - Delphi: Inconsistency in serialization of optional fields
- [THRIFT-1922](https://issues.apache.org/jira/browse/THRIFT-1922) - C#: Inconsistency in serialization of optional fields
- [THRIFT-1961](https://issues.apache.org/jira/browse/THRIFT-1961) - C# tests should be in lib/csharp/test/...
- [THRIFT-1822](https://issues.apache.org/jira/browse/THRIFT-1822) - PHP unit test does not work
- [THRIFT-1902](https://issues.apache.org/jira/browse/THRIFT-1902) - C++: Support for Multiplexing Services on any Transport, Protocol and Server
- [THRIFT-1901](https://issues.apache.org/jira/browse/THRIFT-1901) - C#: Support for Multiplexing Services on any Transport, Protocol and Server
- [THRIFT-1899](https://issues.apache.org/jira/browse/THRIFT-1899) - Delphi: Support for Multiplexing Services on any Transport, Protocol and Server
- [THRIFT-563](https://issues.apache.org/jira/browse/THRIFT-563) - Support for Multiplexing Services on any Transport, Protocol and Server

## 0.9

### Bug
- [THRIFT-1438](https://issues.apache.org/jira/browse/THRIFT-1438) - lib/cpp/src/windows/config.h should read version from configure.ac rather than a #define
- [THRIFT-1446](https://issues.apache.org/jira/browse/THRIFT-1446) - Compile error with Delphi 2009 in constant initializer
- [THRIFT-1450](https://issues.apache.org/jira/browse/THRIFT-1450) - Problems building thrift 0.8.0 for Python and Ruby
- [THRIFT-1449](https://issues.apache.org/jira/browse/THRIFT-1449) - Ruby client does not work on solaris (?)
- [THRIFT-1447](https://issues.apache.org/jira/browse/THRIFT-1447) - NullpointerException in ProcessFunction.class :in "oneway" method
- [THRIFT-1433](https://issues.apache.org/jira/browse/THRIFT-1433) - TServerSocket fix for MSVC
- [THRIFT-1429](https://issues.apache.org/jira/browse/THRIFT-1429) - The nonblocking servers is supposed to use TransportFactory to read the data
- [THRIFT-1427](https://issues.apache.org/jira/browse/THRIFT-1427) - PHP library uses non-multibyte safe functions with mbstring function overloading
- [THRIFT-1421](https://issues.apache.org/jira/browse/THRIFT-1421) - Debian Packages can not be built
- [THRIFT-1394](https://issues.apache.org/jira/browse/THRIFT-1394) - Treatment of optional fields is not consistent between C++ and Java
- [THRIFT-1511](https://issues.apache.org/jira/browse/THRIFT-1511) - Server with oneway support ( JAVA )
- [THRIFT-1496](https://issues.apache.org/jira/browse/THRIFT-1496) - PHP compiler not namespacing enums
- [THRIFT-1495](https://issues.apache.org/jira/browse/THRIFT-1495) - PHP TestClient fatals on missing class
- [THRIFT-1508](https://issues.apache.org/jira/browse/THRIFT-1508) - TServerSocket does not allow for the user to specify the IP address to bind to
- [THRIFT-1504](https://issues.apache.org/jira/browse/THRIFT-1504) - Cocoa Generator should use local file imports for base Thrift headers
- [THRIFT-1512](https://issues.apache.org/jira/browse/THRIFT-1512) - Thrift socket support for Windows XP
- [THRIFT-1502](https://issues.apache.org/jira/browse/THRIFT-1502) - TSimpleServer::serve(): Do not print out error message if server was stopped.
- [THRIFT-1501](https://issues.apache.org/jira/browse/THRIFT-1501) - PHP old namespaces not generated for enums
- [THRIFT-1483](https://issues.apache.org/jira/browse/THRIFT-1483) - java compiler does not generate type parameters for services in extended clauses
- [THRIFT-1479](https://issues.apache.org/jira/browse/THRIFT-1479) - Compiled PHP process functions missing writeMessageEnd()
- [THRIFT-1492](https://issues.apache.org/jira/browse/THRIFT-1492) - enabling c_glib render thrift unusable (even for C++ code)
- [THRIFT-1491](https://issues.apache.org/jira/browse/THRIFT-1491) - Uninitialize processorFactory_ member in TServer.h
- [THRIFT-1475](https://issues.apache.org/jira/browse/THRIFT-1475) - Incomplete records generation for Erlang
- [THRIFT-1486](https://issues.apache.org/jira/browse/THRIFT-1486) - Javascript manual testserver not returning content types
- [THRIFT-1488](https://issues.apache.org/jira/browse/THRIFT-1488) - src/concurrency/Thread.h:91:58: error: invalid conversion from 'pthread_t {aka _opaque_pthread_t*}' to 'apache::thrift::concurrency::Thread::id_t {aka long long unsigned int}' [-fpermissive]
- [THRIFT-1490](https://issues.apache.org/jira/browse/THRIFT-1490) - Windows-specific header files - fixes & tweaks
- [THRIFT-1526](https://issues.apache.org/jira/browse/THRIFT-1526) - Union TupleSchemeFactory returns StandardSchemes
- [THRIFT-1527](https://issues.apache.org/jira/browse/THRIFT-1527) - Generated implementation of tupleReadStruct in unions return null when the setfield is unrecognized
- [THRIFT-1524](https://issues.apache.org/jira/browse/THRIFT-1524) - TNonBlockingServer does not compile in Visual Studio 2010
- [THRIFT-1529](https://issues.apache.org/jira/browse/THRIFT-1529) - TupleProtocol can unintentionally include an extra byte in bit vectors when number of optional fields is an integral of 8
- [THRIFT-1473](https://issues.apache.org/jira/browse/THRIFT-1473) - JSON context stack may be left in an incorrect state when an exception is thrown during read or write operations
- [THRIFT-1456](https://issues.apache.org/jira/browse/THRIFT-1456) - System.Net.HttpWebRequest' does not contain a definition for 'Proxy'
- [THRIFT-1468](https://issues.apache.org/jira/browse/THRIFT-1468) - Memory leak in TSaslServerTransport
- [THRIFT-1461](https://issues.apache.org/jira/browse/THRIFT-1461) - Recent TNonblockingServer changes broke --enable-boostthreads=yes, Windows
- [THRIFT-1460](https://issues.apache.org/jira/browse/THRIFT-1460) - why not add unicode strings support to python directly?
- [THRIFT-1464](https://issues.apache.org/jira/browse/THRIFT-1464) - AbstractNonblockingServer.FrameBuffer TNonblockingTransport accessor changed from public to private
- [THRIFT-1467](https://issues.apache.org/jira/browse/THRIFT-1467) - Possible AV with empty strings when using JSON protocol
- [THRIFT-1523](https://issues.apache.org/jira/browse/THRIFT-1523) - clientTimeout not worked as expected in TServerSocket created by TSSLTransportFactory
- [THRIFT-1537](https://issues.apache.org/jira/browse/THRIFT-1537) - TFramedTransport issues
- [THRIFT-1519](https://issues.apache.org/jira/browse/THRIFT-1519) - Thirft Build Failure referencing rb_intern2 symbol
- [THRIFT-1518](https://issues.apache.org/jira/browse/THRIFT-1518) - Generated C++ code only sends the first optional field in the write() function for a struct.
- [THRIFT-1515](https://issues.apache.org/jira/browse/THRIFT-1515) - NameError: global name 'TApplicationException' is not defined
- [THRIFT-1554](https://issues.apache.org/jira/browse/THRIFT-1554) - Inherited service methods are not resolved in derived service implementations
- [THRIFT-1553](https://issues.apache.org/jira/browse/THRIFT-1553) - thrift nodejs service side can't read map structure, key as enum, value as Object
- [THRIFT-1575](https://issues.apache.org/jira/browse/THRIFT-1575) - Typo in server/TThreadPoolServer.h
- [THRIFT-1327](https://issues.apache.org/jira/browse/THRIFT-1327) - Fix Spec Suite under Ruby-1.8.7 (works for MRI Ruby-1.9.2)
- [THRIFT-1326](https://issues.apache.org/jira/browse/THRIFT-1326) - on some platforms, #include <stdint.h> is necessary to be included in Thrift.h
- [THRIFT-1159](https://issues.apache.org/jira/browse/THRIFT-1159) - THttpClient->Flush() issue (connection thru proxy)
- [THRIFT-1277](https://issues.apache.org/jira/browse/THRIFT-1277) - Node.js serializes false booleans as null
- [THRIFT-1224](https://issues.apache.org/jira/browse/THRIFT-1224) - Cannot insert UTF-8 text
- [THRIFT-1267](https://issues.apache.org/jira/browse/THRIFT-1267) - Node.js can't throw exceptions.
- [THRIFT-1338](https://issues.apache.org/jira/browse/THRIFT-1338) - Do not use an unpatched autoconf 2.65 to generate release tarball
- [THRIFT-1128](https://issues.apache.org/jira/browse/THRIFT-1128) - MAC OS X: thrift.h incompatibility with Thrift.h
- [THRIFT-1631](https://issues.apache.org/jira/browse/THRIFT-1631) - Fix C++ server constructor typos
- [THRIFT-1602](https://issues.apache.org/jira/browse/THRIFT-1602) - PHP C Extension is not Compatible with PHP 5.4
- [THRIFT-1610](https://issues.apache.org/jira/browse/THRIFT-1610) - IWebProxy not available on WP7 platform
- [THRIFT-1606](https://issues.apache.org/jira/browse/THRIFT-1606) - Race condition in BoostThreadFactory.cpp
- [THRIFT-1604](https://issues.apache.org/jira/browse/THRIFT-1604) - Python exception handeling for changes from PEP 3110
- [THRIFT-1607](https://issues.apache.org/jira/browse/THRIFT-1607) - Incorrect file modes for several source files
- [THRIFT-1583](https://issues.apache.org/jira/browse/THRIFT-1583) - c_glib leaks memory
- [THRIFT-1582](https://issues.apache.org/jira/browse/THRIFT-1582) - Bad includes of nested thrift files in c_glib
- [THRIFT-1578](https://issues.apache.org/jira/browse/THRIFT-1578) - C_GLib generated code does not compile
- [THRIFT-1597](https://issues.apache.org/jira/browse/THRIFT-1597) - TJSONProtocol.php is missing from Makefile.am
- [THRIFT-1591](https://issues.apache.org/jira/browse/THRIFT-1591) - Enable TCP_NODELAY for ruby gem
- [THRIFT-1624](https://issues.apache.org/jira/browse/THRIFT-1624) - Isset Generated differently on different platforms
- [THRIFT-1622](https://issues.apache.org/jira/browse/THRIFT-1622) - Incorrect size returned on read
- [THRIFT-1621](https://issues.apache.org/jira/browse/THRIFT-1621) - Memory leaks
- [THRIFT-1612](https://issues.apache.org/jira/browse/THRIFT-1612) - Base64 encoding is broken
- [THRIFT-1627](https://issues.apache.org/jira/browse/THRIFT-1627) - compiler built using compilers.vcxproj cannot be used to build some test .thrift files
- [THRIFT-1571](https://issues.apache.org/jira/browse/THRIFT-1571) - Update Ruby HTTP transport for recent Ruby versions
- [THRIFT-1023](https://issues.apache.org/jira/browse/THRIFT-1023) - Thrift encoding  (UTF-8) issue with Ruby 1.9.2
- [THRIFT-1090](https://issues.apache.org/jira/browse/THRIFT-1090) - Document the generation of a file called "Constants.java"
- [THRIFT-1082](https://issues.apache.org/jira/browse/THRIFT-1082) - Thrift::FramedTransport sometimes calls close() on an undefined value
- [THRIFT-956](https://issues.apache.org/jira/browse/THRIFT-956) - Python module's version meta-data should be updated
- [THRIFT-973](https://issues.apache.org/jira/browse/THRIFT-973) - Cocoa library won't compile using clang
- [THRIFT-1632](https://issues.apache.org/jira/browse/THRIFT-1632) - ruby: data corruption in thrift_native implementation of MemoryBufferTransport
- [THRIFT-1665](https://issues.apache.org/jira/browse/THRIFT-1665) - TBinaryProtocol: exceeded message length raises generic TException
- [THRIFT-1664](https://issues.apache.org/jira/browse/THRIFT-1664) - Reference to non-existing variable in build script
- [THRIFT-1663](https://issues.apache.org/jira/browse/THRIFT-1663) - Java Thrift server is not throwing exceptions
- [THRIFT-1662](https://issues.apache.org/jira/browse/THRIFT-1662) - "removeObject:" should be "removeObserver:" in [-TSocketServer dealloc]?
- [THRIFT-1643](https://issues.apache.org/jira/browse/THRIFT-1643) - Denial of Service attack in TBinaryProtocol.readString
- [THRIFT-1674](https://issues.apache.org/jira/browse/THRIFT-1674) - Update Thrift D library to be compatible with 2.060
- [THRIFT-1673](https://issues.apache.org/jira/browse/THRIFT-1673) - Ruby compile flags for extension for multi arch builds (os x)
- [THRIFT-1655](https://issues.apache.org/jira/browse/THRIFT-1655) - Configure still trying to use thrift_generators in output
- [THRIFT-1654](https://issues.apache.org/jira/browse/THRIFT-1654) - c_glib thrift_socket_read() returns corrupted data
- [THRIFT-1653](https://issues.apache.org/jira/browse/THRIFT-1653) - TThreadedSelectorServer leaks CLOSE_WAIT sockets
- [THRIFT-1658](https://issues.apache.org/jira/browse/THRIFT-1658) - Java thrift server is not throwing TApplicationException
- [THRIFT-1656](https://issues.apache.org/jira/browse/THRIFT-1656) - Setting proper headers in THttpServer.cpp so that "Cross-Origin Resource Sharing" on js client can work.
- [THRIFT-1652](https://issues.apache.org/jira/browse/THRIFT-1652) - TSaslTransport does not log the error when kerberos auth fails
- [THRIFT-2272](https://issues.apache.org/jira/browse/THRIFT-2272) - CLONE - Denial of Service attack in TBinaryProtocol.readString
- [THRIFT-2086](https://issues.apache.org/jira/browse/THRIFT-2086) - Invalid generated code for Node.JS when using namespaces
- [THRIFT-1686](https://issues.apache.org/jira/browse/THRIFT-1686) - t_php_generator.cc uses "and" instead of "&&", and causes compiler errors with Visual Studio
- [THRIFT-1693](https://issues.apache.org/jira/browse/THRIFT-1693) - libthrift has dependency on two different versions of httpcore
- [THRIFT-1689](https://issues.apache.org/jira/browse/THRIFT-1689) - don't exit(-1) in TNonblockingServer
- [THRIFT-1679](https://issues.apache.org/jira/browse/THRIFT-1679) - NodeJS: protocol readString() should treat string as utf8, not binary
- [THRIFT-1721](https://issues.apache.org/jira/browse/THRIFT-1721) - Dist broken due to 0.8.0 to 0.9.0 changes
- [THRIFT-1710](https://issues.apache.org/jira/browse/THRIFT-1710) - Minor issues in test case code
- [THRIFT-1709](https://issues.apache.org/jira/browse/THRIFT-1709) - Warning "Bitwise-or operator used on a sign-extended operand; consider casting to a smaller unsigned type first" in TBinaryProtocol.cs at ReadInt64()
- [THRIFT-1707](https://issues.apache.org/jira/browse/THRIFT-1707) - [ruby] Adjust server_spec.rb for RSpec 2.11.x and Ruby 1.9.3
- [THRIFT-1671](https://issues.apache.org/jira/browse/THRIFT-1671) - Cocoa code generator does not put keywords into generated method calls
- [THRIFT-1670](https://issues.apache.org/jira/browse/THRIFT-1670) - Incompatibilities between different versions of a Thrift interface
- [THRIFT-1669](https://issues.apache.org/jira/browse/THRIFT-1669) - NameError: global name 'TApplicationException' is not defined
- [THRIFT-1668](https://issues.apache.org/jira/browse/THRIFT-1668) - Compile error in contrib/fb303, thrift/TDispatchProcessor.h: No such file or directory
- [THRIFT-1845](https://issues.apache.org/jira/browse/THRIFT-1845) - Fix compiler warning caused by implicit string conversion with Xcode 4.6
- [THRIFT-304](https://issues.apache.org/jira/browse/THRIFT-304) - Building the Python library requires development headers
- [THRIFT-369](https://issues.apache.org/jira/browse/THRIFT-369) - sets and maps break equality
- [THRIFT-556](https://issues.apache.org/jira/browse/THRIFT-556) - Ruby compiler does not correctly referred to top-level modules when a submodule masks the top-level name
- [THRIFT-481](https://issues.apache.org/jira/browse/THRIFT-481) - indentation of ruby classes is off by a few

### Improvement
- [THRIFT-1498](https://issues.apache.org/jira/browse/THRIFT-1498) - Allow TThreadedPoolServer.Args to pass a ExecutorService
- [THRIFT-1444](https://issues.apache.org/jira/browse/THRIFT-1444) - FunctionRunner - add syntactic sugar to create shared_ptrs
- [THRIFT-1443](https://issues.apache.org/jira/browse/THRIFT-1443) - define a TProcessor helper class to implement process()
- [THRIFT-1441](https://issues.apache.org/jira/browse/THRIFT-1441) - Generate constructor with parameters for exception class to let it update message property automatically.
- [THRIFT-1520](https://issues.apache.org/jira/browse/THRIFT-1520) - Embed version number in erlang .app file
- [THRIFT-1480](https://issues.apache.org/jira/browse/THRIFT-1480) - python: remove tabs, adjust whitespace and address PEP8 warnings
- [THRIFT-1485](https://issues.apache.org/jira/browse/THRIFT-1485) - Performance: pass large and/or refcounted arguments as "const"
- [THRIFT-1484](https://issues.apache.org/jira/browse/THRIFT-1484) - Introduce phpunit test suite
- [THRIFT-1532](https://issues.apache.org/jira/browse/THRIFT-1532) - The type specifications in the generated Erlang code should include "undefined" where it's used as a default value
- [THRIFT-1534](https://issues.apache.org/jira/browse/THRIFT-1534) - Required fields in the Delphi code generator.
- [THRIFT-1469](https://issues.apache.org/jira/browse/THRIFT-1469) - Java isset space optimization
- [THRIFT-1465](https://issues.apache.org/jira/browse/THRIFT-1465) - Visibility of methods in generated java code
- [THRIFT-1453](https://issues.apache.org/jira/browse/THRIFT-1453) - Don't change types of arguments when serializing with thrift php extension
- [THRIFT-1452](https://issues.apache.org/jira/browse/THRIFT-1452) - generate a swap() method for all generated structs
- [THRIFT-1451](https://issues.apache.org/jira/browse/THRIFT-1451) - FramedTransport: Prevent infinite loop when writing
- [THRIFT-1521](https://issues.apache.org/jira/browse/THRIFT-1521) - Two patches for more Performance
- [THRIFT-1555](https://issues.apache.org/jira/browse/THRIFT-1555) - Delphi version of the tutorial code
- [THRIFT-1535](https://issues.apache.org/jira/browse/THRIFT-1535) - Why thrift don't use wrapped class for optional fields ?
- [THRIFT-1204](https://issues.apache.org/jira/browse/THRIFT-1204) - Ruby autogenerated files should require 'thrift' gem
- [THRIFT-1344](https://issues.apache.org/jira/browse/THRIFT-1344) - Using the httpc module directly rather than the deprecated http layer
- [THRIFT-1343](https://issues.apache.org/jira/browse/THRIFT-1343) - no_auto_import min/2 to avoid compile warning
- [THRIFT-1340](https://issues.apache.org/jira/browse/THRIFT-1340) - Add support of ARC to Objective-C
- [THRIFT-1611](https://issues.apache.org/jira/browse/THRIFT-1611) - Improved code generation for typedefs
- [THRIFT-1593](https://issues.apache.org/jira/browse/THRIFT-1593) - Pass on errors like "connection closed" to the handler module
- [THRIFT-1615](https://issues.apache.org/jira/browse/THRIFT-1615) - PHP Namespace
- [THRIFT-1567](https://issues.apache.org/jira/browse/THRIFT-1567) - Thrift/cpp: Allow alternate classes to be used for
- [THRIFT-1072](https://issues.apache.org/jira/browse/THRIFT-1072) - Missing - (id) initWithSharedProcessor in TSharedProcessorFactory.h
- [THRIFT-1650](https://issues.apache.org/jira/browse/THRIFT-1650) - [ruby] Update clean items and svn:ignore entries for OS X artifacts
- [THRIFT-1661](https://issues.apache.org/jira/browse/THRIFT-1661) - [PATCH] Add --with-qt4 configure option
- [THRIFT-1675](https://issues.apache.org/jira/browse/THRIFT-1675) - Do we have any plan to support scala?
- [THRIFT-1645](https://issues.apache.org/jira/browse/THRIFT-1645) - Replace Object#tee with more conventional Object#tap in specs
- [THRIFT-1644](https://issues.apache.org/jira/browse/THRIFT-1644) - Upgrade RSpec to 2.10.x and refactor specs as needed
- [THRIFT-1672](https://issues.apache.org/jira/browse/THRIFT-1672) - MonoTouch (and Mono for Android) compatibility
- [THRIFT-1702](https://issues.apache.org/jira/browse/THRIFT-1702) - a thrift manual
- [THRIFT-1694](https://issues.apache.org/jira/browse/THRIFT-1694) - Re-Enable serialization for WP7 Silverlight
- [THRIFT-1691](https://issues.apache.org/jira/browse/THRIFT-1691) - Serializer/deserializer support for Delphi
- [THRIFT-1688](https://issues.apache.org/jira/browse/THRIFT-1688) - Update IDL page markup
- [THRIFT-1725](https://issues.apache.org/jira/browse/THRIFT-1725) - Tutorial web pages for Delphi and C#
- [THRIFT-1714](https://issues.apache.org/jira/browse/THRIFT-1714) - [ruby] Explicitly add CWD to Ruby test_suites.rb
- [THRIFT-317](https://issues.apache.org/jira/browse/THRIFT-317) - Issues with Java struct validation
- [THRIFT-164](https://issues.apache.org/jira/browse/THRIFT-164) - Build web tutorial on Incubator web site
- [THRIFT-541](https://issues.apache.org/jira/browse/THRIFT-541) - Cocoa code generator doesn't put keywords before all arguments.
- [THRIFT-681](https://issues.apache.org/jira/browse/THRIFT-681) - The HTML generator does not handle JavaDoc style comments very well

### New Feature
- [THRIFT-1500](https://issues.apache.org/jira/browse/THRIFT-1500) - D programming language support
- [THRIFT-1510](https://issues.apache.org/jira/browse/THRIFT-1510) - There should be an implementation of the JsonProtocol for ruby
- [THRIFT-1115](https://issues.apache.org/jira/browse/THRIFT-1115) - python TBase class for dynamic (de)serialization, and __slots__ option for memory savings
- [THRIFT-1953](https://issues.apache.org/jira/browse/THRIFT-1953) - support for asp.net mvc 3

### Question
- [THRIFT-1235](https://issues.apache.org/jira/browse/THRIFT-1235) - How could I use THttpServerTransportFactory withTNonBlockingServer
- [THRIFT-1368](https://issues.apache.org/jira/browse/THRIFT-1368) - TNonblockingServer usage
- [THRIFT-1061](https://issues.apache.org/jira/browse/THRIFT-1061) - Read an invalid frame size of 0. Are you using TFramedTransport on the client side?
- [THRIFT-491](https://issues.apache.org/jira/browse/THRIFT-491) - Ripping raw pthreads out of TFileTransport and associated test issues

### Sub-task
- [THRIFT-1596](https://issues.apache.org/jira/browse/THRIFT-1596) - Delphi: Test clients should have a return codes that reflect whether they succeeded or not
- [THRIFT-982](https://issues.apache.org/jira/browse/THRIFT-982) - javame: add version Info to the library
- [THRIFT-1722](https://issues.apache.org/jira/browse/THRIFT-1722) - C# WP7 Assembly addition beaks mono build
- [THRIFT-336](https://issues.apache.org/jira/browse/THRIFT-336) - Compact Protocol in C#

### Test
- [THRIFT-1613](https://issues.apache.org/jira/browse/THRIFT-1613) - Add code back into empty source file ToStringTest.java
- [THRIFT-1718](https://issues.apache.org/jira/browse/THRIFT-1718) - Incorrect check in TFileTransportTest

### Wish
- [THRIFT-1463](https://issues.apache.org/jira/browse/THRIFT-1463) - Decouple Thrift IDL from generators
- [THRIFT-1466](https://issues.apache.org/jira/browse/THRIFT-1466) - Proper Documentation for Thrift C Glib
- [THRIFT-1539](https://issues.apache.org/jira/browse/THRIFT-1539) - Build and distribute the fb303 python libraries along with thrift
- [THRIFT-1685](https://issues.apache.org/jira/browse/THRIFT-1685) - Please add "aereo.com" to "Powered by Apache Thrift" list in about page
- [THRIFT-330](https://issues.apache.org/jira/browse/THRIFT-330) - TProcessor - additional method to called when connection is broken

## 0.8

### Bug
- [THRIFT-1436](https://issues.apache.org/jira/browse/THRIFT-1436) - pip install thrift fails on Windows with "Unable to find vcvarsall.bat"
- [THRIFT-1432](https://issues.apache.org/jira/browse/THRIFT-1432) - Javascript struct constants declared in the same file as their struct definition will cause an error
- [THRIFT-1428](https://issues.apache.org/jira/browse/THRIFT-1428) - shared.thrft does not include namespace for php, so thrift compiler generate incorrect name
- [THRIFT-1426](https://issues.apache.org/jira/browse/THRIFT-1426) - Dist package missing files for release 0.8
- [THRIFT-1425](https://issues.apache.org/jira/browse/THRIFT-1425) - The Node package is incompatible with latest node (0.6) & npm (1.0.27)
- [THRIFT-1416](https://issues.apache.org/jira/browse/THRIFT-1416) - Python Unit test is broken on ci
- [THRIFT-1419](https://issues.apache.org/jira/browse/THRIFT-1419) - AbstractNonBlockingServer does not catch errors when invoking the processor
- [THRIFT-1424](https://issues.apache.org/jira/browse/THRIFT-1424) - Ruby specs fail when run with rake
- [THRIFT-1420](https://issues.apache.org/jira/browse/THRIFT-1420) - Nonblocking and HsHa server should make sure to close all their socket connections when the selector exits
- [THRIFT-1413](https://issues.apache.org/jira/browse/THRIFT-1413) - Generated code does not read MapEnd / ListEnd / SetEnd
- [THRIFT-1409](https://issues.apache.org/jira/browse/THRIFT-1409) - Name conflict check does not work properly for exception object(Delphi).
- [THRIFT-1408](https://issues.apache.org/jira/browse/THRIFT-1408) - Delphi Test Server: Exception test case fails due to naming conflict with e.message
- [THRIFT-1407](https://issues.apache.org/jira/browse/THRIFT-1407) - Typo in Python socket server causes Thrift to fail when we enable a global socket timout
- [THRIFT-1397](https://issues.apache.org/jira/browse/THRIFT-1397) - CI server fails during build due to unused parameters in delphi generator
- [THRIFT-1404](https://issues.apache.org/jira/browse/THRIFT-1404) - Delphi compiler generates struct reader code with problem.
- [THRIFT-1400](https://issues.apache.org/jira/browse/THRIFT-1400) - Ruby native extension aborts with __stack_chk_fail in OSX
- [THRIFT-1399](https://issues.apache.org/jira/browse/THRIFT-1399) - One of the TServerImpl.Create CTORs lacks implementation
- [THRIFT-1390](https://issues.apache.org/jira/browse/THRIFT-1390) - Debian packages build fix for Squeeze (build from the official  0.7.0 tarball)
- [THRIFT-1393](https://issues.apache.org/jira/browse/THRIFT-1393) - TTransportException's thrown from THttpClient contain superfluous slashes in the Exception message
- [THRIFT-1392](https://issues.apache.org/jira/browse/THRIFT-1392) - Enabling both namespaces and autoloading in generated PHP code won't work.
- [THRIFT-1406](https://issues.apache.org/jira/browse/THRIFT-1406) - Build error after applying THRIFT-1395
- [THRIFT-1405](https://issues.apache.org/jira/browse/THRIFT-1405) - Delphi compiler does not generates container serializer properly.
- [THRIFT-1411](https://issues.apache.org/jira/browse/THRIFT-1411) - java generator does not provide type parameter for TBaseProcessor
- [THRIFT-1473](https://issues.apache.org/jira/browse/THRIFT-1473) - JSON context stack may be left in an incorrect state when an exception is thrown during read or write operations
- [THRIFT-1331](https://issues.apache.org/jira/browse/THRIFT-1331) - Ruby library deserializes an empty map to nil
- [THRIFT-1330](https://issues.apache.org/jira/browse/THRIFT-1330) - PHP Namespaces no longer generated
- [THRIFT-1328](https://issues.apache.org/jira/browse/THRIFT-1328) - TBaseHelper.toString(...) appends ByteBuffer data outside of valid buffer range
- [THRIFT-1322](https://issues.apache.org/jira/browse/THRIFT-1322) - OCaml lib fail to compile: Thrift.ml line 305, int vs int32 mismatch
- [THRIFT-1143](https://issues.apache.org/jira/browse/THRIFT-1143) - Build doesn't detect correct architecture type on 64bit osx
- [THRIFT-1205](https://issues.apache.org/jira/browse/THRIFT-1205) - port server unduly fragile with arbitrary input
- [THRIFT-1279](https://issues.apache.org/jira/browse/THRIFT-1279) - type set is handled incorrectly when writing object
- [THRIFT-1298](https://issues.apache.org/jira/browse/THRIFT-1298) - Standard scheme doesn't read or write metadata along with field values
- [THRIFT-1265](https://issues.apache.org/jira/browse/THRIFT-1265) - C++ container deserialize
- [THRIFT-1263](https://issues.apache.org/jira/browse/THRIFT-1263) - publish ruby client to rubygems
- [THRIFT-1384](https://issues.apache.org/jira/browse/THRIFT-1384) - Java help menu missing newline near javame flag
- [THRIFT-1382](https://issues.apache.org/jira/browse/THRIFT-1382) - Bundle install doesnot work because thrift crashes
- [THRIFT-1381](https://issues.apache.org/jira/browse/THRIFT-1381) - Thrift C++ libs have incorrectly versioned names
- [THRIFT-1350](https://issues.apache.org/jira/browse/THRIFT-1350) - Go library code does not build as of r60 (most recent release)
- [THRIFT-1365](https://issues.apache.org/jira/browse/THRIFT-1365) - TupleProtocol#writeBitSet unintentionally writes a variable length byte array
- [THRIFT-1359](https://issues.apache.org/jira/browse/THRIFT-1359) - --gen-cob cpp:cob_style does not compile anymore
- [THRIFT-1319](https://issues.apache.org/jira/browse/THRIFT-1319) - Mismatch between how a union reads and writes a container
- [THRIFT-1309](https://issues.apache.org/jira/browse/THRIFT-1309) - libfb303-0.7.0.jar missing in maven repository
- [THRIFT-1238](https://issues.apache.org/jira/browse/THRIFT-1238) - Thrift JS client cannot read map of structures
- [THRIFT-1254](https://issues.apache.org/jira/browse/THRIFT-1254) - Code can't be compiled against a regular JRE: Object.clone() override has a different return type
- [THRIFT-1367](https://issues.apache.org/jira/browse/THRIFT-1367) - Mac OSX build fails with "no such file to load -- spec/rake/spectask"
- [THRIFT-1355](https://issues.apache.org/jira/browse/THRIFT-1355) - Running make in lib/rb doesn't build the native extensions
- [THRIFT-1370](https://issues.apache.org/jira/browse/THRIFT-1370) - Debian packaging should Build-Depend on libglib2.0-dev
- [THRIFT-1342](https://issues.apache.org/jira/browse/THRIFT-1342) - Compilation problem on Windows of fastbinary.c
- [THRIFT-1341](https://issues.apache.org/jira/browse/THRIFT-1341) - TProtocol.h endian detection wrong with boost
- [THRIFT-1583](https://issues.apache.org/jira/browse/THRIFT-1583) - c_glib leaks memory
- [THRIFT-1582](https://issues.apache.org/jira/browse/THRIFT-1582) - Bad includes of nested thrift files in c_glib
- [THRIFT-1578](https://issues.apache.org/jira/browse/THRIFT-1578) - C_GLib generated code does not compile
- [THRIFT-1027](https://issues.apache.org/jira/browse/THRIFT-1027) - 'make -j 16' fails with "unterminated #ifdef" error
- [THRIFT-1121](https://issues.apache.org/jira/browse/THRIFT-1121) - Java server performance regression in 0.6
- [THRIFT-857](https://issues.apache.org/jira/browse/THRIFT-857) - tests run by "make install" fail if generators are disabled
- [THRIFT-380](https://issues.apache.org/jira/browse/THRIFT-380) - Use setuptools for python build

### Dependency upgrade
- [THRIFT-1257](https://issues.apache.org/jira/browse/THRIFT-1257) - thrift's dependency scope on javax.servlet:servlet-api should be 'provided'

### Improvement
- [THRIFT-1445](https://issues.apache.org/jira/browse/THRIFT-1445) - minor C++ generator variable cleanup
- [THRIFT-1435](https://issues.apache.org/jira/browse/THRIFT-1435) - make TException.Message property conformant to the usual expectations
- [THRIFT-1431](https://issues.apache.org/jira/browse/THRIFT-1431) - Rename 'sys' module to 'util'
- [THRIFT-1396](https://issues.apache.org/jira/browse/THRIFT-1396) - Dephi generator has dependacy on boost 1.42 later.
- [THRIFT-1395](https://issues.apache.org/jira/browse/THRIFT-1395) - Patch to prevent warnings for integer types in some cases
- [THRIFT-1275](https://issues.apache.org/jira/browse/THRIFT-1275) -  thrift: always prefix namespaces with " ::"
- [THRIFT-1274](https://issues.apache.org/jira/browse/THRIFT-1274) -  thrift: fail compilation if an unexpected token is
- [THRIFT-1271](https://issues.apache.org/jira/browse/THRIFT-1271) -  thrift: fix missing namespace in generated local
- [THRIFT-1270](https://issues.apache.org/jira/browse/THRIFT-1270) -  thrift: add --allow-neg-keys argument to allow
- [THRIFT-1345](https://issues.apache.org/jira/browse/THRIFT-1345) - Allow building without tests
- [THRIFT-1286](https://issues.apache.org/jira/browse/THRIFT-1286) - Modernize the Thrift Ruby Library Dev Environment
- [THRIFT-1284](https://issues.apache.org/jira/browse/THRIFT-1284) -  thrift: fix processor inheritance
- [THRIFT-1283](https://issues.apache.org/jira/browse/THRIFT-1283) -  thrift: wrap t_cpp_generator::generate_process_function() to 80
- [THRIFT-1282](https://issues.apache.org/jira/browse/THRIFT-1282) - Upgrade httpclient to 4.1.2 (from 4.0.1)
- [THRIFT-1281](https://issues.apache.org/jira/browse/THRIFT-1281) -  add @generated to the docblock
- [THRIFT-1280](https://issues.apache.org/jira/browse/THRIFT-1280) -  Thrift: Improve Monitor exception-free interfaces
- [THRIFT-1278](https://issues.apache.org/jira/browse/THRIFT-1278) - javadoc warnings - compilation
- [THRIFT-1227](https://issues.apache.org/jira/browse/THRIFT-1227) - Erlang implementation of thrift JSON protocol
- [THRIFT-1295](https://issues.apache.org/jira/browse/THRIFT-1295) - Duplicate include in TSocket.cpp
- [THRIFT-1294](https://issues.apache.org/jira/browse/THRIFT-1294) -  thrift: fix log message typos in TSimpleServer
- [THRIFT-1293](https://issues.apache.org/jira/browse/THRIFT-1293) -  thrift: improve handling of exceptions thrown by
- [THRIFT-1292](https://issues.apache.org/jira/browse/THRIFT-1292) -  thrift: silence log spew from TThreadedServer
- [THRIFT-1288](https://issues.apache.org/jira/browse/THRIFT-1288) -  Allow typedefed exceptions in throws clauses
- [THRIFT-1290](https://issues.apache.org/jira/browse/THRIFT-1290) -  thrift: TNonblockingServer: clean up state in the
- [THRIFT-1287](https://issues.apache.org/jira/browse/THRIFT-1287) -  thrift: start refactoring some of the C++ processor
- [THRIFT-1289](https://issues.apache.org/jira/browse/THRIFT-1289) -  thrift: implement TNonblockingServer::stop()
- [THRIFT-1305](https://issues.apache.org/jira/browse/THRIFT-1305) -  thrift: make TConnection a private inner class of
- [THRIFT-1304](https://issues.apache.org/jira/browse/THRIFT-1304) -  TNonblockingServer: pass in the connection context to
- [THRIFT-1302](https://issues.apache.org/jira/browse/THRIFT-1302) -  thrift: raise an exception if send() times out in
- [THRIFT-1301](https://issues.apache.org/jira/browse/THRIFT-1301) -  thrift: consolidate common code in TNonblockingServer
- [THRIFT-1377](https://issues.apache.org/jira/browse/THRIFT-1377) - abort PHP deserialization on unknown field type
- [THRIFT-1379](https://issues.apache.org/jira/browse/THRIFT-1379) - fix uninitialized enum values in thrift C++ objects
- [THRIFT-1376](https://issues.apache.org/jira/browse/THRIFT-1376) - Make port specification option in thrift remote
- [THRIFT-1375](https://issues.apache.org/jira/browse/THRIFT-1375) - fixed a hex char conversion bug in TJSONProtocol
- [THRIFT-1373](https://issues.apache.org/jira/browse/THRIFT-1373) - Fix user-defined exception generation in thrift (python)
- [THRIFT-1361](https://issues.apache.org/jira/browse/THRIFT-1361) - Optional replacement of pthread by boost::thread
- [THRIFT-1320](https://issues.apache.org/jira/browse/THRIFT-1320) - Consistency of configure generated config.h
- [THRIFT-1317](https://issues.apache.org/jira/browse/THRIFT-1317) -  Remove copy constructibility from
- [THRIFT-1316](https://issues.apache.org/jira/browse/THRIFT-1316) -  thrift: update server classes to accept
- [THRIFT-1315](https://issues.apache.org/jira/browse/THRIFT-1315) -  thrift: generate server interface factory classes
- [THRIFT-1314](https://issues.apache.org/jira/browse/THRIFT-1314) -  thrift: add TProcessorFactory
- [THRIFT-1335](https://issues.apache.org/jira/browse/THRIFT-1335) -  Add accept timeout to TServerSocket
- [THRIFT-1334](https://issues.apache.org/jira/browse/THRIFT-1334) -  Add more info to IllegalStateException
- [THRIFT-1333](https://issues.apache.org/jira/browse/THRIFT-1333) -  Make RWGuard not copyable
- [THRIFT-1332](https://issues.apache.org/jira/browse/THRIFT-1332) - TSSLTransportParameters class uses hard coded value keyManagerType: SunX509
- [THRIFT-1251](https://issues.apache.org/jira/browse/THRIFT-1251) - Generated java code should indicate which fields are required and which are optional
- [THRIFT-1387](https://issues.apache.org/jira/browse/THRIFT-1387) - Build MSVC libraries with Boost Threads instead of Pthreads
- [THRIFT-1339](https://issues.apache.org/jira/browse/THRIFT-1339) - Extend Tuple Protocol to TUnions
- [THRIFT-1031](https://issues.apache.org/jira/browse/THRIFT-1031) - Patch to compile Thrift for vc++ 9.0 and 10.0
- [THRIFT-1130](https://issues.apache.org/jira/browse/THRIFT-1130) - Add the ability to specify symbolic default value for optional boolean
- [THRIFT-1123](https://issues.apache.org/jira/browse/THRIFT-1123) - Patch to compile Thrift server and client for vc++ 9.0 and 10.0
- [THRIFT-386](https://issues.apache.org/jira/browse/THRIFT-386) - Make it possible to build the Python library without the extension

### New Feature
- [THRIFT-1401](https://issues.apache.org/jira/browse/THRIFT-1401) - JSON-protocol for Delphi XE Libraries
- [THRIFT-1167](https://issues.apache.org/jira/browse/THRIFT-1167) - Java nonblocking server with more than one thread for select and handling IO
- [THRIFT-1366](https://issues.apache.org/jira/browse/THRIFT-1366) - Delphi generator, lirbrary and unit test.
- [THRIFT-1354](https://issues.apache.org/jira/browse/THRIFT-1354) - Add rake task to build just the gem file
- [THRIFT-769](https://issues.apache.org/jira/browse/THRIFT-769) - Pluggable Serializers

### Sub-task
- [THRIFT-1415](https://issues.apache.org/jira/browse/THRIFT-1415) - delphi: add version Info to the library
- [THRIFT-1391](https://issues.apache.org/jira/browse/THRIFT-1391) - Improved Delphi XE test cases

## 0.7

### Bug
- [THRIFT-1140](https://issues.apache.org/jira/browse/THRIFT-1140) - Framed Transport Client using C (Glib) Library hangs when connecting to Ruby Server
- [THRIFT-1154](https://issues.apache.org/jira/browse/THRIFT-1154) - HttpClient does not specify the connection close parameter
- [THRIFT-1153](https://issues.apache.org/jira/browse/THRIFT-1153) - HttpClient does not specify the connection close parameter
- [THRIFT-1149](https://issues.apache.org/jira/browse/THRIFT-1149) - Nonblocking server fails when client connection is reset
- [THRIFT-1146](https://issues.apache.org/jira/browse/THRIFT-1146) - Android Incompatibility : in Android < 2.3 java.io.IOException doesn't support for Throwable parameter in constructor
- [THRIFT-1133](https://issues.apache.org/jira/browse/THRIFT-1133) - Java and JavaScript tutorial is broken since we have Java maven deployment
- [THRIFT-1132](https://issues.apache.org/jira/browse/THRIFT-1132) - Deserialization error in TApplicationException C#
- [THRIFT-1131](https://issues.apache.org/jira/browse/THRIFT-1131) - C# JSON Protocol is unable to decode escaped characters in string
- [THRIFT-1208](https://issues.apache.org/jira/browse/THRIFT-1208) - python TCompactProtocol.py writeBool and readBool not follow the compact-proto-spec-2.txt spec for CONTAINER_WRITE, CONTAINER_READ
- [THRIFT-1200](https://issues.apache.org/jira/browse/THRIFT-1200) - JS compiler generates code that clobbers existing namespaces
- [THRIFT-1183](https://issues.apache.org/jira/browse/THRIFT-1183) - Pure-ruby CompactProtocol raises ArgumentError when deserializing under Ruby 1.9
- [THRIFT-1182](https://issues.apache.org/jira/browse/THRIFT-1182) - Native deserializer segfaults on incorrect list element type
- [THRIFT-1181](https://issues.apache.org/jira/browse/THRIFT-1181) - AS3 compiler generates incorrect code for setting default values in constructor
- [THRIFT-1234](https://issues.apache.org/jira/browse/THRIFT-1234) - thrift --help is missing doc on py:utf8strings
- [THRIFT-1180](https://issues.apache.org/jira/browse/THRIFT-1180) - AS3 compiler generates uncompilable code for binary types.
- [THRIFT-1194](https://issues.apache.org/jira/browse/THRIFT-1194) - Java lib does not install artifacts to local dir correctly
- [THRIFT-1193](https://issues.apache.org/jira/browse/THRIFT-1193) - Potential infinite loop in nonblocking_server
- [THRIFT-1192](https://issues.apache.org/jira/browse/THRIFT-1192) - Typo: TProtocol.h tests for HAVE_SYS_PARAM_H_
- [THRIFT-1190](https://issues.apache.org/jira/browse/THRIFT-1190) - readBufferBytesAllocated in TNonblockingServer.java should be AtomicLong to fix FD leakage and general server malfunction
- [THRIFT-1187](https://issues.apache.org/jira/browse/THRIFT-1187) - nonblocking_server shutdown race under Ruby 1.9
- [THRIFT-1178](https://issues.apache.org/jira/browse/THRIFT-1178) - Java: TBase signature should be T extends TBase<?,?>
- [THRIFT-1164](https://issues.apache.org/jira/browse/THRIFT-1164) - Segmentation fault on NULL pointer in t_js_generator::generate_const
- [THRIFT-1171](https://issues.apache.org/jira/browse/THRIFT-1171) - Perl write/readDouble assumes little-endian platform
- [THRIFT-1222](https://issues.apache.org/jira/browse/THRIFT-1222) - Unhandled exception for TEvhttpServer request
- [THRIFT-1220](https://issues.apache.org/jira/browse/THRIFT-1220) - TProcessor::process never returns false
- [THRIFT-1285](https://issues.apache.org/jira/browse/THRIFT-1285) - Stable 0.7.0 Windows compiler exe available on the webside is not the good one
- [THRIFT-1218](https://issues.apache.org/jira/browse/THRIFT-1218) - c_glib uses wrong name in pkg-config
- [THRIFT-1215](https://issues.apache.org/jira/browse/THRIFT-1215) - Undefined property Thirft in lib/js/thrift.js
- [THRIFT-1211](https://issues.apache.org/jira/browse/THRIFT-1211) - When using THttpClient, non 200 responses leave the connection open
- [THRIFT-1228](https://issues.apache.org/jira/browse/THRIFT-1228) - The php accelerator module calls flush incorrectly
- [THRIFT-1308](https://issues.apache.org/jira/browse/THRIFT-1308) - libfb303-0.7.0.jar missing in maven repository
- [THRIFT-1255](https://issues.apache.org/jira/browse/THRIFT-1255) - Mismatch of method name between JavaME's lib and generated code (compareTo/compareObjects)
- [THRIFT-1253](https://issues.apache.org/jira/browse/THRIFT-1253) - Code generated for maps is not compiling
- [THRIFT-1252](https://issues.apache.org/jira/browse/THRIFT-1252) - Segfault in Ruby deserializer
- [THRIFT-1094](https://issues.apache.org/jira/browse/THRIFT-1094) - bug in TCompactProto python readMessageEnd method and updated test cases
- [THRIFT-1093](https://issues.apache.org/jira/browse/THRIFT-1093) - several bugs in python TCompactProtocol
- [THRIFT-1092](https://issues.apache.org/jira/browse/THRIFT-1092) - generated validate() method has wrong indentation
- [THRIFT-1011](https://issues.apache.org/jira/browse/THRIFT-1011) - Error generating package imports when using classes from other packages
- [THRIFT-1050](https://issues.apache.org/jira/browse/THRIFT-1050) - Declaring an argument named "manager" to a service method produces code that fails compile due to name conflicts with protected ivars in TAsyncClient
- [THRIFT-1074](https://issues.apache.org/jira/browse/THRIFT-1074) - .keystore and .truststore are missing from the 0.6.0 distribution
- [THRIFT-1067](https://issues.apache.org/jira/browse/THRIFT-1067) - Tons of bugs in php implementation
- [THRIFT-1065](https://issues.apache.org/jira/browse/THRIFT-1065) - Unexpected exceptions not proper handled on JS
- [THRIFT-1076](https://issues.apache.org/jira/browse/THRIFT-1076) - Erlang Thrift socket server has a bug that causes java thrift client of framed binary client to throw "out of sequence" exception
- [THRIFT-1057](https://issues.apache.org/jira/browse/THRIFT-1057) - casts in TBinaryProtocol.tcc causing "dereferencing type-punned pointer will break strict-aliasing rules" warnings from gcc
- [THRIFT-1055](https://issues.apache.org/jira/browse/THRIFT-1055) - csharp TServerSocket and TSocket do not disable Nagle via Socket.NoDelay = true like cpp and java do
- [THRIFT-1054](https://issues.apache.org/jira/browse/THRIFT-1054) - explicit call to PKG_PROG_PKG_CONFIG is missing and first use of PKG_CHECK_MODULES may not happen, causes mono detection to fail
- [THRIFT-1117](https://issues.apache.org/jira/browse/THRIFT-1117) - JavaScript Unit Test does not work anymore because libthrift*.jar where moved by Maven Deployment
- [THRIFT-1111](https://issues.apache.org/jira/browse/THRIFT-1111) - The HTML generator does not distinguish between string and binary types
- [THRIFT-1032](https://issues.apache.org/jira/browse/THRIFT-1032) - "make dist" fails due to c_glib problem
- [THRIFT-1036](https://issues.apache.org/jira/browse/THRIFT-1036) - Auto-generated C++ code fails to compile with "-Werror -Wextra -Wall" g++ compiler flags
- [THRIFT-1041](https://issues.apache.org/jira/browse/THRIFT-1041) - TDeserializer holds onto a reference of the array it reads after it is done deserializing
- [THRIFT-1106](https://issues.apache.org/jira/browse/THRIFT-1106) - C++ code TAsyncProtocolProcessor.h & TAsyncBufferProcessor.h dont have virtual functions but no virtual destructor. Causes warnings on -Wall
- [THRIFT-1105](https://issues.apache.org/jira/browse/THRIFT-1105) - OCaml generator does not prefix methods of included structs with their type
- [THRIFT-1104](https://issues.apache.org/jira/browse/THRIFT-1104) - INSTALLDIRS should be included in configure script
- [THRIFT-1102](https://issues.apache.org/jira/browse/THRIFT-1102) - typo in configure.ac: "==" operator in 'test' (instead of"'=")
- [THRIFT-1101](https://issues.apache.org/jira/browse/THRIFT-1101) - bytebuffer length calculation in TBinaryProtocol writeBinary
- [THRIFT-1098](https://issues.apache.org/jira/browse/THRIFT-1098) - Undefined properties in TBinaryProtocolFactory
- [THRIFT-1081](https://issues.apache.org/jira/browse/THRIFT-1081) - PHP tests broken and somewhat incomplete
- [THRIFT-1080](https://issues.apache.org/jira/browse/THRIFT-1080) - erlang test's 'make' fails on Mac OSX
- [THRIFT-1078](https://issues.apache.org/jira/browse/THRIFT-1078) - ThriftTest.thrift generates invalid PHP library
- [THRIFT-1120](https://issues.apache.org/jira/browse/THRIFT-1120) - proto.WriteListEnd being called in the wrong place
- [THRIFT-1119](https://issues.apache.org/jira/browse/THRIFT-1119) - TJSONProtocol fails to UTF8 decode strings
- [THRIFT-867](https://issues.apache.org/jira/browse/THRIFT-867) - PHP accelerator module's output transport is incompatible with TFramedTransport
- [THRIFT-826](https://issues.apache.org/jira/browse/THRIFT-826) - PHP TSocket Write Timeout
- [THRIFT-835](https://issues.apache.org/jira/browse/THRIFT-835) - Bad AS3 syntax in constructors that set default values
- [THRIFT-788](https://issues.apache.org/jira/browse/THRIFT-788) - thrift_protocol.so: multiget/multiget_slice does not handle more than 17 keys correctly
- [THRIFT-125](https://issues.apache.org/jira/browse/THRIFT-125) - OCaml libraries don't compile with 32-bit ocaml
- [THRIFT-342](https://issues.apache.org/jira/browse/THRIFT-342) - PHP: can't have sets of complex types
- [THRIFT-731](https://issues.apache.org/jira/browse/THRIFT-731) - configure doesn't check for ant >= 1.7
- [THRIFT-690](https://issues.apache.org/jira/browse/THRIFT-690) - Update TApplicationException codes
- [THRIFT-638](https://issues.apache.org/jira/browse/THRIFT-638) - BufferedTransport + C extensions block until recv timeout is reached on last fread call

### Dependency upgrade
- [THRIFT-1177](https://issues.apache.org/jira/browse/THRIFT-1177) - Update thrift to reflect changes in Go's networking libraries

### Improvement
- [THRIFT-1155](https://issues.apache.org/jira/browse/THRIFT-1155) - Remove log4j dependency from java client
- [THRIFT-1151](https://issues.apache.org/jira/browse/THRIFT-1151) - Produce more informative runtime error in case of schema and data mismatch during serialization
- [THRIFT-1207](https://issues.apache.org/jira/browse/THRIFT-1207) - Support DESTDIR on "make install" of ruby libs
- [THRIFT-1199](https://issues.apache.org/jira/browse/THRIFT-1199) - Union structs should have generated methods to test whether a specific field is currently set
- [THRIFT-1233](https://issues.apache.org/jira/browse/THRIFT-1233) - Remove unused include in generated C++ code
- [THRIFT-1189](https://issues.apache.org/jira/browse/THRIFT-1189) - Ruby deserializer speed improvements
- [THRIFT-1170](https://issues.apache.org/jira/browse/THRIFT-1170) - Thrift Generated Code and Java 5
- [THRIFT-1174](https://issues.apache.org/jira/browse/THRIFT-1174) - Publish as3 client implementation via Maven for use by flex-mojos users
- [THRIFT-1225](https://issues.apache.org/jira/browse/THRIFT-1225) - TCompactProtocol for PHP
- [THRIFT-1221](https://issues.apache.org/jira/browse/THRIFT-1221) - Remove SimpleCallback.h
- [THRIFT-1217](https://issues.apache.org/jira/browse/THRIFT-1217) - Use evutil_socketpair instead of pipe (Windows port)
- [THRIFT-1216](https://issues.apache.org/jira/browse/THRIFT-1216) - build Java Library behind a proxy
- [THRIFT-1231](https://issues.apache.org/jira/browse/THRIFT-1231) - Remove bogus include
- [THRIFT-1213](https://issues.apache.org/jira/browse/THRIFT-1213) - Membuffer should provide a way to get back the buffer
- [THRIFT-1237](https://issues.apache.org/jira/browse/THRIFT-1237) - Java fb303 missing some methods
- [THRIFT-1063](https://issues.apache.org/jira/browse/THRIFT-1063) - Fix Erlang Tutorial Files
- [THRIFT-1053](https://issues.apache.org/jira/browse/THRIFT-1053) - Make remote client's IP address available for all socket related transports
- [THRIFT-1109](https://issues.apache.org/jira/browse/THRIFT-1109) - Deploy fb303 along side libthrift to maven repo
- [THRIFT-1107](https://issues.apache.org/jira/browse/THRIFT-1107) - improvement for compiler-generated python for 'None' object comparisons
- [THRIFT-1069](https://issues.apache.org/jira/browse/THRIFT-1069) - Add command line option to prevent thrift from inserting gen-* directories
- [THRIFT-1049](https://issues.apache.org/jira/browse/THRIFT-1049) - Allow for TServerSocket python library to bind to a specific host
- [THRIFT-1126](https://issues.apache.org/jira/browse/THRIFT-1126) - Extending struct_info for erlang bindings
- [THRIFT-1100](https://issues.apache.org/jira/browse/THRIFT-1100) - python TSSLSocket improvements, including certificate validation
- [THRIFT-994](https://issues.apache.org/jira/browse/THRIFT-994) - Don't try to invoke phpize if we don't have it
- [THRIFT-993](https://issues.apache.org/jira/browse/THRIFT-993) - Some improvements in C++ stubs for oneway operations
- [THRIFT-997](https://issues.apache.org/jira/browse/THRIFT-997) - Using valueOf for base types in getFieldValue
- [THRIFT-418](https://issues.apache.org/jira/browse/THRIFT-418) - Don't do runtime sorting of struct fields
- [THRIFT-151](https://issues.apache.org/jira/browse/THRIFT-151) - TSSLServerSocket and TSSLSocket implementation
- [THRIFT-27](https://issues.apache.org/jira/browse/THRIFT-27) - Generated erlang types don't contain default values for records
- [THRIFT-113](https://issues.apache.org/jira/browse/THRIFT-113) - to-string methods should omit optional null fields from output
- [THRIFT-363](https://issues.apache.org/jira/browse/THRIFT-363) - Maven Deploy
- [THRIFT-447](https://issues.apache.org/jira/browse/THRIFT-447) - Make an abstract base Client class so we can generate less code
- [THRIFT-627](https://issues.apache.org/jira/browse/THRIFT-627) - should c++ have setters for optional fields?

### New Feature
- [THRIFT-1236](https://issues.apache.org/jira/browse/THRIFT-1236) - Erlang Reconnecting Thrift Client
- [THRIFT-1021](https://issues.apache.org/jira/browse/THRIFT-1021) - Framed transport support for OCaml
- [THRIFT-1068](https://issues.apache.org/jira/browse/THRIFT-1068) - Python SSL Socket Support
- [THRIFT-1103](https://issues.apache.org/jira/browse/THRIFT-1103) - TZlibTransport for python, a zlib compressed transport
- [THRIFT-1083](https://issues.apache.org/jira/browse/THRIFT-1083) - Preforking python process pool server
- [THRIFT-999](https://issues.apache.org/jira/browse/THRIFT-999) - Add TForkingServer

### Sub-task
- [THRIFT-1152](https://issues.apache.org/jira/browse/THRIFT-1152) - Attributes from private to protected
- [THRIFT-1038](https://issues.apache.org/jira/browse/THRIFT-1038) - Generated Java code for structures containing binary fields (or collections thereof) are not serializable (in the Java sense) even though they implement java.io.Serializable

### Task
- [THRIFT-892](https://issues.apache.org/jira/browse/THRIFT-892) - Refactor erlang build system with rebar

### Wish
- [THRIFT-625](https://issues.apache.org/jira/browse/THRIFT-625) - Add support for 'Go'

## 0.6.1

### Bug
- [THRIFT-1133](https://issues.apache.org/jira/browse/THRIFT-1133) - Java and JavaScript tutorial is broken since we have Java maven deployment
- [THRIFT-1131](https://issues.apache.org/jira/browse/THRIFT-1131) - C# JSON Protocol is unable to decode escaped characters in string
- [THRIFT-1074](https://issues.apache.org/jira/browse/THRIFT-1074) - .keystore and .truststore are missing from the 0.6.0 distribution

### Improvement
- [THRIFT-1109](https://issues.apache.org/jira/browse/THRIFT-1109) - Deploy fb303 along side libthrift to maven repo
- [THRIFT-363](https://issues.apache.org/jira/browse/THRIFT-363) - Maven Deploy

### Question
- [THRIFT-1206](https://issues.apache.org/jira/browse/THRIFT-1206) - did the THRIFT 0.6.1 merge THRIFT-563 ?

### Sub-task
- [THRIFT-1163](https://issues.apache.org/jira/browse/THRIFT-1163) - How can i use multi service in one program?

### Task
- [THRIFT-1112](https://issues.apache.org/jira/browse/THRIFT-1112) - Apply THRIFT-363 to 0.6 branch
- [THRIFT-1113](https://issues.apache.org/jira/browse/THRIFT-1113) - Apply THRIFT-1074 to 0.6 branch

## 0.6

### Bug
- [THRIFT-1020](https://issues.apache.org/jira/browse/THRIFT-1020) - OCaml compiler generates invalid OCaml
- [THRIFT-1015](https://issues.apache.org/jira/browse/THRIFT-1015) - TUnion does not handle ByteBuffer in toString
- [THRIFT-1013](https://issues.apache.org/jira/browse/THRIFT-1013) - generated java code may have name clashes with thrift library
- [THRIFT-1009](https://issues.apache.org/jira/browse/THRIFT-1009) - TUnion does not correctly deep copy a ByteBuffer
- [THRIFT-1032](https://issues.apache.org/jira/browse/THRIFT-1032) - "make dist" fails due to c_glib problem
- [THRIFT-868](https://issues.apache.org/jira/browse/THRIFT-868) - Referencing constant values doesn't work with with typedef types
- [THRIFT-971](https://issues.apache.org/jira/browse/THRIFT-971) - java module can't be compiled without ivy and network connection
- [THRIFT-970](https://issues.apache.org/jira/browse/THRIFT-970) - Under heavy load, THttpClient may fail with "too many open files"
- [THRIFT-969](https://issues.apache.org/jira/browse/THRIFT-969) - Java Tutorial broken, move CalculatorHandler to a separate file
- [THRIFT-807](https://issues.apache.org/jira/browse/THRIFT-807) - JavaScript: Initialization of Base Types with 0 instead of null
- [THRIFT-955](https://issues.apache.org/jira/browse/THRIFT-955) - Thrift compiler for Windows uses lowercase names and directories which is inconsistent with compiling on other platforms
- [THRIFT-992](https://issues.apache.org/jira/browse/THRIFT-992) - Naming convention in C# constructor is not consistent with other fields causes compile errors
- [THRIFT-1008](https://issues.apache.org/jira/browse/THRIFT-1008) - byte[] accessors throw NPE on unset field
- [THRIFT-1006](https://issues.apache.org/jira/browse/THRIFT-1006) - Impossible to correctly qualify an enum constant in an external thrift file
- [THRIFT-950](https://issues.apache.org/jira/browse/THRIFT-950) - Haskell bindings treat 'byte' as unsigned 8-bit int (Data.Word.Word8), java/cpp as signed (byte/int8_t).
- [THRIFT-975](https://issues.apache.org/jira/browse/THRIFT-975) - lib/c_glib/README is missing => breaks make dist
- [THRIFT-944](https://issues.apache.org/jira/browse/THRIFT-944) - Support all version-4s of base
- [THRIFT-939](https://issues.apache.org/jira/browse/THRIFT-939) - optional binary fields throw NPE on default byte[] getters
- [THRIFT-935](https://issues.apache.org/jira/browse/THRIFT-935) - PHP Extension aborts the build if php-config is not installed
- [THRIFT-933](https://issues.apache.org/jira/browse/THRIFT-933) - Haskell's Thrift.cabal has warnings
- [THRIFT-932](https://issues.apache.org/jira/browse/THRIFT-932) - Haskell tests need to be run through 'make check' (and probably 'cabal check') too
- [THRIFT-904](https://issues.apache.org/jira/browse/THRIFT-904) - C# TSocket should disable nagle and linger
- [THRIFT-941](https://issues.apache.org/jira/browse/THRIFT-941) - Make PHP C Extension use the defined Protocol writeMessageBegin function
- [THRIFT-940](https://issues.apache.org/jira/browse/THRIFT-940) - 'make check' fails if boost is not in the std include and link paths
- [THRIFT-924](https://issues.apache.org/jira/browse/THRIFT-924) - Fix generated php structure constants
- [THRIFT-979](https://issues.apache.org/jira/browse/THRIFT-979) - ruby bindings used to work on jruby
- [THRIFT-977](https://issues.apache.org/jira/browse/THRIFT-977) - Hex Conversion Bug in C++ TJSONProtocol
- [THRIFT-347](https://issues.apache.org/jira/browse/THRIFT-347) - PHP TSocket Timeout Issues
- [THRIFT-517](https://issues.apache.org/jira/browse/THRIFT-517) - TExceptions thrown by server result in cryptic error message on client - Tried to read 4 bytes, but only got 0 bytes

### Improvement
- [THRIFT-1024](https://issues.apache.org/jira/browse/THRIFT-1024) - Add Python Twisted example to the Tutorial
- [THRIFT-958](https://issues.apache.org/jira/browse/THRIFT-958) - Change accessmodifer on trans_ field in the FrameBuffer class to public.
- [THRIFT-957](https://issues.apache.org/jira/browse/THRIFT-957) - THsHaServer: Change access modifier of the invoker field.
- [THRIFT-1002](https://issues.apache.org/jira/browse/THRIFT-1002) - CodeStyle: t_c_glib_generator.cc
- [THRIFT-1005](https://issues.apache.org/jira/browse/THRIFT-1005) - Give unions byte[] signature methods to go along with their ByteBuffer counterparts
- [THRIFT-951](https://issues.apache.org/jira/browse/THRIFT-951) - Add a new isServing() method to TServer
- [THRIFT-943](https://issues.apache.org/jira/browse/THRIFT-943) - Silly readme typo fix.
- [THRIFT-961](https://issues.apache.org/jira/browse/THRIFT-961) - JavaScript TestSuite using ant/ivy and Java's ServerTestBase Handler
- [THRIFT-960](https://issues.apache.org/jira/browse/THRIFT-960) - add TestServer, TestNonblockingServer and TestClient again
- [THRIFT-949](https://issues.apache.org/jira/browse/THRIFT-949) - Modify the TEnum interface so it defines a method similar to findByValue
- [THRIFT-946](https://issues.apache.org/jira/browse/THRIFT-946) - Augment FieldValueMetaData so it differentiates 'string' and 'binary' fields.
- [THRIFT-903](https://issues.apache.org/jira/browse/THRIFT-903) - custom ThreadFactory in THsHaServer
- [THRIFT-913](https://issues.apache.org/jira/browse/THRIFT-913) - Test Case for Url encoded strings + simple enhancement to lib/js/test/RunTestServer.sh
- [THRIFT-926](https://issues.apache.org/jira/browse/THRIFT-926) - Miscellaneous C++ improvements
- [THRIFT-929](https://issues.apache.org/jira/browse/THRIFT-929) - Improvements to the C++ test suite
- [THRIFT-893](https://issues.apache.org/jira/browse/THRIFT-893) - add JavaScript to the tutorial examples
- [THRIFT-1003](https://issues.apache.org/jira/browse/THRIFT-1003) - Polishing c_glib code
- [THRIFT-71](https://issues.apache.org/jira/browse/THRIFT-71) - Debian packaging for thrift

### New Feature
- [THRIFT-1033](https://issues.apache.org/jira/browse/THRIFT-1033) - Node.js language target
- [THRIFT-947](https://issues.apache.org/jira/browse/THRIFT-947) - Provide a helper method to determine the TProtocol used to serialize some data.
- [THRIFT-928](https://issues.apache.org/jira/browse/THRIFT-928) - Make more statistics available in C++ servers
- [THRIFT-922](https://issues.apache.org/jira/browse/THRIFT-922) - Templatized [de]serialization code for C++
- [THRIFT-923](https://issues.apache.org/jira/browse/THRIFT-923) - Event-driven client and server support for C++
- [THRIFT-925](https://issues.apache.org/jira/browse/THRIFT-925) - Provide name<->value map for enums in C++
- [THRIFT-927](https://issues.apache.org/jira/browse/THRIFT-927) - Add option to modify the PHP include path
- [THRIFT-377](https://issues.apache.org/jira/browse/THRIFT-377) - TFileTransport port in Java
- [THRIFT-106](https://issues.apache.org/jira/browse/THRIFT-106) - TSSLServerSocket
- [THRIFT-582](https://issues.apache.org/jira/browse/THRIFT-582) - C implementation of Thrift
- [THRIFT-745](https://issues.apache.org/jira/browse/THRIFT-745) - Make it easier to instantiate servers

### Sub-task
- [THRIFT-1038](https://issues.apache.org/jira/browse/THRIFT-1038) - Generated Java code for structures containing binary fields (or collections thereof) are not serializable (in the Java sense) even though they implement java.io.Serializable

### Task
- [THRIFT-862](https://issues.apache.org/jira/browse/THRIFT-862) - Async client issues / improvements

### Test
- [THRIFT-581](https://issues.apache.org/jira/browse/THRIFT-581) - Add a testsuite for txThrift (Twisted)

## Incubating Versions

Thrift 0.5.0 - Incubating
--------------------------------------------------------------------------------
THRIFT-505   Build                Make configure give a summary of the enabled components (David Reiss)
THRIFT-506   Build                Allow Thrift to be built without the C++ library (David Reiss)
THRIFT-844   Build                Build Requirements state autoconf 2.59+ is required, but 2.60+ is needed (Harlan Lieberman-Berg)
THRIFT-850   Build                Perl runtime requires Bit::Vector which may not be installed by default, but configure does not fail (Michael Lum)
THRIFT-854   Build                Provide configure option and make rules to build/install php extension (Anthony Molinaro)
THRIFT-858   Build                Have bootstrap.sh check for a suitable autoconf version before running (David Reiss)
THRIFT-871   Build                Thrift compiler for WIndows (binary distribution) (David Reiss)
THRIFT-323   C#                   TJSONProtocol (Roger Meier)
THRIFT-634   C#                   C# Compiler Generates Incorrect Code For Fields which begin with an uppercase letter (Jon S Akhtar)
THRIFT-881   C#                   add csharp to the tutorial (Roger Meier)
THRIFT-856   C++                  Building cpp library fails on OS X with malloc and free not being declared in scope (James Clarke)
THRIFT-865   C++                  C++ compiler build depends on libfl even when flex/lex not detected (David Reiss)
THRIFT-900   C++                  Unix domain socket (Roger Meier)
THRIFT-920   C++                  C++ Test and Tutorial does not compile anymore due to the change within Enum handling (Roger Meier)
THRIFT-567   C++                  Can't immediately stop a TSimpleServer thread that is idle (Rush Manbert)
THRIFT-756   C++                  Exposing TSocket(int) constructor to public (Rajat Goel)
THRIFT-798   C++                  TNonblockingServer leaks resources when destroyed (David Reiss)
THRIFT-812   C++, Python          Demo of Thrift over ZeroMQ (David Reiss)
THRIFT-629   Cocoa                Unused Field In TSocketServer Appears To Break iPhone Build (Jon S Akhtar)
THRIFT-838   Cocoa                Generated Cocoa classes have useless @dynamic declarations (Kevin Ballard)
THRIFT-805   Cocoa                Don't generate process_XXXX methods for oneway methods (Brad Taylor)
THRIFT-507   Compiler             Remove the compiler's dependency on Boost (David Reiss)
THRIFT-895   Compiler (General)   Thrift compiler does not allow two different enumerations to have the same key name for one of the enum values (David Reiss)
THRIFT-852   Compiler (General)   Missing newline causes many compiler warnings (Anthony Molinaro)
THRIFT-877   Compiler (General)   smalltalk namespace doesn't work (Bruce Lowekamp)
THRIFT-897   Compiler (General)   Don't allow unqualified constant access to enum values (Bryan Duxbury)
THRIFT-9     Compiler (General)   Add a default namespace declaration for all languages (David Reiss)
THRIFT-599   Erlang               Don't use unnecessary processes in the Erlang transports and clients (David Reiss)
THRIFT-646   Erlang               Erlang library is missing install target (David Reiss)
THRIFT-698   Erlang               Generated module list should contain atoms, not strings (Anthony Molinaro)
THRIFT-866   Erlang               term() in spec definitions seems to not work in erlang R12 (Anthony Molinaro)
THRIFT-886   Erlang               Dialyzer warning (Anthony Molinaro)
THRIFT-785   Erlang               Framed transport server problems (Anthony Molinaro)
THRIFT-884   HTML                 HTML Generator: add Key attribute to the Data Types Tables (Roger Meier)
THRIFT-652   Haskell              Generated field name for strut is not capitalized correctly (Christian Lavoie)
THRIFT-743   Haskell              compile error with GHC 6.12.1 (Christian Lavoie)
THRIFT-901   Haskell              Allow the bindings to compile without -fglasgow-exts and with -Wall -Werror (Christian Lavoie)
THRIFT-905   Haskell              Make haskell thrift bindings use automake to compile and install (Christian Lavoie)
THRIFT-906   Haskell              Improve type mappings (Christian Lavoie)
THRIFT-914   Haskell              Make haskell bindings 'easily' compilable (Christian Lavoie)
THRIFT-918   Haskell              Make haskell tests run again (Christian Lavoie)
THRIFT-919   Haskell              Update Haskell bindings README (Christian Lavoie)
THRIFT-787   Haskell              Enums are not read correctly (Christian Lavoie)
THRIFT-250   Java                 ExecutorService as a constructor parameter for TServer (Ed Ceaser)
THRIFT-693   Java                 Thrift compiler generated java code that throws compiler warnings about deprecated methods. (Bryan Duxbury)
THRIFT-843   Java                 TNonblockingSocket connects without a timeout (Bryan Duxbury)
THRIFT-845   Java                 async client does not respect timeout (Ning Liang)
THRIFT-870   Java                 Java constants don't get Javadoc comments (Bryan Duxbury)
THRIFT-873   Java                 Java tests fail due to Too many open files (Todd Lipcon)
THRIFT-876   Java                 Add SASL support (Aaron T. Myers)
THRIFT-879   Java                 Remove @Override from TUnion.clear (Dave Engberg)
THRIFT-882   Java                 deep copy of binary fields does not copy ByteBuffer characteristics (arrayOffset, position) (Bryan Duxbury)
THRIFT-888   Java                 async client should also have nonblocking connect (Eric Jensen)
THRIFT-890   Java                 Java tutorial doesn't work (Todd Lipcon)
THRIFT-894   Java                 Make default accessors for binary fields return byte[]; provide new accessors to get ByteBuffer version (Bryan Duxbury)
THRIFT-896   Java                 TNonblockingSocket.isOpen() returns true even after close() (Eric Jensen)
THRIFT-907   Java                 libfb303 doesn't compile in 0.4.0 (Todd Lipcon)
THRIFT-912   Java                 Improvements and bug fixes to SASL implementation (Todd Lipcon)
THRIFT-917   Java                 THsHaServer should not accept an ExecutorService without catching RejectedExecutionException (Ed Ceaser)
THRIFT-931   Java                 Use log4j for Java tests (Todd Lipcon)
THRIFT-880   JavaME               JavaME code generator and runtime library (Dave Engberg)
THRIFT-846   JavaScript           JavaScript Test Framwork: extended Testcases (Roger Meier)
THRIFT-885   JavaScript           Url encoded strings never get decoded? How do we fix this? (T Jake Luciani)
THRIFT-911   JavaScript           (JavaScript compiler) Const structs, maps, sets, and lists generate a trailing comma (T Jake Luciani)
THRIFT-860   OCaml                copy method and reset method (Lev Walkin)
THRIFT-682   PHP                  PHP extension doesn't compile on Mac OS X (Bryan Duxbury)
THRIFT-851   PHP                  php extension fails to compile on centos 5.x (Todd Lipcon)
THRIFT-840   Perl                 Perl protocol handler could be more robust against unrecognised types (Conrad Hughes)
THRIFT-758   Perl                 incorrect deference in exception handling (Yann Kerherve)
THRIFT-257   Python               Support validation of required fields (Esteve Fernandez)
THRIFT-335   Python               Compact Protocol for Python (David Reiss)
THRIFT-596   Python               Make Python's TBufferedTransport use a configurable input buffer (David Reiss)
THRIFT-597   Python               Python THttpServer performance improvements (David Reiss)
THRIFT-598   Python               Allow Python's threading servers to use daemon threads (David Reiss)
THRIFT-666   Python               Allow the handler to override HTTP responses in THttpServer (David Reiss)
THRIFT-673   Python               Generated Python code has whitespace issues (Ian Eure)
THRIFT-721   Python               THttpClient ignores url parameters (Thomas Kho)
THRIFT-824   Python               TApplicationException.__str__() refers to class constants as globals (Peter Schuller)
THRIFT-855   Python               Include optimized compiled python objects in install (Anthony Molinaro)
THRIFT-859   Python               Allow py:twisted to be generated in different namespace than py (Bruce Lowekamp)
THRIFT-869   Python               TSocket.py on Mac (and FreeBSD) doesn't handle ECONNRESET from recv() (Steven Knight)
THRIFT-875   Python               Include python setup.cfg in dist (Anthony Molinaro)
THRIFT-610   Ruby                 binary_protocol.rb segfaults [line 86] (Unassigned)
THRIFT-899   Ruby                 Ruby read timeouts can sometimes be 2x what they should be (Ryan King)
THRIFT-909   Ruby                 allow block argument to struct constructor (Michael Stockton)
THRIFT-456   Test Suite           Bad IP address string in test/cpp/src/main.cpp (Rush Manbert)


Thrift 0.4.0 - Incubating
--------------------------------------------------------------------------------
THRIFT-650   Build        Make Check fails on Centos/OSX with 0.2.0 tarball (Anthony Molinaro)
THRIFT-770   Build        Get 'make dist' to work without first compiling source code (Anthony Molinaro)
THRIFT-160   C#           Created THttpTransport for the C# library based on WebHttpRequest (Michael Greene)
THRIFT-834   C#           THttpClient resends contents of message after transport errors (Anatoly Fayngelerin)
THRIFT-247   C++          THttpServer Transport (Unassigned)
THRIFT-676   C++          Change C++ code generator so that generated classes can be wrapped with SWIG (Unassigned)
THRIFT-570   Compiler     Thrift compiler does not error when duplicate method names are present (Bruce Simpson)
THRIFT-808   Compiler     Segfault when constant declaration references a struct field that doesn't exist (Bryan Duxbury)
THRIFT-646   Erlang       Erlang library is missing install target (Anthony Molinaro)
THRIFT-544   General      multiple enums with the same key generate invalid code (Ben Taitelbaum)
THRIFT-434   General      ruby compiler should warn when a reserved word is used (Michael Stockton)
THRIFT-799   General      Files missing proper Apache license header (Bryan Duxbury)
THRIFT-832   HTML         HTML generator shows unspecified struct fields as 'required' (Bryan Duxbury)
THRIFT-226   Java         Collections with binary keys or values break equals() (Bryan Duxbury)
THRIFT-484   Java         Ability to use a slice of a buffer instead of a direct byte[] for binary fields (Bryan Duxbury)
THRIFT-714   Java         maxWorkerThreads parameter to THsHaServer has no effect (Bryan Duxbury)
THRIFT-751   Java         Add clear() method to TBase (Bryan Duxbury)
THRIFT-765   Java         Improved string encoding and decoding performance (Bryan Duxbury)
THRIFT-768   Java         Async client for Java (Bryan Duxbury)
THRIFT-774   Java         TDeserializer should provide a partialDeserialize method for primitive types (Piotr Kozikowski)
THRIFT-783   Java         .equals java method is broken on structs containing binary-type fields (Unassigned)
THRIFT-804   Java         CompareTo is broken for unions set to map, set, or list (Bryan Duxbury)
THRIFT-814   Java         Include a TServlet in the standard Thrift distribution (Mathias Herberts)
THRIFT-818   Java         Async client doesn't send method args (Bryan Duxbury)
THRIFT-830   Java         Switch binary field implementation from byte[] to ByteBuffer (Bryan Duxbury)
THRIFT-831   Java         FramedTransport implementation that reuses its buffers (Bryan Duxbury)
THRIFT-833   Java         build.xml in lib/java is missing a classpathref attribute for the javadoc task (Bryan Duxbury)
THRIFT-836   Java         Race condition causes CancelledKeyException in TAsyncClientManager (Bryan Duxbury)
THRIFT-842   Java         Upgrade to current version of commons-lang (2.5 instead of 2.4) and/or change dependency in ivy.xml to not be exact (Bryan Duxbury)
THRIFT-815   JavaScript   Deserialization of lists is critically broken. (T Jake Luciani)
THRIFT-827   OCaml        OCaml generator to take default values into account (Lev Walkin)
THRIFT-647   PHP          PHP library is missing install target (Anthony Molinaro)
THRIFT-682   PHP          PHP extension doesn't compile on Mac OS X (Bryan Duxbury)
THRIFT-718   PHP          Thrift PHP library includes closing tags and extraneous whitespace (Nicholas Telford)
THRIFT-778   PHP          PHP socket listening server (Nick Jones)
THRIFT-780   PHP          PHP extension sometimes causes an abort with two exceptions at the same time (David Reiss)
THRIFT-837   PHP          PHP accelerator bug for writes > 8k (Thomas Kho)
THRIFT-782   Perl         Perl code for writing containers doesn't count length of write*Begin or write*End (Conrad Hughes)
THRIFT-395   Python       Python library + compiler does not support unicode strings (Unassigned)
THRIFT-133   Ruby         'namespace ruby' should error out, or be an alias to 'namespace rb' (Bryan Duxbury)
THRIFT-664   Ruby         Ruby extension fails to build with Ruby 1.9.1 (Rajesh Malepati)
THRIFT-699   Ruby         Excise unused "native protocol method table" stuff from thrift_native (Bryan Duxbury)
THRIFT-767   Ruby         ruby compiler does not keep comments for enum values (Bryan Duxbury)
THRIFT-811   Ruby         http_client_transport.rb: allow custom http headers (Tony Kamenick)
THRIFT-459   Ruby         Ruby installation always tries to write to /Library/Ruby/site (Matthieu Imbert)


Thrift 0.1.0 - Incubating (not released)
--------------------------------------------------------------------------------
Compatibility Breaking Changes:
  C++:
- It's quite possible that regenerating code and rebuilding will be
      required.  Make sure your headers match your libs!

  Java:

  Python:

  Ruby:
- Generated files now have underscored names [THRIFT-421]
- The library has been rearranged to be more Ruby-like [THRIFT-276]

  Erlang:
- Generated code will have to be regenerated, and the new code will
      have to be deployed atomically with the new library code [THRIFT-136]

New Features and Bug Fixes:
  C++:
- Support for TCompactProtocol [THRIFT-333]

  Java:
- Support for TCompactProtocol [THRIFT-110]

  Python:
- Support for Twisted [THRIFT-148]

  Ruby:
- Support for TCompactProtocol [THRIFT-332]


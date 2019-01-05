# Apache Thrift netstd

Thrift client library for Microsoft .NET Standard 

# Build the library

## Folder contents
- Tests/Thrift.PublicInterfaces.Compile.Tests - project for checking public interfaces during adding changes to Thrift library
- Thrift - Thrift library 

## Reused components 
- .NET Standard 1.6 (SDK 2.0.0)

## How to build on Windows
- Get Thrift IDL compiler executable, add to some folder and add path to this folder into PATH variable
- Open the Thrift.sln project with Visual Studio and build.
or 
- Build with scripts

## How to build on Unix/Linux
- Ensure you have .NET Core 2.0.0 SDK installed or use the Ubuntu Xenial docker image
- Follow common build practice for Thrift: bootstrap, configure, and make

## Known issues
- In trace logging mode you can see some not important internal exceptions

# Migration to netstd

## ... from netcore

If you are migrating your code from netcore library, you will have to:

- Switch to `thrift -gen netstd`, but keep all flags as they are
- add `using Thrift.Processors;` in the server code where appropriate
- rename all `T*ClientTransport` to `T*Transport` 
- rename all `TBaseServer` occurrences in your code to `TServer`
- the `SingletonTProcessorFactory` is now called `TSingletonProcessorFactory`
- and the `AsyncBaseServer` is now the `TSimpleAsyncServer`

You may wonder why we changed so many names. The naming scheme has been revised for two reasons: First, we want to get back the established, well-known naming consistency across the Thrift libraries which the netcore library did not fully respect. Second, by achieving that first objective, we get the additional benefit of making migration at least a bit easier for C# projects.

## ... from csharp

Because of the different environment requirements, migration from C# takes slightly more efforts. While the code changes related to Thrift itself are moderate, you may need to upgrade certain dependencies, components or even modules to more recent versions. 

1. Client and server applications must use at least framework 4.6.1, any version below will not work. 
1. Switch to `thrift -gen netstd` and keep all flags, except remove the `async` flag (if used)
1. [Familiarize yourself with the `async/await` model](https://msdn.microsoft.com/en-us/magazine/jj991977.aspx), if you have not already done so. As netstd does not support `ISync` anymore, async is mandatory. The synchronous model is simply no longer available (that's also the reason why we don't need the `async` flag anymore). 
1. Consider proper use of `cancellationToken` parameters. They are optional but may be quite helpful.
1. As you probably already guessed, there are a few names that have been changed:
- add `using Thrift.Processors;` in the server code where appropriate
- the `Thrift.Transports` and `Thrift.Protocols` namespaces now use the plural form
- the `TServerSocket` is now called `TServerSocketTransport`
- change `IProtocolFactory` into `ITProtocolFactory`
- if you are looking for `TSimpleServer`, try `TSimpleAsyncServer` instead
- similarly, the `TThreadPoolServer` is now a `TThreadPoolAsyncServer` 
- the server's `Serve()` method does now `ServeAsync()`
- In case you are using Thrift server event handlers: the `SetEventHandler` method now starts with an uppercase letter
- and you will also have to revise the method names of all `TServerEventHandler` descendants you have in your code










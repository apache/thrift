<!--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements. See the NOTICE file
distributed with this work for additional information
regarding copyright ownership. The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied. See the License for the
specific language governing permissions and limitations
under the License.
-->

# Apache Thrift Compiler

The [Apache Thrift](https://thrift.apache.org/) IDL compiler, packaged as a .NET
tool.

Apache Thrift is a software framework for scalable cross-language services
development. It combines a software stack with a code generation engine to build
services that work efficiently between C++, Java, Python, PHP, Ruby, Erlang,
Perl, Haskell, C#, Cocoa, JavaScript, Node.js, Smalltalk, OCaml, Delphi, Go,
Rust, Swift and other languages.

This package contains the code generator, `thrift`, which turns a `.thrift`
interface definition into client and server code. It does not contain a runtime
library; those are published separately, for .NET as
[`ApacheThrift`](https://www.nuget.org/packages/ApacheThrift/).

## Install

```shell
dotnet tool install --global Apache.Thrift.Compiler
thrift --version
```

Or pin it for a repository, so that everyone building it uses the same compiler
version:

```shell
dotnet new tool-manifest      # once per repository
dotnet tool install Apache.Thrift.Compiler
dotnet tool run thrift --version
```

## Use

```shell
thrift --gen netstd -out generated tutorial.thrift
```

See the [tutorial](https://thrift.apache.org/tutorial/) and the
[Thrift IDL reference](https://thrift.apache.org/docs/idl).

## Requirements

The compiler links the shared C++ runtime, so it needs the **Microsoft Visual
C++ Redistributable (x64)**, which most Windows machines with development tools
already have. If it is missing, `thrift` reports that and points at
<https://aka.ms/vc14/vc_redist.x64.exe>.

## Platforms

This package carries the **Windows** build of the compiler. On other platforms it
installs, because NuGet does not gate on the operating system, but running it
reports that and stops.

For Linux and macOS, see [Download](https://thrift.apache.org/download) - most
distributions package Thrift, and Homebrew has it as `thrift`.

## Provenance

The executable in this package is the Windows compiler that the Apache Thrift
PMC voted on with the source release of the same version, the
`thrift-<version>.exe` on the [Download](https://thrift.apache.org/download)
page. Its checksums and its signature are checked against the project's `KEYS`
before it is packed. The source release is the official artifact, and this
package is a convenience package of it.

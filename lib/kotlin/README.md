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

# Test Only Library for Kotlin

This directory (`/lib/kotlin`) contains test only library code for Kotlin code gen. Because Kotlin code gen produces code that works on top of libthrift (i.e. Java library), the purpose of this library is to encode the cross test server and client to make sure it conforms to the thrift specifications.

The output artifact in this library is _not_ published to Maven central. Unlike the Java library where:

1. source code is put under `main` sources set i.e. `src/main`,
2. unit test code is put under `test` sources set i.e. `src/test`,
3. cross test code is put under `crossTest` sources set, i.e. `src/crossTest` directory which, unlike the default `main` and `test`, is created and configured on demand;

this kotlin library uses a multi-module project setup for separation of concern:

1. root module for configuring unit tests,
2. `cross-test-client` module for bundling a standalone test client,
3. `cross-test-server` module for bundling a standalone test server

## How to compile

This library is managed using Gradle 6.9.2, run the following command (requires C++ thrift compiler):

```bash
gradle build
```

```bash
gradle installDist
```

## How to run cross test server / client

```bash
gradle :cross-test-server:run
```

```bash
gradle :cross-test-client:run
```

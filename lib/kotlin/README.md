# Test Only Library for Kotlin

This directory (`/lib/kotlin`) contains test only library code for Kotlin code gen. Because Kotlin code gen produces code that works on top of libthrift (i.e. Java library), the purpose of this library is to encode the cross test server and client to make sure it conforms to the thrift specifications.

The output artifact in this library is *not* published to Maven central.

## How to compile

This library is managed using Gradle 7+, so the easiest way is to use gradle wrapper (`./gradlew`).

Run the following command (requires C++ compiler):

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

Thrift Go Software Library

License
=======

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


Using Thrift with Go
====================

Thrift supports Go 1.7+

In following Go conventions, we recommend you use the 'go' tool to install
Thrift for go.

    $ go get github.com/apache/thrift/lib/go/thrift/...

Will retrieve and install the most recent version of the package.


A note about optional fields
============================

The thrift-to-Go compiler tries to represent thrift IDL structs as Go structs.
We must be able to distinguish between optional fields that are set to their
default value and optional values which are actually unset, so the generated
code represents optional fields via pointers.

This is generally intuitive and works well much of the time, but Go does not
have a syntax for creating a pointer to a constant in a single expression. That
is, given a struct like

    struct SomeIDLType {
    	OptionalField *int32
    }

, the following will not compile:

    x := &SomeIDLType{
    	OptionalField: &(3),
    }

(Nor is there any other syntax that's built in to the language)

As such, we provide some helpers that do just this under lib/go/thrift/. E.g.,

    x := &SomeIDLType{
    	OptionalField: thrift.Int32Ptr(3),
    }

And so on. The code generator also creates analogous helpers for user-defined
typedefs and enums.

Adding custom tags to generated Thrift structs
==============================================

You can add tags to the auto-generated thrift structs using the following format:

    struct foo {
      1: required string Bar (go.tag = "some_tag:\"some_tag_value\"")
    }
    
which will generate:

    type Foo struct {
      Bar string `thrift:"bar,1,required" some_tag:"some_tag_value"`
    }

A note about server handler implementations
===========================================

The context object passed into the server handler function will be canceled when
the client closes the connection (this is a best effort check, not a guarantee
-- there's no guarantee that the context object is always canceled when client
closes the connection, but when it's canceled you can always assume the client
closed the connection). When implementing Go Thrift server, you can take
advantage of that to abandon requests that's no longer needed:

    func MyEndpoint(ctx context.Context, req *thriftRequestType) (*thriftResponseType, error) {
        ...
        if ctx.Err() == context.Canceled {
            return nil, thrift.ErrAbandonRequest
        }
        ...
    }

This feature would add roughly 1 millisecond of latency overhead to the server
handlers (along with roughly 2 goroutines per request).
If that is unacceptable, it can be disabled by having this line early in your
main function:

    thrift.ServerConnectivityCheckInterval = 0

Please be advised that due to a
[Go runtime bug](https://github.com/golang/go/issues/27707), currently
if this interval is set to a value too low (for example, 1ms), it might cause
excessive cpu overhead.

This feature is also only enabled on non-oneway endpoints.

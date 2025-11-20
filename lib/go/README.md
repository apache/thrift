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


Suppored Go releases
====================

Following the
[official Go release policy](https://golang.org/doc/devel/release#policy),
we support the latest two Go releases at the time of the Thrift release.

For example, at the time of Thrift v0.14.0 release,
the latest two Go releases are go1.15 and go1.14,
and those are the two Go releases supported by Thrift v0.14.*
(including v0.14.1 and v0.14.2 patch releases).

Because of Go's backward compatibility guarantee,
older Thrift libraries usually works with newer Go releases
(e.g. Thrift v0.14.* works with go1.16, although it's not officially supported),
but newer Thrift releases might use new APIs introduced in Go releases and no
longer work with older Go releases.
For example, Thrift v0.14.0 used APIs introduced in go1.13,
and as a result no longer works on go1.12.


Using Thrift with Go
====================

Thrift supports the currently officially supported Go releases (the latest 2).

After initializing the go modules file in your project, use the following
command to add the most recent version of the package:

    $ go get github.com/apache/thrift


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
closed the connection). The cause of the cancellation (via `context.Cause(ctx)`)
would also be set to `thrift.ErrAbandonRequest`.

When implementing Go Thrift server, you can take advantage of that to abandon
requests that's no longer needed by returning `thrift.ErrAbandonRequest`:

    func MyEndpoint(ctx context.Context, req *thriftRequestType) (*thriftResponseType, error) {
        ...
        if ctx.Err() == context.Canceled {
            return nil, thrift.ErrAbandonRequest
            // Or just return ctx.Err(), compiler generated processor code will
            // handle it for you automatically:
            // return nil, ctx.Err()
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

A note about server stop implementations
========================================

[TSimpleServer.Stop](https://pkg.go.dev/github.com/apache/thrift/lib/go/thrift#TSimpleServer.Stop) will wait for all client connections to be closed after 
the last received request to be handled, as the time spent by Stop
 may sometimes be too long:
* When socket timeout is not set, server might be hanged before all active
  clients to finish handling the last received request.
* When the socket timeout is too long (e.g one hour), server will
  hang for that duration before all active clients to finish handling the
  last received request.

To prevent Stop from hanging for too long, you can set 
thrift.ServerStopTimeout in your main or init function:

    thrift.ServerStopTimeout = <max_duration_to_stop>

If it's set to <=0, the feature will be disabled (by default), and server 
will wait for all the client connections to be closed gracefully with 
zero err time. Otherwise, the stop will wait for all the client 
connections to be closed gracefully util thrift.ServerStopTimeout is 
reached, and client connections that are not closed after thrift.ServerStopTimeout 
will be closed abruptly which may cause some client errors.

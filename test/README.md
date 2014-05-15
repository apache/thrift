# Apache Thrift - integration test suite

This is the cross everything integration test suite for Apache Thrift.
executed by

    make cross

at the moment, this starts the test.sh script which does the real cross test
with different transports, protocols and languages.

Unit tests for languages ar usually located under lib/<lang>/test/
cross language tests acording to [ThriftTest.thrift](ThriftTest.thrift) shall be
provided for every language including executables with the following command
line interface for servers:

    $ ./cpp/TestServer -h
    Allowed options:
      -h [ --help ]               produce help message
      --port arg (=9090)          Port number to listen
      --domain-socket arg         Unix Domain Socket (e.g. /tmp/ThriftTest.thrift)
      --server-type arg (=simple) type of server, "simple", "thread-pool", 
                                  "threaded", or "nonblocking"
      --transport arg (=buffered) transport: buffered, framed, http
      --protocol arg (=binary)    protocol: binary, compact, json
      --ssl                       Encrypted Transport using SSL
      --processor-events          processor-events
      -n [ --workers ] arg (=4)   Number of thread pools workers. Only valid for 
                              thread-pool server type

and this for clients:

    $ ./cpp/TestClient -h
    Allowed options:
      -h [ --help ]               produce help message
      --host arg (=localhost)     Host to connect
      --port arg (=9090)          Port number to connect
      --domain-socket arg         Domain Socket (e.g. /tmp/ThriftTest.thrift), 
                                  instead of host and port
      --transport arg (=buffered) Transport: buffered, framed, http, evhttp
      --protocol arg (=binary)    Protocol: binary, compact, json
      --ssl                       Encrypted Transport using SSL
      -n [ --testloops ] arg (=1) Number of Tests

If you have executed the **make check** or **make cross** then you will be able to browse
[gen-html/ThriftTest.html](gen-html/ThriftTest.html) with the test documentation.

return code shall be 0 on success or an integer in the range 1 - 255 on error

## SSL
Test Keys and Certificates are provided in multiple formats under the following
directory [test/keys](keys)


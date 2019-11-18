Thrift Python Software Library

Thrift is a lightweight, language-independent software stack for
point-to-point RPC implementation.
Thrift provides clean abstractions and implementations for data transport,
data serialization, and application level processing. The code generation
system takes a simple definition language as input and generates code
across programming languages that uses the abstracted stack to build
interoperable RPC clients and servers.

Thrift is provided as a set of Python packages. The top level package is
thrift, and there are subpackages for the protocol, transport, and server
code. Each package contains modules using standard Thrift naming conventions
(i.e. TProtocol, TTransport) and implementations in corresponding modules
(i.e. TSocket).  There is also a subpackage reflection, which contains
the generated code for the reflection structures.

The Python libraries can be installed manually using the provided setup.py
file, or automatically using the install hook provided via autoconf/automake.
To use the latter, become superuser and do make install.

For more information:

https://thrift.apache.org/
https://www.github.com/apache/thrift/

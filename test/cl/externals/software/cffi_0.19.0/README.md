[![Build Status](https://travis-ci.org/cffi/cffi.svg?branch=master)](https://travis-ci.org/cffi/cffi)

CFFI, the Common Foreign Function Interface, purports to be a portable
foreign function interface for Common Lisp. The CFFI library is
composed of a Lisp-implementation-specific backend in the CFFI-SYS
package, and a portable frontend in the CFFI package.

The CFFI-SYS backend package defines a low-level interface to the
native FFI support in the Lisp implementation. It offers operators for
allocating and dereferencing foreign memory, calling foreign
functions, and loading shared libraries. The CFFI frontend provides a
declarative interface for defining foreign functions, structures,
typedefs, enumerated types, etc. It is implemented in portable ANSI CL
making use of the low-level operators exported by CFFI-SYS.

Please consult [the manual][1] for further details, including
installation instructions.

[1]: http://common-lisp.net/project/cffi/manual/html_node/

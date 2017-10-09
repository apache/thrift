# Introduction

This is the usocket Common Lisp sockets library: a library to bring
sockets access to the broadest of common lisp implementations as possible.

# The library currently supports:

 - SBCL
 - CMUCL
 - ArmedBear Common Lisp
 - GNU CLISP
 - Allegro Common Lisp
 - LispWorks
 - Clozure CL
 - ECL
 - Scieneer Common Lisp
 - Macintosh Common Lisp
 - MOCL

If your favorite common lisp misses in the list above, please contact
usocket-devel@common-lisp.net and submit a request.  Please include
references to available sockets functions in your lisp implementation.

The library has been ASDF (http://cliki.net/ASDF) enabled, meaning
that you can tar up a checkout and use that to ASDF-INSTALL:INSTALL
the package in your system package site.  (Or use your usual ASDF
tricks to use the checkout directly.)

# Remarks on licensing

Even though the source code has an MIT style license attached to it,
when compiling this code with some of the supported lisp implementations
you may not end up with an MIT style binary version due to the licensing
of the implementations themselves.  ECL is such an example and - when
it will become supported - GCL is like that too.

# Non-support of :external-format

Because of its definition in the hyperspec, there's no common
external-format between lisp implementations: every vendor has chosen
a different way to solve the problem of newline translation or
character set recoding.

Because there's no way to avoid platform specific code in the application
when using external-format, the purpose of a portability layer gets
defeated.  So, for now, usocket doesn't support external-format.

The workaround to get reasonably portable external-format support is to
layer a flexi-stream (from flexi-streams) on top of a usocket stream.

# API definition

 - usocket (class)
 - stream-usocket (class; usocket derivative)
 - stream-server-usocket (class; usocket derivative)
 - socket-connect (function) [ to create an active/connected socket ]
    socket-connect host port &key element-type
      where `host' is a vectorized ip
                      or a string representation of a dotted ip address
                      or a hostname for lookup in the DNS system
 - socket-listen (function) [ to create a passive/listening socket ]
     socket-listen host port &key reuseaddress backlog element-type
       where `host' has the same definition as above
 - socket-accept (method) [ to create an active/connected socket ]
     socket-accept socket &key element-type
       returns (server side) a connected socket derived from a
       listening/passive socket.
 - socket-close (method)
    socket-close socket
      where socket a previously returned socket
 - socket (usocket slot accessor),
      the internal/implementation defined socket representation
 - socket-stream (usocket slot accessor),
    socket-stream socket
      the return value of which satisfies the normal stream interface
 - socket-shutdown

## Errors:
 - address-in-use-error
 - address-not-available-error
 - bad-file-descriptor-error
 - connection-refused-error
 - connection-aborted-error
 - connection-reset-error
 - invalid-argument-error
 - no-buffers-error
 - operation-not-supported-error
 - operation-not-permitted-error
 - protocol-not-supported-error
 - socket-type-not-supported-error
 - network-unreachable-error
 - network-down-error
 - network-reset-error
 - host-down-error
 - host-unreachable-error
 - shutdown-error
 - timeout-error
 - unkown-error

## Non-fatal conditions:
 - interrupted-condition
 - unkown-condition

(for a description of the API methods and functions see
  https://common-lisp.net/project/usocket/api-docs.shtml)

# Test suite

The test suite unfortunately isn't mature enough yet to run without
some manual configuration.  Several elements are required which are
hard to programatically detect.  Please adjust the test file before
running the tests, for these variables:

- +non-existing-host+: The stringified IP address of a host on the
     same subnet.  No physical host may be present.
- +unused-local-port+: A port number of a port not in use on the
     machine the tests run on.
- +common-lisp-net+: A vector with 4 integer elements which make up
     an IP address. This must be the IP "common-lisp.net" resolves to.

# Known problems

- CMUCL error reporting wrt sockets raises only simple-errors
  meaning there's no way to tell different error conditions apart.
  All errors are mapped to unknown-error on CMUCL.

- The ArmedBear backend doesn't do any error mapping (yet). Java
  defines exceptions at the wrong level (IMO), since the exception
  reported bares a relation to the function failing, not the actual
  error that occurred: for example 'Address already in use' (when
  creating a passive socket) is reported as a BindException with
  an error text of 'Address already in use'. There's no way to sanely
  map 'BindException' to a meaningfull error in usocket. [This does not
  mean the backend should not at least map to 'unknown-error'!]

- When using the library with ECL, you need the C compiler installed
  to be able to compile and load the Foreign Function Interface.
  Not all ECL targets support DFFI yet, so on some targets this would
  be the case anyway.  By depending on this technique, usocket can
  reuse the FFI code on all platforms (including Windows).  This benefit
  currently outweighs the additional requirement. (hey, it's *Embeddable*
  Common Lisp, so, you probably wanted to embed it all along, right?)

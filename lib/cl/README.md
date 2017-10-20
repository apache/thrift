Thrift Common Lisp Library

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



Using Thrift with Common Lisp
============================

 Thrift is a protocol and library for language-independent communication between cooperating
 processes. The communication takes the form of request and response messages, of which the forms
 are specified in advance throufh a shared interface definition. A Thrift definition file is translated
 into Lisp source files, which comprise several definitions:

  * Three packages, one for the namespace of the implementation operators, and one each for request and
  response operators.
  * Various type definitions as implementations for Thrift typedef and enum definitions.
  * DEF-STRUCT and DEF-EXCEPTION forms for Thrift struct and exception definitions.
  * DEF-SERVICE forms for thrift service definitions.

 Each service definition expands in a collection of generic function definitions. For each `op`
 in the service definition, two functions are defined

  * `op`-request is defined for use by a client. It accepts an additional initial `protocol` argument,
    to act as the client proxy for the operation and mediate the interaction with a remote process
    through a Thrift-encoded transport stream.
  * `op`-response is defined for use by a server. It accepts a single `protocol` argument. A server
    uses it to decode the request message, invoke the base `op` function with the message arguments,
    encode and send the the result as a response, and handles exceptions.

 The client interface is one operator

  * `with-client (variable location) . body` : creates a connection in a dynamic context and closes it
    upon exit. The variable is bound to a client proxy stream/protocol instance, which wraps the
    base i/o stream - socket, file, etc, with an operators which implement the Thrift protocol
    and transport mechanisms.

 The server interface combines server and service objects

  * `serve (location service)` : accepts connections on the designated port and responds to
    requests of the service's operations.


Building 
--------

The Thrift Common Lisp library is packaged as the ASDF[[1]] system `thrift`.
It depends on the systems

* puri-ppcre[[2]] : for the thrift uri class
* closer-mop[[3]] : for class metadata
* trivial-utf-8[[4]] : for string codecs

In order to build it, register those systems with ASDF and evaluate

    (asdf:load-system :thrift)

This will compile and load the Lisp compiler for Thrift definition files, the
transport and protocol implementations, and the client and server interface
functions. In order to use Thrift in an application, one must also author and/or
load the interface definitions for the remote service.[[5]] If one is implementing a service,
one must also define the actual functions to which Thrift is to act as the proxy
interface. The remainder of this document follows the Thrift tutorial to illustrate how
to perform the steps

  * implement the service
  * translate the Thrift IDL
  * load the Lisp service interfaces
  * run a server for the service
  * use a client to access the service remotely

Note that, if one is to implement a new service, one will also need to author the
IDL files, as there is no facility to generate them from a service implementation.


Implement the Service
---------------------

The tutorial comprises serveral functions: `add`, `ping`, `zip`, and `calculate`.
Each translatd IDL corresponds to three packages. In this case, the packages

  * :tutorial
  * :tutorial-implementation
  * :tutorial-response

The first package is for the service implementation.

    ;; define the base operations
    
    (in-package :tutorial-implementation)
    
    (defun add ( num1 num2)
      (format t "~&Asked to add ~A and ~A." num1 num2)
      (+ num1 num2))
    
    (defun ping ()
      (print :ping))
    
    (defun zip ()
      (print :zip))
    
    (defun calculate (logid task)
      (calculate-op (work-op task) (work-num1 task) (work-num2 task)))
    
    (defgeneric calculate-op (op arg1 arg2)
      (:method :around (op arg1 arg2)
        (let ((result (call-next-method)))
          (format t "~&Asked to calculate: ~d on  ~A and ~A = ~d." op arg1 arg2 result)
          result))
    
      (:method ((op (eql operation.add)) arg1 arg2)
        (+ arg1 arg2))
      (:method ((op (eql operation.subtract)) arg1 arg2)
        (- arg1 arg2))
      (:method ((op (eql operation.multiply)) arg1 arg2)
        (* arg1 arg2))
      (:method ((op (eql operation.divide)) arg1 arg2)
        (/ arg1 arg2)))
    
    (defun zip () (print 'zip))


Translate the Thrift IDL
------------------------

IDL files employ the file type `thrift`. In this case, there are two files to translate
  * `tutorial.thrift`
  * `shared.thrift`
As the former includes the latter, one uses it to generate the interfaces:

    $THRIFT/bin/thrift -O ./ --gen cl $THRIFT/tutorial/tutorial.thrift

For the moment, the Lisp backend is present here as #P"THRIFT:compiler;cpp;src;generate;t_cl_generator.cc".
In order to use it, copy that file into the analogous location in the Thrift release tree prior to
making thrift.


Load the Lisp translated service interfaces
-------------------------------------------

The translator generates two files for each IDL file. For example `tutorial-types.lisp` and
`tutorial-vars.lisp`. As the parameter definitions may istantiate objects defined in the `-types`
file, the ASDF dependencies must reflect this constraint. For the tutorial, the system could be
defined as

    (asdf:defsystem :thrift-tutorial
       :depends-on (:thrift)
       :serial t
       :components ((:file "tutorial")
                    (:file "tutorial-types")
                    (:file "tutorial-vars")))


Run a Server for the Service
----------------------------

The actual service name, as specified in the `def-service` form in `tutorial.lisp`, is `calculator`. 
Each service definition defines a global variable with the service name and binds it to a
service instance whch describes the operations.

In order to start a service, specify a location and the service instance. 

    (in-package :tutorial)
    (serve #u"thrift://127.0.0.1:9091" calculator)


Use a Client to Access the Service Remotely
-------------------------------------------


[in some other process] run the client

    (in-package :cl-user)
    (use-package :tutorial-request)

    (macrolet ((show (form)
                 `(format *trace-output* "~%~s =>~{ ~s~}"
                          ',form
                          (multiple-value-list (ignore-errors ,form)))))
      (with-client (protocol #u"thrift://127.0.0.1:9091")
        (show (ping protocol))
        (show (add protocol 1 2))
        (show (add protocol 1 4))
    
        (show (shared:get-struct protocol 1))
    
        (let ((task (make-instance 'work
                      :op operation.subtract :num1 15 :num2 10)))
          (show (calculate protocol task))
        
          (setf (work-op task) operation.divide
                (work-num1 task) 1
                (work-num2 task) 0)
          (show (calculate protocol task)))
    
        (show (zip protocol))))
    

Status
======

The initial library version serves as an interface to Cassandra[[6]] in order to provide access to
Datagraph's Cassandra-based RDF store[[7]]. The code evolved from an initial version which had been
submitted to Thift in 2008[[8]].

A demonstration of access through the Cassandra API is among the READMES[[9]].

Issues
------

### optional fields 
 Where the IDL declares a field options, the def-struct form includes no
 initform for the slot and the encoding operator skips an unbound slot. This leave some ambiguity
 with bool fields.

### namespace - package equivalence
 The IDL specifies a single namespace. The Lisp binding uses
 three: the implementation, the request interface, and the response interface.
 The current pattern is:

  * _namespace_ : request proxy function, structure types and accessors, exception types,
    enum types, constants; use `:thrift`
  * _namespace_`-implementation` : implementation function, use `:thrift`, use _namespace_, but
    shadow all implementation function names.
  * _namespace_`-response` : response functions

### instantiation protocol :
 struct classes are standard classes and exception classes are
 whatever the implementation prescribes. decoders apply make-struct to an initargs list.
 particularly at the service end, there are advantages to resourcing structs and decoding
 with direct side-effects on slot-values

### maps:

 Maps are now represented as hash tables. As data through the call/reply interface is all statically
 typed, it is not necessary for the objects to themselves indicate the coding form. Association lists
 would be sufficient. As the key type is arbitrary, property lists offer no additional convenience:
 as `getf` operates with `eq` a new access interface would be necessary and they would not be
 available for function application.


 [1]: www.common-lisp.net/asdf
 [2]: http://github.com/lisp/com.b9.puri.ppcre
 [3]: www.common-lisp.net/closer-mop
 [4]: trivial-utf-8
 [5]: http://wiki.apache.org/thrift/ThriftGeneration
 [6]: http://wiki.apache.org/cassandra/FrontPage
 [7]: http://github.com/bendiken/rdf-cassandra
 [8]: http://markmail.org/thread/4tfa3zbweyg2qwne: thrift jira lisp issue
 [9]: ./READMES/readme-cassandra.lisp

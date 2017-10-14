(in-package #:common-lisp-user)

;;; This file defines the packages for the `org.apache.thrift` library.
;;;
;;; copyright 2010 [james anderson](james.anderson@setf.de)
;;;
;;; Licensed to the Apache Software Foundation (ASF) under one
;;; or more contributor license agreements. See the NOTICE file
;;; distributed with this work for additional information
;;; regarding copyright ownership. The ASF licenses this file
;;; to you under the Apache License, Version 2.0 (the
;;; "License"); you may not use this file except in compliance
;;; with the License. You may obtain a copy of the License at
;;; 
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing,
;;; software distributed under the License is distributed on an
;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;; KIND, either express or implied. See the License for the
;;; specific language governing permissions and limitations
;;; under the License.

(defpackage #:org.apache.thrift
  (:nicknames #:thrift)
  (:use)
  
  (:documentation "This is the home package for the symbols in the library's interface.
 It uses no packages, but imports 'string' from :cl. It does export some symbols
 particular to Thrift types and/or operators which conflict with standard Common Lisp symbols.
 These must be selectively shadowed as per application requirements in a using package.")

  (:import-from #:common-lisp
                #:string)
  #+ccl
  (:import-from #:ccl
                #:stream-direction)
  ;; digitools stream-write-string signature requires four arguments. leave it to be shadowed
  #+sbcl
  (:import-from #:sb-gray
                #:stream-write-string)
  #+lispworks
  (:import-from #:stream
                #:stream-write-string)
  (:export 
   #:*binary-transport-element-type*
   #:application-error
   #:binary-protocol
   #:binary-transport
   #:binary
   #:bool
   #:byte
   #:call
   #:class-condition-class
   #:class-field-definitions
   #:class-identifier
   #:class-not-found
   #:class-not-found-error
   #:client #:with-client
   #:def-constant
   #:def-enum
   #:def-exception
   #:def-package
   #:def-service
   #:def-struct
   #:direct-field-definition
   #:double
   #:effective-field-definition
   #:element-type-error
   #:enum
   #:enum-type-error
   #:exception
   #:field-definition-identifier
   #:field-definition-identifier-number
   #:field-definition-initarg
   #:field-definition-name
   #:field-definition-optional
   #:field-definition-reader
   #:field-definition-type
   #:field-size-error
   #:field-type-error
   #:float
   #:method-definition
   #:i8
   #:i16
   #:i32
   #:i64
   #:invalid-element-type
   #:invalid-enum
   #:invalid-field-size
   #:invalid-field-type
   #:invalid-protocol-version
   #:invalid-struct-type
   #:list
   #:map
   #:map-get
   #:protocol
   #:protocol-error
   #:protocol-field-id-mode
   #:protocol-input-transport
   #:protocol-output-transport
   #:protocol-version-error
   #:reply
   #:response-exception
   #:serve
   #:serve #:simple-server #:handler
   #:service
   #:service-base-services
   #:service-identifier
   #:service-package
   #:set
   #:shared-service
   #:stream-direction
   #:stream-read-binary
   #:stream-read-bool
   #:stream-read-double
   #:stream-read-field
   #:stream-read-field-begin
   #:stream-read-field-end
   #:stream-read-float
   #:stream-read-i8
   #:stream-read-i16
   #:stream-read-i32
   #:stream-read-i64
   #:stream-read-list
   #:stream-read-list-begin
   #:stream-read-list-end
   #:stream-read-map
   #:stream-read-map-begin
   #:stream-read-map-end
   #:stream-read-message
   #:stream-read-message-begin
   #:stream-read-message-end
   #:stream-read-message-type
   #:stream-read-set
   #:stream-read-set-begin
   #:stream-read-set-end
   #:stream-read-string
   #:stream-read-struct
   #:stream-read-struct-begin
   #:stream-read-struct-end
   #:stream-read-type
   #:stream-read-type-value
   #:stream-write-binary
   #:stream-write-bool
   #:stream-write-double
   #:stream-write-field
   #:stream-write-float
   #:stream-write-i8
   #:stream-write-i16
   #:stream-write-i32
   #:stream-write-i64
   #:stream-write-list
   #:stream-write-map
   #:stream-write-message
   #:stream-write-message-type
   #:stream-write-set
   #:stream-write-string
   #:stream-write-struct
   #:stream-write-type
   #:stream-write-type-value
   #:string
   #:struct
   #:struct-name
   #:struct-type-error
   #:thrift
   #:thrift-class
   #:thrift-error
   #:thrift-object
   #:thrift-struct-class
   #:thrift-exception-class
   #:transport
   #:transport-error
   #:type-of
   #:unknown-field
   #:unknown-field-error
   #:unknown-method
   #:unknown-method-error
   #:vector-input-stream
   #:vector-output-stream
   #:vector-stream
   #:vector-stream-transport
   #:vector-stream-vector
   #:void
   ))


(defpackage #:org.apache.thrift.implementation
  (:use #:common-lisp #:org.apache.thrift)
  (:nicknames #:thrift.implementation)
  
  (:documentation "The is the package for the thrift implementation. It exports nothing, uses the
 :common-lisp and :thrift package for access to the respective interfaces. Those names which conflict, eg.
 cl:list v/s thrift:list, are imported the :common-lisp package and referenced with an explicit prefix
 from the :thrift package.
  It also imports names as required per run-time for access to standard floating point constants and gray
 stream operators.")

  (:shadowing-import-from #:common-lisp #:byte #:set #:list #:map #:type-of #:float)

  (:import-from #:trivial-gray-streams
		#:stream-write-sequence #:stream-read-sequence
		#:stream-write-byte #:stream-read-byte
		#:stream-force-output #:stream-finish-output)
  
  ;; (:import-from #:de.setf.utility
  ;;               #:stream-reader
  ;;               #:stream-writer
  ;;               )
  #+ccl
  (:import-from #:ccl
                #:stream-write-byte #:stream-read-byte
                #:stream-direction
                #:stream-position
                #:stream-force-output #:stream-finish-output)
  #+mcl
  (:import-from #:ccl
                #:stream-close
                #:stream-read-sequence #:stream-write-sequence
                #:stream-tyi #:stream-tyo #:stream-untyi)
  #+clozure
  (:import-from #:ccl
                #:double-float-positive-infinity
                #:double-float-negative-infinity
                #+ccl-1.4 #:double-float-nan)
  #+sbcl
  (:import-from #:sb-ext
                #:double-float-positive-infinity
                #:double-float-negative-infinity
                #:single-float-positive-infinity
                #:single-float-negative-infinity)  
  )

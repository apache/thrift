(in-package #:common-lisp-user)


;;; This files defines the ASDF system for the `org.apache.thrift` library.
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


(asdf:defsystem #:thrift
  :depends-on (#:puri
               #:usocket
               #:closer-mop 
               #:trivial-utf-8
	       #:ieee-floats
	       #:trivial-gray-streams
	       #:alexandria)
  :description "org.apache.thrift implements a Common Lisp binding for the Apache Thrift cross-language
 services protocol."
  :serial t
  :components ((:file "package")
               (:file "symbols")
               (:file "types")
               (:file "parameters")
               (:file "classes")
               (:file "definition-operators")
               (:file "transport")
               (:file "framed-transport")
               (:file "conditions")
               (:file "protocol")
               (:file "binary-protocol")
               (:file "vector-protocol")
               (:file "client")
               (:file "server"))

  :long-description
  "This library uses the  Thrift[[1]],[[2]] protocol to implement Common Lisp support for cross-language
 access to remote services. See README.md for more information.

 [1]: http://incubator.apache.org/thrift/static/thrift-20070401.pdf
 [2]: http://wiki.apache.org/thrift/
 ")


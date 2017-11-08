;;;; Copyright 2010 James Anderson <james.anderson@setf.de>
;;;;
;;;; Licensed under the Apache License, Version 2.0 (the "License");
;;;; you may not use this file except in compliance with the License.
;;;; You may obtain a copy of the License at
;;;;
;;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing, software
;;;; distributed under the License is distributed on an "AS IS" BASIS,
;;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;; See the License for the specific language governing permissions and
;;;; limitations under the License.

(in-package #:common-lisp-user)

(asdf:defsystem #:thrift-test
  :depends-on (#:thrift
               #:bordeaux-threads
               #:cl-ppcre
               #:fiasco)
  :perform (asdf:test-op (o s) (uiop:symbol-call :fiasco :run-tests :thrift-test))
  :description "tests for com.apache.thrift"
  :serial t
  :components ((:file "package")
               (:file "test")
               (:file "utils")
               (:file "setup")
               (:file "vector-protocol")
               (:file "conditions")
               (:file "definition-operators")
               (:file "protocol")
               #+(or)
               (:module :gen-cl
                :serial t
                :components ((:file "AnnotationTest-types")
                             (:file "AnnotationTest-vars")
                             (:file "AnnotationTest")
                             ; (:file "ConstantsDemo")
                             ; (:file "DebugProtoTest")
                             ; (:file "DenseLinkingTest")
                             ; (:file "DocTest")
                             ;(:file "JavaBeansTest")
                             (:file "ManyTypedefs-types")
                             (:file "ManyTypedefs-vars")
                             (:file "ManyTypedefs")
                             ;(:file "OptionalRequiredTest")
                             (:file "SmallTest-types")
                             (:file "SmallTest-vars")
                             (:file "SmallTest")
                             (:file "StressTest-types.lisp")
                             (:file "StressTest-vars.lisp")
                             (:file "StressTest.lisp")
                             (:file "ThriftTest-types")
                             (:file "ThriftTest-vars") empty
                             (:file "ThriftTest")
                             ))))

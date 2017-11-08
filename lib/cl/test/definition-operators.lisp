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

;;;; Tests for definition operators.

(fiasco:define-test-package (#:definition-operator-tests :in thrift-test:thrift-self-tests)
  (:use #:thrift-test-utils))

(in-package #:definition-operator-tests)

(deftest define-package ()
  (finishes (thrift:def-package :test-package))
  (is (find-package :test-package))
  (is (find-package :test-package-implementation))
  (is (find-package :test-package-response))
  (delete-package :test-package)
  (delete-package :test-package-implementation)
  (delete-package :test-package-response))

(deftest redefine-package ()
  (thrift:def-package :test-package)
  (finishes (thrift:def-package :test-package))
  (is (find-package :test-package))
  (is (find-package :test-package-implementation))
  (is (find-package :test-package-response))
  (delete-package :test-package)
  (delete-package :test-package-implementation)
  (delete-package :test-package-response))

(deftest define-enum ()
  (finishes (thrift:def-enum "TestEnum" ((first . 1) (second . 2))))
  (is (eql (symbol-value 'test-enum.first) 1))
  (is (eql (symbol-value 'test-enum.second) 2)))

(deftest define-constant ()
  (finishes (thrift:def-constant "aConstant" 1))
  (is (eql (symbol-value 'a-constant) 1))
  (unintern 'a-constant))

(defgeneric test-struct-too-field1 (struct))
(defgeneric test-struct-too-field2 (struct))
(defgeneric test-struct-too-field3 (struct))
(defgeneric (setf test-struct-too-field2) (value struct))

(deftest define-struct ()
  (finishes (thrift:def-struct "testStructToo" ()))
  (finishes (thrift:def-struct "testStructToo"
                (("field1" 0 :type i32 :id 1)
                 ("field2" nil :type i16 :id 2 :optional t)
                 ("field3" "string value" :type string :id 3))))
  (let ((struct (make-instance 'test-struct-too :field1 -1)))
    (is (equal (test-struct-too-field3 struct) "string value"))
    (is (not (slot-boundp struct 'field2)))
    (is (equal (test-struct-too-field1 struct) -1))
    (is (typep (nth-value 1 (ignore-errors (setf (test-struct-too-field1 struct) 1.1))) 'null))
    (mapc #'(lambda (method) (remove-method (c2mop:method-generic-function method) method))
          (c2mop:specializer-direct-methods (find-class 'test-struct-too)))
    (setf (find-class 'test-struct-too) nil)))

(defgeneric test-exception-reason (exception))

(thrift:def-exception "testException" (("reason" nil :type string :id 1)))

(deftest define-exception ()
  (let ((ex (make-condition 'test-exception :reason "testing")))
    (is (equal (test-exception-reason ex) "testing"))
    (is (eq (cl:type-of (nth-value 1 (ignore-errors (error ex))))
            'test-exception))
    (is (stringp (princ-to-string ex)))
    (mapc #'(lambda (method) (remove-method (c2mop:method-generic-function method) method))
          (c2mop:specializer-direct-methods (find-class 'test-exception)))
    (mapc #'(lambda (method) (remove-method (c2mop:method-generic-function method) method))
          (c2mop:specializer-direct-methods (find-class 'test-exception-exception-class)))
    (setf (find-class 'test-exception) nil)
    (setf (find-class 'test-exception-exception-class) nil)))

#+(or)
(def-service "TestService" nil
  (:method "someTestMethod" ((("arg1" i32 1) ("arg2" string 2)) string)))

#+(or)
(test def-service
  (progn (eval '(defun thrift-test.test-service-implementation:some-test-method (arg1 arg2) (format nil "~a ~a" arg1 arg2)))
         (let (request-protocol
               response-protocol
               (run-response-result nil))
           (flet ((run-response (request-stream)
                    (rewind request-stream)
                    (multiple-value-bind (identifier type seq)
                                         (stream-read-message-begin response-protocol)
                      (cond ((and (equal identifier "testMethod") (eq type 'call))
                             (setf run-response-result
                                   (funcall 'thrift-test.test-service-response::test-method
                                            t seq response-protocol)))
                            (t
                             (unknown-method response-protocol identifier seq
                                             (prog1 (stream-read-struct response-protocol)
                                               (stream-read-message-end response-protocol))))))))
             (multiple-value-setq (request-protocol response-protocol)
               (make-test-protocol-peers :request-hook #'run-response))

             (prog1 (and (equal (funcall 'thrift-test.test-service::test-method request-protocol 1 "testing")
                                "1 testing")
                         ;; if the first test succeed, this should also be true
                         (equal run-response-result "1 testing"))
               ;;(fmakunbound 'thrift-test.test-service-implementation::test-method)
               ;;(fmakunbound 'thrift-test.test-service::test-method)
               ;;(fmakunbound 'thrift-test.test-service-response::test-method)
               )))))

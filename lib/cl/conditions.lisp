(in-package #:org.apache.thrift.implementation)

;;; This file defines exception classes and signaling operators for the `org.apache.thrift` library.
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


;;; The exception hierarchy is rooted in thrift-error and mixes in other standard
;;; conditions as appropriate for the excpetion attributes
;;;
;;; thrift-error
;;; - application-error
;;; - protocol-error
;;;   - protocol-version-error
;;;   - protocol-type-error (type-error)
;;; - unknown-field-error (cell-error)
;;; - field-type-error (type-error)
;;; - transport-error
;;; 


;;; abstract exceptions

(define-condition thrift-error (simple-error)
  ((type
    :initform *protocol-ex-unknown*
    :reader thrift-error-type))
  (:report (lambda (error stream)
             (apply #'format stream (thrift-error-format-control error)
                    (thrift-error-format-arguments error)))))

(defgeneric thrift-error-format-control (error)
  (:method ((error thrift-error))
    "~(~a~): ~a."))

(defgeneric thrift-error-format-arguments (error)
  (:method ((error thrift-error))
    (list (type-of error) (thrift-error-type error))))

(define-condition protocol-error (thrift-error)
  ((protocol :initarg :protocol :initform nil
             :reader protocol-error-protocol)))

(defmethod thrift-error-format-control ((error protocol-error))
  (concatenate 'string (call-next-method)
               " connection: ~a."))

(defmethod thrift-error-format-arguments ((error protocol-error))
  (append (call-next-method)
          (list (protocol-error-protocol error))))

(define-condition transport-error (thrift-error) ())

(define-condition application-error (protocol-error)
  ((type :initform *application-ex-unknown*)
   (condition :initform nil :initarg :condition :reader application-error-condition)))

(defmethod thrift-error-format-control ((error application-error))
  (concatenate 'string (call-next-method)
               "~@[ condition: ~a.~]"))

(defmethod thrift-error-format-arguments ((error application-error))
  (append (call-next-method)
          (list (application-error-condition error))))

(defmethod thrift:type-of ((value thrift-error))
  'struct)

;;;
;;; general exception response

(thrift:def-exception "ResponseException"
  "The general 'exception' response exception"
  (("why" nil :id 1 :type string)
   ("id" nil :id 2 :type i16)))

;;;
;;; concrete exceptions

(define-condition class-not-found-error (protocol-error)
  ((identifier
    :initarg :identifier :reader class-not-found-error-identifier
    :documentation "The external identifier name for the unknown class.")))

(defmethod thrift-error-format-control ((error class-not-found-error))
  (concatenate 'string (call-next-method)
               " class not found: ~a."))

(defmethod thrift-error-format-arguments ((error class-not-found-error))
  (append (call-next-method)
          (list (class-not-found-error-identifier error))))

(define-condition protocol-version-error (protocol-error type-error)
  ((type :initform *protocol-ex-bad-version*)))

(defmethod thrift-error-format-control ((error protocol-version-error))
  (concatenate 'string (call-next-method)
               " protocol version does not match: ~s, expected ~s."))

(defmethod thrift-error-format-arguments ((error protocol-version-error))
  (append (call-next-method)
          (list (type-error-datum error) (type-error-expected-type error))))

(define-condition element-type-error (protocol-error type-error)
  ((type :initform *protocol-ex-invalid-data*)
   (element-type :initarg :element-type :reader element-type-error-element-type)
   (container-type :initarg :container-type :reader element-type-error-container-type)))

(defmethod thrift-error-format-control ((error element-type-error))
  (concatenate 'string (call-next-method)
               " element type is invalid for container: ~s: ~s, ~s."))

(defmethod thrift-error-format-arguments ((error element-type-error))
  (append (call-next-method)
          (list (element-type-error-container-type error)
                (element-type-error-element-type error)
                (type-error-expected-type error))))

(define-condition enum-type-error (protocol-error type-error)
  ((type :initform *protocol-ex-invalid-data*)))

(defmethod thrift-error-format-control ((error enum-type-error))
  (concatenate 'string (call-next-method)
               " value not of enum type: ~s, expected ~s."))

(defmethod thrift-error-format-arguments ((error enum-type-error))
  (append (call-next-method)
          (list (type-error-datum error) (type-error-expected-type error))))

(define-condition field-size-error (protocol-error type-error cell-error)
  ((type :initform *protocol-ex-size-limit*)
   (number  :initarg :number :reader field-size-error-number))
  (:default-initargs :expected-type 'field-size))

(defmethod thrift-error-format-control ((error field-size-error))
  (concatenate 'string (call-next-method)
               " field size invalid: (~s: ~a): ~s, expected ~s."))

(defmethod thrift-error-format-arguments ((error field-size-error))
  (append (call-next-method)
          (list (field-size-error-number error)
                (cell-error-name error) 
                (type-error-datum error)
                (type-error-expected-type error))))

(define-condition field-type-error (protocol-error type-error cell-error)
  ((type :initform *protocol-ex-invalid-data*)
   (structure-type :initarg :structure-type :reader field-type-error-structure-type)
   (number :initarg :number :reader field-type-error-number)))

(defmethod thrift-error-format-control ((error field-type-error))
  (concatenate 'string (call-next-method)
               " field value type is invalid for structure: ~s: (~s: ~a ~a) = ~s."))

(defmethod thrift-error-format-arguments ((error field-type-error))
  (append (call-next-method)
          (list (field-type-error-structure-type error)
                (field-type-error-number error)
                (type-error-expected-type error) 
                (cell-error-name error) 
                (type-error-datum error))))

(define-condition sequence-number-error (application-error)
  ((type :initform *application-ex-bad-sequence-id*)
   (number :initarg :number :reader sequence-number-error-number)
   (expected-number :initarg :expected-number :reader sequence-number-error-expected-number)))
  

(defmethod thrift-error-format-control ((error sequence-number-error))
  (concatenate 'string (call-next-method)
               " sequence number does not match: ~s, expected ~s."))

(defmethod thrift-error-format-arguments ((error sequence-number-error))
  (append (call-next-method)
          (list (sequence-number-error-number error) (sequence-number-error-expected-number error))))

(define-condition unknown-field-error (protocol-error cell-error)
  ((type :initform *protocol-ex-invalid-data*)
   (structure-type :initarg :structure-type :reader unknown-field-error-structure-type)
   (number :initarg :number :reader unknown-field-error-number)
   (datum :initarg :datum :reader unknown-field-error-datum)))

(defmethod thrift-error-format-control ((error unknown-field-error))
  (concatenate 'string (call-next-method)
               " field is not defined for struct type: ~s: (~s: ~a) = ~s."))

(defmethod thrift-error-format-arguments ((error unknown-field-error))
  (append (call-next-method)
          (list (unknown-field-error-structure-type error)
                (unknown-field-error-number error) 
                (cell-error-name error) 
                (unknown-field-error-datum error))))

(define-condition unknown-method-error (protocol-error )
  ((type :initform *application-ex-unknown-method*)
   (identifier :initarg :identifier :reader unknown-method-error-identifier)
   (request :initarg :request :reader unknown-method-error-request)))

(defmethod thrift-error-format-control ((error unknown-method-error))
  (concatenate 'string (call-next-method)
               " unknown method in request: ~s, ~s."))

(defmethod thrift-error-format-arguments ((error unknown-method-error))
  (append (call-next-method)
          (list (unknown-method-error-identifier error)
                (unknown-method-error-request error))))

(define-condition struct-type-error (protocol-error type-error)
  ((type :initform *protocol-ex-invalid-data*)))

(defmethod thrift-error-format-control ((error struct-type-error))
  (concatenate 'string (call-next-method)
               " struct is not expected type: ~s, expected ~s."))

(defmethod thrift-error-format-arguments ((error struct-type-error))
  (append (call-next-method)
          (list (type-error-datum error)
                (type-error-expected-type error))))

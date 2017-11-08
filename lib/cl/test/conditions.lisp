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

;;;; This file defines tests for exception classes.

(fiasco:define-test-package (#:condition-tests :in thrift-test:thrift-self-tests)
  (:use #:thrift-test-utils))

(in-package #:condition-tests)

(deftest thrift-error ()
  (is (stringp (princ-to-string (make-condition 'thrift:thrift-error)))))

(deftest application-error ()
  (is (stringp
       (princ-to-string
        (make-condition 'thrift:application-error
                        :condition (nth-value 1 (ignore-errors (error "testing errors"))))))))

(deftest protocol-error ()
  (is (stringp (princ-to-string (make-condition 'thrift:protocol-error
                                                :protocol (make-test-protocol))))))

(deftest transport-error ()
  (is (stringp (princ-to-string (make-condition 'thrift:transport-error)))))

(deftest class-not-found-error ()
  (is (stringp (princ-to-string (make-condition 'thrift:class-not-found-error
                                                :protocol (make-test-protocol)
                                                :identifier "UnknownClass"))))
  (is (typep (nth-value 1 (ignore-errors (thrift:class-not-found (make-test-protocol) "UnknownClass")))
             'thrift:class-not-found-error)))

(deftest protocol-version-error ()
  (is (stringp (princ-to-string (make-condition 'thrift:protocol-version-error
                                                :protocol (make-test-protocol)
                                                :datum '(0 . 0)
                                                :expected-type '(1 . 1)))))
  (is (typep (nth-value 1 (ignore-errors (thrift:invalid-protocol-version (make-test-protocol) 0 0)))
             'thrift:protocol-version-error)))

(deftest element-type-error ()
  (is (stringp (princ-to-string (make-condition 'thrift:element-type-error
                                                :protocol (make-test-protocol)
                                                :container-type 'list
                                                :expected-type 'bool
                                                :element-type 'i16))))
  (is (typep (nth-value 1 (ignore-errors (thrift:invalid-element-type (make-test-protocol)
                                                                      'list
                                                                      'bool
                                                                      'i16)))
             'thrift:element-type-error)))

(deftest enum-type-error ()
  (is (stringp (princ-to-string (make-condition 'thrift:enum-type-error
                                                :protocol (make-test-protocol)
                                                :datum 3
                                                :expected-type '(enum "x")))))
  (is (typep (nth-value 1 (ignore-errors (thrift:invalid-enum (make-test-protocol) '(enum "x") 3)))
             'thrift:enum-type-error)))

(deftest field-size-error ()
  (is (stringp (princ-to-string (make-condition 'thrift:field-size-error
                                                :protocol (make-test-protocol)
                                                :name "fieldex"
                                                :number -1
                                                :datum most-negative-fixnum
                                                :expected-type `(integer 0 ,most-positive-fixnum)))))
  (is (typep (nth-value 1 (ignore-errors (thrift:invalid-field-size (make-test-protocol)
                                                                    -1
                                                                    "fieldex"
                                                                    `(integer 0 ,most-positive-fixnum)
                                                                    most-negative-fixnum)))
             'thrift:field-size-error)))

(deftest field-type-error ()
  (is (stringp (princ-to-string (make-condition 'thrift:field-type-error
                                                :protocol (make-test-protocol)
                                                :structure-type 'test-struct
                                                :name "fieldex"
                                                :number 17
                                                :expected-type 'bool
                                                :datum 12345))))
  (is (typep (nth-value 1 (ignore-errors (thrift:invalid-field-type (make-test-protocol)
                                                                    'test-struct 17
                                                                    "fieldex"
                                                                    'bool
                                                                    12345)))
             'thrift:field-type-error)))

(deftest unknown-field-error ()
  (is (stringp (princ-to-string (make-condition 'thrift:unknown-field-error
                                                :protocol (make-test-protocol)
                                                :structure-type 'test-struct
                                                :name "fieldex"
                                                :number 17
                                                :datum 12345))))
  (is (typep (nth-value 1 (ignore-errors (thrift:unknown-field (make-test-protocol)
                                                               17
                                                               "fieldex"
                                                               'i16
                                                               12345)))
             'null)))

(deftest unknown-method-error ()
  (is (stringp (princ-to-string (make-condition 'thrift:unknown-method-error
                                                :protocol (make-test-protocol)
                                                :identifier "methodex"
                                                :request t))))
  (is (typep (nth-value 1 (ignore-errors (thrift:unknown-method (make-test-protocol)
                                                                "methodex"
                                                                12345
                                                                t)))
             'thrift:unknown-method-error)))

(deftest struct-type-error ()
  (is (stringp (princ-to-string (make-condition 'thrift:struct-type-error
                                                :protocol (make-test-protocol)
                                                :expected-type 'test-struct
                                                :datum t))))
  (is (typep (nth-value 1 (ignore-errors (thrift:invalid-struct-type (make-test-protocol)
                                                                     'test-struct
                                                                     t)))
             'thrift:struct-type-error)))

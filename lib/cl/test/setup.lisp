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

;;;; For testing test utils themselves.

(fiasco:define-test-package (#:setup-tests :in thrift-test:thrift-self-tests)
  (:use #:thrift-test-utils))

(in-package #:setup-tests)

(deftest thrift-class ()
  (let ((class (find-class 'test-struct)))
    (is (equal (thrift:class-identifier class) "TestStruct"))
    (is (every #'(lambda (id name)
                   (equal (thrift:field-definition-identifier
                           (find id (thrift:class-field-definitions class)
                                 :key #'thrift:field-definition-identifier-number))
                          name))
               '(1 2)
               '("fieldOne" "fieldTwo")))))

(deftest test-transport ()
  (is (typep (make-test-transport) 'thrift:binary-transport)))

(deftest test-protocol ()
  (is (typep (make-test-protocol) 'thrift:binary-protocol)))

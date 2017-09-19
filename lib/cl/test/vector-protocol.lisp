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

(fiasco:define-test-package (#:vector-protocol-tests :in thrift-test:thrift-self-tests)
  (:use #:thrift-test-utils))

(in-package #:vector-protocol-tests)

(deftest write-byte-test ()
  (finishes (stream-write-byte (make-instance 'thrift:vector-stream-transport) 1))
  (finishes (stream-write-byte (make-instance 'thrift:vector-stream-transport) -1)))

(deftest write-sequence-test ()
  (let* ((data #(0 1 2 3 4 5 6 7 8 9 246 247 248 249 250 251 252 253 254 255))
         (buffer (make-array 2 :element-type thrift::*binary-transport-element-type*))
         (outstream (make-instance 'thrift:vector-output-stream :vector buffer))
         (instream (make-instance 'thrift:vector-input-stream :vector nil)))
    (stream-write-sequence outstream data 0 nil)
    (cl:map nil #'(lambda (c) (stream-write-byte outstream (char-code c))) "asdf")

    (is (every #'eql
               (concatenate 'vector data (cl:map 'vector #'char-code "asdf"))
               (subseq (thrift.implementation::get-vector-stream-vector outstream)
                       0
                       (thrift.implementation::stream-position outstream))))
    (let ((data2 (make-array (length data)))
          (data3 (make-array 4)))
      (thrift.implementation::setf-vector-stream-vector (thrift.implementation::get-vector-stream-vector outstream)
                                                        instream)
      (is (eql (stream-read-sequence instream data2 0 nil) (length data2)))
      (is (equalp data2 data))
      (stream-read-sequence instream data3 0 nil)
      (is (equal (cl:map 'string #'code-char data3) "asdf")))))

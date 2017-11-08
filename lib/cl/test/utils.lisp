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

;;;; A (meta)package for all sorts of test utils. Meant to be :used by test
;;;; packages.

(in-package #:thrift-test-utils)

(defvar *test-location* #u"thrift://127.0.0.1:9091")

(defvar *test-service* (make-instance 'service :identifier "Test Root"))

(defvar *test-server-process* nil)

(def-struct "TestStruct"
    (("fieldOne" nil :id 1 :type string)
     ("fieldTwo" nil :id 2 :type i16)))

(def-struct "TestLargeStruct"
    (("fieldOne" nil :id 1 :type i16 :optional t)
     ("fieldTwo" nil :id 2 :type i16 :optional t)
     ("fieldThree" nil :id 3 :type i16 :optional t)
     ("fieldFour" nil :id 4 :type i16 :optional t)
     ("fieldFive" nil :id 5 :type i16 :optional t)
     ("fieldSix" nil :id 6 :type i16 :optional t)
     ("fieldSeven" nil :id 7 :type i16 :optional t)
     ("fieldEight" nil :id 8 :type i16 :optional t)
     ("fieldNine" nil :id 9 :type i16 :optional t)
     ("fieldTen" nil :id 10 :type i16 :optional t)))

(defun make-test-transport (&rest initargs)
  (apply #'make-instance 'vector-stream-transport initargs))

(defun make-test-protocol (&rest initargs &key
                                            (direction :io)
                                            (input-transport (make-test-transport))
                                            (output-transport input-transport))
  (apply #'make-instance 'binary-protocol
         :direction direction
         :input-transport input-transport
         :output-transport output-transport
         initargs))

(defun make-test-protocol-peers (&key (request-hook 'rewind) (response-hook 'rewind))
  (let ((request-transport (make-test-transport :force-output-hook request-hook))
        (response-transport (make-test-transport :force-output-hook response-hook)))
    (values (make-test-protocol :output-transport request-transport
                                :input-transport response-transport)
            (make-test-protocol :output-transport response-transport
                                :input-transport request-transport))))

(defgeneric rewind (stream)
  (:method ((protocol protocol))
    (rewind (protocol-input-transport protocol))
    (rewind (protocol-output-transport protocol))
    protocol)

  (:method ((stream vector-stream))
    (thrift.implementation::stream-position stream 0)
    stream))

(defgeneric reset (stream)
  (:method ((protocol protocol))
    (rewind protocol)
    (reset (protocol-output-transport protocol))
    protocol)

  (:method ((stream vector-stream))
    (fill (thrift.implementation::get-vector-stream-vector stream) 0)
    stream))

(defun test-server (&optional (location *test-location*))
  (setq *test-location* location)
  (or *test-server-process*
      (setq *test-server-process* (bt:make-thread #'(lambda () (serve location *test-service*))))))

(defun stop-test-server ()
  (when (typep *test-server-process* 'bt:thread)
    (bt:destroy-thread *test-server-process*)
    (setq *test-server-process* nil)))

(defun call-with-test-services (function &rest services)
  (declare (dynamic-extent function))
  (unwind-protect (progn (setf (service-base-services *test-service*)
                               (union (service-base-services *test-service*)
                                      services))
                         (funcall function))
    (setf (service-base-services *test-service*)
          (set-difference (service-base-services *test-service*)
                          services))))

(defmacro with-test-services ((protocol &rest services) &body body)
  (let ((op (gensym)))
    `(flet ((,op () (with-client (,protocol *test-location*) ,@body)))
       ;; (test-server) doesn't work as the connect beats the accept and the client hangs
       (call-with-test-services #',op ,@services))))

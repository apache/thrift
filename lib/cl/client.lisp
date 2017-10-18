(in-package #:org.apache.thrift.implementation)

;;; This file defines client operators for the `org.apache.thrift` library.
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



(defgeneric client (location &key protocol direction element-type &allow-other-keys)
  (:method ((location puri:uri) &rest initargs &key (direction :io) (element-type 'unsigned-byte et-s) &allow-other-keys)
    (when et-s
      (setf initargs (copy-list initargs))
      (remf initargs :element-type))
    (apply #'client (socket-transport location :element-type element-type :direction direction)
           :direction direction
           initargs))

  (:method ((location pathname) &rest initargs &key (direction :io) (element-type 'unsigned-byte et-s) &allow-other-keys)
    (when et-s
      (setf initargs (copy-list initargs))
      (remf initargs :element-type))
    (apply #'client (make-instance 'file-transport
                      :pathname location :element-type element-type :direction direction)
            :direction direction
            initargs))

  (:method ((instance protocol) &rest initargs
            &key (protocol (class-of instance) p-s) (direction (stream-direction instance)) &allow-other-keys)
    "Given a protocol INSTANCE, and a PROTOCOL class, make a new protocol instance which reuses
     the given instance's transports."
    (print "bleh")
    (when p-s
      (setf initargs (copy-list initargs))
      (remf initargs :protocol))
    (apply #'make-instance protocol
      :input-transport (thrift:protocol-input-transport protocol)
      :output-transport (thrift:protocol-output-transport protocol)
      :direction direction
      initargs))

  (:method ((instance binary-transport) &rest initargs
            &key (framed nil f-s) (protocol 'binary-protocol p-s)
              (direction (stream-direction instance)) &allow-other-keys)
    (when p-s
      (setf initargs (copy-list initargs))
      (remf initargs :protocol))
    (when f-s
      (setf initargs (copy-list initargs))
      (remf initargs :framed))
    (let ((transport (if framed
                         (framed-transport instance)
                         instance)))
      (apply #'make-instance protocol :transport transport :direction direction
             initargs))))


(defmacro with-client ((protocol &rest args) &body body)
  (with-gensyms (op)
    `(flet ((,op (,protocol) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-client #',op ,@args))))


(defun call-with-client (op &rest args)
  (declare (dynamic-extent args))
  (let ((protocol (apply #'client args)))
    (unwind-protect (funcall op protocol)
      (when (open-stream-p protocol)
        (close protocol)))))

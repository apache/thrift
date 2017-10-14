(in-package #:org.apache.thrift.implementation)

;;; This file defines the core of the 'transport' layer for the `org.apache.thrift` library.
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


;;; The transport operators focus on the stream interface and supply the equivalents to the
;;; Thrift standard operators in terms of the gray stream interface:
;;;
;;;  * open is superfluous. there is no use case for it, as they are not reused.
;;;     the respective stream is opened as a side-effect of make-instance.
;;;  * isOpen is implemented as methods for open-stream-p
;;;  * close is implemented as transport-close to which stream-close/close delegates as per runtime
;;;  * read-byte is implemented as methods for stream-read-byte
;;;  * read-sequence is implemented as methods for stream-read-sequence
;;;  * write-byte is implemented as methods for stream-write-byte
;;;  * write-sequence is implemented as methods for stream-write-sequence
;;;  * flush is implemented as a method on stream-finish-output


;;;
;;; macros

(macrolet ((def-signed-byte (bit-count)
             (let ((name (cons-symbol :org.apache.thrift.implementation
                                      :signed-byte- (prin1-to-string  bit-count)))
                   (max-positive (1- (expt 2 (1- bit-count))))
                   (mask (1- (expt 2 bit-count))))
             `(progn (defun ,name (byte)
                       (if (> byte ,max-positive)          ;  convert
                         (- (logxor ,mask (1- byte)))
                         byte))
                     (define-compiler-macro ,name (byte &environment env)
                       (let* ((var (if (and (symbolp byte) (eq (macroexpand-1 byte env) byte)) byte (gensym)))
                              (form `(if (> ,var ,,max-positive)          ;  convert
                                      (- (logxor ,,mask (1- ,var)))
                                      ,var)))
                         (if (eq byte var)
                           form
                           `(let ((,var ,byte)) ,form))))))))
  (def-signed-byte 8)
  (def-signed-byte 16)
  (def-signed-byte 32)
  (def-signed-byte 64))

(defun unsigned-byte-8 (datum)
  (logand datum #xff))

(define-compiler-macro unsigned-byte-8 (datum)
  `(logand ,datum #xff))


;;;
;;; classes

(defclass transport (trivial-gray-streams:fundamental-stream)
  ((stream :reader transport-stream)
   (direction :initarg :direction :accessor stream-direction))
  (:documentation "The abstract transport class is a specialized stream which wraps a base binary
 stream - a file or a socket, with methods which codec operators for primitive data types."))


(defclass binary-transport (transport)
  ())


(defclass socket-transport (binary-transport)
  ()
  (:documentation "A specialzed transport which wraps a socket and its stream."))


(defclass file-transport (binary-transport)
  ((pathname :initarg :pathname :accessor transport-pathname :initform (error "pathname is required."))
   ;; delegation, as make-instance does not return a usable stream in all implementations
   (stream :accessor transport-stream)))


;;;
;;; initialization

(defvar *binary-transport-element-type* '(unsigned-byte 8))

(defmethod initialize-instance ((transport socket-transport) &key socket)
  (call-next-method)
  (setf (slot-value transport 'stream) (usocket:socket-stream socket)))


(defun socket-transport (location &rest initargs
                                  &key (element-type *binary-transport-element-type*) (direction :io d-s))
  (when d-s
    (setf initargs (copy-list initargs))
    (remf initargs :direction))
  
  (make-instance 'socket-transport
    :direction direction
    :socket (apply #'usocket:socket-connect (puri:uri-host location) (puri:uri-port location)
                   :element-type element-type
                   initargs)))



(defmethod initialize-instance ((transport file-transport) &key pathname stream
                                (direction :output)
                                (element-type *binary-transport-element-type*)
                                (if-exists :supersede) (if-does-not-exist :create))
  (call-next-method)
  (setf (slot-value transport 'stream)
        (or stream
            (open (or pathname (error "A pathname is required."))
                  :direction direction :element-type element-type
                  :if-exists if-exists :if-does-not-exist if-does-not-exist))))


(defun file-transport (pathname &rest initargs
                                &key (element-type *binary-transport-element-type*))
  (apply #'make-instance 'file-transport
         :pathname pathname :element-type element-type
         initargs))


;;; open-stream-p is the only operator which guards against an unbound slot.
;;; stream-close checks that the stream is still open
;;; all other presume it is open.

#-mcl  ;; mcl defines a plain function in terms of stream-direction
(defmethod open-stream-p ((transport transport))
  (when (slot-boundp transport 'stream)
    (open-stream-p (transport-stream transport))))

(defun transport-close (transport &key abort)
  "The transport close implementation is used by whichever interface the runtime presents for extensions.
 as per the gray interface, close is replaced with a generic function. in other cases, stream-close
 is a generic operator."
  (when (open-stream-p transport)
    (close (transport-stream transport) :abort abort)
    (setf (slot-value transport 'direction) :closed)
    (slot-makunbound transport 'stream)))

(when (fboundp 'stream-close)
  (defmethod stream-close ((transport transport))
    (when (next-method-p) (call-next-method))
    (transport-close transport)))

(when (typep #'close 'generic-function)
  (defmethod close ((stream transport) &rest args)
    (when (next-method-p) (call-next-method))
    (apply #'transport-close stream args)
    t))


#-sbcl
(defmethod stream-finish-output ((transport transport))
  (stream-finish-output (transport-stream transport)))
#+sbcl
(defmethod stream-finish-output ((transport transport))
  (finish-output (transport-stream transport)))

#-sbcl
(defmethod stream-force-output ((transport transport))
  (stream-force-output (transport-stream transport)))
#+sbcl
(defmethod stream-force-output ((transport transport))
  (force-output (transport-stream transport)))


;;;
;;; input

#-sbcl
(defmethod stream-read-byte ((transport binary-transport))
  (let ((unsigned-byte (stream-read-byte (transport-stream transport))))
    (if unsigned-byte
      (signed-byte-8 unsigned-byte)
      (error 'end-of-file :stream (transport-stream transport)))))
#+sbcl
(defmethod stream-read-byte ((transport binary-transport))
  (let ((unsigned-byte (read-byte (transport-stream transport))))
    (signed-byte-8 unsigned-byte)))


#-(or mcl sbcl)
(defmethod stream-read-sequence ((transport binary-transport) (sequence vector) &optional (start 0) (end nil))
  (stream-read-sequence (transport-stream transport) sequence start end))

#+mcl
(defmethod stream-read-sequence ((transport binary-transport) (sequence vector) &rest args)
  (declare (dynamic-extent args))
  (apply #'stream-read-sequence (transport-stream transport) sequence args))

#+sbcl
(defmethod stream-read-sequence ((transport binary-transport) (sequence vector) start end &key)
  (unless (= (read-sequence sequence (transport-stream transport) :start start :end end)
             (or end (length sequence)))
    (error 'end-of-file :stream (transport-stream transport))))

;;;
;;; output

#-sbcl
(defmethod stream-write-byte ((transport binary-transport) byte)
  (stream-write-byte (transport-stream transport) (unsigned-byte-8 byte)))
#+sbcl
(defmethod stream-write-byte ((transport binary-transport) byte)
  (write-byte (unsigned-byte-8 byte) (transport-stream transport)))


#-(or mcl sbcl)
(defmethod stream-write-sequence ((transport binary-transport) (sequence vector) &optional (start 0) (end nil))
  (stream-write-sequence (transport-stream transport) sequence start end))

#+mcl
(defmethod stream-write-sequence ((transport binary-transport) (sequence vector) &rest args)
  (declare (dynamic-extent args))
  (apply #'stream-write-sequence (transport-stream transport) sequence args))

#+sbcl
(defmethod stream-write-sequence ((transport binary-transport) (sequence vector) start end &key)
  (write-sequence sequence (transport-stream transport) :start start :end end))

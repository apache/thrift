(in-package #:org.apache.thrift.implementation)

;;;; Copyright 2017 Rigetti Computing <rigetti.com>
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

;;;; This implements framed transport according to the Apache Thrift
;;;; specification. The way it works is by creating a decorator (wrapper)
;;;; around a "real" binary transport (like a socket or file one). All
;;;; that's needed is to call (framed-transport real-transport) and use the
;;;; returned object in place of the "real" transport.

(defconstant *frame-length-limit* 16384000)

;;; Classes

(defclass transport-decorator (binary-transport)
  ((real-transport :accessor decorator-real-transport
                   :initarg :real-transport)))

(defclass framed-transport (transport-decorator)
  ((write-buffer :accessor framed-write-buffer
                 :initform nil)
   (read-buffer :accessor framed-read-buffer
                :initform nil)
   (message-length :accessor framed-message-length
                   :initform 0)))

;;; Initialization, closing

(defmethod initialize-instance :after ((transport transport-decorator) &key real-transport)
  (setf (slot-value transport 'real-transport) real-transport))

(defmethod initialize-instance :after ((transport framed-transport) &key)
  (reset-write-buffer transport))

(defun framed-transport (real-transport)
  (make-instance 'framed-transport
                 :real-transport real-transport
                 :direction (stream-direction real-transport)))

(defmethod transport-close ((transport transport-decorator) &key abort)
  (transport-close (decorator-real-transport transport) :abort abort))

(defmethod open-stream-p ((transport transport-decorator))
  (open-stream-p (decorator-real-transport transport)))

;;; Helpers

(defun bytes-to-uint (bytes)
  (loop for shift from (1- (length bytes)) downto 0
     for byte across bytes summing
       (ash byte (* 8 shift))))

(defun uint-to-bytes (integer byte-count)
  (let ((bytes (make-array byte-count :element-type '(unsigned-byte 8)))
        (remaining integer))
    (loop for i from 0 below byte-count doing
         (multiple-value-bind (byte remainder)
             (floor remaining (expt 256 (- byte-count i 1)))
           (setf (aref bytes i) byte)
           (setf remaining remainder)))
    bytes))

;;; Buffer ops

(defgeneric ensure-read-buffer (transport))

(defgeneric write-frame (transport))

(defgeneric reset-write-buffer (transport))

(defmethod ensure-read-buffer ((transport framed-transport))
  (when (zerop (length (framed-read-buffer transport)))
    (let ((buffer (make-array 4 :element-type '(unsigned-byte 8))))
      (stream-read-sequence (decorator-real-transport transport) buffer 0 nil)
      (let ((frame-length (bytes-to-uint buffer)))
        (setf buffer (make-array frame-length :element-type '(unsigned-byte 8)))
        (stream-read-sequence (decorator-real-transport transport) buffer 0 nil)
        (setf (framed-read-buffer transport) buffer)))))

(defmethod write-frame ((transport framed-transport))
  (let ((length (length (framed-write-buffer transport))))
    (unless (zerop length)
      (when (> length *frame-length-limit*) (error "The frame is too big to write out."))
      (stream-write-sequence (decorator-real-transport transport) (uint-to-bytes length 4) 0 nil)
      (stream-write-sequence (decorator-real-transport transport) (framed-write-buffer transport) 0 nil)
      (reset-write-buffer transport))))

(defmethod reset-write-buffer ((transport framed-transport))
  (setf (framed-write-buffer transport) (make-array 50
                                                    :adjustable t
                                                    :fill-pointer 0
                                                    :element-type '(unsigned-byte 8))))

;;; Standard I/O methods (trivial-gray-streams)

(defmethod stream-finish-output ((transport framed-transport))
  (write-frame transport)
  (stream-finish-output (decorator-real-transport transport)))

(defmethod stream-force-output ((transport framed-transport))
  (write-frame transport)
  (stream-force-output (decorator-real-transport transport)))

(defmethod stream-write-byte ((transport framed-transport) byte)
  ()
  (vector-push-extend (unsigned-byte-8 byte) (framed-write-buffer transport)))

(defmethod stream-write-sequence ((transport framed-transport) (sequence vector) start end &key)
  (loop for byte across (subseq sequence start end)
     do (vector-push-extend byte (framed-write-buffer transport))))

(defmethod stream-read-byte ((transport framed-transport))
  (ensure-read-buffer transport)
  (let ((buffer (framed-read-buffer transport)))
    (prog1 (signed-byte-8 (aref buffer 0))
      (setf (framed-read-buffer transport) (subseq buffer 1)))))

(defmethod stream-read-sequence ((transport framed-transport) sequence start end &key)
  (ensure-read-buffer transport)
  (let ((length (length (subseq sequence start end))))
    (if (< (length (framed-read-buffer transport)) length)
        (error "End of file reached while reading from a framed transport."))
    (setf (subseq sequence start end) (subseq (framed-read-buffer transport) 0 length))
    (setf (framed-read-buffer transport) (subseq (framed-read-buffer transport) length))))

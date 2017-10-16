(in-package #:org.apache.thrift.implementation)

;;; This file defines the concrete `binary-protocol` layer for the `org.apache.thrift` library.
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

;;;
;;; classes

(defclass binary-protocol (encoded-protocol)
  ((field-id-mode :initform :identifier-number :allocation :class)
   (struct-id-mode :initform :none :allocation :class))
  (:default-initargs
   :version-id #x80
    :version-number #x01))



;;;
;;; type  code <-> name operators are specific to each protocol

(defmethod type-code-name ((protocol binary-protocol) (type-code fixnum))
  (or (car (rassoc type-code *binary-transport-types* :test #'eql))
      (error "Invalid type code: ~s." type-code)))


(defmethod type-name-code ((protocol binary-protocol) (type-name symbol))
  (or (cdr (assoc type-name *binary-transport-types*))
      (error "Invalid type name: ~s." type-name)))

(defmethod type-name-code ((transport binary-protocol) (type-name cons))
  (type-name-code transport (first type-name)))


(defmethod message-type-code ((protocol binary-protocol) (message-name symbol))
  (or (cdr (assoc message-name *binary-message-types*))
      (error "Invalid message type name: ~s." message-name)))

(defmethod message-type-name ((protocol binary-protocol) (type-code fixnum))
  (or (car (rassoc type-code *binary-message-types* :test 'eql))
      (error "Invalid message type code: ~s." type-code)))


;;; input

(defmethod stream-read-type ((protocol binary-protocol))
  (type-code-name protocol (stream-read-byte (protocol-input-transport protocol))))

(defmethod stream-read-message-type ((protocol binary-protocol))
  (message-type-name protocol (stream-read-i16 protocol)))


(defmethod stream-read-bool ((protocol binary-protocol))
  (= (stream-read-byte (protocol-input-transport protocol)) 1))

(defmethod stream-read-i8 ((protocol binary-protocol))
  (stream-read-byte (protocol-input-transport protocol)))

(macrolet ((read-and-decode-integer (protocol byte-count &aux (bit-count (* byte-count 8)))
             `(let ((value 0)
                    (buffer (make-array ,byte-count :element-type '(unsigned-byte 8))))
                (declare (dynamic-extent buffer)
                         (type (simple-array (unsigned-byte 8) (,byte-count)) buffer)
                         (type (unsigned-byte ,(* byte-count 8)) value))
                (stream-read-sequence (protocol-input-transport ,protocol) buffer 0 nil)
                ,@(loop for i from 0 below byte-count
                     collect `(setf value ,(if (= i 0)
                                               `(aref buffer ,i)
                                               `(+ (ash value 8) (aref buffer ,i)))))
                ;; (format *trace-output* "(in 0x~16,'0x)" value)
                (,(cons-symbol :org.apache.thrift.implementation
                               :signed-byte- (prin1-to-string bit-count)) value))))
  (defmethod stream-read-i16 ((protocol binary-protocol))
    (read-and-decode-integer protocol 2))

  (defmethod stream-read-i32 ((protocol binary-protocol))
    (read-and-decode-integer protocol 4))

  (defmethod stream-read-i64 ((protocol binary-protocol))
    (read-and-decode-integer protocol 8)))


(defmethod stream-read-double ((protocol binary-protocol))
  (let ((value 0)
        (buffer (make-array 8 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buffer)
             (type (simple-array (unsigned-byte 8) (8)) buffer)
             (type (unsigned-byte 64) value))
    (stream-read-sequence (protocol-input-transport protocol) buffer 0 nil)
    ;; it it matters, could unwrap it with fewer intermediates saves
    (macrolet ((unpack-buffer ()
                 `(progn
                    ,@(loop for i from 0 below 8
                         collect `(setf value ,(if (= i 0)
                                                   `(aref buffer ,i)
                                                   `(+ (ash value 8) (aref buffer ,i))))))))
      (unpack-buffer)
      (ieee-floats:decode-float64 value))))

(defmethod stream-read-float ((protocol binary-protocol))
  "As a special for for use with rdf - not part of the thrift. used just for specifically
 coded struct declarations."
  ;; this is not part of the thrift spec, but is useful elsewhere
  (let ((value 0)
        (buffer (make-array 4 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buffer)
             (type (simple-array (unsigned-byte 8) (4)) buffer)
             (type (unsigned-byte 32) value))
    (stream-read-sequence (protocol-input-transport protocol) buffer 0 nil)
    ;; it it matters, could unwrap it with fewer intermediates saves
    (macrolet ((unpack-buffer ()
                 `(progn
                    ,@(loop for i from 0 below 4
                         collect `(setf value ,(if (= i 0)
                                                   `(aref buffer ,i)
                                                   `(+ (ash value 8) (aref buffer ,i))))))))
      (unpack-buffer)
      (ieee-floats:decode-float32 value))))


(defmethod stream-read-string ((protocol binary-protocol))
  (let* ((l (stream-read-i32 protocol))
         (a (make-array l :element-type *binary-transport-element-type*)))
    (declare (dynamic-extent a))
    (stream-read-sequence (protocol-input-transport protocol) a 0 nil)
    (funcall (transport-string-decoder protocol) a)))


(defmethod stream-read-binary ((protocol binary-protocol))
  "Read an 'unencoded' binary array.
 Although the spec describes a 'byte' array, and elsewhere specifies bytes to be signed, that makes no
 sense. It contradicts the encoding for UTF and would be generally useless for binary data. The various
 extant language bindings read as if they either the issue or cast."

  (let* ((l (stream-read-i32 protocol))
         (result (make-array l :element-type *binary-transport-element-type*)))
    ;; would need to check the length before trying stack allocation
    (stream-read-sequence (protocol-input-transport protocol) result 0 nil)
    result))




;;; output


(defmethod stream-write-type ((protocol binary-protocol) type-name)
  (stream-write-byte (protocol-output-transport protocol) (type-name-code protocol type-name))
  1)

(defmethod stream-write-message-type ((protocol binary-protocol) message-type-name)
  (stream-write-i16 protocol (message-type-code protocol message-type-name)))



(defmethod stream-write-bool ((protocol binary-protocol) val)
  (stream-write-byte (protocol-output-transport protocol) (if val 1 0))
  1)


(defmethod stream-write-i8 ((protocol binary-protocol) val)
  (stream-write-byte (protocol-output-transport protocol) val)
  1)


(macrolet ((encode-and-write-integer (protocol value byte-count)
             `(let ((buffer (make-array ,byte-count :element-type '(unsigned-byte 8))))
                (declare (dynamic-extent buffer)
                         (type (simple-array (unsigned-byte 8) (,byte-count)) buffer))
                (assert (typep ,value '(signed-byte ,(* byte-count 8))) ()
                        'type-error :datum ,value :expected-type '(signed-byte ,(* byte-count 8)))
                (locally (declare (type (signed-byte ,(* byte-count 8)) ,value))
                  ;; (format *trace-output* "~%(out 0x~16,'0x)" ,value)
                  ,@(loop for i from (1- byte-count) downto 0
                       append `((setf (aref buffer ,i) (logand #xff ,value))
                                (setf ,value (ash ,value -8))))
                  (stream-write-sequence (protocol-output-transport ,protocol) buffer 0 nil)
                  ,byte-count))))
  ;; no sign conversion as shift&mask encodes the sign bit
  (defmethod stream-write-i16 ((protocol binary-protocol) val)
    (encode-and-write-integer protocol val 2))

  (defmethod stream-write-i32 ((protocol binary-protocol) val)
    (encode-and-write-integer protocol val 4))

  (defmethod stream-write-i64 ((protocol binary-protocol) val)
    (encode-and-write-integer protocol val 8)))


(defmethod stream-write-double ((protocol binary-protocol) val)
  ;; distinct from i64, as it's unsigned
  (let ((buffer (make-array 8 :element-type '(unsigned-byte 8)))
        (int-value (ieee-floats:encode-float64 val)))
    (declare (dynamic-extent buffer)
             (type (simple-array (unsigned-byte 8) (8)) buffer)
             (type (unsigned-byte 64) int-value))
    ;; if the conversion is correct, this is redundant, sbcl eliminate it
    (assert  (typep int-value '(unsigned-byte 64)) ()
             'type-error :datum int-value :expected-type '(unsigned-byte 64))
    ;; (format *trace-output* "~%(out 0x~16,'0x)" int-value)
    (macrolet ((pack-buffer ()
                 `(progn ,@(loop for i from 7 downto 0
                              append `((setf (aref buffer ,i) (logand #xff int-value))
                                       (setf int-value (ash int-value -8)))))))
      (pack-buffer))
    (stream-write-sequence (protocol-output-transport protocol) buffer 0 nil)
    8))

(defmethod stream-write-float ((protocol binary-protocol) val)
  " Not part of the spec, but is useful elsewhere"
  ;; distinct from i34, as it's unsigned
  (let ((buffer (make-array 4 :element-type '(unsigned-byte 8)))
        (int-value (ieee-floats:encode-float32 val)))
    (declare (dynamic-extent buffer)
             (type (simple-array (unsigned-byte 8) (4)) buffer)
             (type (unsigned-byte 32) int-value))
    ;; if the conversion is correct, this is redundant, sbcl eliminate it
    (assert (typep int-value '(unsigned-byte 32)) ())
    ;; (format *trace-output* "~%(out 0x~16,'0x)" int-value)
    (macrolet ((pack-buffer ()
                 `(progn ,@(loop for i from 3 downto 0
                              append `((setf (aref buffer ,i) (logand #xff int-value))
                                       (setf int-value (ash int-value -8)))))))
      (pack-buffer))
    (stream-write-sequence (protocol-output-transport protocol) buffer 0 nil)
    4))


(defmethod stream-write-string ((protocol binary-protocol) (string string) &optional (start 0) end)
  (assert (and (zerop start) (or (null end) (= end (length string)))) ()
          "Substring writes are not supported.")
  (let ((bytes (funcall (transport-string-encoder protocol) string)))
    (stream-write-i32 protocol (length bytes))
    (stream-write-sequence (protocol-output-transport protocol) bytes 0 nil)
    (+ 4 (length bytes))))

(defmethod stream-write-string ((protocol binary-protocol) (bytes vector) &optional (start 0) end)
  (assert (and (zerop start) (or (null end) (= end (length bytes)))) ()
          "Substring writes are not supported.")
  (stream-write-i32 protocol (length bytes))
  (stream-write-sequence (protocol-output-transport protocol) bytes 0 nil)
  (+ 4 (length bytes)))


(defmethod stream-write-binary ((protocol binary-protocol) (bytes vector))
  (let ((unsigned-bytes (make-array (length bytes) :element-type '(unsigned-byte 8))))
    (stream-write-i32 protocol (length bytes))
    (map-into unsigned-bytes #'unsigned-byte-8 bytes)
    (stream-write-sequence (protocol-output-transport protocol) unsigned-bytes 0 nil)
    (+ 4 (length bytes))))

(defmethod stream-write-binary ((protocol binary-protocol) (string string))
  (let ((bytes (funcall (transport-string-encoder protocol) string)))
    (stream-write-i32 protocol (length bytes))
    (stream-write-sequence (protocol-output-transport protocol) bytes 0 nil)
    (+ 4 (length bytes))))

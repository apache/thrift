(in-package #:org.apache.thrift.implementation)

;;;; This file defines types for the `org.apache.thrift` library.
;;;;
;;;; copyright 2010 [james anderson](james.anderson@setf.de)
;;;;
;;;; Licensed to the Apache Software Foundation (ASF) under one
;;;; or more contributor license agreements. See the NOTICE file
;;;; distributed with this work for additional information
;;;; regarding copyright ownership. The ASF licenses this file
;;;; to you under the Apache License, Version 2.0 (the
;;;; "License"); you may not use this file except in compliance
;;;; with the License. You may obtain a copy of the License at
;;;;
;;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied. See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.

;;;; Define type analogues between thrift and lisp types.
;;;; The container types are defined to accept element type constraints.
;;;; Distinguish those types which are lisp/thrift homologues.
;;;; Define types for the type specifiers themselves for use at compile-time.

(deftype bool () 'boolean)
(deftype thrift:byte () '(signed-byte 8))
(deftype i8 () '(signed-byte 8))
(deftype i16 () '(signed-byte 16))
(deftype i32 () '(signed-byte 32))
(deftype i64 () '(signed-byte 64))
(deftype thrift:float ()
  "distinguish float from double for explicit struct codecs"
  'single-float)
;; string is standard
(deftype double () 'double-float)
;;; this is not what the spec says (it claims i8), but that makes no sense
(deftype binary () '(array (unsigned-byte 8) (*)))

(deftype thrift:list (&optional element-type)
  "The thrift:list container type is implemented as a cl:list. The element type
 serves for declaration, but not discrimination. An empty list should conform."
  (declare (ignore element-type))
 'list)

(deftype thrift:set (&optional element-type)
  "The thrift:set container type is implemented as a cl:list. The element type
 serves for declaration, but not discrimination. an empty set should conform."
  (declare (ignore element-type))
  'list)

(deftype thrift:map (&optional key-type value-type)
  "The thrift:map container type is implemented as a association list. The key and value types
 serve for declaration, but not discrimination. An empty map should conform."
  (declare (ignore key-type value-type))
  'list)

(deftype base-type ()
  "Indicates the union of thrift base (atomic) types."
  '(member bool thrift:byte i8 i16 i32 i64 double thrift:float string binary))

(defun base-type-p (type)
  (typep type 'base-type))

(deftype container-type () '(cons (member thrift:set thrift:list thrift:map)))

(defun container-type-p (type)
  (typep type 'container-type))

(deftype struct-type () '(cons (eql struct)))

(defun struct-type-p (type)
  (typep type 'struct-type))

(deftype enum-type () '(cons (eql enum)))

(defun enum-type-p (type)
  (typep type 'enum-type))

(deftype primitive-type () `(or base-type container-type enum-type))

(defun primitive-type-p (type)
  (typep type 'primitive-type))

(deftype thrift-type () '(or primitive-type struct-type))

(defun thrift-type-p (type)
  (typep type 'thrift-type))

(deftype enum (set-name)
  (etypecase set-name
    (symbol )
    (string (setf set-name (str-sym set-name))))
  `(member ,@(get set-name 'thrift::enum-members)))

(deftype struct (&optional identifier)
  "The exception class hierarchy is disjount for that of strucs as data."
  (etypecase identifier
    (string (str-sym identifier))
    (null '(or thrift-object thrift-error))
    (symbol identifier)))

(defparameter *container-limit* nil
  "When non-null, the integer value limits the permissible container size.")

(deftype field-size () `(satisfies field-size-p))

(defun field-size-p (x)
  "True for integers if within any asserted size limit."
  (and (integerp x)
       (>= x 0)
       (or (null *container-limit*)
           (< x *container-limit*))))

;;;
;;; type-of equivalent which is specific to thrift types

(defgeneric thrift:type-of (object)
  (:documentation "Implements an equivalent to cl:type-of, but return the most specific thrift
 type instead of the cl type. This is used to determine the encoding for dynamically generated
 messages.")

  (:method ((value null))
    'bool)
  (:method ((value (eql t)))
    'bool)
  (:method ((value integer))
    (etypecase value
      (i8 'i8)
      (i16 'i16)
      (i32 'i32)
      (i64 'i64)))
  (:method ((value float))
    "return double for all floats as the single form is non-standard"
    'double)
  (:method ((value string))
    'string)
  (:method ((value vector))
    'binary)
  (:method ((value list))
    (if (consp (first value))
      'thrift:map
      'thrift:list)))

(defgeneric type-name-class (type-name)
  (:documentation "Return the lisp type equivalent for the given thrift type.
 The value is universal. it is used to construct generic function lambda lists.
 Signal an error If no equivalent exists.")

  (:method ((type-name symbol))
    (declare (special *types-classes*))
    (or (cdr (assoc type-name *types-classes* :test #'eql))
        (error "Invalid type name: ~s." type-name)))

  (:method ((type-name cons))
    (ecase (first type-name)
      (enum 'integer)
      (struct (str-sym (second type-name)))
      ((thrift:list thrift:set) 'list)
      (thrift:map 'list))))

(defgeneric type-category (type)
  (:documentation "Return the type name to match decoded values.")

  (:method ((type symbol)) type)

  (:method ((type cons))
    (let ((first (first type)))
      (if (eql first 'thrift:enum)
          'i32
          first))))

;;;
;;; primitive constructors

(defun thrift:map (&rest pairs)
  "Represent map objects as association lists.
 NB. in order to effect equality when the keys themselves are maps, this and the transport operations
 would need to maintain a global registry."
  (if (consp (first pairs))
    pairs
    (loop for (key value) on pairs by #'cddr
          ;; nb. does not test for completeness
          collect (cons key value))))

(defun thrift:list (&rest values)
  values)

(defun thrift:set (&rest values)
  values)

;;;
;;; primitive accessors
;;; --- in prepration to support association lists as maps

(defun map-get (map key &optional default)
  "Retrieve the map entry for a given key."

  (let ((pair (assoc key map :test #'equalp)))
    (if pair
      (rest pair)
      default)))

(defun map-set (map key value)
  (let ((pair (assoc key map :test #'equalp)))
    (if pair
      (setf (rest pair) value)
      (setf map (acons key value map)))
    map))

(define-setf-expander map-get (map key &environment env)
  (multiple-value-bind (temps vals stores
                        store-form access-form)
                       (get-setf-expansion map env)
    (let ((store (gensym))
          (stemp (first stores))
          (ktemp (gensym)))
      (values (cons ktemp temps) (cons key vals) (list store)
              `(let ((,stemp (map-set ,access-form ,ktemp ,store)))
                 ,store-form
                 ,store)
              `(map-get ,access-form ,ktemp)))))

(defun map-map (function map)
  (loop for (key . value) in map
        do (funcall function key value))
  nil)

(defun map-size (map)
  (length map))

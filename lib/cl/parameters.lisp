;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: org.apache.thrift.implementation; -*-

(in-package :org.apache.thrift.implementation)

;;; This file defines global variables for the `org.apache.thrift` library.
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


(defparameter *binary-transport-types*
  '((stop . 0)
    (void . 1)
    (bool . 2)
    (thrift:byte . 3)
    (i08  . 3)
    (double . 4)
    (thrift:float . 5)                         ; this is not standard
    (i16 . 6)
    (enum . 6)
    (i32 . 8)
    (u64 . 9)
    (i64 . 10)
    (string . 11)
    (utf7   . 11)
    (binary . 11)
    (struct . 12)
    (thrift:map . 13)
    (thrift:set . 14)
    (thrift:list . 15)
    (utf8 . 16)
    (utf16 . 17)))


(defparameter *binary-message-types*
  '((call . 1)
    (reply . 2)
    (exception . 3)
    (oneway . 4)
    (unknown . -1)))

(defparameter *types-classes*
  '((stop . 0)
    (void . null)
    (bool . symbol)
    (thrift:byte . fixnum)
    (i08  . fixnum)
    (double . float)
    (i16 . fixnum)
    (enum . fixnum)
    (i32 . integer)
    (u64 . integer)
    (i64 . integer)
    (string . string)
    (utf7   . string)
    (binary . vector)
    (struct . standard-object)
    (thrift:map . cl:list)
    (thrift:set . cl:list)
    (thrift:list . cl:list)
    (utf8 . vector)
    (utf16 . vector)))

(defparameter *protocol-ex-unknown* 0)
(defparameter *protocol-ex-invalid-data* 1)
(defparameter *protocol-ex-negative-size* 2)
(defparameter *protocol-ex-size-limit* 3)
(defparameter *protocol-ex-bad-version* 4)

(defparameter *application-ex-unknown* 0)
(defparameter *application-ex-unknown-method* 1)
(defparameter *application-ex-invalid-message-type* 2)
(defparameter *application-ex-wrong-method-name* 3)
(defparameter *application-ex-bad-sequence-id* 4)
(defparameter *application-ex-missing-result* 5)

(defparameter *application-ex-unknown-field* 6)
(defparameter *application-ex-invalid-field-type* 7)

(defparameter *transport-ex-unknown* 0)
(defparameter *transport-ex-not-open* 1)
(defparameter *transport-ex-already-open* 2)
(defparameter *transport-ex-timed-out* 3)
(defparameter *transport-ex-end-of-file* 4)

(defparameter *whitespace* #(#\space #\tab #\linefeed #\return))

(defparameter *response-exception-type* 'response-exception)

;;; the thrfit class registry binds class names (_not identifiers_) to either the
;;; 
(defvar *thrift-classes* (make-hash-table :test 'eq)
  "Registers defined struct classes. This includes

  * classes with the class thrift-struct-class, as defined by def-struct
  * classes of the class thrift-exception-class, as defined by def-exception

 The keys are the symbols which as named by the class identifier in the respective IDL def-* forms.
 These respect the IDL file's package, whereby the .thrift -> .lisp translator qualifies any
 cross-referenced names explicitly, which makes the (symbol x class) relation global.
 (see str-sym and find-thrift-class.)

 The values are either the struct class itself or, in the case of exceptions, the proxy exception
 class. An exception class is keyed by the name of its respective condition and serves to
 guide code operation and/or generation. Instantiation delegates to the actual condition class.
 (see make-struct.)")


;;;
;;; floating point support


#+mcl
(unless (boundp 'double-float-positive-infinity)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defconstant double-float-positive-infinity
      (unwind-protect
        (progn
          (ccl::set-fpu-mode :division-by-zero nil)
          (funcall '/ 0d0))
        (ccl::set-fpu-mode :division-by-zero t)))
    
    (defconstant double-float-negative-infinity
      (unwind-protect
        (progn
          (ccl::set-fpu-mode :division-by-zero nil)
          (funcall '/ -0d0))
        (ccl::set-fpu-mode :division-by-zero t)))))

#+(or mcl (and clozure (not ccl-1.4)))
(unless (boundp 'double-float-nan)
  (defconstant double-float-nan
    (unwind-protect
      (locally (declare (special double-float-positive-infinity double-float-negative-infinity))
        (ccl::set-fpu-mode :invalid nil)
        (funcall '+ double-float-positive-infinity double-float-negative-infinity))
      (ccl::set-fpu-mode :invalid t))))

#+(or mcl clozure)
(unless (boundp 'single-float-positive-infinity)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defconstant single-float-positive-infinity
      (unwind-protect
        (progn
          (ccl::set-fpu-mode :division-by-zero nil)
          (funcall '/ 0s0))
        (ccl::set-fpu-mode :division-by-zero t)))
    
    (defconstant single-float-negative-infinity
      (unwind-protect
        (progn
          (ccl::set-fpu-mode :division-by-zero nil)
          (funcall '/ -0s0))
        (ccl::set-fpu-mode :division-by-zero t)))))

#+(or mcl clozure)
(unless (boundp 'single-float-nan)
  (defconstant single-float-nan
    (unwind-protect
      (locally (declare (special single-float-positive-infinity single-float-negative-infinity))
        (ccl::set-fpu-mode :invalid nil)
        (funcall '+ single-float-positive-infinity single-float-negative-infinity))
      (ccl::set-fpu-mode :invalid t))))

#+sbcl  ;; works on osx and linux
(unless (boundp 'single-float-nan)
  (sb-vm::with-float-traps-masked (:invalid)
    (defconstant single-float-nan
      (eval '(+ single-float-positive-infinity single-float-negative-infinity)))
    (defconstant double-float-nan
      (eval '(+ double-float-positive-infinity double-float-negative-infinity)))))

#+lispworks
(progn
  (defconstant double-float-positive-infinity system::*plus-infinity-double*)
  (defconstant double-float-negative-infinity system::*minus-infinity-double*)
  (defconstant single-float-positive-infinity (coerce system::*plus-infinity-double* 'single-float))
  (defconstant single-float-negative-infinity (coerce system::*minus-infinity-double* 'single-float))

  (defconstant single-float-nan (+ single-float-positive-infinity single-float-negative-infinity))
  (defconstant double-float-nan (+ double-float-positive-infinity double-float-negative-infinity))
  )

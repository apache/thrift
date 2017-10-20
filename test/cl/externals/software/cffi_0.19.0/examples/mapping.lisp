;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; mapping.lisp --- An example for mapping Lisp objects to ints.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

;;; This is an example on how to tackle the problem of passing Lisp
;;; object identifiers to foreign code.  It is not a great example,
;;; but might be useful nevertheless.
;;;
;;; Requires trivial-garbage: <http://cliki.net/trivial-garbage>

(defpackage #:cffi-mapping-test
  (:use #:common-lisp #:cffi #:trivial-garbage)
  (:export #:run))

(in-package #:cffi-mapping-test)

(define-foreign-type lisp-object-type ()
  ((weakp :initarg :weakp))
  (:actual-type :unsigned-int))

(define-parse-method lisp-object (&key weak-mapping)
  (make-instance 'lisp-object-type :weakp weak-mapping))

(defvar *regular-hashtable* (make-hash-table))
(defvar *weak-hashtable* (make-weak-hash-table :weakness :value))
(defvar *regular-counter* 0)
(defvar *weak-counter* 0)

(defun increment-counter (value)
  (mod (1+ value) (expt 2 (* 8 (foreign-type-size :unsigned-int)))))

(define-modify-macro incf-counter () increment-counter)

(defmethod translate-to-foreign (value (type lisp-object-type))
  (with-slots (weakp) type
    (let ((id (if weakp
                  (incf-counter *weak-counter*)
                  (incf-counter *regular-counter*)))
          (ht (if weakp *weak-hashtable* *regular-hashtable*)))
      (setf (gethash id ht) value)
      id)))

(defmethod translate-from-foreign (int (type lisp-object-type))
  (with-slots (weakp) type
    (gethash int (if weakp *weak-hashtable* *regular-hashtable*))))

;;;; Silly example.

(defctype weak-mapping (lisp-object :weak-mapping t))

;;; (run) => #<FUNCTION (LAMBDA (X)) {11AB46F5}>
(defun run ()
  (foreign-funcall "abs" weak-mapping (lambda (x) x) weak-mapping))

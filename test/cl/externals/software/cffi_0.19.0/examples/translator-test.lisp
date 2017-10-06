;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; translator-test.lisp --- Testing type translators.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
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

(defpackage #:cffi-translator-test
  (:use #:common-lisp #:cffi))

(in-package #:cffi-translator-test)

;;;# Verbose Pointer Translator
;;;
;;; This is a silly type translator that doesn't actually do any
;;; translating, but it prints out a debug message when the pointer is
;;; converted to/from its foreign representation.

(define-foreign-type verbose-pointer-type ()
  ()
  (:actual-type :pointer))

(defmethod translate-to-foreign (value (type verbose-pointer-type))
  (format *debug-io* "~&;; to foreign: VERBOSE-POINTER: ~S~%" value)
  value)

(defmethod translate-from-foreign (value (type verbose-pointer-type))
  (format *debug-io* "~&;; from foreign: VERBOSE-POINTER: ~S~%" value)
  value)

;;;# Verbose String Translator
;;;
;;; A VERBOSE-STRING extends VERBOSE-POINTER and converts Lisp strings
;;; C strings.  If things are working properly, both type translators
;;; should be called when converting a Lisp string to/from a C string.
;;;
;;; The translators should be called most-specific-first when
;;; translating to C, and most-specific-last when translating from C.

(define-foreign-type verbose-string-type (verbose-pointer-type)
  ()
  (:simple-parser verbose-string))

(defmethod translate-to-foreign ((s string) (type verbose-string-type))
  (let ((value (foreign-string-alloc s)))
    (format *debug-io* "~&;; to foreign: VERBOSE-STRING: ~S -> ~S~%" s value)
    (values (call-next-method value type) t)))

(defmethod translate-to-foreign (value (type verbose-string-type))
  (if (pointerp value)
      (progn
        (format *debug-io* "~&;; to foreign: VERBOSE-STRING: ~S -> ~:*~S~%" value)
        (values (call-next-method) nil))
      (error "Cannot convert ~S to a foreign string: it is not a Lisp ~
              string or pointer." value)))

(defmethod translate-from-foreign (ptr (type verbose-string-type))
  (let ((value (foreign-string-to-lisp (call-next-method))))
    (format *debug-io* "~&;; from foreign: VERBOSE-STRING: ~S -> ~S~%" ptr value)
    value))

(defmethod free-translated-object (ptr (type verbose-string-type) free-p)
  (when free-p
    (format *debug-io* "~&;; freeing VERBOSE-STRING: ~S~%" ptr)
    (foreign-string-free ptr)))

(defun test-verbose-string ()
  (foreign-funcall "getenv" verbose-string "SHELL" verbose-string))

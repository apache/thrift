;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; examples.lisp --- Simple test examples of CFFI.
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

(defpackage #:cffi-examples
  (:use #:cl #:cffi)
  (:export
   #:run-examples
   #:sqrtf
   #:getenv))

(in-package #:cffi-examples)

;; A simple libc function.
(defcfun "sqrtf" :float
  (n :float))

;; This definition uses the STRING type translator to automatically
;; convert Lisp strings to foreign strings and vice versa.
(defcfun "getenv" :string
  (name :string))

;; Calling a varargs function.
(defun sprintf-test ()
  "Test calling a varargs function."
  (with-foreign-pointer-as-string ((buf buf-size) 255)
    (foreign-funcall
     "snprintf" :pointer buf :int buf-size
     :string "%d %f #x%x!" :int 666
     :double (coerce pi 'double-float)
     :unsigned-int #xcafebabe
     :void)))

;; Defining an emerated type.
(defcenum test-enum
  (:invalid 0)
  (:positive 1)
  (:negative -1))

;; Use the absolute value function to test keyword/enum translation.
(defcfun ("abs" c-abs) test-enum
  (n test-enum))

(defun cffi-version ()
  (asdf:component-version (asdf:find-system 'cffi)))

(defun run-examples ()
  (format t "~&;;; CFFI version ~A on ~A ~A:~%"
          (cffi-version) (lisp-implementation-type)
          (lisp-implementation-version))
  (format t "~&;; shell:             ~A~%" (getenv "SHELL"))
  (format t "~&;; sprintf test:      ~A~%" (sprintf-test))
  (format t "~&;; (c-abs :positive): ~A~%" (c-abs :positive))
  (format t "~&;; (c-abs :negative): ~A~%" (c-abs :negative))
  (force-output))

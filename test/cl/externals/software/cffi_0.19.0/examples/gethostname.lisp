;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; gethostname.lisp --- A simple CFFI example.
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

;;;# CFFI Example: gethostname binding
;;;
;;; This is a very simple CFFI example that illustrates calling a C
;;; function that fills in a user-supplied string buffer.

(defpackage #:cffi-example-gethostname
  (:use #:common-lisp #:cffi)
  (:export #:gethostname))

(in-package #:cffi-example-gethostname)

;;; Define the Lisp function %GETHOSTNAME to call the C 'gethostname'
;;; function, which will fill BUF with up to BUFSIZE characters of the
;;; system's hostname.
(defcfun ("gethostname" %gethostname) :int
  (buf :pointer)
  (bufsize :int))

;;; Define a Lispy interface to 'gethostname'.  The utility macro
;;; WITH-FOREIGN-POINTER-AS-STRING is used to allocate a temporary
;;; buffer and return it as a Lisp string.
(defun gethostname ()
  (with-foreign-pointer-as-string ((buf bufsize) 255)
    (%gethostname buf bufsize)))

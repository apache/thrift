;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; init.lisp --- Load libffi and define basics
;;;
;;; Copyright (C) 2009, 2010, 2011 Liam Healy  <lhealy@common-lisp.net>
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

(in-package #:cffi)

;;; See file:///usr/share/doc/libffi-dev/html/The-Basics.html#The-Basics

(defcfun ("ffi_prep_cif" libffi/prep-cif) status
  (ffi-cif :pointer)
  (ffi-abi abi)
  (nargs :uint)
  (rtype :pointer)
  (argtypes :pointer))

(defcfun ("ffi_call" libffi/call) :void
  (ffi-cif :pointer)
  (function :pointer)
  (rvalue :pointer)
  (avalues :pointer))

;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-libffi.asd --- Foreign Structures By Value
;;;
;;; Copyright (C) 2011 Liam M. Healy
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

(in-package :asdf)

(eval-when (:compile-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel)
  (asdf:oos 'asdf:load-op :trivial-features))

(defsystem cffi-libffi
  :description "Foreign structures by value"
  :author "Liam Healy <lhealy@common-lisp.net>"
  :maintainer "Liam Healy <lhealy@common-lisp.net>"
  :defsystem-depends-on (#:trivial-features #:cffi-grovel)
  :components
  ((:module libffi
    :serial t
    :components
    ((:file "libffi")
     (cffi-grovel:grovel-file "libffi-types")
     (:file "libffi-functions")
     (:file "type-descriptors")
     (:file "funcall"))))
  :depends-on (#:cffi #:cffi-grovel #:trivial-features))

;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp --- Toolchain DEFPACKAGE.
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

(uiop:define-package #:cffi-toolchain
  (:mix #:asdf #:uiop #:common-lisp)
  (:import-from #:asdf/bundle
   #:link-op #:bundle-pathname-type #:bundle-type
   #:gather-operation #:gather-type)
  (:export
   ;; Variables
   #:*cc* #:*cc-flags*
   #:*ld* #:*ld-exe-flags* #:*ld-dll-flags*
   #:*linkkit-start* #:*linkkit-end*
   ;; Functions from c-toolchain
   #:make-c-file-name #:make-o-file-name
   #:make-so-file-name #:make-exe-file-name
   #:parse-command-flags #:parse-command-flags-list
   #:invoke #:invoke-build #:cc-compile
   #:link-static-library #:link-shared-library
   #:link-executable #:link-lisp-executable
   ;; ASDF classes
   #:c-file #:o-file
   #:static-runtime-op #:static-image-op #:static-program-op
   ))

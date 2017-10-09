;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; Copyright (C) 2015, Attila Lendvai <attila@lendvai.name>
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

(uiop:define-package #:cffi/c2ffi
  (:mix #:uiop
        #:alexandria
        #:common-lisp)
  (:import-from :asdf
                #:cl-source-file
                #:output-file
                #:output-files
                #:input-files
                #:perform
                #:compile-op
                #:load-op
                #:load-source-op
                #:prepare-op
                #:component-pathname
                #:component-depends-on
                #:downward-operation
                #:load-system
                #:component-loaded-p)
  (:export
   #:c2ffi-file
   #:default-ffi-name-transformer
   #:default-ffi-type-transformer
   #:change-case-to-readtable-case
   #:camelcased?
   #:camelcase-to-dash-separated
   #:maybe-camelcase-to-dash-separated))

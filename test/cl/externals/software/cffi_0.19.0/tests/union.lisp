;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; union.lisp --- Tests on C unions.
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

(in-package #:cffi-tests)

(defcunion uint32-bytes
  (int-value :unsigned-int)
  (bytes :unsigned-char :count 4))

(defctype uint32-bytes (:union uint32-bytes))

(defun int-to-bytes (n)
  "Convert N to a list of bytes using a union."
  (with-foreign-object (obj 'uint32-bytes)
    (setf (foreign-slot-value obj 'uint32-bytes 'int-value) n)
    (loop for i from 0 below 4
          collect (mem-aref
                   (foreign-slot-value obj 'uint32-bytes 'bytes)
                   :unsigned-char i))))

(deftest union.1
    (let ((bytes (int-to-bytes #x12345678)))
      (cond ((equal bytes '(#x12 #x34 #x56 #x78))
             t)
            ((equal bytes '(#x78 #x56 #x34 #x12))
             t)
            (t bytes)))
  t)

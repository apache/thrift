;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; grovel.lisp --- CFFI-Grovel tests.
;;;
;;; Copyright (C) 2014, Luis Oliveira  <loliveira@common-lisp.net>
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

(deftest %invoke
    (cffi-grovel::invoke "echo" "test")
  nil nil 0)

(defun bug-1395242-helper (enum-type base-type constant-name)
  (check-type enum-type (member constantenum cenum))
  (check-type base-type string)
  (check-type constant-name string)
  (let ((enum-name (intern (symbol-name (gensym))))
        (base-type-name (intern (symbol-name (gensym)))))
    (uiop:with-temporary-file (:stream grovel-stream :pathname grovel-file)
      ;; Write the grovel file
      (with-open-stream (*standard-output* grovel-stream)
        (write `(ctype ,base-type-name ,base-type))
        (write `(,enum-type (,enum-name :base-type ,base-type-name)
                            ((:value ,constant-name)))))
      ;; Get the value of :inaddr-broadcast
      (let ((lisp-file (cffi-grovel:process-grovel-file grovel-file)))
        (unwind-protect
             (progn
               (load lisp-file)
               (cffi:foreign-enum-value enum-name :value))
          (uiop/filesystem:delete-file-if-exists lisp-file))))))

(deftest bug-1395242
    (labels
        ((process-expression (expression)
           (loop for enum-type in '(constantenum cenum)
                 always (destructuring-bind (base-type &rest evaluations) expression
                          (loop for (name expected-value) in evaluations
                                for actual-value = (bug-1395242-helper enum-type base-type name)
                                always (or (= expected-value actual-value)
                                           (progn
                                             (format *error-output*
                                                     "Test failed for case: ~A, ~A, ~A (expected ~A, actual ~A)~%"
                                                     enum-type base-type name expected-value actual-value)
                                             nil)))))))
      (every #'process-expression
             '(("uint8_t" ("UINT8_MAX" 255) ("INT8_MAX" 127) ("INT8_MIN" 128))
               ("int8_t" ("INT8_MIN" -128) ("INT8_MAX" 127) ("UINT8_MAX" -1))
               ("uint16_t" ("UINT16_MAX" 65535) ("INT8_MIN" 65408))
               ("int16_t" ("INT16_MIN" -32768) ("INT16_MAX" 32767) ("UINT16_MAX" -1))
               ("uint32_t" ("UINT32_MAX" 4294967295) ("INT8_MIN" 4294967168))
               ("int32_t" ("INT32_MIN" -2147483648) ("INT32_MAX" 2147483647)))))
  t)

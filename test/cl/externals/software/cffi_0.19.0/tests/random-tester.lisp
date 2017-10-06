;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; random-tester.lisp --- Random test generator.
;;;
;;; Copyright (C) 2006, Luis Oliveira  <loliveira(@)common-lisp.net>
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

;;; This code was used to generate the C and Lisp source code for
;;; the CALLBACKS.BFF.[12] and DEFCFUN.BFF.[12] tests.
;;;
;;; The original idea was to test all combinations of argument types
;;; but obviously as soon as you do the maths that it's not quite
;;; feasable for more that 4 or 5 arguments.
;;;
;;; TODO: actually run random tests, ie compile/load/run the tests
;;; this code can generate.

(defpackage #:cffi-random-tester
  (:use #:cl #:cffi #:alexandria #:regression-test))
(in-package #:cffi-random-tester)

(defstruct (c-type (:conc-name type-))
  keyword
  name
  abbrev
  min
  max)

(defparameter +types+
  (mapcar (lambda (type)
            (let ((keyword (first type))
                  (name (second type)))
              (multiple-value-bind (min max)
                  ;; assume we can represent an integer in the range
                  ;; [-2^16 2^16-1] in a float/double without causing
                  ;; rounding errors (probably a lame assumption)
                  (let ((type-size (if (or (eq keyword :float)
                                           (eq keyword :double))
                                       16
                                       (* 8 (foreign-type-size keyword)))))
                    (if (or (eql (char name 0) #\u) (eq keyword :pointer))
                        (values 0 (1- (expt 2 type-size)))
                        (values (- (expt 2 (1- type-size)))
                                (1- (expt 2 (1- type-size))))))
                (make-c-type :keyword keyword :name name :abbrev (third type)
                             :min min :max max))))
          '((:char "char" "c")
            (:unsigned-char "unsigned char" "uc")
            (:short "short" "s")
            (:unsigned-short "unsigned short" "us")
            (:int "int" "i")
            (:unsigned-int "unsigned int" "ui")
            (:long "long" "l")
            (:unsigned-long "unsigned long" "ul")
            (:float "float" "f")
            (:double "double" "d")
            (:pointer "void*" "p")
            (:long-long "long long" "ll")
            (:unsigned-long-long "unsigned long long" "ull"))))

(defun find-type (keyword)
  (find keyword +types+ :key #'type-keyword))

(defun n-random-types (n)
  (loop repeat n collect (nth (random (length +types+)) +types+)))

;;; same as above, without the long long types
(defun n-random-types-no-ll (n)
  (loop repeat n collect (nth (random (- (length +types+) 2)) +types+)))

(defun random-range (x y)
  (+ x (random (+ (- y x) 2))))

(defun random-sum (rettype arg-types)
  "Returns a list of integers that fit in the respective types in the
ARG-TYPES list and whose sum fits in RETTYPE."
  (loop with sum = 0
        for type in arg-types
        for x = (random-range (max (- (type-min rettype) sum) (type-min type))
                              (min (- (type-max rettype) sum) (type-max type)))
        do (incf sum x)
        collect x))

(defun combinations (n items)
  (let ((combs '()))
    (labels ((rec (n accum)
               (if (= n 0)
                   (push accum combs)
                   (loop for item in items
                         do (rec (1- n) (cons item accum))))))
      (rec n '())
      combs)))

(defun function-name (rettype arg-types)
  (format nil "sum_~A_~{_~A~}"
          (type-abbrev rettype)
          (mapcar #'type-abbrev arg-types)))

(defun c-function (rettype arg-types)
  (let ((args (loop for type in arg-types and i from 1
                    collect (list (type-name type) (format nil "a~A" i)))))
    (format t "DLLEXPORT ~A ~A(~{~{~A ~A~}~^, ~})~%~
               { return ~A(~A) ~{~A~^ + ~}~A; }"
            (type-name rettype) (function-name rettype arg-types) args
            (if (eq (type-keyword rettype) :pointer)
                "(void *)((unsigned int)("
                "")
            (type-name rettype)
            (loop for arg-pair in args collect
                  (format nil "~A~A~A"
                          (cond ((string= (first arg-pair) "void*")
                                 "(unsigned int) ")
                                ((or (string= (first arg-pair) "double")
                                     (string= (first arg-pair) "float"))
                                 "((int) ")
                                (t ""))
                          (second arg-pair)
                          (if (member (first arg-pair)
                                      '("void*" "double" "float")
                                      :test #'string=)
                              ")"
                              "")))
            (if (eq (type-keyword rettype) :pointer) "))" ""))))

(defun c-callback (rettype arg-types args)
  (format t "DLLEXPORT ~A call_~A(~A (*func)(~{~A~^, ~}~^))~%~
             { return func(~{~A~^, ~}); }"
          (type-name rettype) (function-name rettype arg-types)
          (type-name rettype) (mapcar #'type-name arg-types)
          (loop for type in arg-types and value in args collect
                (format nil "~A~A"
                        (if (eq (type-keyword type) :pointer)
                            "(void *) "
                            "")
                        value))))

;;; (output-c-code #p"generated.c" 3 5)
(defun output-c-code (file min max)
  (with-open-file (stream file :direction :output :if-exists :error)
    (let ((*standard-output* stream))
      (format t "/* automatically generated functions and callbacks */~%~%")
      (loop for n from min upto max do
            (format t "/* ~A args */" (1- n))
            (loop for comb in (combinations n +types+) do
                  (terpri) (c-function (car comb) (cdr comb))
                  (terpri) (c-callback (car comb) (cdr comb)))))))

(defmacro with-conversion (type form)
  (case type
    (:double `(float ,form 1.0d0))
    (:float `(float ,form))
    (:pointer `(make-pointer ,form))
    (t form)))

(defun integer-conversion (type form)
  (case type
    ((:double :float) `(values (floor ,form)))
    (:pointer `(pointer-address ,form))
    (t form)))

(defun gen-arg-values (rettype arg-types)
  (let ((numbers (random-sum rettype arg-types)))
    (values
     (reduce #'+ numbers)
     (loop for type in arg-types and n in numbers
           collect (case (type-keyword type)
                     (:double (float n 1.0d0))
                     (:float (float n))
                     (:pointer `(make-pointer ,n))
                     (t n))))))

(defun gen-function-test (rettype arg-types)
  (let* ((fun-name (function-name rettype arg-types))
         (fun-sym (cffi::lisp-function-name fun-name)))
    (multiple-value-bind (sum value-forms)
        (gen-arg-values rettype arg-types)
    `(progn
       (defcfun (,fun-name ,fun-sym) ,(type-keyword rettype)
         ,@(loop for type in arg-types and i from 1 collect
                 (list (symbolicate '#:a (format nil "~A" i))
                       (type-keyword type))))
       (deftest ,(symbolicate '#:defcfun. fun-sym)
           ,(integer-conversion (type-keyword rettype)
                                `(,fun-sym ,@value-forms))
         ,sum)))))

(defun gen-callback-test (rettype arg-types sum)
  (let* ((fname (function-name rettype arg-types))
         (cb-sym (cffi::lisp-function-name fname))
         (fun-name (concatenate 'string "call_" fname))
         (fun-sym (cffi::lisp-function-name fun-name))
         (arg-names (loop for i from 1 upto (length arg-types) collect
                          (symbolicate '#:a (format nil "~A" i)))))
    `(progn
       (defcfun (,fun-name ,fun-sym) ,(type-keyword rettype) (cb :pointer))
       (defcallback ,cb-sym ,(type-keyword rettype)
           ,(loop for type in arg-types and name in arg-names
                  collect (list name (type-keyword type)))
         ,(integer-conversion
           (type-keyword rettype)
           `(+ ,@(mapcar (lambda (tp n)
                           (integer-conversion (type-keyword tp) n))
                         arg-types arg-names))))
       (deftest ,(symbolicate '#:callbacks. cb-sym)
           ,(integer-conversion (type-keyword rettype)
                                `(,fun-sym (callback ,cb-sym)))
         ,sum))))

(defun cb-test (&key no-long-long)
  (let* ((rettype (find-type (if no-long-long :long :long-long)))
         (arg-types (if no-long-long
                        (n-random-types-no-ll 127)
                        (n-random-types 127)))
         (args (random-sum rettype arg-types))
         (sum (reduce #'+ args)))
    (c-callback rettype arg-types args)
    (gen-callback-test rettype arg-types sum)))

;; (defmacro define-function-and-callback-tests (min max)
;;   `(progn
;;      ,@(loop for n from min upto max appending
;;              (loop for comb in (combinations n +types+)
;;                    collect (gen-function-test (car comb) (cdr comb))
;;                    collect (gen-callback-test (car comb) (cdr comb))))))

;; (define-function-and-callback-tests 3 5)

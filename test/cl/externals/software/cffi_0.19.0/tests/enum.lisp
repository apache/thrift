;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enum.lisp --- Tests on C enums.
;;;
;;; Copyright (C) 2005-2006, Luis Oliveira  <loliveira(@)common-lisp.net>
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

(defctype numeros-base-type :int)

(defcenum (numeros numeros-base-type)
  (:one 1)
  :two
  :three
  :four
  (:forty-one 41)
  :forty-two)

(defcfun "check_enums" :int
  (%one numeros)
  (%two numeros)
  (%three numeros)
  (%four numeros)
  (%forty-one numeros)
  (%forty-two numeros))

(deftest enum.1
    (check-enums :one :two :three 4 :forty-one :forty-two)
  1)

(defcenum another-boolean :false :true)
(defcfun "return_enum" another-boolean (x :int))

(deftest enum.2
    (and (eq :false (return-enum 0))
         (eq :true (return-enum 1)))
  t)

(defctype yet-another-boolean another-boolean)
(defcfun ("return_enum" return-enum2) yet-another-boolean
  (x yet-another-boolean))

(deftest enum.3
    (and (eq :false (return-enum2 :false))
         (eq :true (return-enum2 :true)))
  t)

(defctype numeros-typedef numeros)

(deftest enum.typedef.1
    (eq (foreign-enum-keyword 'numeros-typedef 1)
        (foreign-enum-keyword 'numeros 1))
  t)

(deftest enum.typedef.2
    (eql (foreign-enum-value 'numeros-typedef :four)
         (foreign-enum-value 'numeros :four))
  t)

(defcenum enum-size.int
  (:one 1)
  (enum-size-int #.(1- (expt 2 (1- (* (foreign-type-size :unsigned-int) 8)))))
  (enum-size-negative-int #.(- (1- (expt 2 (1- (* (foreign-type-size :unsigned-int) 8))))))
  (:two 2))

(defcenum enum-size.uint
  (:one 1)
  (enum-size-uint #.(1- (expt 2 (* (foreign-type-size :unsigned-int) 8))))
  (:two 2))

(deftest enum.size
    (mapcar (alexandria:compose 'cffi::unparse-type
                                'cffi::actual-type
                                'cffi::parse-type)
            (list 'enum-size.int
                  'enum-size.uint))
  ;; The C standard only has weak constraints on the size of integer types, so
  ;; we cannot really test more than one type in a platform independent way due
  ;; to the possible overlaps.
  (:int
   :unsigned-int))

(deftest enum.size.members
    (mapcar (alexandria:conjoin 'boundp 'constantp)
            '(enum-size-int enum-size-negative-int enum-size-uint))
  (t t t))

(deftest enum.size.error-when-too-large
    (expecting-error
      (eval '(defcenum enum-size-too-large
              (:too-long #.(expt 2 129)))))
  :error)

;; There are some projects that use non-integer base type. It's not in
;; adherence with the C standard, but we also don't lose much by
;; allowing it.
(defcenum (enum.double :double)
  (:one 1)
  (:two 2d0)
  (:three 3.42)
  :four)

(deftest enum.double
    (values-list
     (mapcar (alexandria:curry 'foreign-enum-value 'enum.double)
             '(:one :two :three :four)))
  1
  2.0d0
  3.42
  4.42)

;;;# Bitfield tests

;;; Regression test: defbitfield was misbehaving when the first value
;;; was provided.
(deftest bitfield.1
    (eval '(defbitfield (bf1 :long)
             (:foo 0)))
  bf1)

(defbitfield bf2
  one
  two
  four
  eight
  sixteen
  (bf2.outlier 42)
  thirty-two
  sixty-four)

(deftest bitfield.2
    (mapcar (lambda (symbol)
              (foreign-bitfield-value 'bf2 (list symbol)))
            '(one two four eight sixteen thirty-two sixty-four))
  (1 2 4 8 16 32 64))

(deftest bitfield.2.outlier
    (mapcar (lambda (symbol)
              (foreign-bitfield-value 'bf2 (list symbol)))
            '(one two four eight sixteen thirty-two sixty-four))
  (1 2 4 8 16 32 64))

(defbitfield (bf3 :int)
  (three 3)
  one
  (seven 7)
  two
  (eight 8)
  sixteen)

;;; Non-single-bit numbers must not influence the progression of
;;; implicit values.  Single bits larger than any before *must*
;;; influence said progression.
(deftest bitfield.3
    (mapcar (lambda (symbol)
              (foreign-bitfield-value 'bf3 (list symbol)))
            '(one two sixteen))
  (1 2 16))

(defbitfield bf4
  ;; zero will be a simple enum member because it's not a valid mask
  (zero 0)
  one
  two
  four
  (three 3)
  (sixteen 16))

;;; Yet another edge case with the 0...
(deftest bitfield.4
    ;; These should macroexpand to the literals in Slime
    ;; due to the compiler macros. Same below.
    (values (foreign-bitfield-value 'bf4 ())
            (foreign-bitfield-value 'bf4 'one)
            (foreign-bitfield-value 'bf4 '(one two))
            (foreign-bitfield-value 'bf4 '(three)) ; or should it signal an error?
            (foreign-bitfield-value 'bf4 '(sixteen)))
  0
  1
  3
  3
  16)

(deftest bitfield.4b
    (values (foreign-bitfield-symbols 'bf4 0)
            (foreign-bitfield-symbols 'bf4 1)
            (foreign-bitfield-symbols 'bf4 3)
            (foreign-bitfield-symbols 'bf4 8)
            (foreign-bitfield-symbols 'bf4 16))
  nil
  (one)
  (one two)
  nil
  (sixteen))

(deftest bitfield.translators
    (with-foreign-object (bf 'bf4 2)
      (setf (mem-aref bf 'bf4 0) 1)
      (setf (mem-aref bf 'bf4 1) 3)
      (values (mem-aref bf 'bf4 0)
              (mem-aref bf 'bf4 1)))
  (one)
  (one two))

#+nil
(deftest bitfield.base-type-error
    (expecting-error
      (eval '(defbitfield (bf1 :float)
              (:foo 0))))
  :error)

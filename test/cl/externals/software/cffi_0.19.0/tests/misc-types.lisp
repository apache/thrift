;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; misc-types.lisp --- Various tests on the type system.
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

(defcfun ("my_strdup" strdup) :string+ptr (str :string))

(defcfun ("my_strfree" strfree) :void (str :pointer))

(deftest misc-types.string+ptr
    (destructuring-bind (string pointer)
        (strdup "foo")
      (strfree pointer)
      string)
  "foo")

#-(and)
(deftest misc-types.string+ptr.ub8
    (destructuring-bind (string pointer)
        (strdup (make-array 3 :element-type '(unsigned-byte 8)
                            :initial-contents (map 'list #'char-code "foo")))
      (strfree pointer)
      string)
  "foo")

#-(and)
(deftest misc-types.string.ub8.1
    (let ((array (make-array 7 :element-type '(unsigned-byte 8)
                             :initial-contents '(84 117 114 97 110 103 97))))
      (with-foreign-string (foreign-string array)
        (foreign-string-to-lisp foreign-string)))
  "Turanga")

#-(and)
(deftest misc-types.string.ub8.2
    (let ((str (foreign-string-alloc
                (make-array 7 :element-type '(unsigned-byte 8)
                            :initial-contents '(84 117 114 97 110 103 97)))))
      (prog1 (foreign-string-to-lisp str)
        (foreign-string-free str)))
  "Turanga")

(defcfun "equalequal" :boolean
  (a (:boolean :int))
  (b (:boolean :unsigned-int)))

(defcfun "bool_and" (:boolean :char)
  (a (:boolean :unsigned-char))
  (b (:boolean :char)))

(defcfun "bool_xor" (:boolean :unsigned-long)
  (a (:boolean :long))
  (b (:boolean :unsigned-long)))

(deftest misc-types.boolean.1
    (list (equalequal nil nil)
          (equalequal t t)
          (equalequal t 23)
          (bool-and 'a 'b)
          (bool-and "foo" nil)
          (bool-xor t nil)
          (bool-xor nil nil))
  (t t t t nil t nil))

(defcfun "sizeof_bool" :unsigned-int)

(deftest misc-types.sizeof.bool
    (eql (sizeof-bool) (foreign-type-size :bool))
  t)

(defcfun "bool_to_unsigned" :unsigned-int
  (b :bool))

(defcfun "unsigned_to_bool" :bool
  (u :unsigned-int))

(deftest misc-types.bool.convert-to-foreign.mem
    (loop for v in '(nil t)
          collect
          (with-foreign-object (b :bool)
            (setf (mem-ref b :bool) v)
            (mem-ref b #.(cffi::canonicalize-foreign-type :bool))))
  (0 1))

(deftest misc-types.bool.convert-to-foreign.call
    (mapcar #'bool-to-unsigned '(nil t))
  (0 1))

(deftest misc-types.bool.convert-from-foreign.mem
    (loop for v in '(0 1 42)
          collect
          (with-foreign-object (b :bool)
            (setf (mem-ref b #.(cffi::canonicalize-foreign-type :bool)) v)
            (mem-ref b :bool)))
  (nil t t))

(deftest misc-types.bool.convert-from-foreign.call
    (mapcar #'unsigned-to-bool '(0 1 42))
  (nil t t))

;;; Regression test: boolean type only worked with canonicalized
;;; built-in integer types. Should work for any type that canonicalizes
;;; to a built-in integer type.
(defctype int-for-bool :int)
(defcfun ("equalequal" equalequal2) :boolean
  (a (:boolean int-for-bool))
  (b (:boolean :uint)))

(deftest misc-types.boolean.2
    (equalequal2 nil t)
  nil)

(defctype my-string :string+ptr)

(defun funkify (str)
  (concatenate 'string "MORE " (string-upcase str)))

(defun 3rd-person (value)
  (list (concatenate 'string "Strdup says: " (first value))
        (second value)))

;; (defctype funky-string
;;     (:wrapper my-string
;;               :to-c #'funkify
;;               :from-c (lambda (value)
;;                         (list
;;                          (concatenate 'string "Strdup says: "
;;                                       (first value))
;;                          (second value))))
;;   "A useful type.")

(defctype funky-string (:wrapper my-string :to-c funkify :from-c 3rd-person))

(defcfun ("my_strdup" funky-strdup) funky-string
  (str funky-string))

(deftest misc-types.wrapper
    (destructuring-bind (string ptr)
        (funky-strdup "code")
      (strfree ptr)
      string)
  "Strdup says: MORE CODE")

(deftest misc-types.sized-ints
    (mapcar #'foreign-type-size
            '(:int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64))
  (1 1 2 2 4 4 8 8))

(define-foreign-type error-error ()
  ()
  (:actual-type :int)
  (:simple-parser error-error))

(defmethod translate-to-foreign (value (type error-error))
  (declare (ignore value))
  (error "translate-to-foreign invoked."))

(defmethod translate-from-foreign (value (type error-error))
  (declare (ignore value))
  (error "translate-from-foreign invoked."))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmethod expand-to-foreign (value (type error-error))
    value)

  (defmethod expand-from-foreign (value (type error-error))
    value))

(defcfun ("abs" expand-abs) error-error
  (n error-error))

(defcvar ("var_int" *expand-var-int*) error-error)

(defcfun ("expect_int_sum" expand-expect-int-sum) :boolean
  (cb :pointer))

(defcallback expand-int-sum error-error ((x error-error) (y error-error))
  (+ x y))

;;; Ensure that macroexpansion-time translators are called where this
;;; is guaranteed (defcfun, defcvar, foreign-funcall and defcallback)
(deftest misc-types.expand.1
    (expand-abs -1)
  1)

#-cffi-sys::no-foreign-funcall
(deftest misc-types.expand.2
    (foreign-funcall "abs" error-error -1 error-error)
  1)

(deftest misc-types.expand.3
    (let ((old (mem-ref (get-var-pointer '*expand-var-int*) :int)))
      (unwind-protect
           (progn
             (setf *expand-var-int* 42)
             *expand-var-int*)
        (setf (mem-ref (get-var-pointer '*expand-var-int*) :int) old)))
  42)

(deftest misc-types.expand.4
    (expand-expect-int-sum (callback expand-int-sum))
  t)

(define-foreign-type translate-tracker ()
  ()
  (:actual-type :int)
  (:simple-parser translate-tracker))

(declaim (special .fto-called.))

(defmethod free-translated-object (value (type translate-tracker) param)
  (declare (ignore value param))
  (setf .fto-called. t))

(define-foreign-type expand-tracker ()
  ()
  (:actual-type :int)
  (:simple-parser expand-tracker))

(defmethod free-translated-object (value (type expand-tracker) param)
  (declare (ignore value param))
  (setf .fto-called. t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod expand-to-foreign (value (type expand-tracker))
    (declare (ignore value))
    (call-next-method)))

(defcfun ("abs" ttracker-abs) :int
  (n translate-tracker))

(defcfun ("abs" etracker-abs) :int
  (n expand-tracker))

;; free-translated-object must be called when there is no etf
(deftest misc-types.expand.5
    (let ((.fto-called. nil))
      (ttracker-abs -1)
      .fto-called.)
  t)

;; free-translated-object must be called when there is an etf, but
;; they answer *runtime-translator-form*
(deftest misc-types.expand.6
    (let ((.fto-called. nil))
      (etracker-abs -1)
      .fto-called.)
  t)

(define-foreign-type misc-type.expand.7 ()
  ()
  (:actual-type :int)
  (:simple-parser misc-type.expand.7))

(defmethod translate-to-foreign (value (type misc-type.expand.7))
  (values value 'second-value))

;; Auxiliary function to test CONVERT-TO-FOREIGN's compiler macro.
(defun misc-type.expand.7-aux ()
  (convert-to-foreign "foo" 'misc-type.expand.7))

;; Checking that expand-to-foreign doesn't ignore the second value of
;; translate-to-foreign.
(deftest misc-type.expand.7
    (misc-type.expand.7-aux)
  "foo" second-value)

;; Like MISC-TYPE.EXPAND.7 but doesn't depend on compiler macros
;; kicking in.
(deftest misc-type.expand.8
    (eval (expand-to-foreign "foo" (cffi::parse-type 'misc-type.expand.7)))
  "foo" second-value)

;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; funcall.lisp --- Tests function calling.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2005-2007, Luis Oliveira  <loliveira@common-lisp.net>
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

;;;# Calling with Built-In C Types
;;;
;;; Tests calling standard C library functions both passing and
;;; returning each built-in type.

;;; Don't run these tests if the implementation does not support
;;; foreign-funcall.
#-cffi-sys::no-foreign-funcall
(progn

(deftest funcall.char
    (foreign-funcall "toupper" :char (char-code #\a) :char)
  #.(char-code #\A))

(deftest funcall.int.1
    (foreign-funcall "abs" :int -100 :int)
  100)

(defun funcall-abs (n)
  (foreign-funcall "abs" :int n :int))

;;; regression test: lispworks's %foreign-funcall based on creating
;;; and caching foreign-funcallables at macro-expansion time.
(deftest funcall.int.2
    (funcall-abs -42)
  42)

(deftest funcall.long
    (foreign-funcall "labs" :long -131072 :long)
  131072)

#-cffi-sys::no-long-long
(deftest funcall.long-long
    (foreign-funcall "my_llabs" :long-long -9223372036854775807 :long-long)
  9223372036854775807)

#-cffi-sys::no-long-long
(deftest funcall.unsigned-long-long
    (let ((ullong-max (1- (expt 2 (* 8 (foreign-type-size :unsigned-long-long))))))
      (eql ullong-max
           (foreign-funcall "ullong" :unsigned-long-long ullong-max
                                     :unsigned-long-long)))
  t)

(deftest funcall.float
    (foreign-funcall "my_sqrtf" :float 16.0 :float)
  4.0)

(deftest funcall.double
    (foreign-funcall "sqrt" :double 36.0d0 :double)
  6.0d0)

#+(and scl long-float)
(deftest funcall.long-double
    (foreign-funcall "sqrtl" :long-double 36.0l0 :long-double)
  6.0l0)

(deftest funcall.string.1
    (foreign-funcall "strlen" :string "Hello" :int)
  5)

(deftest funcall.string.2
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "strcpy" :pointer s :string "Hello" :pointer)
      (foreign-funcall "strcat" :pointer s :string ", world!" :pointer))
  "Hello, world!")

(deftest funcall.string.3
    (with-foreign-pointer (ptr 100)
      (lisp-string-to-foreign "Hello, " ptr 8)
      (foreign-funcall "strcat" :pointer ptr :string "world!" :string))
  "Hello, world!")

;;;# Calling Varargs Functions

;; The CHAR argument must be passed as :INT because chars are promoted
;; to ints when passed as variable arguments.
(deftest funcall.varargs.char
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%c" :int 65 :int))
  "A")

(deftest funcall.varargs.int
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%d" :int 1000 :int))
  "1000")

(deftest funcall.varargs.long
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%ld" :long 131072 :int))
  "131072")

;;; There is no FUNCALL.VARARGS.FLOAT as floats are promoted to double
;;; when passed as variable arguments.  Currently this fails in SBCL
;;; and CMU CL on Darwin/ppc.
(deftest funcall.varargs.double
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%.2f"
                       :double (coerce pi 'double-float) :int))
  "3.14")

#+(and scl long-float)
(deftest funcall.varargs.long-double
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%.2Lf"
                       :long-double pi :int))
  "3.14")

(deftest funcall.varargs.string
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%s, %s!"
                       :string "Hello" :string "world" :int))
  "Hello, world!")

;;; See DEFCFUN.DOUBLE26.
(deftest funcall.double26
    (foreign-funcall "sum_double26"
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double)
  81.64d0)

;;; See DEFCFUN.FLOAT26.
(deftest funcall.float26
    (foreign-funcall "sum_float26"
                     :float 5.0 :float 5.0 :float 5.0 :float 5.0 :float 5.0
                     :float 5.0 :float 5.0 :float 5.0 :float 5.0 :float 5.0
                     :float 5.0 :float 5.0 :float 5.0 :float 5.0 :float 5.0
                     :float 5.0 :float 5.0 :float 5.0 :float 5.0 :float 5.0
                     :float 5.0 :float 5.0 :float 5.0 :float 5.0 :float 5.0
                     :float 5.0 :float)
  130.0)

;;; Funcalling a pointer.
(deftest funcall.f-s-p.1
    (foreign-funcall-pointer (foreign-symbol-pointer "abs") nil :int -42 :int)
  42)

;;;# Namespaces

#-cffi-sys::flat-namespace
(deftest funcall.namespace.1
    (values (foreign-funcall ("ns_function" :library libtest) :boolean)
            (foreign-funcall ("ns_function" :library libtest2) :boolean))
  t nil)

;;;# stdcall

#+(and x86 windows (not cffi-sys::no-stdcall))
(deftest funcall.stdcall.1
    (flet ((fun ()
             (foreign-funcall ("stdcall_fun@12" :convention :stdcall)
                              :int 1 :int 2 :int 3 :int)))
      (loop repeat 100 do (fun)
            finally (return (fun))))
  6)

;;; RT: NIL arguments are skipped

(defvar *nil-skipped*)

(define-foreign-type check-nil-skip-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser check-nil-skip-type))

(defmethod expand-to-foreign (val (type check-nil-skip-type))
  (declare (ignore val))
  (setf *nil-skipped* nil)
  (null-pointer))

(deftest funcall.nil-skip
    (let ((*nil-skipped* t))
      (compile nil '(lambda ()
                     (foreign-funcall "abs" check-nil-skip-type nil)))
      *nil-skipped*)
  nil)

;;; RT: CLISP returns NIL instead of a null-pointer

(deftest funcall.pointer-not-nil
    (not (null (foreign-funcall "strchr" :string "" :int 1 :pointer)))
  t)

) ;; #-cffi-sys::no-foreign-funcall

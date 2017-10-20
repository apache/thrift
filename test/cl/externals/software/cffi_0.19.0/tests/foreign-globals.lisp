;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; foreign-globals.lisp --- Tests on foreign globals.
;;;
;;; Copyright (C) 2005-2007, Luis Oliveira  <loliveira(@)common-lisp.net>
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

(defcvar ("var_char" *char-var*)  :char)
(defcvar "var_unsigned_char"      :unsigned-char)
(defcvar "var_short"              :short)
(defcvar "var_unsigned_short"     :unsigned-short)
(defcvar "var_int"                :int)
(defcvar "var_unsigned_int"       :unsigned-int)
(defcvar "var_long"               :long)
(defcvar "var_unsigned_long"      :unsigned-long)
(defcvar "var_float"              :float)
(defcvar "var_double"             :double)
(defcvar "var_pointer"            :pointer)
(defcvar "var_string"             :string)
(defcvar "var_long_long"          :long-long)
(defcvar "var_unsigned_long_long" :unsigned-long-long)

;;; The expected failures marked below result from this odd behaviour:
;;;
;;;   (foreign-symbol-pointer "var_char") => NIL
;;;
;;;   (foreign-symbol-pointer "var_char" :library 'libtest)
;;;     => #<Pointer to type :VOID = #xF7F50740>
;;;
;;; Why is this  happening? --luis
#+lispworks
(mapc (lambda (x) (pushnew x rtest::*expected-failures*))
      '(foreign-globals.ref.char foreign-globals.get-var-pointer.1
        foreign-globals.get-var-pointer.2 foreign-globals.symbol-name
        foreign-globals.read-only.1 ))

(deftest foreign-globals.ref.char
    *char-var*
  -127)

(deftest foreign-globals.ref.unsigned-char
    *var-unsigned-char*
  255)

(deftest foreign-globals.ref.short
    *var-short*
  -32767)

(deftest foreign-globals.ref.unsigned-short
    *var-unsigned-short*
  65535)

(deftest foreign-globals.ref.int
    *var-int*
  -32767)

(deftest foreign-globals.ref.unsigned-int
    *var-unsigned-int*
  65535)

(deftest foreign-globals.ref.long
    *var-long*
  -2147483647)

(deftest foreign-globals.ref.unsigned-long
    *var-unsigned-long*
  4294967295)

(deftest foreign-globals.ref.float
    *var-float*
  42.0)

(deftest foreign-globals.ref.double
    *var-double*
  42.0d0)

(deftest foreign-globals.ref.pointer
    (null-pointer-p *var-pointer*)
  t)

(deftest foreign-globals.ref.string
    *var-string*
  "Hello, foreign world!")

#+openmcl (push 'foreign-globals.set.long-long rt::*expected-failures*)

(deftest foreign-globals.ref.long-long
    *var-long-long*
  -9223372036854775807)

(deftest foreign-globals.ref.unsigned-long-long
    *var-unsigned-long-long*
  18446744073709551615)

;; The *.set.* tests restore the old values so that the *.ref.*
;; don't fail when re-run.
(defmacro with-old-value-restored ((place) &body body)
  (let ((old (gensym)))
    `(let ((,old ,place))
       (prog1
           (progn ,@body)
         (setq ,place ,old)))))

(deftest foreign-globals.set.int
    (with-old-value-restored (*var-int*)
      (setq *var-int* 42)
      *var-int*)
  42)

(deftest foreign-globals.set.string
    (with-old-value-restored (*var-string*)
      (setq *var-string* "Ehxosxangxo")
      (prog1
          *var-string*
        ;; free the string we just allocated
        (foreign-free (mem-ref (get-var-pointer '*var-string*) :pointer))))
  "Ehxosxangxo")

(deftest foreign-globals.set.long-long
    (with-old-value-restored (*var-long-long*)
      (setq *var-long-long* -9223000000000005808)
      *var-long-long*)
  -9223000000000005808)

(deftest foreign-globals.get-var-pointer.1
    (pointerp (get-var-pointer '*char-var*))
  t)

(deftest foreign-globals.get-var-pointer.2
    (mem-ref (get-var-pointer '*char-var*) :char)
  -127)

;;; Symbol case.

(defcvar "UPPERCASEINT1"     :int)
(defcvar "UPPER_CASE_INT1"   :int)
(defcvar "MiXeDCaSeInT1"     :int)
(defcvar "MiXeD_CaSe_InT1"   :int)

(deftest foreign-globals.ref.uppercaseint1
    *uppercaseint1*
  12345)

(deftest foreign-globals.ref.upper-case-int1
    *upper-case-int1*
  23456)

(deftest foreign-globals.ref.mixedcaseint1
    *mixedcaseint1*
  34567)

(deftest foreign-globals.ref.mixed-case-int1
    *mixed-case-int1*
  45678)

(when (string= (symbol-name 'nil) "NIL")
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :invert)
    (eval (read-from-string "(defcvar \"UPPERCASEINT2\"     :int)"))
    (eval (read-from-string "(defcvar \"UPPER_CASE_INT2\"   :int)"))
    (eval (read-from-string "(defcvar \"MiXeDCaSeInT2\"     :int)"))
    (eval (read-from-string "(defcvar \"MiXeD_CaSe_InT2\"   :int)"))
    (setf (readtable-case *readtable*) :preserve)
    (eval (read-from-string "(DEFCVAR \"UPPERCASEINT3\"     :INT)"))
    (eval (read-from-string "(DEFCVAR \"UPPER_CASE_INT3\"   :INT)"))
    (eval (read-from-string "(DEFCVAR \"MiXeDCaSeInT3\"     :INT)"))
    (eval (read-from-string "(DEFCVAR \"MiXeD_CaSe_InT3\"   :INT)"))))


;;; EVAL gets rid of SBCL's unreachable code warnings.
(when (string= (symbol-name (eval nil)) "nil")
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :invert)
    (eval (read-from-string "(DEFCVAR \"UPPERCASEINT2\"     :INT)"))
    (eval (read-from-string "(DEFCVAR \"UPPER_CASE_INT2\"   :INT)"))
    (eval (read-from-string "(DEFCVAR \"MiXeDCaSeInT2\"     :INT)"))
    (eval (read-from-string "(DEFCVAR \"MiXeD_CaSe_InT2\"   :INT)"))
    (setf (readtable-case *readtable*) :downcase)
    (eval (read-from-string "(defcvar \"UPPERCASEINT3\"     :int)"))
    (eval (read-from-string "(defcvar \"UPPER_CASE_INT3\"   :int)"))
    (eval (read-from-string "(defcvar \"MiXeDCaSeInT3\"     :int)"))
    (eval (read-from-string "(defcvar \"MiXeD_CaSe_InT3\"   :int)"))))

(deftest foreign-globals.ref.uppercaseint2
    *uppercaseint2*
  12345)

(deftest foreign-globals.ref.upper-case-int2
    *upper-case-int2*
  23456)

(deftest foreign-globals.ref.mixedcaseint2
    *mixedcaseint2*
  34567)

(deftest foreign-globals.ref.mixed-case-int2
    *mixed-case-int2*
  45678)

(deftest foreign-globals.ref.uppercaseint3
    *uppercaseint3*
  12345)

(deftest foreign-globals.ref.upper-case-int3
    *upper-case-int3*
  23456)

(deftest foreign-globals.ref.mixedcaseint3
    *mixedcaseint3*
  34567)

(deftest foreign-globals.ref.mixed-case-int3
    *mixed-case-int3*
  45678)

;;; regression test:
;;; gracefully accept symbols in defcvar

(defcvar *var-char* :char)
(defcvar var-char :char)

(deftest foreign-globals.symbol-name
    (values *var-char* var-char)
  -127 -127)

;;;# Namespace

#-cffi-sys::flat-namespace
(progn
  (deftest foreign-globals.namespace.1
      (values
       (mem-ref (foreign-symbol-pointer "var_char" :library 'libtest) :char)
       (foreign-symbol-pointer "var_char" :library 'libtest2))
    -127 nil)

  (deftest foreign-globals.namespace.2
      (values
       (mem-ref (foreign-symbol-pointer "ns_var" :library 'libtest) :boolean)
       (mem-ref (foreign-symbol-pointer "ns_var" :library 'libtest2) :boolean))
    t nil)

  ;; For its "default" module, Lispworks seems to cache lookups from
  ;; the newest module tried.  If a lookup happens to have failed
  ;; subsequent lookups will fail even the symbol exists in other
  ;; modules.  So this test fails.
  #+lispworks
  (pushnew 'foreign-globals.namespace.3 regression-test::*expected-failures*)

  (deftest foreign-globals.namespace.3
      (values
       (foreign-symbol-pointer "var_char" :library 'libtest2)
       (mem-ref (foreign-symbol-pointer "var_char") :char))
    nil -127)

  (defcvar ("ns_var" *ns-var1* :library libtest) :boolean)
  (defcvar ("ns_var" *ns-var2* :library libtest2) :boolean)

  (deftest foreign-globals.namespace.4
      (values *ns-var1* *ns-var2*)
    t nil))

;;;# Read-only

(defcvar ("var_char" *var-char-ro* :read-only t) :char
  "docstring")

(deftest foreign-globals.read-only.1
    (values *var-char-ro*
            (ignore-errors (setf *var-char-ro* 12)))
  -127 nil)

(deftest defcvar.docstring
    (documentation '*var-char-ro* 'variable)
  "docstring")

;;;# Other tests

;;; RT: FOREIGN-SYMBOL-POINTER shouldn't signal an error when passed
;;; an undefined variable.
(deftest foreign-globals.undefined.1
    (foreign-symbol-pointer "surely-undefined?")
  nil)

(deftest foreign-globals.error.1
    (handler-case (foreign-symbol-pointer 'not-a-string)
      (type-error () t))
  t)

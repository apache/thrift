;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; defcfun.lisp --- Tests function definition and calling.
;;;
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

(deftest defcfun.parse-name-and-options.1
    (multiple-value-bind (lisp-name foreign-name)
        (let ((*package* (find-package '#:cffi-tests)))
          (cffi::parse-name-and-options "foo_bar"))
      (list lisp-name foreign-name))
  (foo-bar "foo_bar"))

(deftest defcfun.parse-name-and-options.2
    (multiple-value-bind (lisp-name foreign-name)
        (let ((*package* (find-package '#:cffi-tests)))
          (cffi::parse-name-and-options "foo_bar" t))
      (list lisp-name foreign-name))
  (*foo-bar* "foo_bar"))

(deftest defcfun.parse-name-and-options.3
    (multiple-value-bind (lisp-name foreign-name)
        (cffi::parse-name-and-options 'foo-bar)
      (list lisp-name foreign-name))
  (foo-bar "foo_bar"))

(deftest defcfun.parse-name-and-options.4
    (multiple-value-bind (lisp-name foreign-name)
        (cffi::parse-name-and-options '*foo-bar* t)
      (list lisp-name foreign-name))
  (*foo-bar* "foo_bar"))

(deftest defcfun.parse-name-and-options.5
    (multiple-value-bind (lisp-name foreign-name)
        (cffi::parse-name-and-options '("foo_bar" foo-baz))
      (list lisp-name foreign-name))
  (foo-baz "foo_bar"))

(deftest defcfun.parse-name-and-options.6
    (multiple-value-bind (lisp-name foreign-name)
        (cffi::parse-name-and-options '("foo_bar" *foo-baz*) t)
      (list lisp-name foreign-name))
  (*foo-baz* "foo_bar"))

(deftest defcfun.parse-name-and-options.7
    (multiple-value-bind (lisp-name foreign-name)
        (cffi::parse-name-and-options '(foo-baz "foo_bar"))
      (list lisp-name foreign-name))
  (foo-baz "foo_bar"))

(deftest defcfun.parse-name-and-options.8
    (multiple-value-bind (lisp-name foreign-name)
        (cffi::parse-name-and-options '(*foo-baz* "foo_bar") t)
      (list lisp-name foreign-name))
  (*foo-baz* "foo_bar"))

;;;# Name translation

(deftest translate-underscore-separated-name.to-symbol
    (let ((*package* (find-package '#:cffi-tests)))
      (translate-underscore-separated-name "some_name_with_underscores"))
  some-name-with-underscores)

(deftest translate-underscore-separated-name.to-string
    (translate-underscore-separated-name 'some-name-with-underscores)
  "some_name_with_underscores")

(deftest translate-camelcase-name.to-symbol
    (let ((*package* (find-package '#:cffi-tests)))
      (translate-camelcase-name "someXmlFunction"))
  some-xml-function)

(deftest translate-camelcase-name.to-string
    (translate-camelcase-name 'some-xml-function)
  "someXmlFunction")

(deftest translate-camelcase-name.to-string-upper
    (translate-camelcase-name 'some-xml-function :upper-initial-p t)
  "SomeXmlFunction")

(deftest translate-camelcase-name.to-symbol-special
    (let ((*package* (find-package '#:cffi-tests)))
      (translate-camelcase-name "someXMLFunction" :special-words '("XML")))
  some-xml-function)

(deftest translate-camelcase-name.to-string-special
    (translate-camelcase-name 'some-xml-function :special-words '("XML"))
  "someXMLFunction")

(deftest translate-name-from-foreign.function
    (let ((*package* (find-package '#:cffi-tests)))
      (translate-name-from-foreign "some_xml_name" *package*))
  some-xml-name)

(deftest translate-name-from-foreign.var
    (let ((*package* (find-package '#:cffi-tests)))
      (translate-name-from-foreign "some_xml_name" *package* t))
  *some-xml-name*)

(deftest translate-name-to-foreign.function
    (translate-name-to-foreign 'some-xml-name *package*)
  "some_xml_name")

(deftest translate-name-to-foreign.var
    (translate-name-to-foreign '*some-xml-name* *package* t)
  "some_xml_name")

;;;# Calling with built-in c types
;;;
;;; Tests calling standard C library functions both passing
;;; and returning each built-in type. (adapted from funcall.lisp)

(defcfun "toupper" :char
  "toupper docstring"
  (char :char))

(deftest defcfun.char
    (toupper (char-code #\a))
  #.(char-code #\A))

(deftest defcfun.docstring
    (documentation 'toupper 'function)
  "toupper docstring")


(defcfun ("abs" c-abs) :int
  (n :int))

(deftest defcfun.int
    (c-abs -100)
  100)


(defcfun "labs" :long
  (n :long))

(deftest defcfun.long
    (labs -131072)
  131072)


#-cffi-features:no-long-long
(progn
  (defcfun "my_llabs" :long-long
    (n :long-long))

  (deftest defcfun.long-long
      (my-llabs -9223372036854775807)
    9223372036854775807)

  (defcfun "ullong" :unsigned-long-long
    (n :unsigned-long-long))

  #+allegro ; lp#914500
  (pushnew 'defcfun.unsigned-long-long rt::*expected-failures*)

  (deftest defcfun.unsigned-long-long
      (let ((ullong-max (1- (expt 2 (* 8 (foreign-type-size :unsigned-long-long))))))
        (eql ullong-max (ullong ullong-max)))
    t))


(defcfun "my_sqrtf" :float
  (n :float))

(deftest defcfun.float
    (my-sqrtf 16.0)
  4.0)


(defcfun ("sqrt" c-sqrt) :double
  (n :double))

(deftest defcfun.double
    (c-sqrt 36.0d0)
  6.0d0)


#+(and scl long-float)
(defcfun ("sqrtl" c-sqrtl) :long-double
  (n :long-double))

#+(and scl long-float)
(deftest defcfun.long-double
    (c-sqrtl 36.0l0)
  6.0l0)


(defcfun "strlen" :int
  (n :string))

(deftest defcfun.string.1
    (strlen "Hello")
  5)


(defcfun "strcpy" (:pointer :char)
  (dest (:pointer :char))
  (src :string))

(defcfun "strcat" (:pointer :char)
  (dest (:pointer :char))
  (src :string))

(deftest defcfun.string.2
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (strcpy s "Hello")
      (strcat s ", world!"))
  "Hello, world!")

(defcfun "strerror" :string
  (n :int))

(deftest defcfun.string.3
    (typep (strerror 1) 'string)
  t)


;;; Regression test. Allegro would warn on direct calls to
;;; functions with no arguments.
;;;
;;; Also, let's check if void functions will return NIL.
;;;
;;; Check if a docstring without arguments doesn't cause problems.

(defcfun "noargs" :int
  "docstring")

(deftest defcfun.noargs
    (noargs)
  42)

(defcfun "noop" :void)

#+(or allegro openmcl ecl) (pushnew 'defcfun.noop rt::*expected-failures*)

(deftest defcfun.noop
    (noop)
  #|no values|#)

;;;# Calling varargs functions

(defcfun "sprintf" :int
  "sprintf docstring"
  (str (:pointer :char))
  (control :string)
  &rest)

;;; CLISP and ABCL discard macro docstrings.
#+(or clisp abcl)
(pushnew 'defcfun.varargs.docstrings rt::*expected-failures*)

(deftest defcfun.varargs.docstrings
    (documentation 'sprintf 'function)
  "sprintf docstring")

(deftest defcfun.varargs.char
    (with-foreign-pointer-as-string (s 100)
      (sprintf s "%c" :char 65))
  "A")

(deftest defcfun.varargs.short
    (with-foreign-pointer-as-string (s 100)
      (sprintf s "%d" :short 42))
  "42")

(deftest defcfun.varargs.int
    (with-foreign-pointer-as-string (s 100)
      (sprintf s "%d" :int 1000))
  "1000")

(deftest defcfun.varargs.long
    (with-foreign-pointer-as-string (s 100)
      (sprintf s "%ld" :long 131072))
  "131072")

(deftest defcfun.varargs.float
    (with-foreign-pointer-as-string (s 100)
      (sprintf s "%.2f" :float (float pi)))
  "3.14")

(deftest defcfun.varargs.double
    (with-foreign-pointer-as-string (s 100)
      (sprintf s "%.2f" :double (float pi 1.0d0)))
  "3.14")

#+(and scl long-float)
(deftest defcfun.varargs.long-double
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (sprintf s "%.2Lf" :long-double pi))
  "3.14")

(deftest defcfun.varargs.string
    (with-foreign-pointer-as-string (s 100)
      (sprintf s "%s, %s!" :string "Hello" :string "world"))
  "Hello, world!")

;;; (let ((rettype (find-type :long))
;;;       (arg-types (n-random-types-no-ll 127)))
;;;   (c-function rettype arg-types)
;;;   (gen-function-test rettype arg-types))

#+(and (not ecl)
       #.(cl:if (cl:>= cl:lambda-parameters-limit 127) '(:and) '(:or)))
(progn
  (defcfun "sum_127_no_ll" :long
    (a1 :long) (a2 :unsigned-long) (a3 :short) (a4 :unsigned-short) (a5 :float)
    (a6 :double) (a7 :unsigned-long) (a8 :float) (a9 :unsigned-char)
    (a10 :unsigned-short) (a11 :short) (a12 :unsigned-long) (a13 :double)
    (a14 :long) (a15 :unsigned-int) (a16 :pointer) (a17 :unsigned-int)
    (a18 :unsigned-short) (a19 :long) (a20 :float) (a21 :pointer) (a22 :float)
    (a23 :int) (a24 :int) (a25 :unsigned-short) (a26 :long) (a27 :long)
    (a28 :double) (a29 :unsigned-char) (a30 :unsigned-int) (a31 :unsigned-int)
    (a32 :int) (a33 :unsigned-short) (a34 :unsigned-int) (a35 :pointer)
    (a36 :double) (a37 :double) (a38 :long) (a39 :short) (a40 :unsigned-short)
    (a41 :long) (a42 :char) (a43 :long) (a44 :unsigned-short) (a45 :pointer)
    (a46 :int) (a47 :unsigned-int) (a48 :double) (a49 :unsigned-char)
    (a50 :unsigned-char) (a51 :float) (a52 :int) (a53 :unsigned-short)
    (a54 :double) (a55 :short) (a56 :unsigned-char) (a57 :unsigned-long)
    (a58 :float) (a59 :float) (a60 :float) (a61 :pointer) (a62 :pointer)
    (a63 :unsigned-int) (a64 :unsigned-long) (a65 :char) (a66 :short)
    (a67 :unsigned-short) (a68 :unsigned-long) (a69 :pointer) (a70 :float)
    (a71 :double) (a72 :long) (a73 :unsigned-long) (a74 :short)
    (a75 :unsigned-int) (a76 :unsigned-short) (a77 :int) (a78 :unsigned-short)
    (a79 :char) (a80 :double) (a81 :short) (a82 :unsigned-char) (a83 :float)
    (a84 :char) (a85 :int) (a86 :double) (a87 :unsigned-char) (a88 :int)
    (a89 :unsigned-long) (a90 :double) (a91 :short) (a92 :short)
    (a93 :unsigned-int) (a94 :unsigned-char) (a95 :float) (a96 :long)
    (a97 :float) (a98 :long) (a99 :long) (a100 :int) (a101 :int)
    (a102 :unsigned-int) (a103 :char) (a104 :char) (a105 :unsigned-short)
    (a106 :unsigned-int) (a107 :unsigned-short) (a108 :unsigned-short)
    (a109 :int) (a110 :long) (a111 :char) (a112 :double) (a113 :unsigned-int)
    (a114 :char) (a115 :short) (a116 :unsigned-long) (a117 :unsigned-int)
    (a118 :short) (a119 :unsigned-char) (a120 :float) (a121 :pointer)
    (a122 :double) (a123 :int) (a124 :long) (a125 :char) (a126 :unsigned-short)
    (a127 :float))

  (deftest defcfun.bff.1
      (sum-127-no-ll
       1442906394 520035521 -4715 50335 -13557.0 -30892.0d0 24061483 -23737.0
       22 2348 4986 104895680 8073.0d0 -571698147 102484400
       (make-pointer 507907275) 12733353 7824 -1275845284 13602.0
       (make-pointer 286958390) -8042.0 -773681663 -1289932452 31199 -154985357
       -170994216 16845.0d0 177 218969221 2794350893 6068863 26327 127699339
       (make-pointer 184352771) 18512.0d0 -12345.0d0 -179853040 -19981 37268
       -792845398 116 -1084653028 50494 (make-pointer 2105239646) -1710519651
       1557813312 2839.0d0 90 180 30580.0 -532698978 8623 9537.0d0 -10882 54
       184357206 14929.0 -8190.0 -25615.0 (make-pointer 235310526)
       (make-pointer 220476977) 7476055 1576685 -117 -11781 31479 23282640
       (make-pointer 8627281) -17834.0 10391.0d0 -1904504370 114393659 -17062
       637873619 16078 -891210259 8107 0 760.0d0 -21268 104 14133.0 10
       588598141 310.0d0 20 1351785456 16159552 -10121.0d0 -25866 24821
       68232851 60 -24132.0 -1660411658 13387.0 -786516668 -499825680
       -1128144619 111849719 2746091587 -2 95 14488 326328135 64781 18204
       150716680 -703859275 103 16809.0d0 852235610 -43 21088 242356110
       324325428 -22380 23 24814.0 (make-pointer 40362014) -14322.0d0
       -1864262539 523684371 -21 49995 -29175.0)
    796447501))

;;; (let ((rettype (find-type :long-long))
;;;       (arg-types (n-random-types 127)))
;;;   (c-function rettype arg-types)
;;;   (gen-function-test rettype arg-types))

#-(or ecl cffi-sys::no-long-long
      #.(cl:if (cl:>= cl:lambda-parameters-limit 127) '(:or) '(:and)))
(progn
  (defcfun "sum_127" :long-long
    (a1 :pointer) (a2 :pointer) (a3 :float) (a4 :unsigned-long) (a5 :pointer)
    (a6 :long-long) (a7 :double) (a8 :double) (a9 :unsigned-short) (a10 :int)
    (a11 :long-long) (a12 :long) (a13 :short) (a14 :unsigned-int) (a15 :long)
    (a16 :unsigned-char) (a17 :int) (a18 :double) (a19 :short) (a20 :short)
    (a21 :long-long) (a22 :unsigned-int) (a23 :unsigned-short) (a24 :short)
    (a25 :pointer) (a26 :short) (a27 :unsigned-short) (a28 :unsigned-short)
    (a29 :int) (a30 :long-long) (a31 :pointer) (a32 :int) (a33 :unsigned-long)
    (a34 :unsigned-long) (a35 :pointer) (a36 :unsigned-long-long) (a37 :float)
    (a38 :int) (a39 :short) (a40 :pointer) (a41 :unsigned-long-long)
    (a42 :long-long) (a43 :unsigned-long) (a44 :unsigned-long)
    (a45 :unsigned-long-long) (a46 :unsigned-long) (a47 :char) (a48 :double)
    (a49 :long) (a50 :unsigned-int) (a51 :int) (a52 :short) (a53 :pointer)
    (a54 :long) (a55 :unsigned-long-long) (a56 :int) (a57 :unsigned-short)
    (a58 :unsigned-long-long) (a59 :float) (a60 :pointer) (a61 :float)
    (a62 :unsigned-short) (a63 :unsigned-long) (a64 :float) (a65 :unsigned-int)
    (a66 :unsigned-long-long) (a67 :pointer) (a68 :double)
    (a69 :unsigned-long-long) (a70 :double) (a71 :double) (a72 :long-long)
    (a73 :pointer) (a74 :unsigned-short) (a75 :long) (a76 :pointer) (a77 :short)
    (a78 :double) (a79 :long) (a80 :unsigned-char) (a81 :pointer)
    (a82 :unsigned-char) (a83 :long) (a84 :double) (a85 :pointer) (a86 :int)
    (a87 :double) (a88 :unsigned-char) (a89 :double) (a90 :short) (a91 :long)
    (a92 :int) (a93 :long) (a94 :double) (a95 :unsigned-short)
    (a96 :unsigned-int) (a97 :int) (a98 :char) (a99 :long-long) (a100 :double)
    (a101 :float) (a102 :unsigned-long) (a103 :short) (a104 :pointer)
    (a105 :float) (a106 :long-long) (a107 :int) (a108 :long-long)
    (a109 :long-long) (a110 :double) (a111 :unsigned-long-long) (a112 :double)
    (a113 :unsigned-long) (a114 :char) (a115 :char) (a116 :unsigned-long)
    (a117 :short) (a118 :unsigned-char) (a119 :unsigned-char) (a120 :int)
    (a121 :int) (a122 :float) (a123 :unsigned-char) (a124 :unsigned-char)
    (a125 :double) (a126 :unsigned-long-long) (a127 :char))

  #+(and sbcl x86) (push 'defcfun.bff.2 rtest::*expected-failures*)

  (deftest defcfun.bff.2
      (sum-127
       (make-pointer 2746181372) (make-pointer 177623060) -32334.0 3158055028
       (make-pointer 242315091) 4288001754991016425 -21047.0d0 287.0d0 18722
       243379286 -8677366518541007140 581399424 -13872 4240394881 1353358999
       226 969197676 -26207.0d0 6484 11150 1241680089902988480 106068320 61865
       2253 (make-pointer 866809333) -31613 35616 11715 1393601698
       8940888681199591845 (make-pointer 1524606024) 805638893 3315410736
       3432596795 (make-pointer 1490355706) 696175657106383698 -25438.0
       1294381547 26724 (make-pointer 3196569545) 2506913373410783697
       -4405955718732597856 4075932032 3224670123 2183829215657835866
       1318320964 -22 -3786.0d0 -2017024146 1579225515 -626617701 -1456
       (make-pointer 3561444187) 395687791 1968033632506257320 -1847773261
       48853 142937735275669133 -17974.0 (make-pointer 2791749948) -14140.0
       2707 3691328585 3306.0 1132012981 303633191773289330
       (make-pointer 981183954) 9114.0d0 8664374572369470 -19013.0d0
       -10288.0d0 -3679345119891954339 (make-pointer 3538786709) 23761
       -154264605 (make-pointer 2694396308) 7023 997.0d0 1009561368 241
       (make-pointer 2612292671) 48 1431872408 -32675.0d0
       (make-pointer 1587599336) 958916472 -9857.0d0 111 -14370.0d0 -7308
       -967514912 488790941 2146978095 -24111.0d0 13711 86681861 717987770
       111 1013402998690933877 17234.0d0 -8772.0 3959216275 -8711
       (make-pointer 3142780851) 9480.0 -3820453146461186120 1616574376
       -3336232268263990050 -1906114671562979758 -27925.0d0 9695970875869913114
       27033.0d0 1096518219 -12 104 3392025403 -27911 60 89 509297051
       -533066551 29158.0 110 54 -9802.0d0 593950442165910888 -79)
    7758614658402721936))

;;; regression test: defining an undefined foreign function should only
;;; throw some sort of warning, not signal an error.

#+(or cmucl (and sbcl (or (not linkage-table) win32)))
(pushnew 'defcfun.undefined rt::*expected-failures*)

(deftest defcfun.undefined
    (progn
      (eval '(defcfun ("undefined_foreign_function" undefined-foreign-function) :void))
      (compile 'undefined-foreign-function)
      t)
  t)

;;; Test whether all doubles are passed correctly. On some platforms, eg.
;;; darwin/ppc, some are passed on registers others on the stack.
(defcfun "sum_double26" :double
  (a1 :double) (a2 :double) (a3 :double) (a4 :double) (a5 :double)
  (a6 :double) (a7 :double) (a8 :double) (a9 :double) (a10 :double)
  (a11 :double) (a12 :double) (a13 :double) (a14 :double) (a15 :double)
  (a16 :double) (a17 :double) (a18 :double) (a19 :double) (a20 :double)
  (a21 :double) (a22 :double) (a23 :double) (a24 :double) (a25 :double)
  (a26 :double))

(deftest defcfun.double26
    (sum-double26 3.14d0 3.14d0 3.14d0 3.14d0 3.14d0 3.14d0 3.14d0
                  3.14d0 3.14d0 3.14d0 3.14d0 3.14d0 3.14d0 3.14d0
                  3.14d0 3.14d0 3.14d0 3.14d0 3.14d0 3.14d0 3.14d0
                  3.14d0 3.14d0 3.14d0 3.14d0 3.14d0)
  81.64d0)

;;; Same as above for floats.
(defcfun "sum_float26" :float
  (a1 :float) (a2 :float) (a3 :float) (a4 :float) (a5 :float)
  (a6 :float) (a7 :float) (a8 :float) (a9 :float) (a10 :float)
  (a11 :float) (a12 :float) (a13 :float) (a14 :float) (a15 :float)
  (a16 :float) (a17 :float) (a18 :float) (a19 :float) (a20 :float)
  (a21 :float) (a22 :float) (a23 :float) (a24 :float) (a25 :float)
  (a26 :float))

(deftest defcfun.float26
    (sum-float26 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0
                 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0)
  130.0)

;;;# Namespaces

#-cffi-sys::flat-namespace
(progn
  (defcfun ("ns_function" ns-fun1 :library libtest) :boolean)
  (defcfun ("ns_function" ns-fun2 :library libtest2) :boolean)

  (deftest defcfun.namespace.1
      (values (ns-fun1) (ns-fun2))
    t nil))

;;;# stdcall

#+(and x86 windows (not cffi-sys::no-stdcall))
(progn
  (defcfun ("stdcall_fun@12" stdcall-fun :convention :stdcall) :int
    (a :int)
    (b :int)
    (c :int))

  (deftest defcfun.stdcall.1
      (loop repeat 100 do (stdcall-fun 1 2 3)
            finally (return (stdcall-fun 1 2 3)))
    6))

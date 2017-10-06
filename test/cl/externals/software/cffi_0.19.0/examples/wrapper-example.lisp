(in-package #:cffi-example)

(defwrapper* "b0" :long ((x :long)) "return x;")
(defwrapper* "b1" :long ((x :long)) "return x;")
(defwrapper* "b2" :long ((x :long)) "return x;")
(defwrapper* "b3" :long ((x :long)) "return x;")
(defwrapper* "b4" :long ((x :long)) "return x;")

(define "b0_cffi_wrap(x)"
  "b0_cffi_wrap(b1_cffi_wrap(b2_cffi_wrap(b3_cffi_wrap(b4_cffi_wrap(+x+x)))))")
(define "b1_cffi_wrap(x)"
  "b0_cffi_wrap(b1_cffi_wrap(b2_cffi_wrap(b3_cffi_wrap(b4_cffi_wrap(+x+x)))))")
(define "b2_cffi_wrap(x)"
  "b0_cffi_wrap(b1_cffi_wrap(b2_cffi_wrap(b3_cffi_wrap(b4_cffi_wrap(+x+x)))))")
;;(define "b3_cffi_wrap(x)"
;;  "b0_cffi_wrap(b1_cffi_wrap(b2_cffi_wrap(b3_cffi_wrap(b4_cffi_wrap(+x+x)))))")
;;(define "b4_cffi_wrap(x)"
;;  "b0_cffi_wrap(b1_cffi_wrap(b2_cffi_wrap(b3_cffi_wrap(b4_cffi_wrap(+x+x)))))")

(defwrapper* "bn" :long ((x :long)) "return b0_cffi_wrap(x);")

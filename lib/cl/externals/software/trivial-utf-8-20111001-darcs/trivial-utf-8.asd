(defpackage :trivial-utf-8-system
  (:use :common-lisp :asdf))
(in-package :trivial-utf-8-system)

(defsystem :trivial-utf-8
  :components ((:file "trivial-utf-8")))

(defsystem :trivial-utf-8-tests
  :depends-on (:trivial-utf-8)
  :components ((:file "tests")))

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

(defpackage :usocket-test
  (:use :common-lisp
	:usocket
	:regression-test)
  (:export #:do-tests
	   #:run-usocket-tests))

;;; The tests here only make sense in the context of a TestServer
;;; being open and the dynamic variable thrift-cross::*prot*
;;; being set with a client connection to the TestServer. Normally,
;;; this is handled in make-test-client.lisp.

(in-package :thrift-cross)

;;; Standard Thrift cross-test error codes
(defparameter *test_basetypes* 1)
(defparameter *test_structs* 2)
(defparameter *test_containers* 4)
(defparameter *test_exceptions* 8)
(defparameter *test_unknown* 64)
(defparameter *test_timeout* 128)

(defun cross-test ()
  "The main cross-test runner."
  (let ((result 0))
    (unless (run-package-tests :package :base-types)
      (incf result *test_basetypes*))
    result))

(fiasco:define-test-package :base-types)

(in-package :base-types)

(fiasco:deftest boolean-test ()
  (is (thrift.test:test-bool thrift-cross::*prot* t))
  (is (not (thrift.test:test-bool thrift-cross::*prot* nil))))

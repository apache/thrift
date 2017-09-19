;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)


(defpackage :thrift-test
  (:shadowing-import-from :thrift :byte :set :list :map :type-of :float)
  (:use :common-lisp :thrift)
  #+ccl
  (:import-from :ccl :stream-tyo :stream-tyi :stream-reader :stream-writer
                :stream-write-byte :stream-read-byte :stream-position
                :stream-read-sequence :stream-write-sequence
                :stream-force-output)
  #+sbcl
  (:import-from :sb-gray 
                :stream-write-byte :stream-read-byte
                :stream-read-sequence :stream-write-sequence
                :stream-force-output :stream-finish-output)
  (:export :test
           :with-test-services
           :*test-location*))

(defpackage :thrift-test-request (:use ))

(defpackage :thrift-test-response (:use ))
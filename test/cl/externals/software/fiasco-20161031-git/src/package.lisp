;;; -*- mode: Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors, 2014 João Távora
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :fiasco
  (:use :alexandria
        :common-lisp)

  (:export #:find-test
           #:deftest
           #:is
           #:signals
           #:not-signals
           #:finishes
           #:with-expected-failures
           #:root-suite
           #:defsuite
           #:without-debugging
           #:without-test-progress-printing
           #:funcall-test-with-feedback-message
           #:run-failed-tests
           #:extract-test-run-statistics

           #:*test-progress-print-right-margin*
           #:*test-result-history*
           #:*last-test-result*

           ;; these are the defaults from which the test context is initialized
           #:*print-test-run-progress*
           #:*debug-on-unexpected-error*
           #:*debug-on-assertion-failure*
           #:*always-show-failed-sexp*
           #:*ignore-package-suite-mismatch*
           #:*warn-about-test-redefinitions*
           #:all-tests
           #:define-test-package
           #:run-package-tests
           #:describe-failed-tests
           #:run-suite-tests))

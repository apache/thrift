;;; -*- mode: Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors,
;;; 2014-2015 João Távora
;;;
;;; See LICENCE for details.

(asdf:defsystem #:fiasco
  :description "A Common Lisp test framework that treasures your failures, logical continuation of Stefil."
  :author "João Távora <https://github.com/capitaomorte>"
  :license "BSD 2-clause"
  :depends-on (#:alexandria)
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "infrastructure")
     (:file "asserts")
     (:file "test")
     (:file "suite")))))

(asdf:defsystem #:fiasco-self-tests
  :licence "BSD / Public domain"
  :depends-on (#:fiasco)
  :serial t
  :components ((:module "test"
                :serial t
                :components
                ((:file "basic")
                 (:file "intro-example")))))


;; Local Variables:
;; coding: utf-8-unix
;; End:

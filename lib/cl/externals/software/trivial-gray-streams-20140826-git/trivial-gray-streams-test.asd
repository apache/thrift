;;; -*- mode: lisp -*-

(defsystem :trivial-gray-streams-test
  :version "2.0"
  :depends-on (:trivial-gray-streams)
  :pathname #P"test/"
  :serial t
  :components ((:file "package")
               (:file "test-framework")
               (:file "test")))

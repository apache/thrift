;;; -*- Lisp -*-

(defsystem :split-sequence
  :author "Arthur Lemmens <alemmens@xs4all.nl>"
  :maintainer "Sharp Lispers <sharplispers@googlegroups.com>"
  :description "Splits a sequence into a list of subsequences
  delimited by objects satisfying a test."
  :license "public domain"
  :version (:read-file-form "version.lisp-expr")
  :components ((:file "split-sequence"))
  :in-order-to ((asdf:test-op (asdf:load-op :split-sequence-tests)))
  :perform (asdf:test-op :after (op c)
             (funcall (intern (symbol-name '#:run!) '#:5am) :split-sequence)))

(defsystem :split-sequence-tests
  :author "Arthur Lemmens <alemmens@xs4all.nl>"
  :maintainer "Sharp Lispers <sharplispers@googlegroups.com>"
  :description "Split-Sequence test suite"
  :license "public domain"
  :depends-on (:split-sequence :fiveam)
  :components ((:file "tests")))

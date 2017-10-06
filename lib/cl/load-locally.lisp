(in-package :cl-user)

(require "asdf")
(load (merge-pathnames "externals/bundle.lisp" *load-truename*))
(asdf:load-asd (merge-pathnames "thrift.asd" *load-truename*))
(asdf:load-system :thrift)

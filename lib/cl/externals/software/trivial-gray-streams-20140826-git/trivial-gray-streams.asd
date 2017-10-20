;;; -*- mode: lisp -*-

(defsystem :trivial-gray-streams
  :description "Compatibility layer for Gray Streams (see http://www.cliki.net/Gray%20streams)."
  :license "MIT"
  :author "David Lichteblau"
  :maintainer "Anton Vodonosov <avodonosov@yandex.ru>"
  :version "2.0"
  :serial t
  :components ((:file "package") (:file "streams")))

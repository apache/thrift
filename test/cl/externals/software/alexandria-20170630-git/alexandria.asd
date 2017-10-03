(defsystem :alexandria
  :version "0.0.0"
  :licence "Public Domain / 0-clause MIT"
  :description "Alexandria is a collection of portable public domain utilities."
  :author "Nikodemus Siivola <nikodemus@sb-studio.net>, and others."
  :long-description
  "Alexandria is a project and a library.

As a project Alexandria's goal is to reduce duplication of effort and improve
portability of Common Lisp code according to its own idiosyncratic and rather
conservative aesthetic. What this actually means is open to debate, but each
project member has a veto on all project activities, so a degree of
conservativism is inevitable.

As a library Alexandria is one of the means by which the project strives for
its goals.

Alexandria is a collection of portable public domain utilities that meet
the following constraints:

 * Utilities, not extensions: Alexandria will not contain conceptual
   extensions to Common Lisp, instead limiting itself to tools and utilities
   that fit well within the framework of standard ANSI Common Lisp.
   Test-frameworks, system definitions, logging facilities, serialization
   layers, etc. are all outside the scope of Alexandria as a library, though
   well within the scope of Alexandria as a project.

 * Conservative: Alexandria limits itself to what project members consider
   conservative utilities. Alexandria does not and will not include anaphoric
   constructs, loop-like binding macros, etc.

 * Portable: Alexandria limits itself to portable parts of Common Lisp. Even
   apparently conservative and useful functions remain outside the scope of
   Alexandria if they cannot be implemented portably. Portability is here
   defined as portable within a conforming implementation: implementation bugs
   are not considered portability issues.

 * Team player: Alexandria will not (initially, at least) subsume or provide
   functionality for which good-quality special-purpose packages exist, like
   split-sequence. Instead, third party packages such as that may be
   \"blessed\"."
  :components
  ((:static-file "LICENCE")
   (:static-file "tests.lisp")
   (:file "package")
   (:file "definitions" :depends-on ("package"))
   (:file "binding" :depends-on ("package"))
   (:file "strings" :depends-on ("package"))
   (:file "conditions" :depends-on ("package"))
   (:file "io" :depends-on ("package" "macros" "lists" "types"))
   (:file "macros" :depends-on ("package" "strings" "symbols"))
   (:file "hash-tables" :depends-on ("package" "macros"))
   (:file "control-flow" :depends-on ("package" "definitions" "macros"))
   (:file "symbols" :depends-on ("package"))
   (:file "functions" :depends-on ("package" "symbols" "macros"))
   (:file "lists" :depends-on ("package" "functions"))
   (:file "types" :depends-on ("package" "symbols" "lists"))
   (:file "arrays" :depends-on ("package" "types"))
   (:file "sequences" :depends-on ("package" "lists" "types"))
   (:file "numbers" :depends-on ("package" "sequences"))
   (:file "features" :depends-on ("package" "control-flow"))))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :alexandria))))
  nil)

(defmethod perform ((o test-op) (c (eql (find-system :alexandria))))
  (operate 'load-op :alexandria-tests)
  (operate 'test-op :alexandria-tests))

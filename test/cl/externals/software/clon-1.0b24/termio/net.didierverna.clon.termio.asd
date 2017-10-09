;;; net.didierverna.clon.termio.asd --- ASDF system definition, termio feature

;; Copyright (C) 2015 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of clon.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

#+sbcl (require :sb-grovel)
(asdf:load-system :net.didierverna.clon.setup)

(asdf:defsystem :net.didierverna.clon.termio
  :description "The Command-Line Options Nuker, termio feature"
  :long-description "Clon is a library for command-line option management.
The termio feature provides ISO6429 coloring on terminals that support it.
For a more complete description of Clon, see the net.didierverna.clon system."
  :author "Didier Verna <didier@didierverna.net>"
  :maintainer "Didier Verna <didier@didierverna.net>"
  :license "BSD"
  :version #.(net.didierverna.clon.setup:version :short)
  :if-feature :net.didierverna.clon.termio
  :defsystem-depends-on
  (:net.didierverna.clon.setup/termio
   #+sbcl ;; BUG in ASDF 3.1.4: d-d-o can't deal dependency expanding to NIL
   (:feature :sbcl (:require :sb-grovel))
   #+(or allegro clisp lispworks)
   (:feature (:or :allegro :clisp :lispworks) :cffi-grovel))
  :depends-on ((:feature :sbcl :sb-posix)
	       (:feature (:and :clisp :net.didierverna.clon.termio) :cffi)
	       :net.didierverna.clon.core)
  :serial t
  :components (;; bug in ASDF 3.1.4: cannot deal with conditionally defined
	       ;; component class!
	       #+sbcl (sb-grovel:grovel-constants-file "sbcl/constants"
		       :package :net.didierverna.clon :if-feature :sbcl)
	       #+(or allegro clisp lispworks)
	       (:grovel-file "cffi/constants"
		:if-feature (:or :allegro :clisp :lispworks))
	       (:file "termio")))

;;; net.didierverna.clon.termio.asd ends here

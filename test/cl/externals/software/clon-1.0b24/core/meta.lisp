;;; meta.lisp --- Meta utilities

;; Copyright (C) 2010-2012, 2015 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Clon.

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

(in-package :cl-user)


;; -------------------
;; Package definition:
;; -------------------

(defpackage :net.didierverna.clon
  (:documentation "The Command-Line Options Nuker package.")
  (:use :cl :net.didierverna.clon.setup)
  (:shadow :*readtable*)
  ;; #### PORTME.
  (:import-from #+sbcl      :sb-mop
		#+cmu       :mop
		#+ccl       :ccl
		#+ecl       :clos
		#+clisp     :clos
		#+abcl      :mop
		#+allegro   :mop
		#+lispworks :clos
		:class-slots :slot-definition-name :validate-superclass)
  (:export
    ;; From the :net.didierverna.clon.setup package:
    :*release-major-level*
    :*release-minor-level*
    :*release-status*
    :*release-status-level*
    :*release-name*
    :version
    ;; From meta.lisp (this file):
    :nickname-package
    ;; From src/util.lisp:
    :exit
    :cmdline
    :dump
    ;; From src/text.lisp:
    :make-text
    ;; From src/options/flag.lisp:
    :make-flag
    ;; From src/options/switch.lisp:
    :make-switch
    ;; From src/options/stropt.lisp:
    :make-stropt
    ;; From src/options/lispobj.lisp:
    :make-lispobj
    ;; From src/options/path.lisp:
    :make-path
    ;; From src/options/enum.lisp:
    :make-enum
    ;; From src/options/xswitch.lisp:
    :make-xswitch
    ;; From src/group.lisp:
    :make-group :defgroup
    ;; From src/synopsis.lisp:
    :*synopsis*
    :make-synopsis :defsynopsis
    ;; From src/context.lisp:
    :*context*
    :make-context
    :with-context
    :progname
    :remainder
    :cmdline-options-p
    :cmdline-p
    :getopt
    :getopt-cmdline
    :multiple-value-getopt-cmdline
    :do-cmdline-options
    :help))


(in-package :net.didierverna.clon)


;; -------------------
;; External utilities:
;; -------------------

(defun nickname-package (&optional (nickname :clon))
  "Add NICKNAME (:CLON by default) to the :NET.DIDIERVERNA.CLON package."
  (rename-package :net.didierverna.clon
		  (package-name :net.didierverna.clon)
		  (adjoin nickname (package-nicknames :net.didierverna.clon)
			  :test #'string-equal)))


;; -------------------
;; Internal utilities:
;; -------------------

(defvar *readtable* (copy-readtable)
  "The Clon readtable.")


;; String concatenation
;; --------------------
(defun tilde-reader (stream char)
  "Read a series of ~\"string\" to be concatenated together."
  (declare (ignore char))
  (flet ((read-string (&aux (string (read stream t nil t)))
	   (check-type string string "a string")
	   string))
    (apply #'concatenate 'string
	   (read-string)
	   (loop :while (char= (peek-char t stream nil nil t) #\~)
		 :do (read-char stream t nil t)
		 :collect (read-string)))))

(set-macro-character #\~ #'tilde-reader nil *readtable*)

;; Emacs indentation
;; -----------------
(defun clindent (symbol indent)
  "Set SYMBOL's indentation to INDENT in (X)Emacs.
This function sets SYMBOL's common-lisp-indent-function property.
If INDENT is a symbol, use its indentation definition.
Otherwise, INDENT is considered as an indentation definition."
  (when (and (member :swank *features*)
	     (configuration :swank-eval-in-emacs))
    ;; #### NOTE: case portability
    (funcall (intern (string :eval-in-emacs) :swank)
	     `(put ',symbol 'common-lisp-indent-function
		   ,(if (symbolp indent)
			`(get ',indent 'common-lisp-indent-function)
		      `',indent))
	     t)))

(defmacro defindent (symbol indent)
  "Set SYMBOL's indentation to INDENT in (X)Emacs.
SYMBOL and INDENT need not be quoted.
See CLINDENT for more information."
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (clindent ',symbol ',indent)))

(defun i-reader (stream subchar arg)
  "Read an argument list for the DEFINDENT macro."
  (declare (ignore subchar arg))
  (cons 'defindent (read stream)))

(set-dispatch-macro-character #\# #\i #'i-reader *readtable*)


;; ECL, CLISP, Allegro and LispWorks do not like to see undefined reader
;; macros in expressions that belong to other compilers. For instance this
;; will break: #+ccl (#_ccl-only-function) It seems to be a correct behavior
;; (see *read-suppress* in CLHS), although other implementations like SBCL and
;; CMUCL are more gentle. The solution I use is to define those reader macros
;; to simply return nil.

;; #### PORTME.
#+(or ecl clisp allegro lispworks)
(progn
  (defun dummy-reader (stream subchar args)
    "Return nil."
    (declare (ignore stream subchar args))
    nil)
  (set-dispatch-macro-character #\# #\_ #'dummy-reader *readtable*)
  (set-dispatch-macro-character #\# #\$ #'dummy-reader *readtable*))

(defmacro in-readtable (name)
  "Set the current readtable to the value of NAME::*READTABLE*."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf cl:*readtable*
	   ;; #### NOTE: case portability
	   (symbol-value (find-symbol (string :*readtable*) ,name)))))


;;; package.lisp ends here

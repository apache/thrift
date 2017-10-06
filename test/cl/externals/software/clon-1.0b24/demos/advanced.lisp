;;; advanced.lisp --- Advanced usage demonstration program

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

;; This demonstration program shows how to use multiple synopsis, contexts and
;; (virtual) command-lines in the same application in order to manage more
;; complex command-line syntax where options and non-options parts can be
;; freely intermixed. See section 5 "Advanced Usage" in the Clon User Manual.

;; #### NOTE: some trickery is needed below in order to make this code
;; ECL-compliant, due to ECL's specific way of generating executables. This
;; includes:
;; - setting *load-verbose* to nil,
;; - passing a nil :verbose flag to asdf:load-system,
;; - wrapping nickname-package in an eval-when form.
;; None of these tweaks are needed for the other compilers.


;;; Code:

(in-package :cl-user)

(setq *load-verbose* nil)

(require "asdf"
	 ;; #### PORTME.
	 ;; #### NOTE: the trickery below (which works for me) is because I've
	 ;; told the various Lisps to not load user init files for compiling
	 ;; the demo programs.
	 #-(or sbcl cmu ccl ecl allegro
	       (and lispworks (not lispworks-personal-edition)))
	 '(#p"/usr/local/share/common-lisp/source/asdf/build/asdf.lisp"))

(asdf:load-system :net.didierverna.clon :verbose nil)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (net.didierverna.clon:nickname-package))

(clon:defsynopsis (:postfix "cmd [OPTIONS]")
  (text :contents "Available commands: push pull.
Use 'cmd --help' to get command-specific help.")
  (flag :short-name "h" :long-name "help"
	:description "Print this help and exit.")
  (switch :short-name "d" :long-name "debug"
	  :description "Turn debugging on or off."
	  :argument-style :on/off
	  :env-var "DEBUG"))

(defconstant +push-synopsis+
    (clon:defsynopsis (:make-default nil)
      (text :contents "Push local changes to the remote server.")
      (flag :short-name "h" :long-name "help"
	    :description "Print this help and exit.")
      (flag :short-name "d" :long-name "dry-run"
	    :description "Fake the push operation.")
      (stropt :long-name "remote"
	      :argument-name "SERVER"
	      :description "Use SERVER instead of default remote."))
  "The synopsis for the PUSH operation.")

(defconstant +pull-synopsis+
    (clon:defsynopsis (:make-default nil)
      (text :contents "Pull remote changes to the local server.")
      (flag :short-name "h" :long-name "help"
	    :description "Print this help and exit.")
      (flag :short-name "d" :long-name "dry-run"
	    :description "Fake the push operation.")
      (switch :long-name "update"
	      :default-value t
	      :description "Also update the working directory."))
  "The synopsis for the PULL operation.")


(defun main ()
  "Entry point for the standalone application."
  (clon:make-context)
  (cond ((or (clon:getopt :short-name "h")
	     (not (clon:cmdline-p)))
	 (clon:help))
	(t
	 (unless (clon:remainder)
	   (format t "Missing command.~%")
	   (clon:exit 1))
	 (clon:make-context
	  :synopsis (cond ((string= (first (clon:remainder)) "push")
			   +push-synopsis+)
			  ((string= (first (clon:remainder)) "pull")
			   +pull-synopsis+)
			  (t
			   (format t "Unknown command.~%")
			   (clon:exit 1)))
	  :cmdline (clon:remainder))
	 (cond ((clon:getopt :short-name "h")
		(clon:help))
	       (t
		(format t "Command name: ~A~%~%" (clon:progname))
		(format t "Options:")
		(clon:do-cmdline-options (option name value source)
		  (print (list option name value source)))
		(terpri)
		(format t "Remainder: ~A~%" (clon:remainder))))))
  (clon:exit))

(clon:dump "advanced" main)


;;; advanced.lisp ends here

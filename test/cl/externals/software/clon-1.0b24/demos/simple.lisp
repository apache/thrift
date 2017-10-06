;;; simple.lisp --- Basic usage demonstration program

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

;; This demonstration program shows how to define your application's
;; command-line syntax, initialize the library, retrieve option values and
;; generate help strings.

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

(clon:defsynopsis (:postfix "FILES...")
  (text :contents
	"Demonstration of Clon (use --clon-help for built-in options).")
  (group (:header "Flags (non valued options):")
    (flag :short-name "h" :long-name "help"
	  :description "Print this help and exit."))
  (group (:header "Built-in valued option types:")
    (group (:header "String options:")
	   (stropt :short-name "n" :long-name "name"
		   :description "Set your name to NAME."
		   :argument-name "NAME"))
    (group (:header "Lisp objects:")
	   (lispobj :short-name "e" :long-name "eval"
		    :description "Evaluate EXPR."
		    :argument-name "EXPR"))
    (group (:header "Enumerations:")
	   (enum :long-name "copyright"
		 :description "Set the copyright to LICENSE.
Possible values are: none, gpl, lppl, bsd or mit."
		 :argument-name "LICENSE"
		 :argument-type :optional
		 :enum '(:none :gpl :lppl :bsd :mit)
		 :fallback-value :gpl
		 :default-value :none))
    (group (:header "Path options:")
      (path :long-name "tmpdir"
	    :description "Set the temporary directory to DIR."
	    :argument-name "DIR"
	    :type :directory
	    :default-value #p"/tmp/")
      (path :short-name "o" :long-name "output"
	    :description "Output to FILE."
	    :argument-name "FILE"
	    :type :file
	    :default-value #p"a.out")
      (path :short-name "I"
	    :description "Set the include search path to SEARCH-PATH.
SEARCH-PATH is a colon-separated list of directories. Use an empty argument to
remove all search paths."
	    :argument-name "SEARCH-PATH"
	    :type :directory-list
	    :default-value '(#p"/usr/local/share/" #p"/usr/share/")))
    (group (:header "Switches:")
      (switch :short-name "d" :long-name "debug"
	      :description "Turn debugging on or off."
	      :argument-style :on/off
	      :env-var "DEBUG"))
    (group (:header "Extended switches:")
      (xswitch :short-name "c" :long-name "connect"
	       :description "Connect to server.
Possible values are yes, no or try. If try, no errors are reported."
	       :enum '(:try)))))

(defun main ()
  "Entry point for the standalone application."
  (clon:make-context)
  (cond ((clon:getopt :short-name "h")
	 (clon:help))
	(t
	 (format t "Program name: ~A~%~%" (clon:progname))
	 (format t "Options:")
	 (clon:do-cmdline-options (option name value source)
	   (print (list option name value source)))
	 (terpri)
	 (format t "Remainder: ~A~%" (clon:remainder))))
  (clon:exit))

(clon:dump "simple" main)


;;; simple.lisp ends here

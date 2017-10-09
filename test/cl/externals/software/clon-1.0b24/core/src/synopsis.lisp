;;; synopsis.lisp --- Synopsis management

;; Copyright (C) 2010-2013, 2015 Didier Verna

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

(in-package :net.didierverna.clon)
(in-readtable :net.didierverna.clon)


(defvar *synopsis* nil "The current synopsis.")



;; ==========================================================================
;; The Option Mapping Protocol
;; ==========================================================================

;; #### NOTE: there are two very good reasons for updating the traversal state
;; of item objects in an after method, as done below:
;; 1/ this is obviously the right thing to do,
;; 2/ moving it away from the primary method makes this primary method return
;;    the value of FUNC itself when FUNC is actually called. This is an
;;    important idiom because callers might want to rely on the return value
;;    of (the last computation of) mapoptions, especially when used through
;;    the DO-OPTIONS below. See for example the function SEARCH-OPTION-BY-NAME
;;    in context.lisp.
(defgeneric mapoptions (func there)
  (:documentation "Map FUNC over all options in THERE.")
  (:method (func elsewhere)
    "Do nothing by default."
    #+(or ccl ecl clisp allegro) (declare (ignore func elsewhere))
    (values))
  (:method :after (func (item item))
    "Mark TRAVERSABLE as traversed."
    #+(or ccl ecl clisp allegro) (declare (ignore func))
    (setf (traversedp item) t))
  (:method (func (container container))
    "Map FUNC over all containers or options in CONTAINER."
    (unless (traversedp container)
      (dolist (item (items container))
	(mapoptions func item))))
  (:method (func (option option))
    "Call FUNC on OPTION."
    (unless (traversedp option)
      (funcall func option))))

(defmacro do-options ((opt there) &body body)
  "Execute BODY with OPT bound to every option in THERE."
  `(mapoptions (lambda (,opt) ,@body)
	       (untraverse ,there)))



;; ==========================================================================
;; The Synopsis Class
;; ==========================================================================

(defclass synopsis (container)
  ((postfix :documentation "A postfix to the program synopsis."
	    :type (or null string)
	    :initarg :postfix
	    :initform nil
	    :reader postfix)
   (short-pack :documentation "The short pack string."
	       :type (or null string)
	       :reader short-pack)
   (negated-pack :documentation "The negated pack string."
		 :type (or null string)
		 :reader negated-pack)
   (potential-pack :documentation "The potential pack string."
		   :type (or null string)
		   :reader potential-pack)
   (clon-options-group :documentation "The Clon options group."
		       :type group
		       :initarg :clon-options-group
		       :reader clon-options-group))
  (:documentation "The SYNOPSIS class.
This class handles the description of the program's command-line options."))


;; ---------------------------
;; Help specification protocol
;; ---------------------------

(defmethod help-spec ((synopsis synopsis) &key program)
  "Return SYNOPSIS's help specification."
  (list* (accumulate (synopsis)
	   '(header "Usage:")
	   `(program ,program)
	   (accumulate (short-pack)
	     (when (short-pack synopsis)
	       (format nil "[-~A]" (short-pack synopsis))))
	   (accumulate (negated-pack)
	     (when (negated-pack synopsis)
	       (format nil "[+~A]" (negated-pack synopsis))))
	   '(options "[OPTIONS]")
	   (accumulate (postfix)
	     (postfix synopsis)))
	 ;; This calls the container's method.
	 (call-next-method)))



;; ==========================================================================
;; The Potential Pack Protocol
;; ==========================================================================

;; #### NOTE: a generic function is a bit overkill here, because its use is
;; only to provide a convenience wrapper for contexts.
(defgeneric potential-pack-p (pack there)
  (:documentation "Return t if PACK is a potential pack in THERE.")
  (:method (pack (synopsis synopsis))
    "Return t if PACK is a potential pack for SYNOPSIS."
    ;; #### NOTE: if there's no potential pack in SYNOPSIS, the call to
    ;; STRING-LEFT-TRIM gets a nil CHAR-BAG which is ok and gives the expected
    ;; result.
    (zerop (length (string-left-trim (potential-pack synopsis) pack)))))



;; ==========================================================================
;; Synopsis Instance Creation
;; ==========================================================================

(defmethod initialize-instance :around ((synopsis synopsis) &rest keys)
  "Prepare Clon specific options."
  (let ((grp (%defgroup t (:header "Clon specific options:" :hidden t)
	       (flag "banner" "Display the full Clon banner.")
	       (enum "version"
		     "Display Clon's version number.
FMT can be `number', `short' or `long'."
		     :argument-name "FMT"
		     :argument-type :optional
		     :enum '(:number :short :long)
		     :fallback-value :long
		     #|:env-var "VERSION_FORMAT"|#)
	       (flag "lisp-information"
		     ~"Display information about the underlying Lisp "
		     ~"implementation.")
	       (flag "help" "Display Clon-specific help.")
	       (group (:header "Option retrieval:")
		 (enum "error-handler"
		       "Set the option retrieval error handler.
HDL can be `interactive', `quit' or `none'."
		       :argument-name "HDL"
		       :argument-type :optional
		       :enum '(:interactive :quit :none)
		       :fallback-value :none
		       :default-value :quit
		       :env-var "ERROR_HANDLER"))
	       (group (:header "Help string output:")
		 (path "search-path"
		       "Set Clon's search path.
If you don't want any search path at all, use this option with no argument."
		       :argument-type :optional
		       :type :directory-list
		       :fallback-value nil
		       ;; #### PORTME. I'm using Unix-like default for
		       ;; everything here, plus OSX specific values that I
		       ;; know of. Not sure about Windows or anything else.
		       :default-value
		       ;; #### FIXME: this is wrong. If defsynopsis is used as
		       ;; a toplevel form, the fallback below will be
		       ;; computed at compile-time although it contains things
		       ;; that should be computed at run-time only (like the
		       ;; user home directory).
		       (let ((path '(#p"/usr/local/share/clon/"
				     #p"/usr/share/clon/"))
			     (home-directory (home-directory)))
			 (when (macosp)
			   (push #p"/Library/Application Support/Clon/" path))
			 (when home-directory
			   (let ((local-path '("share/clon/")))
			     (when (macosp)
			       (push "Library/Application Support/Clon/"
				     local-path))
			     (push ".clon/" local-path)
			     (setq path (append
					 (mapcar
					  (lambda (subdir)
					    (merge-pathnames subdir
							     home-directory))
					  local-path)
					 path))))
			 path)
		       :env-var "SEARCH_PATH")
		 (path "theme"
		       ~"Set Clon's output theme.
If you don't want any theme at all, use this option with no argument. "
		       ~"Unless starting with /, ./ or ../, files are looked "
		       ~"for in the Clon search path. The cth extension can "
		       ~"be omitted."
		       :argument-name "FILE"
		       :argument-type :optional
		       :type :file
		       :fallback-value nil
		       :default-value (make-pathname :name "raw")
		       :env-var "THEME")
		 (lispobj "line-width"
			  ~"Set Clon's output line width.
If not given, the value of the COLUMNS environment variable, the terminal "
			  ~"size, or a default of 80 columns will be used."
			  :argument-name "WIDTH"
			  :env-var "LINE_WIDTH"
			  :typespec '(integer 1))
		 (xswitch "highlight"
			  "Set Clon's output highlighting to on/off/auto.
Auto (the default) means on for tty output and off otherwise."
			  :enum '(:auto)
			  :env-var "HIGHLIGHT"
			  :default-value :auto)))))
    (apply #'call-next-method synopsis
	   :clon-options-group grp
	   (nconc keys (list :item grp)))))

(defmethod initialize-instance :after
    ((synopsis synopsis) &key &aux potential-pack short-pack negated-pack)
  "Compute SYNOSPSIS's short and negated packs."
  (do-options (option synopsis)
    (let ((potential-pack-char (potential-pack-char option :as-string))
	  (short-pack-char (short-pack-char option :as-string))
	  (negated-pack-char (negated-pack-char option  :as-string)))
      (when potential-pack-char
	(setq potential-pack
	      (concatenate 'string potential-pack potential-pack-char)))
      (when short-pack-char
	(setq short-pack
	      (concatenate 'string short-pack short-pack-char)))
      (when negated-pack-char
	(setq negated-pack
	      (concatenate 'string negated-pack negated-pack-char)))))
  (setf (slot-value synopsis 'potential-pack) potential-pack)
  (setf (slot-value synopsis 'short-pack) short-pack)
  (setf (slot-value synopsis 'negated-pack) negated-pack))

(defun make-synopsis (&rest keys &key postfix item (make-default t))
  "Make a new SYNOPSIS.
- POSTFIX is a string to append to the program synopsis, in case it accepts a
remainder.
- If MAKE-DEFAULT, make the new synopsis the default one."
  (declare (ignore postfix item))
  (let ((synopsis (apply #'make-instance 'synopsis
			 (remove-keys keys :make-default))))
    (when make-default
      (setq *synopsis* synopsis))
    synopsis))

(defmacro defsynopsis ((&rest keys &key postfix make-default) &body forms)
  "Define a new synopsis."
  (declare (ignore postfix make-default))
  `(make-synopsis
    ,@keys
    ,@(loop :for form :in forms
	    :nconc (list :item
			 (let ((item-name
				 (when (consp form)
				   (car (member (symbol-name (car form))
						*item-names*
						:test #'string=)))))
			   (if item-name
			       (list*
				(intern
				 ;; #### NOTE: case portability
				 (cond ((string= item-name (string :group))
					(string :defgroup))
				       (t
					(format nil "~A~A"
					  (string :make-)
					  item-name)))
				 :net.didierverna.clon)
				(cdr form))
			     form))))))


;;; synopsis.lisp ends here

;;; cmdline.lisp --- Command-line management

;; Copyright (C) 2010, 2011, 2015 Didier Verna

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


;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun option-call-p (str)
  "Return true if STR looks like an option call."
  (unless (zerop (length str))
    (or (eq (elt str 0) #\-)
	(eq (elt str 0) #\+))))

(defun argument-popable-p (cmdline)
  "Return true if the first CMDLINE item is an argument."
  (and (car cmdline)
       (not (option-call-p (car cmdline)))))

(defmacro maybe-pop-argument (cmdline option cmdline-argument)
  "Pop OPTION's argument from CMDLINE if needed.
If so, store it in CMDLINE-ARGUMENT."
  ;; At the time this macro is called, CMDLINE-ARGUMENT may already contain
  ;; something, provided by either a sticky argument from a short call, or an
  ;; =-syntax from a long call. Remember that these are the only 2 ways to
  ;; provide optional arguments, so the need to pop something occurs only
  ;; when an argument is mandatory, and it is still missing.
  `(when (and (null ,cmdline-argument)
	      (argument-required-p ,option)
	      (argument-popable-p ,cmdline))
     (setq ,cmdline-argument (pop ,cmdline))))



;; ==========================================================================
;; Command-line error management (regarding known options)
;; ==========================================================================

(define-condition cmdline-error (error)
  ((item :documentation "The concerned command-line item."
	 :type string
	 :initarg :item
	 :reader item))
  (:documentation "An error related to a command-line item."))

(define-condition cmdline-option-error (cmdline-error option-error)
  ((item ;; inherited from the CMDLINE-ERROR condition
    :documentation "The option's name as it appears on the command-line."
    :initarg :name
    :reader name))
  (:documentation "An error related to a command-line (known) option."))

(define-condition spurious-cmdline-argument (cmdline-option-error)
  ((argument :documentation "The spurious argument."
	     :type string
	     :initarg :argument
	     :reader argument))
  (:report (lambda (error stream)
	     (format stream "Option '~A': spurious argument ~S."
	       (name error) (argument error))))
  (:documentation "A spurious command-line argument error."))

;; #### NOTE: this macro is currently used only once: in the flags long call
;; retrieval process.
(defmacro restartable-spurious-cmdline-argument-error
    ((option name argument) &body body)
  "Restartably throw a spurious-cmdline-argument error.
The error relates to the command-line use of OPTION called by NAME with
ARGUMENT.
BODY constitutes the body of the only restart available, discard-argument, and
should act as if ARGUMENT had not been provided."
  `(restart-case (error 'spurious-cmdline-argument
			:option ,option
			:name ,name
			:argument ,argument)
     (discard-argument ()
       :report "Discard spurious argument."
       ,@body)))

(define-condition invalid-negated-syntax (cmdline-option-error)
  ()
  (:report (lambda (error stream)
	     (format stream "Option '~A': invalid negated syntax."
	       (name error))))
  (:documentation "An invalid negated syntax error."))

(defmacro restartable-invalid-negated-syntax-error ((option) &body body)
  "Restartably throw an invalid-negated-syntax error.
The error relates to the command-line use of OPTION.
BODY constitutes the body of the only restart available,
use-short-call, and should act as if OPTION had been normally called by short
name."
  `(restart-case (error 'invalid-negated-syntax
			:option ,option
			:name (short-name ,option))
     (use-short-call ()
       :report "Fake a normal call by short name."
       ,@body)))

(define-condition invalid-cmdline-argument (cmdline-option-error invalid-argument)
  ()
  (:report (lambda (error stream)
	     (format stream "Option '~A': invalid argument ~S.~@[~%~A~]"
	       (name error) (argument error) (comment error))))
  (:documentation "An invalid command-line argument error."))

(define-condition missing-cmdline-argument (cmdline-option-error)
  ()
  (:report (lambda (error stream)
	     (format stream "Option '~A': missing argument." (name error))))
  (:documentation "A missing command-line argument error."))



;; ==========================================================================
;; The Command-Line Retrieval Protocol
;; ==========================================================================

;; #### FIXME: we should split this into flag.lisp and valued.lisp but we
;; can't because this file is loaded only after the options files.

(defun cmdline-convert (valued-option cmdline-name cmdline-argument)
  "Convert CMDLINE-ARGUMENT to VALUED-OPTION's value.
This function is used when the conversion comes from a command-line usage of
VALUED-OPTION, called by CMDLINE-NAME, and intercepts invalid-argument errors
to raise the higher level invalid-cmdline-argument error instead."
  (handler-case (restartable-convert valued-option cmdline-argument)
    (invalid-argument (error)
      (error 'invalid-cmdline-argument
	     :option valued-option
	     :name cmdline-name
	     :argument cmdline-argument
	     :comment (comment error)))
    ;; #### NOTE: by catching all other errors to just re-signal them here, we
    ;; ensure that the lower-level restarts established by RESTARTABLE-CONVERT
    ;; (see valued.lisp) are discarded. Otherwise, they would double those
    ;; established in RESTARTABLE-CMDLINE-CONVERT below.
    (error (error)
      (error error))))

(defun restartable-cmdline-convert (valued-option cmdline-name cmdline-argument)
  "Restartably convert CMDLINE-ARGUMENT to VALUED-OPTION's value.
This function is used when the conversion comes from a command-line usage of
VALUED-OPTION, called by CMDLINE-NAME.

As well as conversion errors, this function might raise a
missing-cmdline-argument error if CMDLINE-ARGUMENT is nil and an argument is
required.

Available restarts are (depending on the context):
- use-fallback-value: return FALLBACK-VALUE,
- use-default-value: return VALUED-OPTION's default value,
- use-value: return another (already converted) value,
- use-argument: return the conversion of another argument.

Return two values: VALUED-OPTION's value and the actual value source.
The value source may be :cmdline, :fallback or :default."
  (restart-case
      (cond ((argument-required-p valued-option)
	     (if cmdline-argument
		 (values
		  (cmdline-convert valued-option cmdline-name cmdline-argument)
		  :cmdline)
	       (error 'missing-cmdline-argument
		      :option valued-option :name cmdline-name)))
	    (cmdline-argument
	     (values
	      (cmdline-convert valued-option cmdline-name cmdline-argument)
	      :cmdline))
	    (t
	     (if (slot-boundp valued-option 'fallback-value)
		 (values (fallback-value valued-option) :fallback)
	       (values (default-value valued-option) :default))))
    (use-fallback-value ()
      :test (lambda (error)
	      (declare (ignore error))
	      (and (not (argument-required-p valued-option))
		   (slot-boundp valued-option 'fallback-value)))
      :report (lambda (stream)
		(format stream "Use fallback value (~S)."
		  (fallback-value valued-option)))
      (values (fallback-value valued-option) :fallback))
    (use-default-value ()
      :test (lambda (error)
	      (declare (ignore error))
	      (slot-boundp valued-option 'default-value))
      :report (lambda (stream)
		(format stream "Use option's default value (~S)."
		  (default-value valued-option)))
      (values (default-value valued-option) :default))
    (use-value (value)
      :report "Use an alternate value (already converted)."
      :interactive read-value
      (values (restartable-check valued-option value) :cmdline))
    (use-argument (cmdline-argument)
      :report "Use an alternate argument (subject to conversion)."
      :interactive read-argument
      (restartable-cmdline-convert
       valued-option cmdline-name cmdline-argument))))

(defgeneric retrieve-from-long-call
    (option cmdline-name &optional cmdline-argument cmdline)
  (:documentation "Retrieve OPTION's value from a long call.
CMDLINE-NAME is the name used on the command-line.
CMDLINE-ARGUMENT is a potentially already parsed cmdline argument.
Otherwise, CMDLINE is where to find an argument.
This function returns three values:
- the retrieved value,
- the value source,
- the new command-line (possibly with the first item popped if the option
  requires an argument).")
  ;; Method for non-valued options (currently, only flags):
  (:method ((option option) cmdline-name  &optional cmdline-argument cmdline)
    ;; CMDLINE-ARGUMENT might be non-nil when a non-valued option was given a
    ;; spurious argument through an =-syntax.
    (if cmdline-argument
	(restartable-spurious-cmdline-argument-error
	    (option cmdline-name cmdline-argument)
	  (values t :cmdline cmdline))
      (values t :cmdline cmdline)))
  ;; Method for all valued options:
  (:method ((option valued-option) cmdline-name &optional cmdline-argument cmdline)
    (maybe-pop-argument cmdline option cmdline-argument)
    (multiple-value-bind (value source)
	(restartable-cmdline-convert option cmdline-name cmdline-argument)
      (values value source cmdline))))

(defgeneric retrieve-from-short-call (option &optional cmdline-argument cmdline)
  (:documentation "Retrieve OPTION's value from a short call.
CMDLINE-ARGUMENT is a potentially already parsed cmdline argument.
Otherwise, CMDLINE is where to find an argument.
This function returns three values:
- the retrieved value,
- the value source,
- the new command-line (possibly with the first item popped if the option
  requires an argument).")
  ;; Method for non-valued options (currently, only flags):
  (:method ((option option) &optional cmdline-argument cmdline)
    ;; See comment about this assertion in OPTION-STICKY-DISTANCE.
    #+ecl (declare (ignore option))
    (assert (null cmdline-argument))
    (values t :cmdline cmdline))
  ;; Method for valued options:
  (:method ((option valued-option) &optional cmdline-argument cmdline)
    (maybe-pop-argument cmdline option cmdline-argument)
    (multiple-value-bind (value source)
	(restartable-cmdline-convert option (short-name option)
				     cmdline-argument)
      (values value source cmdline))))

(defgeneric retrieve-from-negated-call (option)
  (:documentation "Retrieve OPTION's value from a negated call.")
  ;; Method for non-valued options (currently, only flags):
  (:method ((option option))
    (restartable-invalid-negated-syntax-error (option)
      (values t :cmdline)))
  ;; Method for valued options:
  (:method ((option valued-option))
    (restartable-invalid-negated-syntax-error (option)
      (retrieve-from-short-call option)))
  ;; Method for negatable options (currently, the switch-based ones):
  (:method ((negatable negatable))
    #+ecl (declare (ignore negatable))
    (values nil :cmdline)))


;;; cmdline.lisp ends here

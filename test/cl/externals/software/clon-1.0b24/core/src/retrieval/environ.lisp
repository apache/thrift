;;; environ.lisp --- Environment management

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
;; Environmental Error Management
;; ==========================================================================

;; #### NOTE: currently, there is only one environment error: an invalid value
;; for an environment variable associated with an option. This means that the
;; ENV-VAR slot below is redundant, because the environment variable can be
;; accessed through the option object. However, the design is cleaner this way
;; (it is coherent with the command-line one), and maybe if one day I have to
;; extend it, I'll be happy I got it right in the first place.
;; #### NOTE: as a matter of fact, I just thought of something: what about
;; supporting a /list/ of associated environment variables? This would
;; perfectly justify the comment above.
(define-condition environment-error (error)
  ((env-var :documentation "The concerned environment variable."
	    :type string
	    :initarg :env-var
	    :reader env-var))
  (:documentation "An error related to an environment variable."))

(define-condition environmental-option-error (environment-error option-error)
  ()
  (:documentation "An error related to an option's environment variable."))

(define-condition invalid-environment-value
    (environmental-option-error invalid-argument)
  ((argument ;; inherited from the INVALID-ARGUMENT condition
    :documentation "The invalid environment variable value."
    :initarg :env-val
    :reader env-val))
  (:report
   (lambda (error stream)
     (format stream "Environment variable ~A (for option ~S): ~
		    invalid value ~S.~@[~%~A~]"
       (env-var error)
       (or (long-name (option error)) (short-name (option error)))
       (env-val error)
       (comment error))))
  (:documentation "An invalid environment variable's value error."))



;; ==========================================================================
;; The Environement Retrieval Protocol
;; ==========================================================================

;; #### WARNING: given the idea of supporting a list of env vars, I would need
;; to modify this function in order to pass the env-var itself. This protocol
;; would become even more similar to the command-line one. Yummy...
(defun environment-convert (valued-option env-val)
  "Convert ENV-VAL to VALUED-OPTION's value.
This function is used when the conversion comes from an environment variable
associated with VALUED-OPTION, and intercepts invalid-argument errors
to raise the higher level invalid-environment-value error instead."
  (handler-case (restartable-convert valued-option env-val)
    (invalid-argument (error)
      (error 'invalid-environment-value
	     :option valued-option
	     :env-var (env-var valued-option)
	     :env-val env-val
	     :comment (comment error)))
    ;; #### NOTE: by catching all other errors to just re-signal them here, we
    ;; ensure that the lower-level restarts established by RESTARTABLE-CONVERT
    ;; (see valued.lisp) are discarded. Otherwise, they would double those
    ;; established in RESTARTABLE-ENVIRONMENT-CONVERT below.
    (error (error)
      (error error))))

(defun read-env-val (env-var)
  "Read ENV-VAR's new value from standard input."
  (format t "Please type in a new value for the ~A environment variable:~%"
    env-var)
  (list (read-line)))

;; #### WARNING: given the idea of supporting a list of env vars, I would need
;; to modify this function in order to pass the env-var itself. This protocol
;; would become even more similar to the command-line one. Yummy...
(defun restartable-environment-convert (valued-option env-val)
  "Restartably convert ENV-VAL to VALUED-OPTION's value.
This function is used when the conversion comes from an environment variable
associated with VALUED-OPTION.

Available restarts are:
- use-default-value: return VALUED-OPTION's default value,
- use-value: return another (already converted) value,
- use-argument: return the conversion of another argument,
- modify-env: modify the environment variable's value."
  (restart-case (environment-convert valued-option env-val)
    (use-default-value ()
      :test (lambda (error)
	      (declare (ignore error))
	      (slot-boundp valued-option 'default-value))
      :report (lambda (stream)
		(format stream "Use option's default value (~S)."
		  (default-value valued-option)))
      (default-value valued-option))
    (use-value (value)
      :report "Use an alternate value (already converted)."
      :interactive read-value
      (restartable-check valued-option value))
    (use-argument (argument)
      :report "Use an alternate argument (subject to conversion)."
      :interactive read-argument
      (restartable-environment-convert valued-option argument))
    ;; #### NOTE: JAVA doesn't provide a way to set an environment variable.
    ;; I've seen tricks around to modify the startup environment memory
    ;; mapping instead of doing a real putenv, but I'll just disable this
    ;; restart for now.
    #-abcl
    (modify-environment (env-val)
      :report "Modify the environment variable's value."
      :interactive (lambda () (read-env-val (env-var valued-option)))
      (putenv (env-var valued-option) env-val)
      (restartable-environment-convert valued-option env-val))))

;; #### FIXME: we should split this into flag.lisp and valued.lisp but we
;; can't because this fiel is loaded only after the options files.


;; #### WARNING: given the idea of supporting a list of env vars, I would need
;; to modify this function in order to pass the env-var itself. This protocol
;; would become even more similar to the command-line one. Yummy...
(defgeneric retrieve-from-environment (option env-val)
  (:documentation "Retrieve OPTION's value from the environment.
ENV-VAL is the value stored in the associated environment variable.")
  (:method :before (option env-val)
    "Assert that ENV-VAL is not null."
    ;; That's because getopt is not supposed to call this function unless
    ;; there is actually something to retrieve.
    #+(or ccl ecl clisp allegro) (declare (ignore option))
    (assert env-val))
  ;; Method for flags:
  (:method ((flag flag) env-val)
    #+(or ccl ecl clisp allegro) (declare (ignore env-val))
    #+ecl                        (declare (ignore flag))
    ;; #### NOTE: there's no way of providing an env var /without/ a value
    ;; (the value is at least the empty string). Consequently, we decide that
    ;; the presence of the env var, regardless of its value, stands for the
    ;; presence of the flag.
    t)
  ;; Method for all valued options:
  (:method ((option valued-option) env-val)
    (restartable-environment-convert option env-val)))


;;; environ.lisp ends here

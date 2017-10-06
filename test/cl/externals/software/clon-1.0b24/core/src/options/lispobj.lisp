;;; lispobj.lisp --- Read-from-string options

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

(in-package :net.didierverna.clon)
(in-readtable :net.didierverna.clon)


;; ==========================================================================
;; The LispObj Option Class
;; ==========================================================================

(defoption lispobj ()
  ((argument-name ;; inherited from the VALUED-OPTION class
    :initform "OBJ")
   (typespec
    :documentation "A type specifier the option's value should satisfy."
    :initform t
    :initarg :typespec
    :reader typespec))
  (:documentation "The LISPOBJ class.
This class implements read-from-string options."))


;; ------------------------------
;; Value Stringification protocol
;; ------------------------------

(defmethod stringify ((lispobj lispobj) value)
  "Transform LISPOBJ's VALUE into an argument."
  #+ecl (declare (ignore lispobj))
  (prin1-to-string value))


;; --------------------
;; Value Check protocol
;; --------------------

(defmethod check ((lispobj lispobj) value)
  "Check that VALUE is valid for LISPOBJ."
  (unless (typep value (typespec lispobj))
    (error 'invalid-value
	   :option lispobj
	   :value value
	   :comment (format nil "Value must satisfy ~A." (typespec lispobj))))
  value)


;; ----------------------------
;; Argument Conversion protocol
;; ----------------------------

;; #### FIXME: I need to handle other errors than just end-of-file. Probably
;; all reader errors, and why not simply all errors.
(defmethod convert ((lispobj lispobj) argument)
  "Convert ARGUMENT to a LISPOBJ value."
  (multiple-value-bind (value position)
      (handler-case (read-from-string argument)
	(end-of-file ()
	  (error 'invalid-argument
		 :option lispobj
		 :argument argument
		 :comment (format nil "Cannot parse argument ~S." argument))))
    (cond ((< position (length argument))
	   (error 'invalid-argument
		  :option lispobj
		  :argument argument
		  :comment (format nil "Cannot parse argument ~S." argument)))
	  ((typep value (typespec lispobj))
	   value)
	  (t
	   (error 'invalid-argument
		  :option lispobj
		  :argument argument
		  :comment (format nil "Argument ~S must evaluate to ~A."
			     argument (typespec lispobj)))))))



;; ==========================================================================
;; LispObj Instance Creation
;; ==========================================================================

(defun make-lispobj (&rest keys
		     &key short-name long-name description
			  argument-name argument-type
			  env-var
			  typespec fallback-value default-value
			  hidden)
  "Make a new lispobj option.
- SHORT-NAME is the option's short name (without the dash).
  It defaults to nil.
- LONG-NAME is the option's long name (without the double-dash).
  It defaults to nil.
- DESCRIPTION is the option's description appearing in help strings.
  It defaults to nil.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENV-VAR is the option's associated environment variable.
  It defaults to nil.
- TYPESPEC is a type specifier the option's value should satisfy.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore short-name long-name description
		   argument-name argument-type
		   env-var
		   typespec fallback-value default-value
		   hidden))
  (apply #'make-instance 'lispobj keys))

#i(make-internal-lispobj 2)
(defun make-internal-lispobj (long-name description
			      &rest keys
			      &key argument-name argument-type
				   env-var
				   typespec fallback-value default-value
				   hidden)
  "Make a new internal (Clon-specific) string option.
- LONG-NAME is the option's long-name, sans the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the options's description.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENV-VAR is the option's associated environment variable, sans the 'CLON_'
  prefix. It defaults to nil.
- TYPESPEC is a type specifier the option's value should satisfy.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore argument-name argument-type
		   env-var
		   typespec fallback-value default-value
		   hidden))
  (apply #'make-instance 'lispobj
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;;; lispobj.lisp ends here

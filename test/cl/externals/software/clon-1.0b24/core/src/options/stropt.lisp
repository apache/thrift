;;; stropt.lisp --- String Options

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
;; The String Option Class
;; ==========================================================================

(defoption stropt ()
  ((argument-name ;; inherited from the VALUED-OPTION class
    :initform "STR"))
  (:documentation "The STROPT class.
This class implements options the values of which are strings."))


;; ------------------------------
;; Value Stringification protocol
;; ------------------------------

(defmethod stringify ((stropt stropt) value)
  "Transform STROPT's VALUE into an argument."
  #+ecl (declare (ignore stropt))
  value)


;; --------------------
;; Value Check protocol
;; --------------------

(defmethod check ((stropt stropt) value)
  "Check that VALUE is valid for STROPT."
  (if (stringp value)
      value
    (error 'invalid-value
	   :option stropt
	   :value value
	   :comment "Value must be a string.")))


;; ----------------------------
;; Argument Conversion protocol
;; ----------------------------

(defmethod convert ((stropt stropt) argument)
  "Convert ARGUMENT to an STROPT value."
  #+ecl (declare (ignore stropt))
  argument)



;; ==========================================================================
;; Stropt Instance Creation
;; ==========================================================================

(defun make-stropt (&rest keys
		    &key short-name long-name description
			 argument-name argument-type
			 env-var fallback-value default-value
			 hidden)
  "Make a new string option.
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
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore short-name long-name description
		   argument-name argument-type
		   env-var fallback-value default-value
		   hidden))
  (apply #'make-instance 'stropt keys))

#i(make-internal-stropt 2)
(defun make-internal-stropt (long-name description
			     &rest keys
			     &key argument-name argument-type
				  env-var fallback-value default-value
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
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore argument-name argument-type
		   env-var fallback-value default-value
		   hidden))
  (apply #'make-instance 'stropt
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;;; stropt.lisp ends here

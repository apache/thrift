;;; switch.lisp --- Switch options

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
;; The Switch Class
;; ==========================================================================

(defoption switch (switch-base)
  ()
  (:documentation "The SWITCH class.
This class implements boolean options."))


;; ------------------------------
;; Value Stringification protocol
;; ------------------------------

(defmethod stringify ((switch switch) value)
  "Transform SWITCH's VALUE into an argument."
  (let ((position (position (argument-style switch)
			    (argument-styles switch))))
    (nth position
	 (if value
	     (yes-values switch)
	   (no-values switch)))))


;; --------------------
;; Value Check protocol
;; --------------------

(defmethod check ((switch switch) value)
  "Check that VALUE is valid for SWITCH."
  (unless (member value '(t nil))
    (error 'invalid-value
	   :option switch
	   :value value
	   :comment "Valid values are T or NIL."))
  value)


;; ----------------------------
;; Argument Conversion protocol
;; ----------------------------

(defmethod convert ((switch switch) argument)
  "Convert ARGUMENT to a SWITCH value."
  (let ((match (closest-match argument
			      (append (yes-values switch) (no-values switch))
			      :ignore-case t)))
    (cond ((member match (yes-values switch) :test #'string-equal)
	   t)
	  ((member match (no-values switch) :test #'string-equal)
	   nil)
	  (t
	   (error 'invalid-argument
		  :option switch
		  :argument argument
		  :comment (format nil "Valid arguments are: ~A."
			     (list-to-string (append (yes-values switch)
						     (no-values switch)))))))))



;; ==========================================================================
;; Switch Instance Creation
;; ==========================================================================

(defmethod initialize-instance :after ((switch switch) &key)
  "Provide an argument name conformant to the selected argument style."
  (setf (slot-value switch 'argument-name)
	(string-downcase (symbol-name (argument-style switch)))))

(defun make-switch (&rest keys &key short-name long-name description
				   argument-style argument-type
				   env-var default-value hidden)
  "Make a new switch.
- SHORT-NAME is the switch's short name (without the dash).
  It defaults to nil.
- LONG-NAME is the switch's long name (without the double-dash).
  It defaults to nil.
- DESCRIPTION is the switch's description appearing in help strings.
  It defaults to nil.
- ARGUMENT-STYLE is the switch's argument display style. It can be one of
  :yes/no, :on/off, :true/false, :yup/nope or :yeah/nah.
  It defaults to :yes/no.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENV-VAR is the switch's associated environment variable.
  It defaults to nil.
- DEFAULT-VALUE is the switch's default value, if any.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore short-name long-name description
		   argument-style argument-type
		   env-var default-value hidden))
  (apply #'make-instance 'switch keys))

#i(make-internal-switch 2)
(defun make-internal-switch (long-name description
			     &rest keys &key argument-style argument-type
					    env-var default-value hidden)
  "Make a new internal (Clon-specific) switch.
- LONG-NAME is the switch's long-name, sans the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the switch's description.
- ARGUMENT-STYLE is the switch's argument display style. It can be one of
  :yes/no, :on/off, :true/false, :yup/nope or :yeah/nah.
  It defaults to :yes/no.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENV-VAR is the switch's associated environment variable, sans the 'CLON_'
  prefix. It defaults to nil.
- DEFAULT-VALUE is the switch's default value, if any.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore  argument-style argument-type env-var default-value hidden))
  (apply #'make-instance 'switch
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;;; switch.lisp ends here

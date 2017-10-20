;;; flag.lisp --- Flag options

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
;; The Flag Class
;; ==========================================================================

(defclass flag (option)
  ()
  (:documentation "The FLAG class.
This class implements options that don't take any argument."))



;; ==========================================================================
;; Flag Instance Creation
;; ==========================================================================

(defun make-flag (&rest keys &key short-name long-name description env-var
				  hidden)
  "Make a new flag.
- SHORT-NAME is the option's short name (without the dash).
  It defaults to nil.
- LONG-NAME is the option's long name (without the double-dash).
  It defaults to nil.
- DESCRIPTION is the option's description appearing in help strings.
  It defaults to nil.
- ENV-VAR is the flag's associated environment variable.
  It defaults to nil.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore short-name long-name description env-var hidden))
  (apply #'make-instance 'flag keys))

#i(make-internal-flag 2)
(defun make-internal-flag (long-name description
			   &rest keys &key env-var hidden)
  "Make a new internal (Clon-specific) flag.
- LONG-NAME is the flag's long-name, sans the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the flag's description.
- ENV-VAR is the flag's associated environment variable, sans the 'CLON_'
  prefix. It default to nil.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore env-var hidden))
  (apply #'make-instance 'flag
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;;; flag.lisp ends here

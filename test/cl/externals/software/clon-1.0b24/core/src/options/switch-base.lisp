;;; switch-base.lisp --- Base for switch-like options

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
;; The SWITCH-BASE Class
;; ==========================================================================

;; #### NOTE: previously, the argument-styles, yes-values and no-values slots
;; were implemented as shared slots. I changed this when porting to ABCL
;; because contrary to the other supported implementations, ABCL initialized
;; shared slot when the first instance is created instead of when the class is
;; finalized. As a consequence, this breaks the :before method below in which
;; the argument-styles slot would be unbound.
(defabstract switch-base (negatable)
  ((argument-styles :documentation "The possible argument styles.
The position of every argument style in the list must correspond to the
position of the associated strings in the yes-values and no-values slots."
		    :type list
		    :initarg :argument-styles
		    :accessor argument-styles)
   (yes-values :documentation "The possible 'yes' values."
	       :type list
	       :initarg :yes-values
	       :accessor yes-values)
   (no-values :documentation "The possible 'no' values."
	      :type list
	      :initarg :no-values
	      :accessor no-values)
   (argument-style :documentation "The selected argument style."
		   :type keyword
		   :initform :yes/no
		   :initarg :argument-style
		   :reader argument-style))
  (:default-initargs
   :argument-type :optional
   :argument-styles '(:yes/no :on/off :true/false :yup/nope :yeah/nah)
   :yes-values '("yes" "on" "true" "yup" "yeah")
   :no-values '("no" "off" "false" "nope" "nah"))
  (:documentation "The SWITCH-BASE abstract class.
This class provides support for options including boolean values."))

(defmethod initialize-instance :before
    ((switch-base switch-base)
     &key (argument-style :yes/no argument-style-supplied-p) argument-styles)
  "Check for validity of the :ARGUMENT-STYLE initarg."
  (when argument-style-supplied-p
    (unless (member argument-style argument-styles)
      (error "Invalid argument style initarg: ~S." argument-style))))

;; #### NOTE: ideally, I would like to do this in an :after method. But I
;; can't because of the :before method for the VALUED-OPTION class which
;; checks that when the argument type is optional, there's at least a fallback
;; or a default value provided. Sometimes, I think I'm too maniac with checks.
;; If I were to do all this is in :after methods, I would be stuck as well
;; because the one for the VALUED-OPTION class would be called *before* the
;; one for the SWITCH-BASED class. :-(
(defmethod initialize-instance :around
    ((switch-base switch-base) &rest keys &key argument-type)
  "Provide a fallback value of t when ARGUMENT-TYPE is optional."
  (when (eq argument-type :optional)
    (setq keys (list* :fallback-value t keys)))
  (apply #'call-next-method switch-base keys))


;;; switch-base.lisp ends here

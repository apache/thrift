;;; group.lisp --- Group management

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
;; The Group class
;; ==========================================================================

(defclass group (container)
  ((header :documentation "The group's header."
	   :initform nil
	   :initarg :header
	   :reader header))
  (:documentation "The GROUP class.
This class groups other groups, options or strings together, effectively
implementing hierarchical program command-line."))


;; ---------------------------
;; Help specification protocol
;; ---------------------------

(defmethod help-spec ((group group) &key)
  "Return GROUP's help specification."
  (accumulate (group)
    (accumulate (header)
      (header group))
    ;; This calls the container's method.
    (let ((items (call-next-method)))
      (when items
	(push 'items items)))))



;; ==========================================================================
;; Group Instance Creation
;; ==========================================================================

(defun make-group (&rest keys &key header item hidden)
  "Make a new group."
  (declare (ignore header item hidden))
  (apply #'make-instance 'group keys))

(defmacro %defgroup (internalp (&rest keys &key header hidden) &body forms)
  "Define a new group."
  (declare (ignore header hidden))
  `(make-group
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
					(string :%defgroup))
				       (t
					(format nil "~A~:[~*~;~A~]~A"
					  (string :make-)
					  internalp
					  (string :internal-)
					  item-name)))
				 :net.didierverna.clon)
				(if (string= item-name (string :group))
				    (list* internalp (cdr form))
				  (cdr form)))
			     form))))))

(defmacro defgroup ((&rest keys &key header hidden) &body forms)
  "Define a new group.
KEYS are initargs to MAKE-GROUP (currently, only :header).
Each form in FORMS will be treated as a new :item.
The CAR of each form is the name of the operation to perform: TEXT, GROUP, or
an option class name. The rest are the arguments to the MAKE-<OP> function or
the DEFGROUP macro."
  (declare (ignore header hidden))
  `(%defgroup nil ,keys ,@forms))


;;; group.lisp ends here

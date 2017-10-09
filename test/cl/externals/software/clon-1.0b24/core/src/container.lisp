;;; container.lisp --- Container management

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
;; The Container Class
;; ==========================================================================

(defabstract container (item)
  ((items :documentation "The items in the container."
	  :type list
	  :initform nil
	  :initarg :items
	  :reader items))
  (:documentation "The CONTAINER class.
This class is a mixin used in synopsis and groups to represent the program's
command-line hierarchy."))


;; ------------------
;; Traversal protocol
;; ------------------

(defmethod untraverse ((container container))
  "Untraverse all CONTAINER items."
  (dolist (item (items container))
    (untraverse item))
  container)


;; -------------------------
;; Name clash check protocol
;; -------------------------

(defmethod check-name-clash ((container container) item2)
  "Check for name clash between CONTAINER's options and ITEM2's ones."
  (dolist (item1 (items container))
    (check-name-clash item1 item2)))

(defmethod check-name-clash (item1 (container container))
  "Check for name clash between ITEM1's options and CONTAINER's ones."
  (dolist (item2 (items container))
    (check-name-clash item1 item2)))

(defmethod check-name-clash ((container1 container) (container2 container))
  "Check for name clash between CONTAINER1's options and CONTAINER2's ones."
  (dolist (item1 (items container1))
    (dolist (item2 (items container2))
      (check-name-clash item1 item2))))


;; -------------------------
;; Help specifation protocol
;; -------------------------

(defmethod help-spec ((container container) &key)
  "Return CONTAINER's help specification."
  (loop :for item :in (items container)
	:when (help-spec item)
	  :collect :it))



;; ==========================================================================
;; Container Instance Creation
;; ==========================================================================

(defmethod initialize-instance :around
    ((container container) &rest keys &key item)
  "Canonicalize initialization arguments.
This involves:
- computing the :items initarg from the :item ones."
  (declare (ignore item))
  (apply #'call-next-method container
	 :items (remove :item (select-keys keys :item))
	 (remove-keys keys :item)))

(defmethod initialize-instance :after ((container container) &key)
  "Perform name clash check on CONTAINER's items."
  (loop :for items :on (items container)
	:while (cdr items)
	:do (loop :for item2 in (cdr items)
		  :do (check-name-clash (car items) item2))))


;;; container.lisp ends here

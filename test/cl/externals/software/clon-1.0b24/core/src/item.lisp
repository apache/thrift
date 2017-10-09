;;; item.lisp --- Item objects

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
;; The Item Class
;; ==========================================================================

(defabstract item ()
  ((traversedp :documentation "The item's traversal state."
	       :initform nil
	       :accessor traversedp)
   (hiddenp :documentation "Whether the item is hidden in help strings."
	    :initform nil
	    ;; #### NOTE: the initarg below doesn't follow the *p convention
	    ;; because that would look strange in a declarative group
	    ;; construction.
	    :initarg :hidden
	    :reader hiddenp))
  (:documentation "The ITEM class.
This class is the base class for all synopsis items."))



;; ==========================================================================
;; The Traversal Protocol
;; ==========================================================================

(defgeneric untraverse (item)
  (:documentation "Reset ITEM's traversal state, and return ITEM.")
  (:method :after ((item item))
    "Mark ITEM as untraversed."
    (setf (traversedp item) nil)))



;; ==========================================================================
;; The Help Specification Protocol
;; ==========================================================================

(defgeneric help-spec (item &key &allow-other-keys)
  (:documentation "Return ITEM's help specification.")
  (:method :around ((item item) &key unhide)
    "Call the actual method only when ITEM is not hidden or UNHIDE."
    (when (or (not (hiddenp item)) unhide)
      (call-next-method))))


;;; item.lisp ends here

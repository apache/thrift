;;; text.lisp --- Text management

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
;; The Text Class
;; ==========================================================================

(defclass text (item)
  ((contents :documentation "The actual text string."
	     :type string
	     :initarg :contents
	     :reader contents))
  (:documentation "The TEXT class.
This class implements plain text objects appearing in a synopsis."))


;; ------------------
;; Traversal protocol
;; ------------------

(defmethod untraverse ((text text))
  "TEXT is a terminal object: just return it."
  text)


;; ---------------------------
;; Help specification protocol
;; ---------------------------

(defmethod help-spec ((text text) &key)
  "Return TEXT's help specification."
  (accumulate (text)
    (contents text)))



;; ==========================================================================
;; Text Instance Creation
;; ==========================================================================

(defun make-text (&rest keys &key contents hidden)
  "Make a new text.
- CONTENTS is the actual text to display.
- When HIDDEN, the text doesn't appear in help strings."
  (declare (ignore contents hidden))
  (apply #'make-instance 'text keys))

;; #### NOTE: currently, there is no difference between internal and external
;; text, but this might change in the future. Besides, having this function it
;; simplifies the defgroup and defsynopsis macros.
(defun make-internal-text (&rest keys &key contents hidden)
  (declare (ignore contents hidden))
  (apply #'make-text keys))

;;; text.lisp ends here

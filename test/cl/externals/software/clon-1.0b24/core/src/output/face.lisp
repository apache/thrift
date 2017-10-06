;;; face.lisp --- Face management

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


;; =========================================================================
;; The Face Class
;; =========================================================================

;; Note that although we have some boolean slots below, their initargs don't
;; follow the usual *p convention. That's because they would look strange when
;; used as declarative properties in a theme file, where it is not obvious to
;; the end-user that she's actually calling instantiation functions with
;; initargs (or even using Lisp at all).
(defclass face ()
  ((name :documentation "The face name."
	 :initarg :name
	 :reader name)
   ;; Layout properties:
   ;; #### FIXME: currently, layout properties are not inherited (see the
   ;; end-user manual). However, a nicer policy would be to inherit from the
   ;; same faces and not from the other ones. E.g. group to group, syntax to
   ;; syntax etc.
   (visiblep :documentation "Whether the face is visible."
	     :initarg :visible
	     :initform t
	     :reader visiblep)
   (left-padding :documentation "The face left padding.
This property can take the following forms:
- <NUMBER>: the padding is relative to the enclosing face,
- SELF: the padding is set to wherever the face happens to be opened,
- (<NUMBER> ABSOLUTE): the padding is set in absolute value,
- (<NUMBER> :RELATIVE-TO <FACE-NAME>): the padding is set relatively to a
  parent face named FACE-NAME."
		 :initarg :padding-left
		 :initform 0
		 :reader left-padding)
   (right-padding :documentation "The face right padding.
This property can take the following forms:
- <NUMBER>: the padding is relative to the enclosing face,
- SELF: the padding is set to wherever the face happens to be closed,
- (<NUMBER> ABSOLUTE): the padding is set in absolute value,
- (<NUMBER> :RELATIVE-TO <FACE-NAME>): the padding is set relatively to a
  parent face named FACE-NAME."
		  :initarg :padding-right
		  :initform 'self
		  :reader right-padding)
   (top-padding :documentation "The face top padding.
This property can take the following forms:
- nil: the output can start right away,
- 0: the output should start on the next line,
- N>0: there should be N empty lines before the output."
		:initarg :padding-top
		:initform nil
		:reader top-padding)
   (bottom-padding :documentation "The face bottom padding.
This property can take the following forms:
- nil: the next output can start right at the end of this face's,
- 0: the next output should start on the next line,
- N>0: there should be N empty lines before the next output."
		   :initarg :padding-bottom
		   :initform nil
		   :reader bottom-padding)
   (item-separator :documentation "The face item separator."
		   :initarg :item-separator
		   :initform #\space
		   :reader item-separator)
   ;; Highlight properties (ISO/IEC 6429 SGR):
   ;; Note that the readers are in fact not used anywhere, because highlight
   ;; properties are treated in batch.
   (intensity :documentation "The face intensity."
	      :initarg :intensity
	      :reader intensity)
   (italicp :documentation "The face's italic status."
	    :initarg :italic
	    :reader italicp)
   (underline :documentation "The face's underline level."
	      :initarg :underline
	      :reader underline)
   (blink :documentation "The face's blink speed."
	  :initarg :blink
	  :reader blink)
   (inversep :documentation "The face's inverse video status."
	     :initarg :inverse
	     :reader inversep)
   (concealedp :documentation "The face's concealed status."
	       :initarg :concealed
	       :reader concealedp)
   (crossed-out-p :documentation "The face's crossed out status."
		  :initarg :crossed-out
		  :reader crossed-out-p)
   (framedp :documentation "The face's framed status."
	    :initarg :framed
	    :reader framedp)
   (foreground :documentation "The face foreground."
	       :initarg :foreground
	       :reader foreground)
   (background :documentation "The face background."
	       :initarg :background
	       :reader background)
   ;; Tree structure:
   (subfaces :documentation "The face children."
	     :initarg :subfaces
	     :reader subfaces)
   (parent :documentation "The face parent."
	   :initform nil
	   :reader parent))
  (:documentation "The FACE class."))

(defun add-subface (face subface)
  "Add SUBFACE to FACE's subfaces and return it."
  (setf (slot-value subface 'parent) face)
  (push subface (slot-value face 'subfaces))
  subface)


;; =========================================================================
;; The Face Property Access Protocol
;; =========================================================================

(defparameter *highlight-properties*
    '(intensity italicp underline blink inversep concealedp crossed-out-p
      framedp foreground background)
  "The highlight face properties.")

(defmethod slot-unbound (class (face face) slot)
  "Look up SLOT's value in FACE's parent if it's a highlight property.
If FACE has no parent, return nil.
For other properties, trigger an error."
  #+(or ccl ecl clisp allegro) (declare (ignore class))
  (let ((property (member slot *highlight-properties*)))
    (if property
	(when (parent face)
	  (slot-value (parent face) slot))
      (call-next-method))))

(defun face-highlight-property-set-p (face property)
  "Return t if PROPERTY is set explicitely in FACE."
  (slot-boundp face property))

(defun face-highlight-property-value (face property)
  "Return PROPERTY's value in FACE.
Since faces inherit highlight properties, the actual value might come from one
of FACE's ancestors.
if PROPERTY is not et, return nil."
  (slot-value face property))



;; =========================================================================
;; The Face Tree Copy Protocol
;; =========================================================================

(defun attach-face-tree (face face-tree
			 &aux (new-tree (copy-instance face-tree)))
  "Create a copy of FACE-TREE, attach it to FACE and return it.
Apart from the parenting information, the copied faces share slot values with
the original ones."
  (setf (slot-value new-tree 'subfaces)
	(mapcar (lambda (subtree)
		  (attach-face-tree new-tree subtree))
		(subfaces new-tree)))
  (add-subface face new-tree))

(defgeneric subface (face |name(s)|)
  (:documentation "Return subface of FACE named NAME(S) or nil.
If a list of names is provided instead of a single one, follow a subface
branch matching those names to find the leaf face.")
  (:method (face (name symbol))
    "Return FACE'subface named NAME, or nil."
    (find name (subfaces face) :key #'name))
  (:method (face (names list) &aux (branch (subface face (car names))))
    "Return the leaf face from FACE'subbranch matching NAMES, or nil."
    (or (when (null (cdr names))
	  branch)
	(when branch
	  (subface branch (cdr names))))))

(defun search-branch (face names)
  "Search for a branch of faces named NAMES starting at FACE.
The branch is searched for as a direct subbranch of FACE, or as a direct
subbranch of FACE's ancestors.
If a branch is found, return its leaf face. Otherwise return nil."
  (or (subface face names)
      (loop :for parent := (parent face) :then (parent parent)
	    :while parent
	    :when (subface parent names)
	      :return :it
	    :finally (return nil))))

(defun search-face (face name &optional error-me)
  "Search for a face named NAME starting at FACE.
The face is looked for as a direct subface of FACE (in which case it is simply
returned), or up in the hierarchy and by successive upper branches (in which
case it is copied and attached to FACE).
If ERROR-ME, trigger an error if no face is found; otherwise, return nil."
  (or (subface face name)
      (and (parent face)
	   (loop :for names :on (loop :with names := (list name)
				      :for parent := face :then (parent parent)
				      :while parent
				      :do (push (name parent) names)
				      :finally (return names))
		 :for found := (search-branch (parent face) names)
		 :when found
		   :return (attach-face-tree face found)))
      (when error-me (error "Face ~A not found." name))))

(defun parent-generation (face parent-name)
  "Return FACE's parent generation for PARENT-NAME.
That is, 1 if PARENT-NAME names FACE's parent, 2 if it names its grand-parent
etc. If PARENT-NAME does not name one of FACE's ancestors, trigger an error."
  (loop :for generation :from 1
	:for parent := (parent face) :then (parent parent)
	:while parent
	:when (eql (name parent) parent-name)
	  :return generation
	:finally (error "Parent face ~A for face ~A not found."
			parent-name (name face))))



;; =========================================================================
;; Face Instance Creation
;; =========================================================================

;; #### NOTE: although we don't use them explicitely, the SUBFACE, BOLD,
;; DISPLAY, HIDDEN and REVEALED initargs are declared valid below.
(defmethod initialize-instance :around
    ((instance face) &rest keys &key face bold display hidden revealed)
  "Canonicalize initialization arguments.
This involves:
- computing the :subfaces initarg from the :face ones,
- handling convenience highlight properties."
  (declare (ignore face bold display hidden revealed))
  (apply #'call-next-method instance
	 :subfaces (remove :face (select-keys keys :face))
	 (replace-keys keys :face
		       '(:bold :intensity (t bold) (nil normal))
		       '(:display ((nil hidden) :visible nil)
				  ((t visible revealed) :concealed nil)
				  (concealed :concealed t))
		       '(:hidden :visible (t nil) (nil t))
		       '(:revealed :concealed (t nil) (nil t)))))

;; #### NOTE: we use the NAME keyword here because we're in the before method,
;; hence FACE name has not been initialized yet.
(defmethod initialize-instance :before ((face face) &key name subfaces)
  "Check for unicity of FACE subfaces."
  #+ecl (declare (ignore face))
  (loop :for faces :on subfaces
	:while (cdr faces)
	:when (member (name (car faces))
		      (mapcar #'name (cdr faces)))
	  :do (error "Duplicate subface ~A for face ~A."
		     (name (car faces)) name)))

(defmethod initialize-instance :after ((face face) &key)
  "Fill in the parent slot of all subfaces."
  (mapc (lambda (child)
	  (setf (slot-value child 'parent) face))
	(subfaces face)))

(defgeneric make-face-tree (definition &optional face-class)
  (:documentation "Make a FACE-CLASS face tree from DEFINITION.")
  (:method ((definition list) &optional (face-class 'face))
    "Make a FACE-CLASS face tree from a list of face name and initargs."
    (apply #'make-instance face-class
	   :name (car definition)
	   (loop :for key :in (cdr definition) :by #'cddr
		 :for val :in (cddr definition) :by #'cddr
		 :if (eq key :face)
		   :nconc (list :face (make-face-tree val face-class))
		 :else
		   :nconc (list key val))))
  (:method ((name symbol) &optional (face-class 'face))
    "Create a face named NAME."
    (funcall #'make-instance face-class :name name)))

(defun make-raw-face-tree (&optional (face-class 'face))
  "Make a raw (boring yet functional) face tree."
  (make-face-tree '(toplevel
		    :face (synopsis
			   :padding-bottom 1
			   :face header
			   :face program
			   :face short-pack
			   :face negated-pack
			   :face options
			   :face postfix)
		    :face (text
			   :padding-top 0
			   :padding-bottom 0)
		    :face (option
			   :padding-left 2
			   :padding-top 0
			   :padding-bottom 0
			   :face (syntax
				  :item-separator ", "
				  :face (short
					 :item-separator nil
					 :face name
					 :face argument)
				  :face (long
					 :item-separator nil
					 :face name
					 :face argument))
			   :face (usage
				  :padding-left (30 absolute)
				  :face description
				  :face (fallback
					 :padding-top 0
					 :face header
					 :face value)
				  :face (default
					  :padding-top 0
					  :face header
					  :face value)
				  :face (environment
					 :padding-top 0
					 :face header
					 :face variable)))
		    :face (group
			   :padding-top 0
			   :padding-bottom 0
			   :face (header
				  :padding-top 0
				  :padding-bottom 0)
			   :face (items
				  :padding-top 0
				  :padding-bottom 0
				  :face (group
					 :padding-top 0
					 :padding-bottom 0
					 :padding-left 2))))
		  face-class))


;;; face.lisp ends here

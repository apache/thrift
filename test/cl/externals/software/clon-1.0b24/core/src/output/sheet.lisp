;;; sheet.lisp --- Sheet handling

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
;; The Sheet Class
;; ==========================================================================

(defclass sheet ()
  ((output-stream :documentation "The sheet's output stream."
		  :type stream
		  :reader output-stream
		  :initarg :output-stream)
   (line-width :documentation "The sheet's line width."
	       :type (integer 1)
	       :reader line-width
	       :initarg :line-width)
   (highlightp :documentation "Whether to highlight SHEET's output."
	       :initarg :highlightp
	       :reader highlightp)
   (sface-tree :documentation "The sheet's sface tree."
	       :reader sface-tree)
   (column :documentation "The sheet's current column."
	   :type (integer 0)
	   :accessor column
	   :initform 0)
   (frames :documentation "The stack of currently open frames."
	   :type list
	   :accessor frames
	   :initform nil))
  (:documentation "The SHEET class.
This class implements the notion of sheet for printing Clon help."))


;; ------------
;; Frame access
;; ------------

(defun push-frame (sheet frame)
  "Push a new frame to SHEET's frames."
  (push frame (frames sheet)))

(defun pop-frame (sheet)
  "Pop SHEET's current frame."
  (pop (frames sheet)))

(defun current-frame (sheet)
  "Return SHEET's current frame."
  (car (frames sheet)))

(defmacro map-frames (function (sheet &key reverse))
  "Map FUNCTION over SHEET's frames.
If REVERSE, map in reverse order."
  `(mapc ,function
     ,(if reverse
	  `(nreverse (copy-list (frames ,sheet)))
	`(frames ,sheet))))



;; ==========================================================================
;; Sheet Processing
;; ==========================================================================

;; -------------------------
;; ISO/IEC 6429 SGR handling
;; -------------------------

(defstruct highlight-property-instance
  "The HIGHLIGHT-PROPERTY-INSTANCE structure."
  name
  value)

(defmacro highlight-property-ecase (property value &body clauses)
  "Create an ECASE form to extract PROPERTY's VALUE escape sequence.
Each clause looks like: (PROPERTY-NAME (VALUE-OR-VALUE-LIST ESCAPE-SEQUENCE)*).
The value-matching part will itself be enclosed in an ECASE expression.
In addition, the special clause syntax (BOOLEAN <PROPERTY-NAME> <YES> <NO>)
is a shortcut for: (PROPERTY-NAME ((on t) YES) ((off nil) NO))."
  `(ecase ,property
     ,@(mapcar (lambda (clause)
		 (if (eq (car clause) 'boolean)
		     `(,(cadr clause)
		       (ecase ,value
			 ((on t)    ,(caddr  clause))
			 ((off nil) ,(cadddr clause))))
		   `(,(car clause)
		     (ecase ,value
		       ,@(cdr clause)))))
	clauses)))

(defun highlight-property-instance-escape-sequence (instance)
  "Return highlight property INSTANCE's escape sequence."
  (highlight-property-ecase
      (highlight-property-instance-name instance)
      (highlight-property-instance-value instance)
    ;; FAINT is not well supported
    (intensity (bold 1) (faint 2) ((normal nil) 22))
    (boolean italicp 3 23)
    ;; DOUBLE is not well supported
    (underline ((single on t) 4) (double 21) ((none off nil) 24))
    ;; RAPID is not well supported
    (blink ((slow on t) 5) (rapid 6) ((off nil) 25))
    (boolean inversep 7 27)
    (boolean concealedp 8 28)
    ;; I've seen the following two properties in some code, but I'm not sure
    ;; I've seen them work anywhere.
    (boolean crossed-out-p 9 29)
    (boolean framedp 51 54)
    (foreground (black 30) (red 31) (green 32) (yellow 33) (blue 34)
		(magenta 35) (cyan 36) (white 37) ((reset nil) 39))
    (background (black 40) (red 41) (green 42) (yellow 43) (blue 44)
		(magenta 45) (cyan 46) (white 47) ((reset nil) 49))))

(defun princ-highlight-property-instances (sheet instances)
  "Princ highlight proeprty INSTANCES on SHEET's stream."
  (when instances
    (format (output-stream sheet) "~C[~A~{;~A~}m"
      ;; #### NOTE: #\escape is not EBCDIC (it's ANSI), and CLHS 13.1.7
      ;; doesn't mention ANSI (thanks Pascal Bourguignon for spotting this),
      ;; so technically, #\escape is not a standard Common Lisp character. All
      ;; the lisp implementations supported by Clon seem to understand it
      ;; however (previously, I used #\esc but ABCL doesn't like it).
      #\escape
      (highlight-property-instance-escape-sequence (car instances))
      (mapcar #'highlight-property-instance-escape-sequence
	      (cdr instances)))))


;; ----------------
;; Low level output
;; ----------------

(defun princ-char (sheet char)
  "Princ CHAR on SHEET's stream and increment the column position.
The effect of printing CHAR must be exactly to move right by one column, so
control characters, as well as newlines and tabs are forbidden here."
  ;; #### FIXME: control chars not handled.
  (assert (not (member char '(#\newline #\tab))))
  (princ char (output-stream sheet))
  (incf (column sheet)))

(defun princ-string (sheet string)
  "Princ STRING on SHEET's stream and update the column position.
The effect of printing STRING must be exactly to move right by the
corresponding string length, so control characters, as well as newlines and
tabs are forbidden here."
  ;; #### FIXME: control chars not handled.
  (assert (notany (lambda (char) (member char '(#\newline #\tab))) string))
  (princ string (output-stream sheet))
  (incf (column sheet) (length string)))

(defun princ-spaces (sheet number)
  "Princ NUMBER spaces to SHEET's stream and update the column position."
  (princ-string sheet (make-string number :initial-element #\space)))

;; #### NOTE: the current column might in fact be already past the desired
;; one. For instance, since we don't do hyphenation, something too big to fit
;; in the current frame will overfull it.
(defun reach-column (sheet column)
  "Reach COLUMN on SHEET by princ'ing spaces."
  (when (< (column sheet) column)
    (princ-spaces sheet (- column (column sheet)))))


;; --------------
;; Logical output
;; --------------

(defclass sface (face)
  ((sibling :documentation "The SFace's raw sibling."
	    :reader sibling))
  (:documentation "The SFACE class.
An SFace is the association of a face and its raw sibling. The sibling is used
to create subfaces which would be missing from the original, user defined one."))

(defun make-raw-sface (sibling &aux (sface (copy-instance sibling 'sface)))
  "Return a new SFace based on SIBLING.
This function does not consider SIBLING as a face tree:
only face properties are copied; the face parent and children are set to nil."
  (setf (slot-value sface 'parent) nil)
  (setf (slot-value sface 'subfaces) nil)
  (setf (slot-value sface 'sibling) sibling)
  sface)

(defstruct frame
  "The FRAME structure.
This structure hold layout properties used for printing."
  sface
  left-margin
  right-margin)

(defstruct (highlight-frame (:include frame))
  "The HIGHLIGHT-FRAME structure.
This structure holds both layout and highlight properties used for printing."
  highlight-property-instances)

;; Shortcut accessors to the top frame:
(defun current-sface (sheet)
  "Return SHEET's current sface or nil."
  (when (frames sheet)
    (frame-sface (current-frame sheet))))

(defun current-left-margin (sheet)
  "Return SHEET's current left margin."
  (if (frames sheet)
      (frame-left-margin (current-frame sheet))
    0))

(defun current-right-margin (sheet)
  "Return SHEET's current right margin."
  (if (frames sheet)
      (frame-right-margin (current-frame sheet))
    (line-width sheet)))

(defun available-right-margin (sheet)
  "Return SHEET's available right margin.
This margin is the first non-self margin specified by a frame. All inner self
frames can potentially write until the available right margin."
  (map-frames (lambda (frame)
		(let ((right-margin (frame-right-margin frame)))
		  (when (numberp right-margin)
		    (return-from available-right-margin right-margin))))
      (sheet))
  (line-width sheet))

(defgeneric open-frame (sheet frame)
  (:documentation "Open FRAME on SHEET.")
  (:method-combination progn :most-specific-last)
  (:method progn (sheet (frame frame))
    "Reach the frame's left margin."
    (reach-column sheet (frame-left-margin frame)))
  (:method progn (sheet (frame highlight-frame))
    "Reach the frame's left margin and output its highlight properties."
    (princ-highlight-property-instances
     sheet (highlight-frame-highlight-property-instances frame))))

(defgeneric close-frame (sheet frame)
  (:documentation "Close FRAME on SHEET.")
  (:method-combination progn :most-specific-last)
  (:method progn (sheet (frame frame)
		  &aux (right-margin (frame-right-margin frame)))
    "Reach FRAME's right margin if it has one."
    (when (numberp right-margin)
      (reach-column sheet right-margin)))
  (:method progn (sheet (frame highlight-frame))
    "Restore the upper frame's highlight properties."
    (princ-highlight-property-instances
     sheet
     (mapcar (lambda (instance)
	       (make-highlight-property-instance
		:name (highlight-property-instance-name instance)
		:value (when (parent (frame-sface frame))
			 (face-highlight-property-value
			  (parent (frame-sface frame))
			  (highlight-property-instance-name instance)))))
	     (highlight-frame-highlight-property-instances frame)))))

(defun close-line (sheet)
  "Close all frames on SHEET's current line and go to next line."
  (map-frames (lambda (frame)
		(close-frame sheet frame))
      (sheet))
  (terpri (output-stream sheet))
  (setf (column sheet) 0))

(defun open-line (sheet)
  "Open all frames on SHEET's current line."
  (assert (zerop (column sheet)))
  (map-frames (lambda (frame)
		(open-frame sheet frame))
      (sheet :reverse t)))

(defun open-next-line (sheet)
  "Close SHEET's current line and open the next one."
  (close-line sheet)
  (open-line sheet))

;; #### FIXME: control chars not handled.
(defun print-string (sheet string)
  "Output STRING to SHEET.
STRING is output within the current frame's bounds.
Spacing characters are honored but newlines might replace spaces when the
output reaches the rightmost bound."
  (assert (and string (not (zerop (length string)))))
  ;; #### FIXME: I don't remember, but this might not work: don't I need to
  ;; honor the frames'sfaces here instead of blindly spacing ?? Or am I sure
  ;; I'm in the proper frame/sface ?
  ;; First, adjust the tabbing.
  (loop :with len := (length string) :and i := 0
	:while (< i len)
	:do (case (aref string i)
	      (#\space
	       (if (>= (column sheet) (available-right-margin sheet))
		   ;; If we're at the end of the line, turn the space into a
		   ;; newline.
		   (open-next-line sheet)
		 ;; Otherwise, just output it.
		 (princ-char sheet #\space))
	       (incf i))
	      (#\tab
	       ;; Here, we get the real number of spaces to insert in order to
	       ;; reach the next tab position with respect to the current
	       ;; frame. #### FIXME: get a real tabsize
	       (let ((spaces (+ (- (* (ceiling (/ (- (column sheet)
						     (current-left-margin
						      sheet))
						  8))
				      8)
				   (column sheet))
				(current-left-margin sheet))))
		 (cond ((< (+ (column sheet) spaces)
			   (available-right-margin sheet))
			(princ-spaces sheet spaces))
		       (t
			;; If the requested tab position is too far away, we
			;; simply go next line. There's not much that we can
			;; do to repair the layout anyway.
			(open-next-line sheet))))
	       (incf i))
	      (#\newline
	       (open-next-line sheet)
	       (incf i))
	      (otherwise
	       (let* ((end (or (position-if
				(lambda (char)
				  (member char '(#\space #\tab #\newline)))
				string
				:start i)
			       len))
		      (chunk-width (- end i))
		      (available-width (- (available-right-margin sheet)
					  (column sheet)))
		      (full-width (- (available-right-margin sheet)
				     (current-left-margin sheet))))
		 (cond ((<= chunk-width available-width)
			;; The chunk fits right here, so go for it.
			(princ-string sheet (subseq string i end))
			(setq i end))
		       ((<= chunk-width full-width)
			;; The chunk fits if we put it on the next line, so
			;; open the next line. Note that we don't actually
			;; output the word right now. This will be handled by
			;; the next LOOP iteration.
			(open-next-line sheet))
		       ;; The chunk wouldn't even fit on a line of its own, so
		       ;; we have no other choice than splitting it at a
		       ;; non-space position. When we do, we also insert an
		       ;; hyphenation mark at the end of the chunk. We know
		       ;; from open-sface that every frame has is least two
		       ;; characters wide (in other words, we know here that
		       ;; full-width >= 2). However, available-with might
		       ;; already be too small. If that is the case, we must
		       ;; go next-line first.
		       ((< available-width 2)
			(open-next-line sheet))
		       (t
			(setq end (+ i available-width -1))
			(princ-string sheet (subseq string i end))
			(princ-char sheet #\-)
			(setq i end)
			(open-next-line sheet))))))))


;; ---------------
;; SFace management
;; ---------------

(defun find-sface (sface name
		   &aux (sibling (search-face (sibling sface) name :error-me))
			(sub-sface (search-face sface name)))
  "Find an sface starting at SFACE named NAME.
If the sface can't be found in SFACE's face tree, find one in SFACE's sibling
instead, and make a copy of it."
  (cond (sub-sface
	 ;; #### NOTE: this is a bit dirty. The sibling might already have
	 ;; been set before. It might be better to turn the search procdedure
	 ;; into a generic function, and specialized its behavior.
	 (setf (slot-value sub-sface 'sibling) sibling)
	 sub-sface)
	(t
	 ;; #### NOTE: here, we create the missing face *only*. That is, we
	 ;; don't copy a whole raw face tree. Copying the whole raw face tree
	 ;; would perhaps create (hence override) other faces previously
	 ;; defined by the user upper in the face hierarchy, and we want to
	 ;; avoid that.
	 (add-subface sface (make-raw-sface sibling)))))

;; In practice, it could happen that the level of indentation exceeds the
;; line-width (either the theme has something crazy in it, or we just have too
;; many nested levels of indentation) ... We're in trouble here, so let's just
;; stay where we are.
(defun safe-left-margin (sheet margin)
  "Return either MARGIN or a safe value instead.
To be safe, margin must be greater than the current left margin and smaller
than the currently available margin."
  (or (when (or (< margin (current-left-margin sheet))
		(>= margin (available-right-margin sheet)))
	(current-left-margin sheet))
      margin))

(defun safe-right-margin (sheet left-margin margin)
  "Return either MARGIN or a safe value instead.
To be safe, margin must be greater than LEFT-MARGIN and smaller
than the currently available right margin."
  (or (when (or (<= margin left-margin)
		(> margin (available-right-margin sheet)))
	(available-right-margin sheet))
      margin))

(defun open-sface (sheet sface)
  "Create a frame for SFACE and open it."
  (assert (visiblep sface))
  ;; Create the new frame:
  (let* ((left-margin
	   (safe-left-margin
	    sheet
	    (let ((padding-spec (left-padding sface)))
	      (econd
		((eq padding-spec 'self)
		 (column sheet))
		((numberp padding-spec)
		 (+ (current-left-margin sheet) padding-spec))
		((listp padding-spec)
		 (destructuring-bind (padding relative-to &optional face-name)
		     padding-spec
		   ;; #### FIXME: should provide better error handling
		   (econd ((and (eq relative-to 'absolute)
				(null face-name))
			   padding)
			  ((and (eq relative-to :relative-to)
				(symbolp face-name))
			   (let* ((generation (parent-generation sface
								 face-name))
				  (left-margin
				    (frame-left-margin
				   ;; #### WARNING: we have not open the new
				   ;; frame yet, so decrement the generation
				   ;; level !!
				     (nth (1- generation) (frames sheet)))))
			     (+ left-margin padding))))))))))
	 (right-margin
	   (let ((padding-spec (right-padding sface)))
	     (econd
	       ((eq padding-spec 'self)
		nil)
	       ((numberp padding-spec)
		(if (numberp (current-right-margin sheet))
		    (safe-right-margin sheet left-margin
				       (- (current-right-margin sheet)
					  padding-spec))
		  (error "Right padding (face ~A) can't be :relative-to a self right margin (face ~A)."
		   (name sface) (name (current-sface sheet)))))
	       ((listp padding-spec)
		(destructuring-bind (padding relative-to &optional face-name)
		    padding-spec
		  ;; #### FIXME: should provide better error handling
		  (econd ((and (eq relative-to 'absolute)
			       (null face-name))
			  (safe-right-margin sheet left-margin padding))
			 ((and (eq relative-to :relative-to)
			       (symbolp face-name))
			  (let* ((generation
				   (parent-generation sface face-name))
				 (right-margin
				   (frame-right-margin
				 ;; #### WARNING: we have not
				 ;; open the new frame yet, so
				 ;; decrement the generation
				 ;; level !!
				    (nth (1- generation) (frames sheet)))))
			    (if (numberp right-margin)
				(safe-right-margin sheet left-margin
						   (- right-margin padding))
			      (error "Can't be :relative-to a self right margin.")))))))))))
    ;; Despite the "safe" computations above, we still need to check that our
    ;; new left and right margins let us actually display something.
    ;; Otherwise, we don't move at all because the layout is too fucked up. A
    ;; strict minimum is room for 2 characters, so that we can at least
    ;; display one character and an hyphen. But really, 2 characters wide is
    ;; already cmpletely insane...
    (let ((actual-right-margin
	    (or right-margin (available-right-margin sheet))))
      (unless (>= (- actual-right-margin left-margin) 2)
	(setq left-margin (current-left-margin sheet)
	      right-margin (current-right-margin sheet))))
    (push-frame sheet
		(if (highlightp sheet)
		    (let ((highlight-property-instances
			    (loop :for property :in *highlight-properties*
				  :when (face-highlight-property-set-p
					 sface property)
				    :collect (make-highlight-property-instance
					      :name property
					      :value
					      (face-highlight-property-value
					       sface property)))))
		      (make-highlight-frame :sface sface
					    :left-margin left-margin
					    :right-margin right-margin
					    :highlight-property-instances
					    highlight-property-instances))
		  (make-frame :sface sface
			      :left-margin left-margin
			      :right-margin right-margin))))
  ;; Open the new frame:
  (open-frame sheet (current-frame sheet)))

(defun close-sface (sheet)
  "Close SHEET's current sface."
  (close-frame sheet (current-frame sheet))
  (pop-frame sheet))



;; =========================================================================
;; The Print Help Protocol
;; =========================================================================

(defun help-spec-items-will-print (sface items)
  "Return t if at least one of ITEMS will print under SFACE."
  (assert (visiblep sface))
  (some (lambda (help-spec)
	  (help-spec-will-print sface help-spec))
	items))

(defgeneric help-spec-will-print (sface help-spec)
  (:documentation "Return t if HELP-SPEC will print under FACE.")
  (:method :before (sface help-spec)
    #+(or ccl ecl clisp allegro) (declare (ignore help-spec))
    (assert (visiblep sface)))
  (:method (sface help-spec)
    "Basic help specifications (chars, strings etc) do print."
    #+(or ccl ecl clisp allegro) (declare (ignore sface help-spec))
    t)
  (:method (sface (help-spec list))
    "Return t if HELP-SPEC's items will print under HELP-SPEC's face."
    (let ((subsface (find-sface sface (car help-spec))))
      (and (visiblep subsface)
	   (help-spec-items-will-print subsface (cdr help-spec))))))

(defgeneric get-bottom-padding (sface help-spec)
  (:documentation "Get HELP-SPEC's bottom-padding under SFACE.")
  (:method (sface help-spec)
    #+(or ccl ecl clisp allegro) (declare (ignore sface help-spec))
    "Basic help specifications (chars, strings etc) don't provide a bottom padding."
    nil)
  (:method (sface (help-spec list))
    "Return the bottom padding of HELP-SPEC's face."
    (bottom-padding (find-sface sface (car help-spec)))))

(defmethod top-padding (other)
  #+(or ccl ecl clisp allegro) (declare (ignore other))
  nil)

(defmethod top-padding ((help-spec list))
  (top-padding (car help-spec)))

;; #### NOTE: right now, we get the top-padding of the first item that prints.
;; Note that in case of nested help specifications, the retrieved value is
;; that of the topmost face, and not that of the leaf (which actually prints
;; something). It is not clear to me whether this is good or bad, or whether
;; "it depends". However, I spotted one case in which it gives an unexpected
;; result.

;; Suppose we have two nested groups with header but no contents. The help
;; spec is like this: (group (header "Foo") (contents (group (header
;; "Bar")))). With the refcard theme, the two headers will appear on the same
;; line despite any padding specification. That is because the separation
;; between the first header and the sub-group is a #\space. Since the second
;; group is the first item in the enclosing contents face, its top-padding
;; value is not used.

;; This is bad. I'm not sure what I should do about it. Use the leaf ? Use a
;; max of *all* enclosing faces padding[1] ? Provide padding options specific
;; to what's after/before ? Dammit. This is not worth the trouble...

;; Footnotes: [1] the more I think of it, the more I like this option...

(defun get-top-padding (sface items)
  "Return top padding of the next item in ITEMS that will print under SFACE."
  (loop :for help-spec :in items
	:when (help-spec-will-print sface help-spec)
	  :return (when (listp help-spec)
		    (top-padding (find-sface sface (car help-spec))))))

;; #### NOTE: this is where I would like a more expressive dispatch in CLOS.
;; This function should be part of print-help-spec, with two cases:
;; - (face-name items...)
;; - (sface items...)
(defun print-faced-help-spec (sheet sface items)
  "Print all help specification ITEMS on SHEET with SFACE."
  (when (and (visiblep sface)
	     (help-spec-items-will-print sface items))
    (open-sface sheet sface)
    (loop :for help-specs :on items
	  :for help-spec := (car help-specs)
	  :do
	     (when (help-spec-will-print (current-sface sheet) help-spec)
	       (print-help-spec sheet help-spec)
	       (when (help-spec-items-will-print (current-sface sheet)
						 (cdr help-specs))
		 (let ((vertical-padding
			 (max (or (get-bottom-padding (current-sface sheet)
						      help-spec)
				  -1)
			      (or (get-top-padding (current-sface sheet)
						   (cdr help-specs))
				  -1))))
		   (cond ((>= vertical-padding 0)
			  (print-help-spec sheet
					   (make-string (1+ vertical-padding)
					     :initial-element #\newline)))
			 ((item-separator (current-sface sheet))
			  (print-help-spec sheet (item-separator
						  (current-sface sheet)))))))))
    (close-sface sheet)))

(defgeneric print-help-spec (sheet help-spec)
  (:documentation "Print HELP-SPEC on SHEET.")
  (:method :before (sheet help-spec)
    #+(or ccl ecl clisp allegro) (declare (ignore help-spec))
    (assert (visiblep (current-sface sheet))))
  (:method (sheet (char character))
    "Print CHAR on SHEET with the current face."
    (print-help-spec sheet (make-string 1 :initial-element char)))
  ;; #### PORTME.
  ;; ECL, CLISP and Allegro don't have a SIMPLE-VECTOR class, so we use just
  ;; VECTOR instead.
  (:method (sheet (char-vector #+(or ecl clisp allegro) vector
			       #-(or ecl clisp allegro) simple-vector))
    "Print CHAR-VECTOR on SHEET with the current face."
    (print-help-spec sheet (coerce char-vector 'string)))
  (:method (sheet (string string))
    "Print STRING on SHEET with the current face."
    (print-string sheet string))
  (:method (sheet (help-spec list))
    "Open HELP-SPEC's face and print all of its items with it."
    (let ((sface (find-sface (current-sface sheet) (car help-spec))))
      (print-faced-help-spec sheet sface (cdr help-spec)))))

(defun print-help (sheet help)
  "Open the toplevel help face and print HELP on SHEET with it."
  (let ((items
	  (if (and (listp help) (not (symbolp (car help))))
	      ;; There's already an enclosing list when help for a container
	      ;; is requested directly, or when the complete help is
	      ;; requested, in which case we have the list of synopsis and all
	      ;; synopsis items.
	      help
	    (list help))))
    (print-faced-help-spec sheet (sface-tree sheet) items)))



;; ==========================================================================
;; Sheet Instance Creation
;; ==========================================================================

;; #### NOTE: I need to bind output-stream here (which is early) because it is
;; required to do the TIOCGWINSZ ioctl business.
(defmethod initialize-instance :around
    ((sheet sheet) &rest keys &key (output-stream *standard-output*)
				   line-width
				   (highlight :auto))
  "Handle unset line width and AUTO highlight according to OUTPUT-STREAM."
  ;; In both of the cases below, we must know whether we're printing to a
  ;; terminal or a simple file.
  (when (or (not line-width) (eq highlight :auto))
    (multiple-value-bind (tty-line-width error-message)
	(when (fboundp 'stream-line-width) ; depends on the termio feature
	  (funcall 'stream-line-width output-stream))
      (when error-message
	;; #### FIXME: a better error printing would be nice.
	(let (*print-escape*)
	  (format *error-output* "Error: ~A.~%" error-message)))
      ;; Next, set highlighting.
      (when (eq highlight :auto)
	(setq highlight tty-line-width))
      ;; Finally, set line width.
      (unless line-width
	(setq line-width
	      (let ((columns (getenv "COLUMNS")))
		(if columns
		    (handler-case
			(coerce (read-from-string columns) '(integer 1))
		      (error (error)
			;; #### FIXME: a better error printing would be nice.
			(let (*print-escape*)
			  (print-object error *error-output*))
			(or tty-line-width 80)))
		  ;; Yuck. Code duplication.
		  (or tty-line-width 80)))))))
  (apply #'call-next-method sheet
	 :output-stream output-stream
	 :line-width line-width
	 :highlightp highlight
	 ;; #### NOTE: technically, the call to REMOVE-KEYS below is not
	 ;; needed because in case of duplication, the leftmost initarg is
	 ;; used (see section 7.1.4 "Rules for Initialization Arguments" of
	 ;; the Hyperspec).
	 (remove-keys keys :output-stream :line-width :highlight)))

(defun read-sface-tree (pathname)
  "Read an sface tree from PATHNAME."
  (make-face-tree
   (list* 'toplevel
	  (with-open-file (stream pathname)
	    (let ((*package* (find-package :net.didierverna.clon)))
	      (loop :for item := (read stream nil stream)
		    :if (eql item stream)
		      :return items
		    :else
		      :collect item :into items))))
   'sface))

(defun try-read-sface-tree (pathname)
  "Read an sface tree from PATHNAME if it exists or return nil."
  (when (open pathname :direction :probe)
    (read-sface-tree pathname)))

(defun try-read-theme (pathname)
  "Read a theme from PATHNAME or PATHNAME.cth if it exists or return nil."
  ;; #### FIXME: should warn or err if file doesn't exist.
  (or (try-read-sface-tree pathname)
      (unless (string= (pathname-type pathname) "cth")
	(try-read-sface-tree (merge-pathnames pathname
					      (make-pathname :type "cth"))))))

;; #### FIXME: when trying Mac OSX paths, we have a mix of upcase and downcase
;; directory names, for instance, share/clon and Application Support/Clon.
;; Normally, when we append a "themes" subdirectory somewhere, we should
;; respect that. However (at least by default but can this be customized?),
;; the OSX file system is case insensitive. For other file systems, this might
;; break someday.
(defmethod initialize-instance :after ((sheet sheet) &key theme search-path)
  "Finish initialization of SHEET.
This involves:
- computing SHEET's sface tree from THEME and SEARCH-PATH,
- initializing SHEET's toplevel sface's sibling to a raw face tree."
  (setf (slot-value sheet 'sface-tree)
	(or (cond ((and theme (or (not search-path)
				  (pathname-directory theme)))
		   (try-read-theme theme))
		  (theme
		   (setq theme
			 (merge-pathnames theme
					  (make-pathname
					   :directory `(:relative "themes"))))
		   (loop :for path :in search-path
			 :for sface-tree := (try-read-theme
					     (merge-pathnames theme path))
			 :until sface-tree
			 :finally (return sface-tree))))
	    (make-raw-face-tree 'sface)))
  (setf (slot-value (sface-tree sheet) 'sibling) (make-raw-face-tree)))

(defun make-sheet
    (&rest keys &key output-stream search-path theme line-width highlight)
  "Make a new SHEET."
  (declare (ignore output-stream search-path theme line-width highlight))
  (apply #'make-instance 'sheet keys))

(defun flush-sheet (sheet)
  "Flush SHEET."
  (assert (null (current-sface sheet)))
  (terpri (output-stream sheet)))


;;; sheet.lisp ends here

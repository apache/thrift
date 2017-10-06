;;; option.lisp --- Basic Option management

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
;; The Option Class
;; ==========================================================================

(defabstract option (item)
  ((short-name :documentation "The option's short name."
	       :type (or null string)
	       :initarg :short-name
	       :initform nil
	       :reader short-name)
   (long-name :documentation "The option's long name."
	      :type (or null string)
	      :initarg :long-name
	      :initform nil
	      :reader long-name)
   (description :documentation "The option's description."
		:type (or null string)
		:initarg :description
		:initform nil
		:reader description)
   (env-var :documentation "The option's associated environment variable."
	    :type (or null string)
	    :initarg :env-var
	    :initform nil
	    :reader env-var))
  (:default-initargs
    :internal nil)
  (:documentation "The OPTION class.
This is the base class for all options."))


;; ------------------
;; Traversal protocol
;; ------------------

(defmethod untraverse ((option option))
  "OPTION is a terminal object: just return it."
  option)


;; ---------------------------
;; Help specification protocol
;; ---------------------------

(defmethod help-spec ((option option) &key)
  "Return OPTION's help specification."
  (accumulate (option)
    (accumulate (syntax)
      (accumulate (short)
	(accumulate (name)
	  (when (short-name option)
	    (format nil "-~A" (short-name option)))))
      (accumulate (long)
	(accumulate (name)
	  (when (long-name option)
	    (format nil "--~A" (long-name option))))))
    (accumulate (usage)
      (accumulate (description)
	(description option))
      (when (env-var option)
	`(environment
	  (header "Environment:")
	  (variable ,(env-var option)))))))



;; ==========================================================================
;; Error Management
;; ==========================================================================

(define-condition option-error (error)
  ((option :documentation "The concerned option."
	   :type option
	   :initarg :option
	   :reader option))
  (:documentation "An error related to an option."))



;; ==========================================================================
;; The Name Clash Check Protocol
;; ==========================================================================

(defgeneric check-name-clash (item1 item2)
  (:documentation ~"Check for name clash between ITEM1's options "
		  ~"and ITEM2's options.")
  (:method (item1 (text text))
    "Do nothing (no name clash with a text object."
    #+(or ccl ecl clisp allegro) (declare (ignore item1))
    #+ecl                        (declare (ignore text))
    (values))
  (:method ((text text) item2)
    "Do nothing (no name clash with a text object."
    #+(or ccl ecl clisp allegro) (declare (ignore item2))
    #+ecl                        (declare (ignore text))
    (values))
  ;; #### NOTE: currently, name clashes are considered on short and long names
  ;; independently. That is, it is possible to have a short name identical to
  ;; a long one, although I don't see why you would want to do that, and I
  ;; should probably prohibit it altogether.
  (:method ((option1 option) (option2 option))
    "Ensure that there is no name clash between OPTION1 and OPTION2."
    (unless (eq option1 option2)
      (when (and (short-name option1) (short-name option2)
		 (string= (short-name option1) (short-name option2)))
	(error "Options ~A and ~A: indentical short name ~S."
	       option1 option2 (short-name option1)))
      (when (and (long-name option1) (long-name option2)
		 (string= (long-name option1) (long-name option2)))
	(error "Options ~A and ~A: identical Long name ~S."
	       option1 option2 (long-name option1))))))



;; ==========================================================================
;; The Option Search protocol
;; ==========================================================================

(defun option-abbreviation-distance (option partial-name)
  "Return the distance between OPTION's long name and PARTIAL-NAME.
If PARTIAL-NAME does not abbreviate OPTION's long name, return
MOST-POSITIVE-FIXNUM."
  (with-slots (long-name) option
    (if (beginning-of-string-p partial-name long-name)
	(- (length long-name) (length partial-name))
      most-positive-fixnum)))

(defun match-option (option &key short-name long-name)
  "Try to match OPTION against SHORT-NAME, LONG-NAME.
If OPTION matches, return the name that matched."
  (econd (short-name
	  (when (string= short-name (short-name option))
	    short-name))
	 (long-name
	  (when (string= long-name (long-name option))
	    long-name))))

(defgeneric option-sticky-distance (option namearg)
  (:documentation ~"Try to match OPTION's short name with a sticky argument "
		  ~"against NAMEARG.
If OPTION matches, return the length of OPTION's short name; otherwise 0.")
  ;; #### NOTE: this method currently only applies to flags.
  (:method ((option option) namearg)
    "Return 0 (non-valued options don't take any argument, sticky or not)."
    ;; #### NOTE: the consequence of this method returning 0 is that
    ;; non-valued options (i.e. flags) won't ever get a cmdline-argument in
    ;; retrieve-from-short-call, hence the assertion there.
    #+(or ccl ecl clisp allegro) (declare (ignore namearg))
    #+ecl                        (declare (ignore option))
    0))



;; ==========================================================================
;; The Char Packs Protocol
;; ==========================================================================

;; When examining the command-line, we first try to spot an option, then a
;; short or negated pack, and then fall back to an unknown option. When things
;; are messed up, we prefer to try to spot options misplaced in a pack rather
;; than directly an unknown option. That's what a "potential" pack is: a pack
;; composed of single character options that are potentially misused.
;; Potential misuse means non switch-based in a negated pack, options with
;; mandatory arguments in the middle of a pack and so on.
(defun potential-pack-char (option &optional as-string)
  "Return OPTION's potential pack character, if any.
If AS-STRING, return a string of that character."
  (with-slots (short-name) option
    (when (and short-name (= (length short-name) 1))
      (if as-string
	  short-name
	(coerce short-name 'character)))))

(defgeneric short-pack-char (option &optional as-string)
  (:documentation "Return OPTION's short pack character, if any.
If AS-STRING, return a string of that character.")
  ;; #### NOTE: this method currently only applies to flags.
  (:method ((option option) &optional as-string)
    "Return OPTION's potential pack character."
    ;; Since non-valued options don't take any argument, being short-pack'able
    ;; is the same as being potentially packable.
    (potential-pack-char option as-string)))

(defgeneric negated-pack-char (option &optional as-string)
  (:documentation "Return OPTION's negated pack character, if any.
If AS-STRING, return a string of that character.")
  (:method ((option option) &optional as-string)
    "Return nil (only the switch hierarchy is negated-pack'able)."
    (declare (ignore as-string))
    #+ecl (declare (ignore option))
    nil))



;; ==========================================================================
;; Option Instance Creation
;; ==========================================================================

(defmethod initialize-instance :before
    ((option option) &key short-name long-name description internal)
  "Check validity of the name-related initargs."
  (when internal
    (assert (not (or (zerop (length long-name))
		     (zerop (length description))))))
  (unless (or short-name long-name)
    (error "Option ~A: no name given." option))
  (when (and long-name (zerop (length long-name)))
    (error "Option ~A: empty long name." option))
  (when (and short-name (zerop (length short-name)))
    (error "Option ~A: empty short name." option))
  (when (and short-name long-name (string= short-name long-name))
    (error "Option ~A: short and long names identical." option))
  ;; Short names can't begin with a dash because that would conflict with
  ;; the long name syntax.
  (when (and short-name (beginning-of-string-p "-" short-name))
    (error "Option ~A: short name begins with a dash." option))
  ;; Clon uses only long names, not short ones. But it's preferable to
  ;; reserve the prefix in both cases.
  (unless internal
    (dolist (name (list short-name long-name))
      (when (and name (or (string= name "clon")
			  (beginning-of-string-p "clon-" name)))
	(error "Option ~A: name ~S reserved by Clon." option name)))))

(defmethod initialize-instance :around
    ((option option) &rest keys &key long-name env-var internal)
  "If INTERNAL, prefix LONG-NAME with \"clon-\" and ENV-VAR with \"CLON_\"."
  (when internal
    ;; #### NOTE: technically, the calls to REMOVE-KEYS below are not needed
    ;; because in case of duplication, the leftmost initarg is used (see
    ;; section 7.1.4 "Rules for Initialization Arguments" of the Hyperspec).
    (setq long-name (concatenate 'string "clon-" long-name))
    (setq keys (list* :long-name long-name (remove-keys keys :long-name)))
    (when env-var
      (setq env-var (concatenate 'string "CLON_" env-var))
      (setq keys (list* :env-var env-var (remove-keys keys :env-var)))))
  (apply #'call-next-method option keys))


;;; option.lisp ends here

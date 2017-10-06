;;; path.lisp --- Path options

;; Copyright (C) 2010-2013, 2015 Didier Verna

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
;; The Path Option Class
;; ==========================================================================

(defoption path ()
  ((argument-name ;; inherited from the VALUED-OPTION class
    :initform "PATH")
   (path-type :documentation "The path type."
	      :initarg :type
	      :initform nil
	      :reader path-type))
  (:documentation "The PATH class.
This class implements options whose values are (colon-separated lists of)
pathnames."))


;; Some parts of the code below are stolen or adapted from Peter Seibel's
;; Practical Common Lisp book, Chapter 15: A Portable Pahtname Library.

(defun pathname-component-null-p (component)
  "Return true if COMPONENT is either null or :unspecific."
  (or (null component) (eql component :unspecific)))

(defun directory-pathname-p  (pathname)
  "Return true if PATHNAME denotes a directory."
  (and (pathname-component-null-p (pathname-name pathname))
       (pathname-component-null-p (pathname-type pathname))
       pathname))


;; ------------------------------
;; Value Stringification protocol
;; ------------------------------

(defmethod stringify ((path path) value)
  "Transform PATH's VALUE into an argument."
  #+ecl (declare (ignore path))
  (typecase value
    (null "<none>")
    (list (list-to-string value :key #'namestring :separator ":"))
    (t
     (namestring value))))


;; --------------------
;; Value Check protocol
;; --------------------

(defmethod check ((path path) value)
  "Check that VALUE is valid for PATH."
  (unless (null value)
    (flet ((check-pathname (pathname &key as-file as-directory)
	     "Check that PATHNAME is valid.
If AS-FILE, ensure that it denotes a file.
If AS-DIRECTORY, ensure that it denotes a directory."
	     (unless (pathnamep pathname)
	       (error 'invalid-value
		      :option path
		      :value value
		      :comment (format nil "~S is not a pathname." pathname)))
	     (when (and as-file (directory-pathname-p pathname))
	       (error 'invalid-value
		      :option path
		      :value value
		      :comment (format nil "~S denotes a directory."
				 pathname)))
	     (when (and as-directory (not (directory-pathname-p pathname)))
	       (error 'invalid-value
		      :option path
		      :value value
		      :comment (format nil "~S does not denote a directory."
				 pathname)))
	     (when (wild-pathname-p pathname)
	       (error 'invalid-value
		      :option path
		      :value value
		      :comment (format nil "~S contains wildcards." pathname)))
	     (when (string= (cadr (pathname-directory pathname)) "~")
	       (error 'invalid-value
		      :option path
		      :value value
		      :comment
		      (format nil "~S contains a ~~/ abbreviation." pathname)))
	     pathname))
      (ecase (path-type path)
	(:file
	 (check-pathname value :as-file t))
	(:directory
	 (check-pathname value :as-directory t))
	(:file-list
	 (mapc (lambda (pathname)
		 (check-pathname pathname :as-file t))
	       value)
	 value)
	(:directory-list
	 (mapc (lambda (pathname)
		 (check-pathname pathname :as-directory t))
	       value)
	 value)
	((nil)
	 (mapc (lambda (pathname)
		 (check-pathname pathname))
	       value)
	 value)))))

(defun split-path (path)
  "Split PATH into a list of directories."
  (loop :for split-point := (position #\: path)
	:for path-elt := (subseq path 0 split-point)
	:unless (zerop (length path-elt)) :collect path-elt
	  :do (setq path (when split-point (subseq path (1+ split-point))))
	:while path))


;; ----------------------------
;; Argument Conversion protocol
;; ----------------------------

(defmethod convert ((path path) argument)
  "Convert ARGUMENT to a PATH value."
  (if (zerop (length argument))
      nil
    (flet ((string-pathname (string &key as-file as-directory specify)
	     "Make a pathname from STRING.
If AS-FILE, make sure the resulting pathname does not denote a directory.
If AS-DIRECTORY, make sure the resulting pathname denotes a directory.
If SPECIFY, print the path as part of a potential error message. This is
useful to specify which part of ARGUMENT is concerned when it is a list."
	     (let ((pathname (pathname string)))
	       (when (wild-pathname-p pathname)
		 (error 'invalid-argument
			:option path
			:argument argument
			:comment (format nil
				     "Path~@[ ~S~] contains a wildcard."
				   (and specify string))))
	       (when (and as-file (directory-pathname-p pathname))
		 (error 'invalid-argument
			:option path
			:argument argument
			:comment (format nil "Path~@[ ~S~] is a directory."
				   (and specify string))))
	       ;; #### NOTE: instead of forcing users to end their directories
	       ;; with a slash (and hence triggering an error here if it
	       ;; doesn't), we simply convert a normal pathname into a
	       ;; directory one.
	       (when (and as-directory (not (directory-pathname-p pathname)))
		 (setq pathname
		       (make-pathname
			:directory (append (or (pathname-directory pathname)
					       (list :relative))
					   (list (file-namestring pathname)))
			:name nil
			:type nil
			:defaults pathname)))
	       (when (string= (cadr (pathname-directory pathname)) "~")
		 (setq pathname
		       (merge-pathnames
			(make-pathname
			 :directory
			 (list* :relative (cddr (pathname-directory pathname)))
			 :defaults pathname)
			;; #### FIXME: this will break when the home directory
			;; cannot be found (HOME-DIRECTORY returns NIL in that
			;; case). I need to throw a conversion error.
			(home-directory))))
	       pathname)))
      (ecase (path-type path)
	(:file
	 (string-pathname argument :as-file t))
	(:directory
	 (string-pathname argument :as-directory t))
	(:file-list
	 (mapcar (lambda (filename)
		   (string-pathname filename :as-file t :specify t))
		 (split-path argument)))
	(:directory-list
	 (mapcar (lambda (dirname)
		   (string-pathname dirname :as-directory t :specify t))
		 (split-path argument)))
	((nil)
	 (let ((paths (split-path argument)))
	   (if (= (length paths) 1)
	       (string-pathname paths)
	     (mapcar (lambda (pathname)
		       (string-pathname pathname :specify t))
		     paths))))))))



;; ==========================================================================
;; Path Instance Creation
;; ==========================================================================

(defun make-path (&rest keys
		  &key short-name long-name description
		       argument-name argument-type
		       env-var fallback-value default-value
		       type hidden)
  "Make a new path option.
- SHORT-NAME is the option's short name (without the dash).
  It defaults to nil.
- LONG-NAME is the option's long name (without the double-dash).
  It defaults to nil.
- DESCRIPTION is the option's description appearing in help strings.
  It defaults to nil.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENV-VAR is the option's associated environment variable.
  It defaults to nil.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- TYPE is the pathname type. It can be one of :file, :directory, :file-list,
  :directory-list or nil meaning that everything is allowed.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore short-name long-name description env-var
		   argument-name argument-type fallback-value default-value
		   type hidden))
  (apply #'make-instance 'path keys))

#i(make-internal-path 2)
(defun make-internal-path (long-name description
			   &rest keys
			   &key argument-name argument-type
				env-var fallback-value default-value
				type hidden)
  "Make a new internal (Clon-specific) path option.
- LONG-NAME is the option's long-name, sans the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the options's description.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENV-VAR is the option's associated environment variable, sans the 'CLON_'
  prefix. It defaults to nil.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- TYPE is the pathname type. It can be one of :file, :directory, :file-list,
  :directory-list or nil meaning that everything is allowed.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore argument-name argument-type
		   env-var fallback-value default-value
		   type hidden))
  (apply #'make-instance 'path
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;;; path.lisp ends here

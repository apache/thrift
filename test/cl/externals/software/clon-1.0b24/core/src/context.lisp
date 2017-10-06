;;; context.lisp --- Context management

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

(defvar *context* nil "The current context.")



;; ==========================================================================
;; Command-line error management (not regarding known options)
;; ==========================================================================

(define-condition invalid-short-equal-syntax (cmdline-error)
  ()
  (:report (lambda (error stream)
	     (format stream "Invalid = syntax in short call: ~S."
	       (item error))))
  (:documentation "An error related to a short-equal syntax."))

(define-condition invalid-negated-equal-syntax (cmdline-error)
  ()
  (:report (lambda (error stream)
	     (format stream "Invalid = syntax in negated call: ~S."
	       (item error))))
  (:documentation "An error related to a negated-equal syntax."))

(define-condition cmdline-junk-error (cmdline-error)
  ((item ;; inherited from the CMDLINE-ERROR condition
    :documentation "The piece of junk appearing on the command-line."
    :initarg :junk
    :reader junk))
  (:report (lambda (error stream)
	     (format stream "Junk on the command-line: ~S." (junk error))))
  (:documentation "An error related to a command-line piece of junk."))

(defun restartable-cmdline-junk-error (junk)
  (restart-case (error 'cmdline-junk-error :junk junk)
    (discard ()
      :report "Discard junk."
      nil)))

(define-condition unrecognized-short-call-error (cmdline-error)
  ((item ;; inherited from the CMDLINE-ERROR condition
    :documentation "The unrecognized short call on the command-line."
    :initarg :short-call
    :reader short-call))
  (:report (lambda (error stream)
	     (format stream "Unrecognized option or short pack: ~S."
	       (short-call error))))
  (:documentation "An error related to an unrecognized short call."))

(define-condition unrecognized-negated-call-error (cmdline-error)
  ((item ;; inherited from the CMDLINE-ERROR condition
    :documentation "The unrecognized negated call on the command-line."
    :initarg :negated-call
    :reader negated-call))
  (:report (lambda (error stream)
	     (format stream "Unrecognized option or negated pack: ~S."
	       (negated-call error))))
  (:documentation "An error related to an unrecognized negated call."))

(define-condition unknown-cmdline-option-error (cmdline-error)
  ((item ;; inherited from the CMDLINE-ERROR condition
    :documentation "The option's name as it appears on the command-line."
    :initarg :name
    :reader name)
   (argument :documentation "The option's command-line argument."
	     :initarg :argument
	     :reader argument))
  (:report (lambda (error stream)
	     (format stream "Unknown command-line option ~
			    ~S~@[ with argument ~S~]."
	       (name error)
	       (argument error))))
  (:documentation "An error related to an unknown command-line option."))


(defun print-error (error
		    &optional interactivep
		    &aux (stream (if interactivep *query-io* *error-output*))
			 *print-escape*)
  "Print ERROR on *ERROR-OUTPUT*.
When INTERACTIVEP, print on *QUERY-IO* instead."
  (print-object error stream)
  (terpri stream))

(defun exit-abnormally (error)
  "Print ERROR on *ERROR-OUTPUT* and exit with status code 1."
  (print-error error)
  (exit 1))

;; Adapted from the Hyperspec
(defun restart-on-error (error)
  "Print ERROR and offer available restarts on *QUERY-IO*."
  (print-error error :interactive)
  (format *query-io* "Available options:~%")
  (let ((restarts (compute-restarts)))
    (do ((i 0 (+ i 1)) (r restarts (cdr r))) ((null r))
      (format *query-io* "~&~D: ~A~%" i (car r)))
    (loop :with k := (length restarts) :and n := nil
	  :until (and (typep n 'integer) (>= n 0) (< n k))
	  :do (progn (format *query-io* "~&Option [0-~A]: " (1- k))
		     (finish-output *query-io*)
		     (setq n (read *query-io*))
		     (fresh-line *query-io*))
	  :finally (invoke-restart-interactively (nth n restarts)))))

;; #### NOTE: this macro used to bind only for Clon errors, but other kinds of
;; errors may actually occur, for instance when retreiving a lispobj option's
;; value.
(defmacro with-context-error-handler (context &body body)
  "Execute BODY with CONTEXT's error handler bound for CONDITION."
  `(handler-bind ((error
		    (lambda (error)
		      (ecase (error-handler ,context)
			(:interactive
			 (restart-on-error error))
			(:quit
			 (exit-abnormally error))
			(:none)))))
     ,@body))



;; ==========================================================================
;; The Context Class
;; ==========================================================================

(defstruct cmdline-option
  name ;; the option's name as used on the cmdline
  option ;; the corresponding option object
  value ;; the converted option's cmdline value
  source ;; the value source
  )

(defclass context ()
  ((synopsis :documentation "The program synopsis."
	     :type synopsis
	     :initarg :synopsis
	     :reader synopsis
	     :initform *synopsis*)
   (progname :documentation ~"The program name "
			    ~"as it appears on the command-line."
	     :type string) ;; see below for reader
   (cmdline-options :documentation "The options from the command-line."
	  :type list ;; of cmdline-option objects
	  :initform nil  ;; see the warning in initialize-instance
	  :accessor cmdline-options)
   (remainder :documentation "The non-Clon part of the command-line."
	      :type list) ;; see below for reader
   (search-path :documentation "The search path for Clon files."
		:reader search-path)
   (theme :documentation "The theme filename."
	  :reader theme)
   (line-width :documentation "The line width for help display."
	       :reader line-width
	       :initform nil)
   (highlight :documentation "Clon's output highlight mode."
	      :reader highlight)
   (error-handler :documentation ~"The behavior to adopt "
				 ~"on option retrieval errors."
		  :type symbol
		  :initform :quit ;; see the warning in initialize-instance
		  :reader error-handler))
  (:default-initargs
   :cmdline (cmdline))
  (:documentation "The CONTEXT class.
This class represents the associatiion of a synopsis and a set of command-line
options based on it."))

(defun progname (&key (context *context*))
  "Return CONTEXT's program name."
  (slot-value context 'progname))

(defun remainder (&key (context *context*))
  "Return CONTEXT's remainder."
  (slot-value context 'remainder))

(defun cmdline-options-p (&key (context *context*))
  "Return T if CONTEXT has any unprocessed options left."
  (if (cmdline-options context) t nil))

(defun cmdline-p (&key (context *context*))
  "Return T if CONTEXT has anything on its command-line."
  (or (cmdline-options-p :context context)
      (remainder :context context)))


;; --------------------------------------------
;; Convenience wrappers around context synopsis
;; --------------------------------------------

(defmethod postfix ((context context))
  "Return the postfix of CONTEXT's synopsis."
  (postfix (synopsis context)))

(defmethod short-pack ((context context))
  "Return the short pack of CONTEXT's synopsis."
  (short-pack (synopsis context)))

(defmethod negated-pack ((context context))
  "Return the negated pack of CONTEXT's synopsis."
  (negated-pack (synopsis context)))

(defmethod clon-options-group ((context context))
  "Return the Clon options group of CONTEXT's synopsis."
  (clon-options-group (synopsis context)))

(defmethod potential-pack-p (pack (context context))
  "Return t if PACK (a string) is a potential pack in CONTEXT."
  (potential-pack-p pack (synopsis context)))


;; #### WARNING: the two wrappers below are here to make the DO-OPTIONS macro
;; work directly on contexts. Beware that both of them are needed.

(defmethod mapoptions (func (context context))
  "Map FUNC over all options in CONTEXT synopsis."
  (mapoptions func (synopsis context)))

(defmethod untraverse ((context context))
  "Untraverse CONTEXT synopsis."
  (untraverse (synopsis context)))



;; =========================================================================
;; The Help Protocol
;; =========================================================================

;; #### NOTE: all help related parameters but OUTPUT-STREAM have a
;; corresponding slot in contexts that act as a default value. The idea is
;; that users can in turn specify their preferences in the corresponding
;; environment variables. I don't think it would make much sense to provide an
;; option for OUTPUT-STREAM. At the end-user level, you can redirect to a file
;; from the shell.

(defun help (&key (context *context*)
		  (item (synopsis context))
		  (output-stream *standard-output*)
		  (search-path (search-path context))
		  (theme (theme context))
		  (line-width (line-width context))
		  (highlight (highlight context)))
  "Print CONTEXT's help."
  (let ((sheet (make-sheet :output-stream output-stream
			   :search-path search-path
			   :theme theme
			   :line-width line-width
			   :highlight highlight)))
    (print-help sheet
		(help-spec item
			   :program (pathname-name (progname :context context))
			   :unhide t))
    (flush-sheet sheet)))



;; =========================================================================
;; The Option Search Protocol
;; =========================================================================

(defun search-option-by-name (context &rest keys &key short-name long-name)
  "Search for option with either SHORT-NAME or LONG-NAME in CONTEXT.
When such an option exists, return two values:
- the option itself,
- the name that matched."
  (declare (ignore short-name long-name))
  (do-options (option context)
    (let ((name (apply #'match-option option keys)))
      (when name
	(return-from search-option-by-name (values option name))))))

(defun search-option-by-abbreviation (context partial-name)
  "Search for option abbreviated with PARTIAL-NAME in CONTEXT.
When such an option exists, return two values:
- the option itself,
- the completed name."
  (let ((shortest-distance most-positive-fixnum)
	closest-option)
    (do-options (option context)
      (let ((distance (option-abbreviation-distance option partial-name)))
	(when (< distance shortest-distance)
	  (setq shortest-distance distance)
	  (setq closest-option option))))
    (when closest-option
      (values closest-option
	      ;; When long names are abbreviated (for instance --he instead of
	      ;; --help), we register the command-line name like this: he(lp).
	      ;; In case of error report, this will help the user spot where
	      ;; he did something wrong.
	      (complete-string partial-name (long-name closest-option))))))

#i(search-option 1)
(defun search-option
    (context &rest keys &key short-name long-name partial-name)
  "Search for an option in CONTEXT.
The search is done with SHORT-NAME, LONG-NAME, or PARTIAL-NAME.
In case of a PARTIAL-NAME search, look for an option the long name of which
begins with it.
In case of multiple matches by PARTIAL-NAME, the longest match is selected.
When such an option exists, return wo values:
- the option itself,
- the name used to find the option, possibly completed if partial."
  (econd ((or short-name long-name)
	  (apply #'search-option-by-name context keys))
	 (partial-name
	  (search-option-by-abbreviation context partial-name))))

(defun search-sticky-option (context namearg)
  "Search for a sticky option in CONTEXT, matching NAMEARG.
NAMEARG is the concatenation of the option's short name and its argument.
In case of multiple matches, the option with the longest name is selected.
When such an option exists, return two values:
- the option itself,
- the argument part of NAMEARG."
  (let ((longest-distance 0)
	closest-option)
    (do-options (option context)
      (let ((distance (option-sticky-distance option namearg)))
	(when (> distance longest-distance)
	  (setq longest-distance distance)
	  (setq closest-option option))))
    (when closest-option
      (values closest-option (subseq namearg longest-distance)))))



;; ==========================================================================
;; The Option Retrieval Protocol
;; ==========================================================================

(defun getopt (&rest keys
	       &key (context *context*) short-name long-name option)
  "Get an option's value in CONTEXT.
The option can be specified either by SHORT-NAME, LONG-NAME, or directly via
an OPTION object.
Return two values:
- the retrieved value,
- the value's source."
  (unless option
    (setq option
	  (apply #'search-option context (remove-keys keys :context))))
  (unless option
    (error "Getting option ~S from synopsis ~A in context ~A: unknown option."
	   (or short-name long-name)
	   (synopsis context)
	   context))
  ;; Try the command-line:
  (let ((cmdline-options (list)))
    (do ((cmdline-option
	  (pop (cmdline-options context))
	  (pop (cmdline-options context))))
	((null cmdline-option))
      (cond ((eq (cmdline-option-option cmdline-option) option)
	     (setf (cmdline-options context)
		   ;; Actually, I *do* have a use for nreconc ;-)
		   (nreconc cmdline-options (cmdline-options context)))
	     (return-from getopt
	       (values (cmdline-option-value cmdline-option)
		       (list (cmdline-option-source cmdline-option)
			     (cmdline-option-name cmdline-option)))))
	    (t
	     (push cmdline-option cmdline-options))))
    (setf (cmdline-options context) (nreverse cmdline-options)))
  ;; Try an environment variable:
  (with-context-error-handler context
    (let* ((env-var (env-var option))
	   (env-val (getenv env-var)))
      (when env-val
	(return-from getopt
	  (values (retrieve-from-environment option env-val)
		  (list :environement env-var))))))
  ;; Try a default value:
  (when (and (typep option 'valued-option)
	     (slot-boundp option 'default-value))
    (values (default-value option) :default)))

(defun getopt-cmdline (&key (context *context*))
  "Get the next command-line option in CONTEXT.
When there is no next command-line option, return nil.
Otherwise, return four values:
- the option object,
- the option's name used on the command-line,
- the retrieved value,
- the value source."
  (let ((cmdline-option (pop (cmdline-options context))))
    (when cmdline-option
      (values (cmdline-option-option cmdline-option)
	      (cmdline-option-name cmdline-option)
	      (cmdline-option-value cmdline-option)
	      (cmdline-option-source cmdline-option)))))

(defmacro multiple-value-getopt-cmdline
    ((option name value source &key context) &body body)
  "Get the next command-line option in CONTEXT. and evaluate BODY.
OPTION, NAME and VALUE are bound to the values returned by GETOPT-CMDLINE.
BODY is executed only if there is a next command-line option."
  `(multiple-value-bind (,option ,name ,value ,source)
       (getopt-cmdline :context (or ,context *context*))
     (when ,option
       ,@body)))

(defmacro do-cmdline-options
    ((option name value source &key context) &body body)
  "Evaluate BODY over all command-line options in CONTEXT.
OPTION, NAME and VALUE are bound to each option's object, name used on the
command-line and retrieved value."
  (let ((ctx (gensym "context")))
    `(let ((,ctx (or ,context *context*)))
       (do () ((null (cmdline-options ,ctx)))
	 (multiple-value-getopt-cmdline
	     (,option ,name ,value ,source :context ,ctx)
	   ,@body)))))



;; ==========================================================================
;; Context Instance Creation
;; ==========================================================================

(defun read-long-name ()
  "Read an option's long name from standard input."
  (format t "Please type in the correct option's long name:~%")
  (let (line)
    (loop (setq line (read-line))
	  (if (position #\= line)
	      (format t "Option names can't contain equal signs. Try again:~%")
	    (return (list line))))))

(defun read-call (&optional negated)
  "Read an option's call or pack from standard input.
If NEGATED, read a negated call or pack. Otherwise, read a short call or pack."
  (format t "Please type in the correct ~
	    ~:[short~;negated~] call or pack:~%"
    negated)
  (list (read-line)))

#i(push-cmdline-option 1)
#i(push-retrieved-option 3)
(defmethod initialize-instance :after ((context context) &key cmdline)
  "Parse CMDLINE."
  (setf (slot-value context 'progname) (pop cmdline))
  ;; #### WARNING: we have a chicken-and-egg problem here. The error handler
  ;; is supposed to be set from the --clon-error-handler option, but in order
  ;; to retrieve this option, we need to parse the command-line, which might
  ;; trigger some errors. Since we want the correct error handler to be set as
  ;; soon as possible, we need to treat this option in a very special way.
  ;; Here is what we do.
  ;; 1/ A very early value of :quit is provided in the class definition,
  ;;    thanks to an :initform.
  ;; 2/ Before doing anything else, we try to get a value from the
  ;;    environment. This is actually very simple: we can already use our
  ;;    context, eventhough it is not completely initialized yet. The only
  ;;    thing we need is a nil CMDLINE-OPTIONS slot, so that GETOPT directly
  ;;    goes to environment retrieval. If there's an error during this
  ;;    process, the handler is :quit. If nothing is found in the environment
  ;;    variable, the default value is retrieved, which is also :quit.
  (setf (slot-value context 'error-handler)
	(getopt :long-name "clon-error-handler" :context context))
  ;; 3/ Finally, during command-line parsing, we check if we got that
  ;; particular option, and handle it immediately.
  ;; Step one: parse the command-line =======================================
  (let ((cmdline-options (list))
	(remainder (list)))
    (macrolet ((push-cmdline-option (place &rest body)
		 "Push a new CMDLINE-OPTION created with BODY onto PLACE."
		 `(push (make-cmdline-option ,@body) ,place))
	       (push-retrieved-option (place func option
					     &optional cmdline-value cmdline)
		 "Retrieve OPTION from a FUNC call and push it onto PLACE.
- FUNC must be either :short or :negated,
- CMDLINE-VALUE is a potentially already parsed option argument,
- CMDILNE is where to find a potentially required argument."
		 (let* ((value (gensym "value"))
			(source (gensym "source"))
			(vars (list source value))
			(call (list option
				    (find-symbol
				     (concatenate 'string
				       ;; #### NOTE: case portability
				       (string :retrieve-from-)
				       (symbol-name func)
				       (string :-call))
				     :net.didierverna.clon)))
			new-cmdline)
		   (when cmdline-value
		     (push cmdline-value call))
		   (when cmdline
		     (setq new-cmdline (gensym "new-cmdline"))
		     (push new-cmdline vars)
		     (unless cmdline-value
		       (push nil call))
		     (push cmdline call))
		   `(multiple-value-bind ,(reverse vars) ,(reverse call)
		      ,(when cmdline `(setq ,cmdline ,new-cmdline))
		      (push-cmdline-option ,place
			:name (short-name ,option)
			:option ,option
			:value ,value
			:source ,source))))
	       (do-pack ((option pack context) &body body)
		 "Evaluate BODY with OPTION bound to each option from PACK.
CONTEXT is where to look for the options."
		 (let ((char (gensym "char"))
		       (name (gensym "name")))
		   `(loop :for ,char :across ,pack
			  :do (let* ((,name (make-string 1
					      :initial-element ,char))
				     (,option (search-option ,context
						:short-name ,name)))
				(assert ,option)
				,@body)))))
      (with-context-error-handler context
	(do ((arg (pop cmdline) (pop cmdline)))
	    ((null arg))
	  (cond ((string= arg "--")
		 ;; The Clon separator.
		 (setq remainder cmdline)
		 (setq cmdline nil))
		((beginning-of-string-p "--" arg)
		 ;; A long call.
		 (let* ((value-start (position #\= arg :start 2))
			(cmdline-name (subseq arg 2 value-start))
			(cmdline-value (when value-start
					 (subseq arg (1+ value-start))))
			option-name option)
		   (tagbody find-option
		      (setq option-name cmdline-name)
		      (setq option
			    (search-option context :long-name cmdline-name))
		      (unless option
			(multiple-value-setq (option option-name)
			  (search-option context :partial-name cmdline-name)))
		      (if option
			  (multiple-value-bind (value source new-cmdline)
			      (retrieve-from-long-call option
						       option-name
						       cmdline-value
						       cmdline)
			    (setq cmdline new-cmdline)
			    ;; #### NOTE: see comment at the top of this
			    ;; function about this hack.
			    (if (string= (long-name option)
					 "clon-error-handler")
				(setf (slot-value context 'error-handler)
				      value)
			      (push-cmdline-option cmdline-options
				:name option-name
				:option option
				:value value
				:source source)))
			(restart-case (error 'unknown-cmdline-option-error
					     :name cmdline-name
					     :argument cmdline-value)
			  (discard ()
			    :report "Discard unknown option."
			    nil)
			  (fix-option-name (new-cmdline-name)
			    :report "Fix the option's long name."
			    :interactive read-long-name
			    (setq cmdline-name new-cmdline-name)
			    (go find-option)))))))
		;; A short call, or a short pack.
		((beginning-of-string-p "-" arg)
		 (tagbody figure-this-short-call
		    (let* ((value-start (position #\= arg :start 2))
			   (cmdline-name (subseq arg 1 value-start))
			   (cmdline-value (when value-start
					    (subseq arg (1+ value-start))))
			   option)
		      (when cmdline-value
			(restart-case
			    (error 'invalid-short-equal-syntax :item arg)
			  (discard-argument ()
			    :report "Discard the argument."
			    (setq cmdline-value nil))
			  (stick-argument ()
			    :report "Stick argument to option name."
			    (setq cmdline-name
				  (concatenate 'string
				    cmdline-name cmdline-value))
			    (setq cmdline-value nil))
			  (separate-argument ()
			    :report "Separate option from its argument."
			    (push cmdline-value cmdline)
			    (setq cmdline-value nil))))
		      (setq option
			    (search-option context :short-name cmdline-name))
		      (unless option
			(multiple-value-setq (option cmdline-value)
			  (search-sticky-option context cmdline-name)))
		      (cond (option
			     (push-retrieved-option cmdline-options :short
						    option cmdline-value
						    cmdline))
			    ((potential-pack-p cmdline-name context)
			     ;; #### NOTE: When parsing a short pack, only the
			     ;; last option gets a cmdline argument because
			     ;; only the last one is allowed to retrieve an
			     ;; argument from there.
			     (do-pack (option
				       (subseq cmdline-name 0
					       (1- (length cmdline-name)))
				       context)
			       (push-retrieved-option cmdline-options :short
						      option))
			     (let* ((name (subseq cmdline-name
						  (1- (length cmdline-name))))
				    (option (search-option context
					      :short-name name)))
			       (assert option)
			       (push-retrieved-option
				cmdline-options :short option nil cmdline)))
			    (t
			     (restart-case
				 (error 'unrecognized-short-call-error
					:short-call cmdline-name)
			       (discard ()
				 :report "Discard this short call."
				 nil)
			       (fix-short-call (new-cmdline-name)
				 :report "Fix this short call."
				 :interactive (lambda () (read-call))
				 (setq arg (concatenate 'string
					     "-" new-cmdline-name))
				 (go figure-this-short-call))))))))
		;; A negated call or pack.
		((beginning-of-string-p "+" arg)
		 (block processing-negated-call
		   (tagbody figure-this-negated-call
		      (let* ((value-start (position #\= arg :start 2))
			     (cmdline-name (subseq arg 1 value-start))
			     (cmdline-value (when value-start
					      (subseq arg (1+ value-start))))
			     option)
			(when cmdline-value
			  (restart-case
			      (error 'invalid-negated-equal-syntax :item arg)
			    (discard-argument ()
			      :report "Discard the argument."
			      (setq cmdline-value nil))
			    (convert-to-short-and-stick ()
			      :report
			      "Convert to short call and stick argument."
			      (push (concatenate 'string
				      "-" cmdline-name cmdline-value)
				    cmdline)
			      (return-from processing-negated-call))
			    (convert-to-short-and-split ()
			      :report
			      "Convert to short call and split argument."
			      (push cmdline-value cmdline)
			      (push (concatenate 'string "-" cmdline-name)
				    cmdline)
			      (return-from processing-negated-call))))
			;; #### NOTE: in theory, we could allow partial
			;; matches on short names when they're used with the
			;; negated syntax, because there's no sticky argument
			;; or whatever. But we don't. That's all. Short names
			;; are not meant to be long (otherwise, that would be
			;; long names right?), so they're not meant to be
			;; abbreviated.
			(setq option
			      (search-option context :short-name cmdline-name))
			(cond (option
			       (push-retrieved-option cmdline-options :negated
						      option))
			      ((potential-pack-p cmdline-name context)
			       (do-pack (option cmdline-name context)
				 (push-retrieved-option cmdline-options
							:negated option)))
			      (t
			       (restart-case
				   (error 'unrecognized-negated-call-error
					  :negated-call cmdline-name)
				 (discard ()
				   :report "Discard this negated call."
				   nil)
				 (fix-negated-call (new-cmdline-name)
				   :report "Fix this negated call."
				   :interactive (lambda ()
						  (read-call :negated))
				   (setq arg (concatenate 'string
					       "+" new-cmdline-name))
				   (go figure-this-negated-call)))))))))
		(t
		 ;; Not an option call. Consider this as an implicit remainder
		 ;; if one is expected. Contrary to the case of an explicit
		 ;; one however (separated from the rest of the cmdline by
		 ;; --), trigger an error if a remainder is not expected.
		 (cond ((null (postfix context))
			(setq arg (cons arg cmdline))
			(setq cmdline nil)
			;; Note that here, the whole remainder of the
			;; cmdline might be discraded at once.
			(restartable-cmdline-junk-error arg))
		       (t
			(setq remainder (cons arg cmdline))
			(setq cmdline nil)))))))
      (setf (cmdline-options context) (nreverse cmdline-options))
      (setf (slot-value context 'remainder) remainder)))
  ;; Step two: handle internal options ======================================
  (when (getopt :context context :long-name "clon-banner")
    (format t "~A's command-line is powered by Clon,
the Command-Line Options Nuker library, version ~A,
written by Didier Verna <didier@didierverna.net>.

Copyright (C) 2010, 2011, 2015 Didier Verna
Clon is released under the terms of the BSD license.
See http://www.opensource.org/licenses/bsd-license for more information.
Clon is provided with NO warranty; not even for MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.~%"
      (pathname-name (progname :context context))
      (version :long))
    (exit))
  (let ((version-format (getopt :context context :long-name "clon-version")))
    (when version-format
      (format t "~A~%" (version version-format))
      (exit)))
  (when (getopt :context context :long-name "clon-lisp-information")
    (format t "~A ~A~%"
      (lisp-implementation-type)
      (lisp-implementation-version))
    (exit))
  (setf (slot-value context 'search-path)
	(getopt :context context :long-name "clon-search-path"))
  (setf (slot-value context 'theme)
	(getopt :context context :long-name "clon-theme"))
  (setf (slot-value context 'line-width)
	(getopt :context context :long-name "clon-line-width"))
  (setf (slot-value context 'highlight)
	(getopt :context context :long-name "clon-highlight"))
  ;; #### NOTE: do this one last because the output may depend on the values
  ;; from the above four.
  (when (getopt :context context :long-name "clon-help")
    (help :context context :item (clon-options-group context))
    (exit)))

(defun make-context (&rest keys &key synopsis cmdline (make-current t))
  "Make a new context.
- SYNOPSIS is the program synopsis to use in that context.
  It defaults to *SYNOPSIS*.
- CMDLINE is the argument list (strings) to process.
  It defaults to a POSIX conformant argv.
- If MAKE-CURRENT, make the new context current."
  (declare (ignore synopsis cmdline))
  (let ((context (apply #'make-instance 'context
			(remove-keys keys :make-current))))
    (when make-current
      (setq *context* context))
    context))



;; ==========================================================================
;; Context Manipulation Utilities
;; ==========================================================================

(defmacro with-context (context &body body)
  "Execute BODY with *context* bound to CONTEXT."
  `(let ((*context* ,context))
     ,@body))


;;; context.lisp ends here

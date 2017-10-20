;;; util.lisp --- General utilities

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
;; Miscellaneous Auxiliary Routines
;; ==========================================================================

#i(econd cond)
(defmacro econd (&body clauses)
  "Like COND, but signal an error if no clause evaluates to t."
  `(cond ,@(append clauses
	    '((t (error "Fell out of ECOND clauses."))))))

(defmacro endpush (object place)
  "Like push, but at the end."
  `(setf ,place (nconc ,place (list ,object))))

(defmacro maybe-push (object place &aux (the-object (gensym "object")))
  "Like push, but only if OBJECT is non-nil."
  `(let ((,the-object ,object))
     (when ,the-object (push ,the-object ,place))))

(defmacro accumulate
    ((initial-value) &body body
		     &aux (place (gensym "place"))
			  (initial-place (gensym "initial-place")))
  "Accumulate BODY forms in a list beginning with INITIAL-VALUE.
INITIAL-VALUE is not evaluated. BODY forms are accumulated only when their
value is non-nil.
If nothing to accumulate, then return nil instead of the list of
INITIAL-VALUE."
  `(let* ((,place (list ',initial-value))
	  (,initial-place ,place))
     ,@(mapcar (lambda (body-form)
		 `(maybe-push ,body-form ,place))
	       body)
     (when (not (eq ,initial-place ,place))
       (nreverse ,place))))

(defun beginning-of-string-p
    (beginning string &optional ignore-case &aux (length (length beginning)))
  "Check that STRING starts with BEGINNING.
If IGNORE-CASE, well, ignore case."
  (and (>= (length string) length)
       (funcall (if ignore-case #'string-equal #'string=)
		beginning string :end2 length)))

#i(closest-match 2)
(defun closest-match
    (match list &key ignore-case (key #'identity)
		&aux (match-length (length match))
		     (shortest-distance most-positive-fixnum)
		     closest-match)
  "Return the LIST element closest to MATCH, or nil.
If IGNORE-CASE, well, ignore case.
KEY should provide a way to get a string from each LIST element."
  (dolist (elt list)
    (let ((elt-string (funcall key elt))
	  distance)
      (when (and (beginning-of-string-p match elt-string ignore-case)
		 (< (setq distance (- (length elt-string) match-length))
		    shortest-distance))
	(setq shortest-distance distance)
	(setq closest-match elt))))
  closest-match)

(defun complete-string (beginning complete)
  "Complete BEGINNING with the rest of COMPLETE in parentheses.
For instance, completing 'he' with 'help' will produce 'he(lp)'."
  (assert (beginning-of-string-p beginning complete))
  (assert (not (string= beginning complete)))
  (concatenate 'string beginning "(" (subseq complete (length beginning)) ")"))

(defun list-to-string (list &key (key #'identity) (separator ", "))
  "Return a SEPARATOR-separated string of all LIST elements.
- KEY should provide a way to get a string from each LIST element.
- SEPARATOR is the string to insert between elements."
  (reduce (lambda (str1 str2) (concatenate 'string str1 separator str2))
      list
    :key key))



;; ==========================================================================
;; Key-Value Pairs Manipulation
;; ==========================================================================

#i(select-keys 1)
(defun select-keys (keys &rest selected)
  "Return a new property list from KEYS with only SELECTED ones."
  (loop :for key :in keys :by #'cddr
	:for val :in (cdr keys) :by #'cddr
	:when (member key selected)
	  :nconc (list key val)))

#i(remove-keys 1)
(defun remove-keys (keys &rest removed)
  "Return a new property list from KEYS without REMOVED ones."
  (loop :for key :in keys :by #'cddr
	:for val :in (cdr keys) :by #'cddr
	:unless (member key removed)
	  :nconc (list key val)))

#i(replace-in-keys 3)
(defmacro replace-in-keys ((key val) keys the-key form)
  "Replace every occurrence of THE-KEY in KEYS with FORM.
At every KEYS round, KEY and VAL are bound to the current key-value pair.
FORM is evaluated each time and should return a key-value list."
  `(loop :for ,key :in ,keys :by #'cddr
	 :for ,val :in (cdr ,keys) :by #'cddr
	 :if (eql ,key ,the-key)
	   :append ,form
	 :else
	   :nconc (list ,key ,val)))

;; #### NOTE: that's the typical situation where I would like a
;; destructuring-cond, but it seems difficult to do so because of the
;; standard imprecision of the reported error in case of a pattern matching
;; failure.
;; #### NOTE: I could extend this utility by supporting a global :test, or
;; even a per-replacement local one.
(defun replace-key (replacement keys)
  "Return a new property list from KEYS with REPLACEMENT.
REPLACEMENT can take the following forms:
- :KEY
  The effect is to remove :KEY from KEYS, as per REMOVE-KEYS.
- (:KEY :NEW-KEY)
  The effect is to replace :KEY with :NEW-KEY, leaving the values unchanged.
- (:KEY :NEW-KEY (VAL-OR-VALS NEW-VAL)*), with VAL-OR-VALS being
  either a value or a list of values. The effect is to replace :KEY with
  :NEW-KEY and a value matching one of the VAL-OR-VALS with the
  corresponding NEW-VAL. Values not matching any VAL-OR-VALS remain unchanged.
- (:KEY (VAL-OR-VALS :NEW-KEY NEW-VAL...)*), with VAL-OR-VALS as above. The
  effect is the same as above, but :NEW-KEY additionally depends on the
  matched value. If multiple :NEW-KEY NEW-VAL couples are provided, that many
  new keys are inserted along with their values. For values not matching any
  VAL-OR-VALS, :KEY and its value remain unchanged."
  (econd ((symbolp replacement)
	  (remove-keys keys replacement))
	 ((and (consp replacement)
	       (= (length replacement) 2)
	       (symbolp (car replacement))
	       (symbolp (cadr replacement)))
	  (destructuring-bind (old-key new-key) replacement
	    (replace-in-keys (key val) keys old-key
	      (list new-key val))))
	 ((and (consp replacement)
	       (> (length replacement) 2)
	       (symbolp (car replacement))
	       (symbolp (cadr replacement)))
	  (destructuring-bind (old-key new-key &rest replacements) replacement
	    (replace-in-keys (key val) keys old-key
	      (list new-key
		    (let ((match
			      (assoc val replacements
				     :test (lambda (val val-or-vals)
					     (if (consp val-or-vals)
						 (member val val-or-vals)
					       (eql val val-or-vals))))))
		      (if match (cadr match) val))))))
	 ((and (consp replacement)
	       (> (length replacement) 1)
	       (symbolp (car replacement)))
	  (destructuring-bind (old-key &rest replacements) replacement
	    (replace-in-keys (key val) keys old-key
	      (let ((match (assoc val replacements
				  :test (lambda (val val-or-vals)
					  (if (consp val-or-vals)
					      (member val val-or-vals)
					    (eql val val-or-vals))))))
		(if match
		    (cdr match)
		  (list key val))))))))

#i(replace-keys 1)
(defun replace-keys (keys &rest replacements &aux (new-keys keys))
  "Return a new property list from KEYS with REPLACEMENTS.
See REPLACE-KEY for more information on the replacement syntax."
  (dolist (replacement replacements)
    (setq new-keys (replace-key replacement new-keys)))
  new-keys)



;; ==========================================================================
;; CLOS Utility Routines
;; ==========================================================================

;; --------------------
;; Portability wrappers
;; --------------------

(defmacro declare-valid-superclass (class superclass)
  "Validate SUPERCLASS classes for CLASS classes."
  ;; #### PORTME.
  `(defmethod validate-superclass((class ,class) (superclass ,superclass))
     #+ecl (declare (ignore class superclass))
     t))


;; ----------------
;; Abstract classes
;; ----------------

(defclass abstract-class (standard-class)
  ()
  (:documentation "The ABSTRACT-CLASS class.
This is the meta-class for abstract classes."))

(defmacro defabstract (class super-classes slots &rest options)
  "Like DEFCLASS, but define an abstract class."
  (when (assoc :metaclass options)
    (error "Defining abstract class ~S: explicit meta-class option." class))
  `(defclass ,class ,super-classes ,slots ,@options
	     (:metaclass abstract-class)))

(defmethod make-instance ((class abstract-class) &rest initargs)
  (declare (ignore initargs))
  (error "Instanciating class ~S: is abstract." (class-name class)))

(declare-valid-superclass abstract-class standard-class)
(declare-valid-superclass standard-class abstract-class)


;; ----------------
;; Instance copying
;; ----------------

(defgeneric copy-instance (instance &optional subclass)
  (:documentation "Return a copy of INSTANCE.
Copy is either an object of INSTANCE's class, or INSTANCE's SUBCLASS if given.")
  (:method (instance &optional subclass)
    "Return a copy of INSTANCE.
Both instances share the same slot values."
    (let* ((class (class-of instance))
	   (slots (class-slots class))
	   (new-instance (make-instance (or subclass class))))
      (loop :for slot :in slots
	    :when (slot-boundp instance (slot-definition-name slot))
	      :do (setf (slot-value new-instance (slot-definition-name slot))
			(slot-value instance (slot-definition-name slot))))
      new-instance)))



;; ==========================================================================
;; System-related utilities
;; ==========================================================================

;; #### FIXME: this condition could be improved by having a pathname and a
;; better error-string message. But that needs to be done in a
;; compiler-dependent way. We know that the error comes from TRUENAME because
;; USER-HOMEDIR-PATHNAME cannot return nil when called without a HOST
;; option. But the actual error object signaled by TRUENAME is not standard.
(define-condition home-directory (warning)
  ((error-string :initarg :error-string :accessor error-string))
  (:report (lambda (warning stream)
	     (format stream "cannot find home directory: ~A."
		     (error-string warning)))))

;; #### FIXME: Anyway, this function is not the place to handle the error. It
;; should only provide a restart. There are currently two places where
;; HOME-DIRECTORY is used. The first one is for computing the default value of
;; the clon-search-path option, where this error is not critical. The second
;; is in the CONVERT method for the PATH options. There, the error is
;; critical.
(defun home-directory ()
  "Return user's home directory in canonical form.
If the user's home directory cannot be computed, signal a warning and return
NIL."
  (handler-case (truename (user-homedir-pathname))
    (file-error (error)
      (warn 'home-directory
	    :error-string (with-output-to-string (stream)
			    (let (*print-escape*)
			      (print-object error stream))))
      nil)))

(defun macosp ()
  "Return t if running on Mac OS."
  (string= (software-type) "Darwin"))



;; ==========================================================================
;; Wrappers around non ANSI features
;; ==========================================================================

(defun exit (&optional (status 0))
  "Quit the current application with STATUS."
  ;; #### PORTME.
  #+sbcl      (sb-ext:exit :code status)
  #+cmu       (unix:unix-exit status)
  #+ccl       (ccl:quit status)
  #+ecl       (ext:quit status)
  #+clisp     (ext:exit status)
  #+abcl      (extensions:exit :status status)
  #+allegro   (excl:exit status :quiet t)
  #+lispworks (lispworks:quit :status status))

(defvar *executablep* nil
  "Whether the current Lisp image is a standalone executable dumped by Clon.
This information is needed in some implementations that treat their
command-line differently in dumped images.")

(defun cmdline ()
  "Get the current application's command-line.
This command-line is not supposed to contain any Lisp implementation specific
option; only user-level ones. When a standalone executable is dumped, this is
always the case. When used interactively, this depends on the underlying Lisp
implementation. See appendix A.5 of the user manual for more information."
  ;; #### PORTME.
  #+sbcl      sb-ext:*posix-argv*
  #+cmu       (if *executablep*
		  lisp::lisp-command-line-list
		(cons (car lisp::lisp-command-line-list)
		      ext:*command-line-application-arguments*))
  #+ccl       (if *executablep*
		  ccl:*command-line-argument-list*
		(cons (car ccl:*command-line-argument-list*)
		      ccl:*unprocessed-command-line-arguments*))
  #+ecl       (if *executablep*
		  (ext:command-args)
		(cons (car (ext:command-args))
		      (cdr (member "--" (ext:command-args) :test #'string=))))
  #+clisp     (cons (aref (ext:argv) 0) ext:*args*)
  ;; #### NOTE: the trickery below is here to make CMDLINE work even when Clon
  ;; is loaded into ABCL without dumping the Clon way (see
  ;; +ABCL-MAIN-CLASS-TEMPLATE+).
  #+abcl      (cons (or (let ((symbol (find-symbol "*ARGV0*" 'extensions)))
			  (when symbol
			    (symbol-value symbol)))
			"abcl")
		    extensions:*command-line-argument-list*)
  #+allegro   (system:command-line-arguments)
  #+lispworks (if *executablep*
		  system:*line-arguments-list*
		(cons (car system:*line-arguments-list*)
		      (cdr (member "--" system:*line-arguments-list*
				   :test #'string=)))))

(defun getenv (variable)
  "Get environment VARIABLE's value. VARIABLE may be null."
  ;; #### PORTME.
  (when variable
    (#+sbcl      sb-posix:getenv
     #+cmu       unix:unix-getenv
     #+ccl       ccl:getenv
     #+ecl       ext:getenv
     #+clisp     ext:getenv
     #+abcl      extensions:getenv
     #+allegro   system:getenv
     #+lispworks hcl:getenv
     variable)))

;; #### NOTE: JAVA doesn't provide a way to set an environment variable. I've
;; seen tricks around to modify the startup environment memory mapping instead
;; of doing a real putenv, but I'll just disable the "modify-environment"
;; restart in environ.lisp for now.
#-abcl
(defun putenv (variable value)
  "Set environment VARIABLE to VALUE."
  ;; #### PORTME.
  #+sbcl      (sb-posix:putenv (concatenate 'string variable "=" value))
  #+cmu       (unix:unix-putenv (concatenate 'string variable "=" value))
  #+ccl       (ccl:setenv variable value)
  #+ecl       (ext:setenv variable value)
  #+clisp     (setf (ext:getenv variable) value)
  #+allegro   (excl.osi:putenv (concatenate 'string variable "=" value))
  #+lispworks (setf (hcl:getenv variable) value))

#+abcl
(defparameter *abcl-main-class-template*
  "import org.armedbear.lisp.*;

public class ~A
{
  public static void main (final String[] argv)
  {
    Runnable r = new Runnable ()
    {
      public void run()
      {
	try
	{
	  LispObject cmdline = Lisp.NIL;
	  for (String arg : argv)
	    cmdline = new Cons (arg, cmdline);
	  cmdline.nreverse ();
	  Lisp._COMMAND_LINE_ARGUMENT_LIST_.setSymbolValue (cmdline);

	  Interpreter interpreter = Interpreter.createInstance ();
	  interpreter.eval (\"(defvar extensions::*argv0* \\\"~A\\\")\");
	  interpreter.eval (\"(export 'extensions::*argv0* 'extensions)\");

	  interpreter.eval (\"(require \\\"asdf\\\")\");
	  interpreter.eval (\"(asdf:load-system :net.didierverna.clon.setup)\");
	  interpreter.eval (\"(net.didierverna.clon.setup:configure :restricted t)\");

	  Load.loadSystemFile (\"/~A\", false, false, false);
	}
	catch (ProcessingTerminated e)
	{
	  System.exit (e.getStatus ());
	}
      }
    };

    new Thread (null, r, \"interpreter\", 4194304L).start();
  }
}~%"
  "Main class template for ABCL.")

(defmacro dump (name function &rest args)
  "Dump a standalone executable named NAME starting with FUNCTION.
ARGS may be any arguments understood by the underlying implementation's
dumping facility. They will simply be passed along. Note that DUMP already
passes some such arguments. Some of them are critical for the dumping facility
(e.g. :executable) and cannot be overridden. Some others, however, will be if
you provide them as well (e.g. :load-init-file).

Since executable dumping is not available in all supported implementations,
this function behaves differently in some cases, as described below.

- ECL doesn't create executables by dumping a Lisp image, but relies on having
  toplevel code to execute instead, so this macro simply expands to a call to
  FUNCTION. This also means that ARGS is unused.
- ABCL can't dump executables at all because of the underlying Java
  implementation, so this macro expands to just (PROGN) but creates a Java
  class file with a main function that creates an interpreter, loads
  the file in which this macro call appears and calls FUNCTION. This also
  means that ARGS is unused."
  ;; #### PORTME.
  #+ecl     (declare (ignore name))
  #+allegro (declare (ignore function))
  #+sbcl    `(progn
	       (setq *executablep* t) ; not used but here for correctness
	       (sb-ext:save-lisp-and-die ,name
		 :toplevel #',function
		 :executable t
		 :save-runtime-options t
		 ,@args))
  #+cmu     `(progn
	       (setq *executablep* t)
	       (ext:save-lisp ,name
		 :init-function #',function
		 :executable t
		 :process-command-line nil
		 ,@args
		 :load-init-file nil
		 :site-init nil
		 :print-herald nil))
  #+ccl     `(progn
	       (setq *executablep* t)
	       (ccl:save-application ,name
		 :toplevel-function #',function
		 :prepend-kernel t
		 ,@args
		 :init-file nil
		 :error-handler :quit))
  ;; #### NOTE: ECL works differently: it needs an entry point (i.e. actual
  ;; code to execute) instead of a main function. So we expand DUMP to just
  ;; call that function.
  #+ecl     `(progn
	       (setq *executablep* t)
	       (,function))
  ;; CLISP's saveinitmem function doesn't quit, so we need to do so here.
  #+clisp   `(progn
	       (setq *executablep* t) ; not used but here for correctness
	       (ext:saveinitmem ,name
		 :init-function #',function
		 :executable 0
		 ,@args
		 :quiet t
		 :norc t)
	       (exit))
  #+abcl   (if (configuration :dump)
	       (let ((source-pathname (or *compile-file-pathname*
					  *load-pathname*))
		     (class-name (copy-seq name)))
		 (setf (aref class-name 0) (char-upcase (aref class-name 0)))
		 (with-open-file
		     (*standard-output*
		      (merge-pathnames
		       (make-pathname :name class-name :type "java")
		       source-pathname)
		      :direction :output :if-exists :supersede)
		   (format t *abcl-main-class-template*
		     class-name name (namestring source-pathname)))
		 '(progn))
	     `(progn
		(setq *executablep* t) ; not used but here for correctness
		(,function)))
  ;; ACL's dumplisp function doesn't quit, so we need to do so here.
  #+allegro `(progn
	       (setq *executablep* t) ; not used but here for correctness
	       (excl:dumplisp :name (concatenate 'string ,name ".dxl")
			      ,@args
			      :suppress-allegro-cl-banner t)
	       (exit))
  #+lispworks `(progn
		 (setq *executablep* t)
		 (lispworks:load-all-patches)
		 (lispworks:deliver ',function ,name 0 ,@args)))


;;; util.lisp ends here

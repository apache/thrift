;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; functions.lisp --- High-level interface to foreign functions.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2005-2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package #:cffi)

;;;# Calling Foreign Functions
;;;
;;; FOREIGN-FUNCALL is the main primitive for calling foreign
;;; functions.  It converts each argument based on the installed
;;; translators for its type, then passes the resulting list to
;;; CFFI-SYS:%FOREIGN-FUNCALL.
;;;
;;; For implementation-specific reasons, DEFCFUN doesn't use
;;; FOREIGN-FUNCALL directly and might use something else (passed to
;;; TRANSLATE-OBJECTS as the CALL-FORM argument) instead of
;;; CFFI-SYS:%FOREIGN-FUNCALL to call the foreign-function.

(defun translate-objects (syms args types rettype call-form &optional indirect)
  "Helper function for FOREIGN-FUNCALL and DEFCFUN.  If 'indirect is T, all arguments are represented by foreign pointers, even those that can be represented by CL objects."
  (if (null args)
      (expand-from-foreign call-form (parse-type rettype))
      (funcall
       (if indirect
           #'expand-to-foreign-dyn-indirect
           #'expand-to-foreign-dyn)
       (car args) (car syms)
       (list (translate-objects (cdr syms) (cdr args)
                                (cdr types) rettype call-form indirect))
       (parse-type (car types)))))

(defun parse-args-and-types (args)
  "Returns 4 values: types, canonicalized types, args and return type."
  (let* ((len (length args))
         (return-type (if (oddp len) (lastcar args) :void)))
    (loop repeat (floor len 2)
          for (type arg) on args by #'cddr
          collect type into types
          collect (canonicalize-foreign-type type) into ctypes
          collect arg into fargs
          finally (return (values types ctypes fargs return-type)))))

;;; While the options passed directly to DEFCFUN/FOREIGN-FUNCALL have
;;; precedence, we also grab its library's options, if possible.
(defun parse-function-options (options &key pointer)
  (destructuring-bind (&key (library :default libraryp)
                            (cconv nil cconv-p)
                            (calling-convention cconv calling-convention-p)
                            (convention calling-convention))
      options
    (when cconv-p
      (warn-obsolete-argument :cconv :convention))
    (when calling-convention-p
      (warn-obsolete-argument :calling-convention :convention))
    (list* :convention
           (or convention
               (when libraryp
                 (let ((lib-options (foreign-library-options
                                     (get-foreign-library library))))
                   (getf lib-options :convention)))
               :cdecl)
           ;; Don't pass the library option if we're dealing with
           ;; FOREIGN-FUNCALL-POINTER.
           (unless pointer
             (list :library library)))))

(defun structure-by-value-p (ctype)
  "A structure or union is to be called or returned by value."
  (let ((actual-type (ensure-parsed-base-type ctype)))
    (or (and (typep actual-type 'foreign-struct-type)
             (not (bare-struct-type-p actual-type)))
        #+cffi::no-long-long (typep actual-type 'emulated-llong-type))))

(defun fn-call-by-value-p (argument-types return-type)
  "One or more structures in the arguments or return from the function are called by value."
  (or (some 'structure-by-value-p argument-types)
      (structure-by-value-p return-type)))

(defvar *foreign-structures-by-value*
  (lambda (&rest args)
    (declare (ignore args))
    (restart-case
        (error "Unable to call structures by value without cffi-libffi loaded.")
      (load-cffi-libffi () :report "Load cffi-libffi."
        (asdf:operate 'asdf:load-op 'cffi-libffi))))
  "A function that produces a form suitable for calling structures by value.")

(defun foreign-funcall-form (thing options args pointerp)
  (multiple-value-bind (types ctypes fargs rettype)
      (parse-args-and-types args)
    (let ((syms (make-gensym-list (length fargs)))
          (fsbvp (fn-call-by-value-p ctypes rettype)))
      (if fsbvp
          ;; Structures by value call through *foreign-structures-by-value*
          (funcall *foreign-structures-by-value*
                   thing
                   fargs
                   syms
                   types
                   rettype
                   ctypes
                   pointerp)
          (translate-objects
           syms fargs types rettype
           `(,(if pointerp '%foreign-funcall-pointer '%foreign-funcall)
             ;; No structures by value, direct call
             ,thing
             (,@(mapcan #'list ctypes syms)
              ,(canonicalize-foreign-type rettype))
             ,@(parse-function-options options :pointer pointerp)))))))

(defmacro foreign-funcall (name-and-options &rest args)
  "Wrapper around %FOREIGN-FUNCALL that translates its arguments."
  (let ((name (car (ensure-list name-and-options)))
        (options (cdr (ensure-list name-and-options))))
    (foreign-funcall-form name options args nil)))

(defmacro foreign-funcall-pointer (pointer options &rest args)
  (foreign-funcall-form pointer options args t))

(defun promote-varargs-type (builtin-type)
  "Default argument promotions."
  (case builtin-type
    (:float :double)
    ((:char :short) :int)
    ((:unsigned-char :unsigned-short) :unsigned-int)
    (t builtin-type)))

(defun foreign-funcall-varargs-form (thing options fixed-args varargs pointerp)
  (multiple-value-bind (fixed-types fixed-ctypes fixed-fargs)
      (parse-args-and-types fixed-args)
    (multiple-value-bind (varargs-types varargs-ctypes varargs-fargs rettype)
        (parse-args-and-types varargs)
      (let ((fixed-syms (make-gensym-list (length fixed-fargs)))
            (varargs-syms (make-gensym-list (length varargs-fargs))))
        (translate-objects
         (append fixed-syms varargs-syms)
         (append fixed-fargs varargs-fargs)
         (append fixed-types varargs-types)
         rettype
         `(,(if pointerp '%foreign-funcall-pointer '%foreign-funcall)
            ,thing
            ,(append
              (mapcan #'list
                      (nconc fixed-ctypes
                             (mapcar #'promote-varargs-type varargs-ctypes))
                      (append fixed-syms
                              (loop for sym in varargs-syms
                                    and type in varargs-ctypes
                                    if (eq type :float)
                                    collect `(float ,sym 1.0d0)
                                    else collect sym)))
              (list (canonicalize-foreign-type rettype)))
            ,@options))))))

;;; For now, the only difference between this macro and
;;; FOREIGN-FUNCALL is that it does argument promotion for that
;;; variadic argument. This could be useful to call an hypothetical
;;; %foreign-funcall-varargs on some hypothetical lisp on an
;;; hypothetical platform that has different calling conventions for
;;; varargs functions. :-)
(defmacro foreign-funcall-varargs (name-and-options fixed-args
                                   &rest varargs)
  "Wrapper around %FOREIGN-FUNCALL that translates its arguments
and does type promotion for the variadic arguments."
  (let ((name (car (ensure-list name-and-options)))
        (options (cdr (ensure-list name-and-options))))
    (foreign-funcall-varargs-form name options fixed-args varargs nil)))

(defmacro foreign-funcall-pointer-varargs (pointer options fixed-args
                                           &rest varargs)
  "Wrapper around %FOREIGN-FUNCALL-POINTER that translates its
arguments and does type promotion for the variadic arguments."
  (foreign-funcall-varargs-form pointer options fixed-args varargs t))

;;;# Defining Foreign Functions
;;;
;;; The DEFCFUN macro provides a declarative interface for defining
;;; Lisp functions that call foreign functions.

;; If cffi-sys doesn't provide a defcfun-helper-forms,
;; we define one that uses %foreign-funcall.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'defcfun-helper-forms)
    (defun defcfun-helper-forms (name lisp-name rettype args types options)
      (declare (ignore lisp-name))
      (values
       '()
       `(%foreign-funcall ,name ,(append (mapcan #'list types args)
                                         (list rettype))
                          ,@options)))))

(defun %defcfun (lisp-name foreign-name return-type args options docstring)
  (let* ((arg-names (mapcar #'first args))
         (arg-types (mapcar #'second args))
         (syms (make-gensym-list (length args)))
         (call-by-value (fn-call-by-value-p arg-types return-type)))
    (multiple-value-bind (prelude caller)
        (if call-by-value
            (values nil nil)
            (defcfun-helper-forms
             foreign-name lisp-name (canonicalize-foreign-type return-type)
             syms (mapcar #'canonicalize-foreign-type arg-types) options))
      `(progn
         ,prelude
         (defun ,lisp-name ,arg-names
           ,@(ensure-list docstring)
           ,(if call-by-value
                `(foreign-funcall
                  ,(cons foreign-name options)
                  ,@(append (mapcan #'list arg-types arg-names)
                            (list return-type)))
                (translate-objects
                 syms arg-names arg-types return-type caller)))))))

(defun %defcfun-varargs (lisp-name foreign-name return-type args options doc)
  (with-unique-names (varargs)
    (let ((arg-names (mapcar #'car args)))
      `(defmacro ,lisp-name (,@arg-names &rest ,varargs)
         ,@(ensure-list doc)
         `(foreign-funcall-varargs
           ,'(,foreign-name ,@options)
           ,,`(list ,@(loop for (name type) in args
                            collect `',type collect name))
           ,@,varargs
           ,',return-type)))))

(defgeneric translate-underscore-separated-name (name)
  (:method ((name string))
    (values (intern (canonicalize-symbol-name-case (substitute #\- #\_ name)))))
  (:method ((name symbol))
    (substitute #\_ #\- (string-downcase (symbol-name name)))))

(defun collapse-prefix (l special-words)
  (unless (null l)
    (multiple-value-bind (newpre skip) (check-prefix l special-words)
      (cons newpre (collapse-prefix (nthcdr skip l) special-words)))))

(defun check-prefix (l special-words)
  (let ((pl (loop for i from (1- (length l)) downto 0
                  collect (apply #'concatenate 'simple-string (butlast l i)))))
    (loop for w in special-words
          for p = (position-if #'(lambda (s) (string= s w)) pl)
          when p do (return-from check-prefix (values (nth p pl) (1+ p))))
    (values (first l) 1)))

(defgeneric translate-camelcase-name (name &key upper-initial-p special-words)
  (:method ((name string) &key upper-initial-p special-words)
    (declare (ignore upper-initial-p))
    (values (intern (reduce #'(lambda (s1 s2)
                                (concatenate 'simple-string s1 "-" s2))
                            (mapcar #'string-upcase
                                    (collapse-prefix
                                     (split-if #'(lambda (ch)
                                                   (or (upper-case-p ch)
                                                       (digit-char-p ch)))
                                               name)
                                     special-words))))))
  (:method ((name symbol) &key upper-initial-p special-words)
    (apply #'concatenate
           'string
           (loop for str in (split-if #'(lambda (ch) (eq ch #\-))
                                          (string name)
                                      :elide)
                 for first-word-p = t then nil
                 for e = (member str special-words
                                 :test #'equal :key #'string-upcase)
                 collect (cond
                           ((and first-word-p (not upper-initial-p))
                            (string-downcase str))
                           (e (first e))
                           (t (string-capitalize str)))))))

(defgeneric translate-name-from-foreign (foreign-name package &optional varp)
  (:method (foreign-name package &optional varp)
    (declare (ignore package))
    (let ((sym (translate-underscore-separated-name foreign-name)))
      (if varp
          (values (intern (format nil "*~A*"
                                  (canonicalize-symbol-name-case
                                   (symbol-name sym)))))
          sym))))

(defgeneric translate-name-to-foreign (lisp-name package &optional varp)
  (:method (lisp-name package &optional varp)
    (declare (ignore package))
    (let ((name (translate-underscore-separated-name lisp-name)))
      (if varp
          (string-trim '(#\*) name)
          name))))

(defun lisp-name (spec varp)
  (check-type spec string)
  (translate-name-from-foreign spec *package* varp))

(defun foreign-name (spec varp)
  (check-type spec (and symbol (not null)))
  (translate-name-to-foreign spec *package* varp))

(defun foreign-options (opts varp)
  (if varp
      (funcall 'parse-defcvar-options opts)
      (parse-function-options opts)))

(defun lisp-name-p (name)
  (and name (symbolp name) (not (keywordp name))))

(defun %parse-name-and-options (spec varp)
  (cond
    ((stringp spec)
     (values (lisp-name spec varp) spec nil))
    ((symbolp spec)
     (assert (not (null spec)))
     (values spec (foreign-name spec varp) nil))
    ((and (consp spec) (stringp (first spec)))
     (destructuring-bind (foreign-name &rest options)
         spec
       (cond
         ((or (null options)
              (keywordp (first options)))
          (values (lisp-name foreign-name varp) foreign-name options))
         (t
          (assert (lisp-name-p (first options)))
          (values (first options) foreign-name (rest options))))))
    ((and (consp spec) (lisp-name-p (first spec)))
     (destructuring-bind (lisp-name &rest options)
         spec
       (cond
         ((or (null options)
              (keywordp (first options)))
          (values lisp-name (foreign-name spec varp) options))
         (t
          (assert (stringp (first options)))
          (values lisp-name (first options) (rest options))))))
    (t
     (error "Not a valid foreign function specifier: ~A" spec))))

;;; DEFCFUN's first argument has can have the following syntax:
;;;
;;;     1.  string
;;;     2.  symbol
;;;     3.  \( string [symbol] options* )
;;;     4.  \( symbol [string] options* )
;;;
;;; The string argument denotes the foreign function's name. The
;;; symbol argument is used to name the Lisp function. If one isn't
;;; present, its name is derived from the other. See the user
;;; documentation for an explanation of the derivation rules.
(defun parse-name-and-options (spec &optional varp)
  (multiple-value-bind (lisp-name foreign-name options)
      (%parse-name-and-options spec varp)
    (values lisp-name foreign-name (foreign-options options varp))))

;;; If we find a &REST token at the end of ARGS, it means this is a
;;; varargs foreign function therefore we define a lisp macro using
;;; %DEFCFUN-VARARGS. Otherwise, a lisp function is defined with
;;; %DEFCFUN.
(defmacro defcfun (name-and-options return-type &body args)
  "Defines a Lisp function that calls a foreign function."
  (let ((docstring (when (stringp (car args)) (pop args))))
    (multiple-value-bind (lisp-name foreign-name options)
        (parse-name-and-options name-and-options)
      (if (eq (lastcar args) '&rest)
          (%defcfun-varargs lisp-name foreign-name return-type
                            (butlast args) options docstring)
          (%defcfun lisp-name foreign-name return-type args options
                    docstring)))))

;;;# Defining Callbacks

(defun inverse-translate-objects (args types declarations rettype call)
  `(let (,@(loop for arg in args and type in types
                 collect (list arg (expand-from-foreign
                                    arg (parse-type type)))))
     ,@declarations
     ,(expand-to-foreign call (parse-type rettype))))

(defun parse-defcallback-options (options)
  (destructuring-bind (&key (cconv :cdecl cconv-p)
                            (calling-convention cconv calling-convention-p)
                            (convention calling-convention))
      options
    (when cconv-p
      (warn-obsolete-argument :cconv :convention))
    (when calling-convention-p
      (warn-obsolete-argument :calling-convention :convention))
    (list :convention convention)))

(defmacro defcallback (name-and-options return-type args &body body)
  (multiple-value-bind (body declarations)
      (parse-body body :documentation t)
    (let ((arg-names (mapcar #'car args))
          (arg-types (mapcar #'cadr args))
          (name (car (ensure-list name-and-options)))
          (options (cdr (ensure-list name-and-options))))
      `(progn
         (%defcallback ,name ,(canonicalize-foreign-type return-type)
             ,arg-names ,(mapcar #'canonicalize-foreign-type arg-types)
           ,(inverse-translate-objects
             arg-names arg-types declarations return-type
             `(block ,name ,@body))
           ,@(parse-defcallback-options options))
         ',name))))

(declaim (inline get-callback))
(defun get-callback (symbol)
  (%callback symbol))

(defmacro callback (name)
  `(%callback ',name))

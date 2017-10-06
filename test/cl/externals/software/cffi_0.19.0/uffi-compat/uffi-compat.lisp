;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; uffi-compat.lisp --- UFFI compatibility layer for CFFI.
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

;;; Code borrowed from UFFI is Copyright (c) Kevin M. Rosenberg.

(defpackage #:cffi-uffi-compat
  (:nicknames #:uffi) ;; is this a good idea?
  (:use #:cl)
  (:export

   ;; immediate types
   #:def-constant
   #:def-foreign-type
   #:def-type
   #:null-char-p

   ;; aggregate types
   #:def-enum
   #:def-struct
   #:get-slot-value
   #:get-slot-pointer
   #:def-array-pointer
   #:deref-array
   #:def-union

   ;; objects
   #:allocate-foreign-object
   #:free-foreign-object
   #:with-foreign-object
   #:with-foreign-objects
   #:size-of-foreign-type
   #:pointer-address
   #:deref-pointer
   #:ensure-char-character
   #:ensure-char-integer
   #:ensure-char-storable
   #:null-pointer-p
   #:make-null-pointer
   #:make-pointer
   #:+null-cstring-pointer+
   #:char-array-to-pointer
   #:with-cast-pointer
   #:def-foreign-var
   #:convert-from-foreign-usb8
   #:def-pointer-var

   ;; string functions
   #:convert-from-cstring
   #:convert-to-cstring
   #:free-cstring
   #:with-cstring
   #:with-cstrings
   #:convert-from-foreign-string
   #:convert-to-foreign-string
   #:allocate-foreign-string
   #:with-foreign-string
   #:with-foreign-strings
   #:foreign-string-length              ; not implemented
   #:string-to-octets
   #:octets-to-string
   #:foreign-encoded-octet-count

   ;; function call
   #:def-function

   ;; libraries
   #:find-foreign-library
   #:load-foreign-library
   #:default-foreign-library-type
   #:foreign-library-types

   ;; os
   #:getenv
   #:run-shell-command
   ))

(in-package #:cffi-uffi-compat)

#+clisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (equal (machine-type) "POWER MACINTOSH")
    (pushnew :ppc *features*)))

(defun convert-uffi-type (uffi-type)
  "Convert a UFFI primitive type to a CFFI type."
  ;; Many CFFI types are the same as UFFI.  This list handles the
  ;; exceptions only.
  (case uffi-type
    (:cstring :pointer)
    (:pointer-void :pointer)
    (:pointer-self :pointer)
    ;; Although UFFI's documentation claims dereferencing :CHAR and
    ;; :UNSIGNED-CHAR returns characters, it actually returns
    ;; integers.
    (:char :char)
    (:unsigned-char :unsigned-char)
    (:byte :char)
    (:unsigned-byte :unsigned-char)
    (t
     (if (listp uffi-type)
         (case (car uffi-type)
           ;; this is imho gross but it is what uffi does
           (quote (convert-uffi-type (second uffi-type)))
           (* :pointer)
           (:array `(uffi-array ,(convert-uffi-type (second uffi-type))
                                ,(third uffi-type)))
           (:union (second uffi-type))
           (:struct (convert-uffi-type (second uffi-type)))
           (:struct-pointer :pointer))
         uffi-type))))

(cffi:define-foreign-type uffi-array-type ()
  ;; ELEMENT-TYPE should be /unparsed/, suitable for passing to mem-aref.
  ((element-type :initform (error "An element-type is required.")
                 :accessor element-type :initarg :element-type)
   (nelems :initform (error "nelems is required.")
           :accessor nelems :initarg :nelems))
  (:actual-type :pointer)
  (:documentation "UFFI's :array type."))

(cffi:define-parse-method uffi-array (element-type count)
  (make-instance 'uffi-array-type :element-type element-type
                 :nelems (or count 1)))

(defmethod cffi:foreign-type-size ((type uffi-array-type))
  (* (cffi:foreign-type-size (element-type type)) (nelems type)))

(defmethod cffi::aggregatep ((type uffi-array-type))
  t)

;; UFFI's :(unsigned-)char
#+#:ignore
(cffi:define-foreign-type uffi-char ()
  ())

#+#:ignore
(cffi:define-parse-method uffi-char (base-type)
  (make-instance 'uffi-char :actual-type base-type))

#+#:ignore
(defmethod cffi:translate-to-foreign ((value character) (type uffi-char))
  (char-code value))

#+#:ignore
(defmethod cffi:translate-from-foreign (obj (type uffi-char))
  (code-char obj))

(defmacro def-type (name type)
  "Define a Common Lisp type NAME for UFFI type TYPE."
  (declare (ignore type))
  `(deftype ,name () t))

(defmacro def-foreign-type (name type)
  "Define a new foreign type."
  `(cffi:defctype ,name ,(convert-uffi-type type)))

(defmacro def-constant (name value &key export)
  "Define a constant and conditionally export it."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defconstant ,name ,value)
     ,@(when export `((export ',name)))
     ',name))

(defmacro null-char-p (val)
  "Return true if character is null."
  `(zerop (char-code ,val)))

(defmacro def-enum (enum-name args &key (separator-string "#"))
  "Creates a constants for a C type enum list, symbols are
created in the created in the current package. The symbol is the
concatenation of the enum-name name, separator-string, and
field-name"
  (let ((counter 0)
        (cmds nil)
        (constants nil))
    (declare (fixnum counter))
    (dolist (arg args)
      (let ((name (if (listp arg) (car arg) arg))
            (value (if (listp arg)
                       (prog1
                           (setq counter (cadr arg))
                         (incf counter))
                       (prog1
                           counter
                         (incf counter)))))
        (setq name (intern (concatenate 'string
                                        (symbol-name enum-name)
                                        separator-string
                                        (symbol-name name))))
        (push `(def-constant ,name ,value) constants)))
    (setf cmds (append '(progn) `((cffi:defctype ,enum-name :int))
                       (nreverse constants)))
    cmds))

(defmacro def-struct (name &body fields)
  "Define a C structure."
  `(cffi:defcstruct ,name
     ,@(loop for (name uffi-type) in fields
             for cffi-type = (convert-uffi-type uffi-type)
             collect (list name cffi-type))))

;; TODO: figure out why the compiler macro is kicking in before
;; the setf expander.
(defun %foreign-slot-value (obj type field)
  (cffi:foreign-slot-value obj `(:struct ,type) field))

(defun (setf %foreign-slot-value) (value obj type field)
  (setf (cffi:foreign-slot-value obj `(:struct ,type) field) value))

(defmacro get-slot-value (obj type field)
  "Access a slot value from a structure."
  `(%foreign-slot-value ,obj ,type ,field))

;; UFFI uses a different function when accessing a slot whose
;; type is a pointer. We don't need that in CFFI so we use
;; foreign-slot-value too.
(defmacro get-slot-pointer (obj type field)
  "Access a pointer slot value from a structure."
  `(cffi:foreign-slot-value ,obj ,type ,field))

(defmacro def-array-pointer (name type)
  "Define a foreign array type."
  `(cffi:defctype ,name (uffi-array ,(convert-uffi-type type) 1)))

(defmacro deref-array (array type position)
  "Dereference an array."
  `(cffi:mem-aref ,array
                  ,(if (constantp type)
                       `',(element-type (cffi::parse-type
                                         (convert-uffi-type (eval type))))
                       `(element-type (cffi::parse-type
                                       (convert-uffi-type ,type))))
                  ,position))

;; UFFI's documentation on DEF-UNION is a bit scarce, I'm not sure
;; if DEFCUNION and DEF-UNION are strictly compatible.
(defmacro def-union (name &body fields)
  "Define a foreign union type."
  `(cffi:defcunion ,name
     ,@(loop for (name uffi-type) in fields
             for cffi-type = (convert-uffi-type uffi-type)
             collect (list name cffi-type))))

(defmacro allocate-foreign-object (type &optional (size 1))
  "Allocate one or more instance of a foreign type."
  `(cffi:foreign-alloc ,(if (constantp type)
                            `',(convert-uffi-type (eval type))
                            `(convert-uffi-type ,type))
                       :count ,size))

(defmacro free-foreign-object (ptr)
  "Free a foreign object allocated by ALLOCATE-FOREIGN-OBJECT."
  `(cffi:foreign-free ,ptr))

(defmacro with-foreign-object ((var type) &body body)
  "Wrap the allocation of a foreign object around BODY."
  `(cffi:with-foreign-object (,var (convert-uffi-type ,type))
     ,@body))

;; Taken from UFFI's src/objects.lisp
(defmacro with-foreign-objects (bindings &rest body)
  (if bindings
      `(with-foreign-object ,(car bindings)
         (with-foreign-objects ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

(defmacro size-of-foreign-type (type)
  "Return the size in bytes of a foreign type."
  `(cffi:foreign-type-size (convert-uffi-type ,type)))

(defmacro pointer-address (ptr)
  "Return the address of a pointer."
  `(cffi:pointer-address ,ptr))

(defmacro deref-pointer (ptr type)
  "Dereference a pointer."
  `(cffi:mem-ref ,ptr (convert-uffi-type ,type)))

(defsetf deref-pointer (ptr type) (value)
  `(setf (cffi:mem-ref ,ptr (convert-uffi-type ,type)) ,value))

(defmacro ensure-char-character (obj &environment env)
  "Convert OBJ to a character if it is an integer."
  (if (constantp obj env)
      (if (characterp obj) obj (code-char obj))
      (let ((obj-var (gensym)))
        `(let ((,obj-var ,obj))
           (if (characterp ,obj-var)
               ,obj-var
               (code-char ,obj-var))))))

(defmacro ensure-char-integer (obj &environment env)
  "Convert OBJ to an integer if it is a character."
  (if (constantp obj env)
      (let ((the-obj (eval obj)))
        (if (characterp the-obj) (char-code the-obj) the-obj))
      (let ((obj-var (gensym)))
        `(let ((,obj-var ,obj))
           (if (characterp ,obj-var)
               (char-code ,obj-var)
               ,obj-var)))))

(defmacro ensure-char-storable (obj)
  "Ensure OBJ is storable as a character."
  `(ensure-char-integer ,obj))

(defmacro make-null-pointer (type)
  "Create a NULL pointer."
  (declare (ignore type))
  `(cffi:null-pointer))

(defmacro make-pointer (address type)
  "Create a pointer to ADDRESS."
  (declare (ignore type))
  `(cffi:make-pointer ,address))

(defmacro null-pointer-p (ptr)
  "Return true if PTR is a null pointer."
  `(cffi:null-pointer-p ,ptr))

(defparameter +null-cstring-pointer+ (cffi:null-pointer)
  "A constant NULL string pointer.")

(defmacro char-array-to-pointer (obj)
  obj)

(defmacro with-cast-pointer ((var ptr type) &body body)
  "Cast a pointer, does nothing in CFFI."
  (declare (ignore type))
  `(let ((,var ,ptr))
     ,@body))

(defmacro def-foreign-var (name type module)
  "Define a symbol macro to access a foreign variable."
  (declare (ignore module))
  (flet ((lisp-name (name)
           (intern (cffi-sys:canonicalize-symbol-name-case
                    (substitute #\- #\_ name)))))
    `(cffi:defcvar ,(if (listp name)
                        name
                        (list name (lisp-name name)))
         ,(convert-uffi-type type))))

(defmacro def-pointer-var (name value &optional doc)
  #-openmcl `(defvar ,name ,value ,@(if doc (list doc)))
  #+openmcl `(ccl::defloadvar ,name ,value ,doc))

(defmacro convert-from-cstring (s)
  "Convert a cstring to a Lisp string."
  (let ((ret (gensym)))
    `(let ((,ret (cffi:foreign-string-to-lisp ,s)))
       (if (equal ,ret "")
           nil
           ,ret))))

(defmacro convert-to-cstring (obj)
  "Convert a Lisp string to a cstring."
  (let ((str (gensym)))
    `(let ((,str ,obj))
       (if (null ,str)
           (cffi:null-pointer)
           (cffi:foreign-string-alloc ,str)))))

(defmacro free-cstring (ptr)
  "Free a cstring."
  `(cffi:foreign-string-free ,ptr))

(defmacro with-cstring ((foreign-string lisp-string) &body body)
  "Binds a newly creating string."
  (let ((str (gensym)) (body-proc (gensym)))
    `(flet ((,body-proc (,foreign-string) ,@body))
       (let ((,str ,lisp-string))
         (if (null ,str)
             (,body-proc (cffi:null-pointer))
             (cffi:with-foreign-string (,foreign-string ,str)
               (,body-proc ,foreign-string)))))))

;; Taken from UFFI's src/strings.lisp
(defmacro with-cstrings (bindings &rest body)
  (if bindings
      `(with-cstring ,(car bindings)
         (with-cstrings ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

(defmacro def-function (name args &key module (returning :void))
  "Define a foreign function."
  (declare (ignore module))
  `(cffi:defcfun ,name ,(convert-uffi-type returning)
     ,@(loop for (name type) in args
             collect `(,name ,(convert-uffi-type type)))))

;;; Taken from UFFI's src/libraries.lisp

(defvar *loaded-libraries* nil
  "List of foreign libraries loaded. Used to prevent reloading a library")

(defun default-foreign-library-type ()
  "Returns string naming default library type for platform"
  #+(or win32 cygwin mswindows) "dll"
  #+(or macos macosx darwin ccl-5.0) "dylib"
  #-(or win32 cygwin mswindows macos macosx darwin ccl-5.0) "so")

(defun foreign-library-types ()
  "Returns list of string naming possible library types for platform,
sorted by preference"
  #+(or win32 cygwin mswindows) '("dll" "lib" "so")
  #+(or macos macosx darwin ccl-5.0) '("dylib" "bundle")
  #-(or win32 cygwin mswindows macos macosx darwin ccl-5.0) '("so" "a" "o"))

(defun find-foreign-library (names directories &key types drive-letters)
  "Looks for a foreign library. directories can be a single
string or a list of strings of candidate directories. Use default
library type if type is not specified."
  (unless types
    (setq types (foreign-library-types)))
  (unless (listp types)
    (setq types (list types)))
  (unless (listp names)
    (setq names (list names)))
  (unless (listp directories)
    (setq directories (list directories)))
  #+(or win32 mswindows)
  (unless (listp drive-letters)
    (setq drive-letters (list drive-letters)))
  #-(or win32 mswindows)
  (setq drive-letters '(nil))
  (dolist (drive-letter drive-letters)
    (dolist (name names)
      (dolist (dir directories)
   (dolist (type types)
     (let ((path (make-pathname
             #+lispworks :host
             #+lispworks (when drive-letter drive-letter)
             #-lispworks :device
             #-lispworks (when drive-letter drive-letter)
             :name name
             :type type
             :directory
             (etypecase dir
          (pathname
           (pathname-directory dir))
          (list
           dir)
          (string
           (pathname-directory
            (parse-namestring dir)))))))
       (when (probe-file path)
         (return-from find-foreign-library path)))))))
  nil)

(defun convert-supporting-libraries-to-string (libs)
  (let (lib-load-list)
    (dolist (lib libs)
      (push (format nil "-l~A" lib) lib-load-list))
    (nreverse lib-load-list)))

(defun load-foreign-library (filename &key module supporting-libraries
                             force-load)
  #+(or allegro mcl sbcl clisp) (declare (ignore module supporting-libraries))
  #+(or cmucl scl sbcl) (declare (ignore module))

  (when (and filename (or (null (pathname-directory filename))
                          (probe-file filename)))
    (if (pathnamep filename) ;; ensure filename is a string to check if
   (setq filename (namestring filename))) ; already loaded

    (if (and (not force-load)
        (find filename *loaded-libraries* :test #'string-equal))
        t ;; return T, but don't reload library
        (progn
          ;; FIXME: Hmm, what are these two for?
          #+cmucl
          (let ((type (pathname-type (parse-namestring filename))))
            (if (string-equal type "so")
                (sys::load-object-file filename)
                (alien:load-foreign filename
                                    :libraries
                                    (convert-supporting-libraries-to-string
                                     supporting-libraries))))
          #+scl
          (let ((type (pathname-type (parse-namestring filename))))
            (if (string-equal type "so")
                (sys::load-dynamic-object filename)
                (alien:load-foreign filename
                                    :libraries
                                    (convert-supporting-libraries-to-string
                                     supporting-libraries))))

          #-(or cmucl scl)
          (cffi:load-foreign-library filename)
          (push filename *loaded-libraries*)
          t))))

;; Taken from UFFI's src/os.lisp
(defun getenv (var)
  "Return the value of the environment variable."
  #+allegro (sys::getenv (string var))
  #+clisp (sys::getenv (string var))
  #+(or cmucl scl) (cdr (assoc (string var) ext:*environment-list* :test #'equalp
                             :key #'string))
  #+(or ecl gcl) (si:getenv (string var))
  #+lispworks (lw:environment-variable (string var))
  #+lucid (lcl:environment-variable (string var))
  #+(or mcl ccl) (ccl::getenv var)
  #+sbcl (sb-ext:posix-getenv var)
  #-(or allegro clisp cmucl ecl scl gcl lispworks lucid mcl ccl sbcl)
  (error 'not-implemented :proc (list 'getenv var)))

;; Taken from UFFI's src/os.lisp
;; modified from function ASDF -- Copyright Dan Barlow and Contributors
(defun run-shell-command (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *trace-output*.  Returns the shell's exit code."
  (let ((command (apply #'format nil control-string args))
        (output *trace-output*))
    #+sbcl
    (sb-impl::process-exit-code
     (sb-ext:run-program
      "/bin/sh"
      (list "-c" command)
      :input nil :output output))

    #+(or cmucl scl)
    (ext:process-exit-code
     (ext:run-program
      "/bin/sh"
      (list "-c" command)
      :input nil :output output))

    #+allegro
    (excl:run-shell-command command :input nil :output output)

    #+lispworks
    (system:call-system-showing-output
     command
     :shell-type "/bin/sh"
     :output-stream output)

    #+clisp             ;XXX not exactly *trace-output*, I know
    (ext:run-shell-command  command :output :terminal :wait t)

    #+openmcl
    (nth-value 1
           (ccl:external-process-status
        (ccl:run-program "/bin/sh" (list "-c" command)
                 :input nil :output output
                 :wait t)))

    #+ecl
    (nth-value 1
               (ext:run-program
                "/bin/sh" (list "-c" command)
                :input nil :output output :error nil :wait t))

    #-(or openmcl ecl clisp lispworks allegro scl cmucl sbcl)
    (error "RUN-SHELL-PROGRAM not implemented for this Lisp")
    ))

;;; Some undocumented UFFI operators...

(defmacro convert-from-foreign-string
    (obj &key length (locale :default)
     (encoding 'cffi:*default-foreign-encoding*)
     (null-terminated-p t))
  ;; in effect, (eq NULL-TERMINATED-P (null LENGTH)). Hopefully,
  ;; that's compatible with the intended semantics, which are
  ;; undocumented.  If that's not the case, we can implement
  ;; NULL-TERMINATED-P in CFFI:FOREIGN-STRING-TO-LISP.
  (declare (ignore locale null-terminated-p))
  (let ((ret (gensym)))
    `(let ((,ret (cffi:foreign-string-to-lisp ,obj
                                              :count ,length
                                              :encoding ,encoding)))
       (if (equal ,ret "")
           nil
           ,ret))))

;; What's the difference between this and convert-to-cstring?
(defmacro convert-to-foreign-string
    (obj &optional (encoding 'cffi:*default-foreign-encoding*))
  (let ((str (gensym)))
    `(let ((,str ,obj))
       (if (null ,str)
           (cffi:null-pointer)
           (cffi:foreign-string-alloc ,str :encoding ,encoding)))))

(defmacro allocate-foreign-string (size &key unsigned)
  (declare (ignore unsigned))
  `(cffi:foreign-alloc :char :count ,size))

;; Ditto.
(defmacro with-foreign-string ((foreign-string lisp-string) &body body)
  (let ((str (gensym)))
    `(let ((,str ,lisp-string))
       (if (null ,str)
           (let ((,foreign-string (cffi:null-pointer)))
             ,@body)
           (cffi:with-foreign-string (,foreign-string ,str)
             ,@body)))))

(defmacro with-foreign-strings (bindings &body body)
  `(with-foreign-string ,(car bindings)
    ,@(if (cdr bindings)
          `((with-foreign-strings ,(cdr bindings) ,@body))
          body)))

;; This function returns a form? Where is this used in user-code?
(defun foreign-string-length (foreign-string)
  (declare (ignore foreign-string))
  (error "FOREIGN-STRING-LENGTH not implemented."))

;; This should be optimized.
(defun convert-from-foreign-usb8 (s len)
  (let ((a (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len a)
      (setf (aref a i) (cffi:mem-ref s :unsigned-char i)))))

;;;; String Encodings

(defmacro string-to-octets (str &key encoding null-terminate)
  `(babel:concatenate-strings-to-octets
    (or ,encoding cffi:*default-foreign-encoding*)
    ,str
    (if ,null-terminate
        #.(string #\Nul)
        "")))

(defmacro octets-to-string (octets &key encoding)
  `(babel:octets-to-string ,octets
                           :encoding (or ,encoding
                                         cffi:*default-foreign-encoding*)))

(defun foreign-encoded-octet-count (str &key encoding)
  (babel:string-size-in-octets str
                               :encoding (or encoding
                                             cffi:*default-foreign-encoding*)))

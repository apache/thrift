;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-ecl.lisp --- ECL backend for CFFI.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
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

;;;# Administrivia

(defpackage #:cffi-sys
  (:use #:common-lisp #:alexandria)
  (:import-from #:si #:null-pointer-p)
  (:export
   #:*cffi-ecl-method*
   #:canonicalize-symbol-name-case
   #:foreign-pointer
   #:pointerp
   #:pointer-eq
   #:%foreign-alloc
   #:foreign-free
   #:with-foreign-pointer
   #:null-pointer
   #:null-pointer-p
   #:inc-pointer
   #:make-pointer
   #:pointer-address
   #:%mem-ref
   #:%mem-set
   #:%foreign-funcall
   #:%foreign-funcall-pointer
   #:%foreign-type-alignment
   #:%foreign-type-size
   #:%load-foreign-library
   #:%close-foreign-library
   #:native-namestring
   #:make-shareable-byte-vector
   #:with-pointer-to-vector-data
   #:%defcallback
   #:%callback
   #:%foreign-symbol-pointer))

(in-package #:cffi-sys)

;;;
;;; ECL allows many ways of calling a foreign function, and also many
;;; ways of finding the pointer associated to a function name. They
;;; depend on whether the FFI relies on libffi or on the C/C++ compiler,
;;; and whether they use the shared library loader to locate symbols
;;; or they are linked by the linker.
;;;
;;;  :DFFI
;;;
;;;  ECL uses libffi to call foreign functions. The only way to find out
;;;  foreign symbols is by loading shared libraries and using dlopen()
;;;  or similar.
;;;
;;;  :DLOPEN
;;;
;;;  ECL compiles FFI code as C/C++ statements. The names are resolved
;;;  at run time by the shared library loader every time the function
;;;  is called
;;;
;;;  :C/C++
;;;
;;;  ECL compiles FFI code as C/C++ statements, but the name resolution
;;;  happens at link time. In this case you have to tell the ECL
;;;  compiler which are the right ld-flags (c:*ld-flags*) to link in
;;;  the library.
;;;
(defvar *cffi-ecl-method*
  #+dffi :dffi
  #+(and dlopen (not dffi)) :dlopen
  #-(or dffi dlopen) :c/c++
  "The type of code that CFFI generates for ECL: :DFFI when using the
dynamical foreign function interface; :DLOPEN when using C code and
dynamical references to symbols; :C/C++ for C/C++ code with static
references to symbols.")

;;;# Mis-features

#-long-long
(pushnew 'no-long-long *features*)
(pushnew 'flat-namespace *features*)

;;;# Symbol Case

(defun canonicalize-symbol-name-case (name)
  (declare (string name))
  (string-upcase name))

;;;# Allocation

(defun %foreign-alloc (size)
  "Allocate SIZE bytes of foreign-addressable memory."
  (si:allocate-foreign-data :void size))

(defun foreign-free (ptr)
  "Free a pointer PTR allocated by FOREIGN-ALLOC."
  (si:free-foreign-data ptr))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  The
pointer in VAR is invalid beyond the dynamic extent of BODY, and
may be stack-allocated if supported by the implementation.  If
SIZE-VAR is supplied, it will be bound to SIZE during BODY."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  `(let* ((,size-var ,size)
          (,var (%foreign-alloc ,size-var)))
     (unwind-protect
          (progn ,@body)
       (foreign-free ,var))))

;;;# Misc. Pointer Operations

(deftype foreign-pointer ()
  'si:foreign-data)

(defun null-pointer ()
  "Construct and return a null pointer."
  (si:allocate-foreign-data :void 0))

(defun inc-pointer (ptr offset)
  "Return a pointer OFFSET bytes past PTR."
  (ffi:make-pointer (+ (ffi:pointer-address ptr) offset) :void))

(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (typep ptr 'si:foreign-data))

(defun pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (= (ffi:pointer-address ptr1) (ffi:pointer-address ptr2)))

(defun make-pointer (address)
  "Return a pointer pointing to ADDRESS."
  (ffi:make-pointer address :void))

(defun pointer-address (ptr)
  "Return the address pointed to by PTR."
  (ffi:pointer-address ptr))

;;;# Shareable Vectors
;;;
;;; This interface is very experimental.  WITH-POINTER-TO-VECTOR-DATA
;;; should be defined to perform a copy-in/copy-out if the Lisp
;;; implementation can't do this.

(defun make-shareable-byte-vector (size)
  "Create a Lisp vector of SIZE bytes that can passed to
WITH-POINTER-TO-VECTOR-DATA."
  (make-array size :element-type '(unsigned-byte 8)))

(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
  `(let ((,ptr-var (si:make-foreign-data-from-array ,vector)))
     ,@body))

;;;# Type Operations

(defconstant +translation-table+
  '((:char            :byte            "char")
    (:unsigned-char   :unsigned-byte   "unsigned char")
    (:short           :short           "short")
    (:unsigned-short  :unsigned-short  "unsigned short")
    (:int             :int             "int")
    (:unsigned-int    :unsigned-int    "unsigned int")
    (:long            :long            "long")
    (:unsigned-long   :unsigned-long   "unsigned long")
    #+long-long
    (:long-long       :long-long       "long long")
    #+long-long
    (:unsigned-long-long :unsigned-long-long "unsigned long long")
    (:float           :float           "float")
    (:double          :double          "double")
    (:pointer         :pointer-void    "void*")
    (:void            :void            "void")))

(defun cffi-type->ecl-type (type-keyword)
  "Convert a CFFI type keyword to an ECL type keyword."
  (or (second (find type-keyword +translation-table+ :key #'first))
      (error "~S is not a valid CFFI type" type-keyword)))

(defun ecl-type->c-type (type-keyword)
  "Convert a CFFI type keyword to an valid C type keyword."
  (or (third (find type-keyword +translation-table+ :key #'second))
      (error "~S is not a valid CFFI type" type-keyword)))

(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (nth-value 0 (ffi:size-of-foreign-type
                (cffi-type->ecl-type type-keyword))))

(defun %foreign-type-alignment (type-keyword)
  "Return the alignment in bytes of a foreign type."
  (nth-value 1 (ffi:size-of-foreign-type
                (cffi-type->ecl-type type-keyword))))

;;;# Dereferencing

(defun %mem-ref (ptr type &optional (offset 0))
  "Dereference an object of TYPE at OFFSET bytes from PTR."
  (let* ((type (cffi-type->ecl-type type))
         (type-size (ffi:size-of-foreign-type type)))
    (si:foreign-data-ref-elt
     (si:foreign-data-recast ptr (+ offset type-size) :void) offset type)))

(defun %mem-set (value ptr type &optional (offset 0))
  "Set an object of TYPE at OFFSET bytes from PTR."
  (let* ((type (cffi-type->ecl-type type))
         (type-size (ffi:size-of-foreign-type type)))
    (si:foreign-data-set-elt
     (si:foreign-data-recast ptr (+ offset type-size) :void)
     offset type value)))

;;; Inline versions that use C expressions instead of function calls.

(defparameter +mem-ref-strings+
  (loop for (cffi-type ecl-type c-string) in +translation-table+
        for string = (format nil "*((~A *)(((char*)#0)+#1))" c-string)
        collect (list cffi-type ecl-type string)))

(defparameter +mem-set-strings+
  (loop for (cffi-type ecl-type c-string) in +translation-table+
        for string = (format nil "*((~A *)(((char*)#0)+#1))=#2" c-string)
        collect (list cffi-type ecl-type string)))

(define-compiler-macro %mem-ref (&whole whole ptr type &optional (offset 0))
  (if (and (constantp type) (constantp offset))
      (let ((record (assoc (eval type) +mem-ref-strings+)))
        `(ffi:c-inline (,ptr ,offset)
                       (:pointer-void :cl-index) ; argument types
                       ,(second record)          ; return type
                       ,(third record)  ; the precomputed expansion
                       :one-liner t))
      whole))

(define-compiler-macro %mem-set (&whole whole value ptr type &optional (offset 0))
  (if (and (constantp type) (constantp offset))
      (let ((record (assoc (eval type) +mem-set-strings+)))
        `(ffi:c-inline (,ptr ,offset ,value) ; arguments with type translated
                       (:pointer-void :cl-index ,(second record))
                       :void            ; does not return anything
                       ,(third record)  ; precomputed expansion
                       :one-liner t))
      whole))

;;;# Calling Foreign Functions

(defconstant +ecl-inline-codes+ "#0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#a,#b,#c,#d,#e,#f,#g,#h,#i,#j,#k,#l,#m,#n,#o,#p,#q,#r,#s,#t,#u,#v,#w,#x,#y,#z")

(defun c-inline-function-pointer-call (pointer types values return-type)
  (cond ((not (stringp pointer))
         `(ffi:c-inline
           ,(list* pointer values)
           ,(list* :pointer-void types) ,return-type
           ,(with-output-to-string (s)
              (let ((types (mapcar #'ecl-type->c-type types)))
                ;; On AMD64, the following code only works with the extra
                ;; argument ",...". If this is not present, functions
                ;; like sprintf do not work
                (format s "((~A (*)(~@[~{~A,~}...~]))(#0))(~A)"
                        (ecl-type->c-type return-type) types
                        (subseq +ecl-inline-codes+ 3
                                (max 3 (+ 2 (* (length values) 3)))))))
           :one-liner t :side-effects t))
        ((eq *cffi-ecl-method* :c/c++)
         `(ffi:c-inline ,values ,types ,return-type
           ,(with-output-to-string (s)
              (let ((types (mapcar #'ecl-type->c-type types)))
                ;; On AMD64, the following code only works with the extra
                ;; argument ",...". If this is not present, functions
                ;; like sprintf do not work
                (format s "{ extern ~A ~A(~@[~{~A~^, ~}~]); ~A~A(~A); }"
                        (ecl-type->c-type return-type) pointer types
                        (if (eq return-type :void) "" "@(return) = ")
                        pointer
                        (subseq +ecl-inline-codes+ 0
                                (max 0 (1- (* (length values) 3)))))))
           :one-liner nil :side-effects t))
        (t
         (c-inline-function-pointer-call
          `(%foreign-symbol-pointer ,pointer nil)
          types values return-type))))

(defun dffi-function-pointer-call (pointer types values return-type)
  (when (stringp pointer)
    (setf pointer `(%foreign-symbol-pointer ,pointer nil)))
  #-dffi
  `(error "In interpreted code, attempted to call a foreign function~% ~A~%~
             but ECL was built without support for that." ,pointer)
  #+dffi
  `(si::call-cfun ,pointer ,return-type (list ,@types) (list ,@values)))

#.(cl:when (>= ext:+ecl-version-number+ 100402)
    (cl:pushnew :ecl-with-backend cl:*features*)
    cl:nil)

(defun produce-function-pointer-call (pointer types values return-type)
  #-ecl-with-backend
  (progn
    (if (eq *cffi-ecl-method* :dffi)
        (dffi-function-pointer-call pointer types values return-type)
        (c-inline-function-pointer-call pointer types values return-type)))
  #+ecl-with-backend
  `(ext:with-backend
     :bytecodes
     ,(dffi-function-pointer-call pointer types values return-type)
     :c/c++
     (if (eq *cffi-ecl-method* :dffi)
         ,(dffi-function-pointer-call pointer types values return-type)
         ,(c-inline-function-pointer-call pointer types values return-type))))

(defun foreign-funcall-parse-args (args)
  "Return three values, lists of arg types, values, and result type."
  (let ((return-type :void))
    (loop for (type arg) on args by #'cddr
          if arg collect (cffi-type->ecl-type type) into types
          and collect arg into values
          else do (setf return-type (cffi-type->ecl-type type))
          finally (return (values types values return-type)))))

(defmacro %foreign-funcall (name args &key library convention)
  "Call a foreign function."
  (declare (ignore library convention))
  (multiple-value-bind (types values return-type)
      (foreign-funcall-parse-args args)
    (produce-function-pointer-call name types values return-type)))

(defmacro %foreign-funcall-pointer (ptr args &key convention)
  "Funcall a pointer to a foreign function."
  (declare (ignore convention))
  (multiple-value-bind (types values return-type)
      (foreign-funcall-parse-args args)
    (produce-function-pointer-call ptr types values return-type)))

;;;# Foreign Libraries

(defun %load-foreign-library (name path)
  "Load a foreign library."
  (declare (ignore name))
  #-dffi (error "LOAD-FOREIGN-LIBRARY requires ECL's DFFI support. Use ~
                 FFI:LOAD-FOREIGN-LIBRARY with a constant argument instead.")
  #+dffi
  (handler-case (si:load-foreign-module path)
    (file-error ()
      (error "file error while trying to load `~A'" path))))

(defun %close-foreign-library (handle)
  "Close a foreign library."
  (handler-case (si::unload-foreign-module handle)
    (undefined-function ()
      (restart-case (error "Detected ECL prior to version 15.2.21. ~
                            Function CFFI:CLOSE-FOREIGN-LIBRARY isn't implemented yet.")
        (ignore () :report "Continue anyway (foreign library will remain opened).")))))

(defun native-namestring (pathname)
  (namestring pathname))

;;;# Callbacks

;;; Create a package to contain the symbols for callback functions.
;;; We want to redefine callbacks with the same symbol so the internal
;;; data structures are reused.
(defpackage #:cffi-callbacks
  (:use))

(defvar *callbacks* (make-hash-table))

;;; Intern a symbol in the CFFI-CALLBACKS package used to name the
;;; internal callback for NAME.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun intern-callback (name)
    (intern (format nil "~A::~A"
                    (if-let (package (symbol-package name))
                      (package-name package)
                      "#")
                    (symbol-name name))
            '#:cffi-callbacks)))

(defmacro %defcallback (name rettype arg-names arg-types body
                        &key convention)
  (declare (ignore convention))
  (let ((cb-name (intern-callback name))
        (cb-type #.(if (> ext:+ecl-version-number+ 160102)
                       :default :cdecl)))
    `(progn
       (ffi:defcallback (,cb-name ,cb-type)
           ,(cffi-type->ecl-type rettype)
           ,(mapcar #'list arg-names
                    (mapcar #'cffi-type->ecl-type arg-types))
         ,body)
       (setf (gethash ',name *callbacks*) ',cb-name))))

(defun %callback (name)
  (multiple-value-bind (symbol winp)
      (gethash name *callbacks*)
    (unless winp
      (error "Undefined callback: ~S" name))
    (ffi:callback symbol)))

;;;# Foreign Globals

(defun %foreign-symbol-pointer (name library)
  "Returns a pointer to a foreign symbol NAME."
  (declare (ignore library))
  (handler-case
      (si:find-foreign-symbol (coerce name 'base-string)
                              :default :pointer-void 0)
    (error (c) nil)))

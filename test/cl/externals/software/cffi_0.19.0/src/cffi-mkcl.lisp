;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-mkcl.lisp --- MKCL backend for CFFI.
;;;
;;; Copyright (C) 2010-2012, Jean-Claude Beaudoin
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
  (:export
   #:canonicalize-symbol-name-case
   #:foreign-pointer
   #:pointerp
   #:pointer-eq
   #:null-pointer
   #:null-pointer-p
   #:inc-pointer
   #:make-pointer
   #:pointer-address
   #:%foreign-alloc
   #:foreign-free
   #:with-foreign-pointer
   #:%foreign-funcall
   #:%foreign-funcall-pointer
   #:%foreign-type-alignment
   #:%foreign-type-size
   #:%load-foreign-library
   #:%close-foreign-library
   #:native-namestring
   #:%mem-ref
   #:%mem-set
   #:make-shareable-byte-vector
   #:with-pointer-to-vector-data
   #:%foreign-symbol-pointer
   #:%defcallback
   #:%callback))

(in-package #:cffi-sys)

;;;# Mis-features

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
  (si:free-foreign-data ptr)
  nil)

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
  'si:foreign)

(defun null-pointer ()
  "Construct and return a null pointer."
  (si:make-foreign-null-pointer))

(defun null-pointer-p (ptr)
  "Return true if PTR is a null pointer."
  (si:null-pointer-p ptr))

(defun inc-pointer (ptr offset)
  "Return a pointer OFFSET bytes past PTR."
  (ffi:make-pointer (+ (ffi:pointer-address ptr) offset) :void))

(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  ;;(typep ptr 'si:foreign)
  (si:foreignp ptr))

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

;;; MKCL, built with the Boehm GC never moves allocated data, so this
;;; isn't nearly as hard to do.
(defun %vector-address (vector)
  "Return the address of VECTOR's data."
  (check-type vector (vector (unsigned-byte 8)))
  #-mingw64
  (ffi:c-inline (vector) (object) 
                :unsigned-long
                "(uintptr_t) #0->vector.self.b8"
                :side-effects nil
                :one-liner t)
  #+mingw64
  (ffi:c-inline (vector) (object) 
                :unsigned-long-long
                "(uintptr_t) #0->vector.self.b8"
                :side-effects nil
                :one-liner t))

(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
  `(let ((,ptr-var (make-pointer (%vector-address ,vector))))
     ,@body))

;;;# Dereferencing

(defun %mem-ref (ptr type &optional (offset 0))
  "Dereference an object of TYPE at OFFSET bytes from PTR."
  (let* ((type (cffi-type->mkcl-type type))
         (type-size (ffi:size-of-foreign-type type)))
    (si:foreign-ref-elt
     (si:foreign-recast ptr (+ offset type-size) :void) offset type)))

(defun %mem-set (value ptr type &optional (offset 0))
  "Set an object of TYPE at OFFSET bytes from PTR."
  (let* ((type (cffi-type->mkcl-type type))
         (type-size (ffi:size-of-foreign-type type)))
    (si:foreign-set-elt
     (si:foreign-recast ptr (+ offset type-size) :void)
     offset type value)))

;;;# Type Operations

(defconstant +translation-table+
  '((:char               :byte               "char")
    (:unsigned-char      :unsigned-byte      "unsigned char")
    (:short              :short              "short")
    (:unsigned-short     :unsigned-short     "unsigned short")
    (:int                :int                "int")
    (:unsigned-int       :unsigned-int       "unsigned int")
    (:long               :long               "long")
    (:unsigned-long      :unsigned-long      "unsigned long")
    (:long-long          :long-long          "long long")
    (:unsigned-long-long :unsigned-long-long "unsigned long long")
    (:float              :float              "float")
    (:double             :double             "double")
    (:pointer            :pointer-void       "void*")
    (:void               :void               "void")))

(defun cffi-type->mkcl-type (type-keyword)
  "Convert a CFFI type keyword to an MKCL type keyword."
  (or (second (find type-keyword +translation-table+ :key #'first))
      (error "~S is not a valid CFFI type" type-keyword)))

(defun mkcl-type->c-type (type-keyword)
  "Convert a CFFI type keyword to an valid C type keyword."
  (or (third (find type-keyword +translation-table+ :key #'second))
      (error "~S is not a valid CFFI type" type-keyword)))

(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (nth-value 0 (ffi:size-of-foreign-type
                (cffi-type->mkcl-type type-keyword))))

(defun %foreign-type-alignment (type-keyword)
  "Return the alignment in bytes of a foreign type."
  (nth-value 1 (ffi:size-of-foreign-type
                (cffi-type->mkcl-type type-keyword))))

;;;# Calling Foreign Functions

#|
(defconstant +mkcl-inline-codes+ "#0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#a,#b,#c,#d,#e,#f,#g,#h,#i,#j,#k,#l,#m,#n,#o,#p,#q,#r,#s,#t,#u,#v,#w,#x,#y,#z")
|#

(defun produce-function-pointer-call (pointer types values return-type)
#|
  (if (stringp pointer)
      (produce-function-pointer-call
       `(%foreign-symbol-pointer ,pointer nil) types values return-type)
      `(ffi:c-inline
        ,(list* pointer values)
        ,(list* :pointer-void types) ,return-type
        ,(with-output-to-string (s)
           (let ((types (mapcar #'mkcl-type->c-type types)))
             ;; On AMD64, the following code only works with the extra
             ;; argument ",...". If this is not present, functions
             ;; like sprintf do not work
             (format s "((~A (*)(~@[~{~A,~}...~]))(#0))(~A)"
                     (mkcl-type->c-type return-type) types
                     (subseq +mkcl-inline-codes+ 3
                             (max 3 (+ 2 (* (length values) 3)))))))
        :one-liner t :side-effects t))
|#
  ;; The version here below is definitely not as efficient as the one above
  ;; but it has the great vertue of working in all cases, (contrary to the
  ;; silent and unsafe limitations of the one above). JCB
  ;; I should re-optimize this one day, when I get time... JCB
  (progn
    (when (stringp pointer)
      (setf pointer `(%foreign-symbol-pointer ,pointer nil)))
    `(si:call-cfun ,pointer ,return-type (list ,@types) (list ,@values))))


(defun foreign-funcall-parse-args (args)
  "Return three values, lists of arg types, values, and result type."
  (let ((return-type :void))
    (loop for (type arg) on args by #'cddr
          if arg collect (cffi-type->mkcl-type type) into types
          and collect arg into values
          else do (setf return-type (cffi-type->mkcl-type type))
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
  (handler-case (si:load-foreign-module path)
    (file-error ()
      (error "file error while trying to load `~A'" path))))

(defun %close-foreign-library (handle)
  ;;(declare (ignore handle))
  ;;(error "%CLOSE-FOREIGN-LIBRARY unimplemented.")
  (si:unload-foreign-module handle))

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
  (let ((cb-name (intern-callback name)))
    `(progn
       (ffi:defcallback (,cb-name :cdecl)
                        ,(cffi-type->mkcl-type rettype)
                        ,(mapcar #'list arg-names
                                 (mapcar #'cffi-type->mkcl-type arg-types))
                        ;;(block ,cb-name ,@body)
                        (block ,cb-name ,body))
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
  (values (ignore-errors (si:find-foreign-symbol name :default :pointer-void 0))))


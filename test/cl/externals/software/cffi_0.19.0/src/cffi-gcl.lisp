;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-gcl.lisp --- CFFI-SYS implementation for GNU Common Lisp.
;;;
;;; Copyright (C) 2005-2006, Luis Oliveira  <loliveira(@)common-lisp.net>
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

;;; GCL specific notes:
;;;
;;; On ELF systems, a library can be loaded with the help of this:
;;;   http://www.copyleft.de/lisp/gcl-elf-loader.html
;;;
;;; Another way is to link the library when creating a new image:
;;;   (compiler::link nil "new_image" "" "-lfoo")
;;;
;;; As GCL's FFI is not dynamic, CFFI declarations will only work
;;; after compiled and loaded.

;;; *** this port is broken ***
;;; gcl doesn't compile the rest of CFFI anyway..

;;;# Administrivia

(defpackage #:cffi-sys
  (:use #:common-lisp #:alexandria)
  (:export
   #:canonicalize-symbol-name-case
   #:pointerp
   #:%foreign-alloc
   #:foreign-free
   #:with-foreign-ptr
   #:null-ptr
   #:null-ptr-p
   #:inc-ptr
   #:%mem-ref
   #:%mem-set
   #:%foreign-funcall
   #:%foreign-type-alignment
   #:%foreign-type-size
   #:%load-foreign-library
   ;#:make-shareable-byte-vector
   ;#:with-pointer-to-vector-data
   #:foreign-var-ptr
   #:make-callback))

(in-package #:cffi-sys)

;;;# Mis-*features*
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cffi/no-foreign-funcall *features*))

;;; Symbol case.

(defun canonicalize-symbol-name-case (name)
  (declare (string name))
  (string-upcase name))

;;;# Allocation
;;;
;;; Functions and macros for allocating foreign memory on the stack
;;; and on the heap.  The main CFFI package defines macros that wrap
;;; FOREIGN-ALLOC and FOREIGN-FREE in UNWIND-PROTECT for the common
;;; usage when the memory has dynamic extent.

(defentry %foreign-alloc (int) (int "malloc"))

;(defun foreign-alloc (size)
;  "Allocate SIZE bytes on the heap and return a pointer."
;  (%foreign-alloc size))

(defentry foreign-free (int) (void "free"))

;(defun foreign-free (ptr)
;  "Free a PTR allocated by FOREIGN-ALLOC."
;  (%free ptr))

(defmacro with-foreign-ptr ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  The
pointer in VAR is invalid beyond the dynamic extent of BODY, and
may be stack-allocated if supported by the implementation.  If
SIZE-VAR is supplied, it will be bound to SIZE during BODY."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  `(let* ((,size-var ,size)
          (,var (foreign-alloc ,size-var)))
     (unwind-protect
          (progn ,@body)
       (foreign-free ,var))))

;;;# Misc. Pointer Operations

(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (integerp ptr))

(defun null-ptr ()
  "Construct and return a null pointer."
  0)

(defun null-ptr-p (ptr)
  "Return true if PTR is a null pointer."
  (= ptr 0))

(defun inc-ptr (ptr offset)
  "Return a pointer OFFSET bytes past PTR."
  (+ ptr offset))

;;;# Shareable Vectors
;;;
;;; This interface is very experimental.  WITH-POINTER-TO-VECTOR-DATA
;;; should be defined to perform a copy-in/copy-out if the Lisp
;;; implementation can't do this.

;(defun make-shareable-byte-vector (size)
;  "Create a Lisp vector of SIZE bytes that can passed to
;WITH-POINTER-TO-VECTOR-DATA."
;  (make-array size :element-type '(unsigned-byte 8)))

;(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
;  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
;  `(ccl:with-pointer-to-ivector (,ptr-var ,vector)
;     ,@body))

;;;# Dereferencing

(defmacro define-mem-ref/set (type gcl-type &optional c-name)
  (unless c-name
    (setq c-name (substitute #\_ #\Space type)))
  (let ((ref-fn (concatenate 'string "ref_" c-name))
        (set-fn (concatenate 'string "set_" c-name)))
    `(progn
       ;; ref
       (defcfun ,(format nil "~A ~A(~A *ptr)" type ref-fn type)
           0 "return *ptr;")
       (defentry ,(intern (string-upcase (substitute #\- #\_ ref-fn)))
           (int) (,gcl-type ,ref-fn))
       ;; set
       (defcfun ,(format nil "void ~A(~A *ptr, ~A value)" set-fn type type)
           0 "*ptr = value;")
       (defentry ,(intern (string-upcase (substitute #\- #\_ set-fn)))
           (int ,gcl-type) (void ,set-fn)))))

(define-mem-ref/set "char" char)
(define-mem-ref/set "unsigned char" char)
(define-mem-ref/set "short" int)
(define-mem-ref/set "unsigned short" int)
(define-mem-ref/set "int" int)
(define-mem-ref/set "unsigned int" int)
(define-mem-ref/set "long" int)
(define-mem-ref/set "unsigned long" int)
(define-mem-ref/set "float" float)
(define-mem-ref/set "double" double)
(define-mem-ref/set "void *" int "ptr")

(defun %mem-ref (ptr type &optional (offset 0))
  "Dereference an object of TYPE at OFFSET bytes from PTR."
  (unless (zerop offset)
    (incf ptr offset))
  (ecase type
    (:char            (ref-char ptr))
    (:unsigned-char   (ref-unsigned-char ptr))
    (:short           (ref-short ptr))
    (:unsigned-short  (ref-unsigned-short ptr))
    (:int             (ref-int ptr))
    (:unsigned-int    (ref-unsigned-int ptr))
    (:long            (ref-long ptr))
    (:unsigned-long   (ref-unsigned-long ptr))
    (:float           (ref-float ptr))
    (:double          (ref-double ptr))
    (:pointer         (ref-ptr ptr))))

(defun %mem-set (value ptr type &optional (offset 0))
  (unless (zerop offset)
    (incf ptr offset))
  (ecase type
    (:char            (set-char ptr value))
    (:unsigned-char   (set-unsigned-char ptr value))
    (:short           (set-short ptr value))
    (:unsigned-short  (set-unsigned-short ptr value))
    (:int             (set-int ptr value))
    (:unsigned-int    (set-unsigned-int ptr value))
    (:long            (set-long ptr value))
    (:unsigned-long   (set-unsigned-long ptr value))
    (:float           (set-float ptr value))
    (:double          (set-double ptr value))
    (:pointer         (set-ptr ptr value)))
  value)

;;;# Calling Foreign Functions

;; TODO: figure out if these type conversions make any sense...
(defun convert-foreign-type (type-keyword)
  "Convert a CFFI type keyword to a GCL type."
  (ecase type-keyword
    (:char            'char)
    (:unsigned-char   'char)
    (:short           'int)
    (:unsigned-short  'int)
    (:int             'int)
    (:unsigned-int    'int)
    (:long            'int)
    (:unsigned-long   'int)
    (:float           'float)
    (:double          'double)
    (:pointer         'int)
    (:void            'void)))

(defparameter +cffi-types+
  '(:char :unsigned-char :short :unsigned-short :int :unsigned-int
    :long :unsigned-long :float :double :pointer))

(defcfun "int size_of(int type)" 0
  "switch (type) {
     case 0:  return sizeof(char);
     case 1:  return sizeof(unsigned char);
     case 2:  return sizeof(short);
     case 3:  return sizeof(unsigned short);
     case 4:  return sizeof(int);
     case 5:  return sizeof(unsigned int);
     case 6:  return sizeof(long);
     case 7:  return sizeof(unsigned long);
     case 8:  return sizeof(float);
     case 9:  return sizeof(double);
     case 10: return sizeof(void *);
     default: return -1;
   }")

(defentry size-of (int) (int "size_of"))

;; TODO: all this is doable inside the defcfun; figure that out..
(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (size-of (position type-keyword +cffi-types+)))

(defcfun "int align_of(int type)" 0
  "switch (type) {
     case 0:  return __alignof__(char);
     case 1:  return __alignof__(unsigned char);
     case 2:  return __alignof__(short);
     case 3:  return __alignof__(unsigned short);
     case 4:  return __alignof__(int);
     case 5:  return __alignof__(unsigned int);
     case 6:  return __alignof__(long);
     case 7:  return __alignof__(unsigned long);
     case 8:  return __alignof__(float);
     case 9:  return __alignof__(double);
     case 10: return __alignof__(void *);
     default: return -1;
   }")

(defentry align-of (int) (int "align_of"))

;; TODO: like %foreign-type-size
(defun %foreign-type-alignment (type-keyword)
  "Return the alignment in bytes of a foreign type."
  (align-of (position type-keyword +cffi-types+)))

#+ignore
(defun convert-external-name (name)
  "Add an underscore to NAME if necessary for the ABI."
  #+darwinppc-target (concatenate 'string "_" name)
  #-darwinppc-target name)

(defmacro %foreign-funcall (function-name &rest args)
  "Perform a foreign function all, document it more later."
  `(format t "~&;; Calling ~A with args ~S.~%" ,name ',args))

(defun defcfun-helper-forms (name rettype args types)
  "Return 2 values for DEFCFUN. A prelude form and a caller form."
  (let ((ff-name (intern (format nil "%foreign-function/TildeA:~A" name))))
    (values
     `(defentry ,ff-name ,(mapcar #'convert-foreign-type types)
        (,(convert-foreign-type rettype) ,name))
     `(,ff-name ,@args))))

;;;# Callbacks

;;; XXX unimplemented
(defmacro make-callback (name rettype arg-names arg-types body-form)
  0)

;;;# Loading Foreign Libraries

(defun %load-foreign-library (name)
  "_Won't_ load the foreign library NAME."
  (declare (ignore name)))

;;;# Foreign Globals

;;; XXX unimplemented
(defmacro foreign-var-ptr (name)
  "Return a pointer pointing to the foreign symbol NAME."
  0)

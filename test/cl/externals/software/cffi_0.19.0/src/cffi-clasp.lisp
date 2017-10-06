;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-clasp.lisp --- CFFI-SYS implementation for Clasp.
;;;
;;; Copyright (C) 2017 Frank Goenninger  <frank.goenninger@goenninger.net>
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

;;;# Mis-features

(pushnew 'flat-namespace cl:*features*)

;;;# Symbol Case

(defun canonicalize-symbol-name-case (name)
  (declare (string name))
  (string-upcase name))

;;;# Allocation

(defun %foreign-alloc (size)
  "Allocate SIZE bytes of foreign-addressable memory."
  (clasp-ffi:%foreign-alloc size))

(defun foreign-free (ptr)
  "Free a pointer PTR allocated by FOREIGN-ALLOC."
  (clasp-ffi:%foreign-free ptr))

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
  'clasp-ffi:foreign-data)

(defun null-pointer-p (ptr)
  "Test if PTR is a null pointer."
  (clasp-ffi:%null-pointer-p ptr))

(defun null-pointer ()
  "Construct and return a null pointer."
  (clasp-ffi:%make-nullpointer))

(defun make-pointer (address)
  "Return a pointer pointing to ADDRESS."
  (clasp-ffi:%make-pointer address))

(defun inc-pointer (ptr offset)
  "Return a pointer OFFSET bytes past PTR."
  (clasp-ffi:%inc-pointer ptr offset))

(defun pointer-address (ptr)
  "Return the address pointed to by PTR."
  (clasp-ffi:%foreign-data-address ptr))

(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (typep ptr 'clasp-ffi:foreign-data))

(defun pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (check-type ptr1 clasp-ffi:foreign-data)
  (check-type ptr2 clasp-ffi:foreign-data)
  (eql (pointer-address ptr1) (pointer-address ptr2)))


;;;# Shareable Vectors
;;;
;;; This interface is very experimental.  WITH-POINTER-TO-VECTOR-DATA
;;; should be defined to perform a copy-in/copy-out if the Lisp
;;; implementation can't do this.

(defun make-shareable-byte-vector (size)
  "Create a Lisp vector of SIZE bytes that can passed to
WITH-POINTER-TO-VECTOR-DATA."
  (make-array size :element-type '(unsigned-byte 8)))

;; frgo, 2016-07-02: TODO: Implemenent!
;; (defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
;;   "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
;;   `(let ((,ptr-var (si:make-foreign-data-from-array ,vector)))
;;      ,@body))

(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (clasp-ffi:%foreign-type-size type-keyword))

(defun %foreign-type-alignment (type-keyword)
  "Return the alignment in bytes of a foreign type."
  (clasp-ffi:%foreign-type-alignment type-keyword))

;;;# Dereferencing

(defun %mem-ref (ptr type &optional (offset 0))
  "Dereference an object of TYPE at OFFSET bytes from PTR."
  (clasp-ffi:%mem-ref ptr type offset))

(defun %mem-set (value ptr type &optional (offset 0))
  "Set an object of TYPE at OFFSET bytes from PTR."
  (clasp-ffi:%mem-set ptr type value offset))

(defmacro %foreign-funcall (name args &key library convention)
  "Call a foreign function."
  (declare (ignore library convention))
  `(clasp-ffi:%foreign-funcall ,name ,@args))

(defmacro %foreign-funcall-pointer (ptr args &key convention)
  "Funcall a pointer to a foreign function."
  (declare (ignore convention))
  `(clasp-ffi:%foreign-funcall-pointer ,ptr ,@args))

;;;# Foreign Libraries

(defun %load-foreign-library (name path)
  "Load a foreign library."
  (clasp-ffi:%load-foreign-library name path))

(defun %close-foreign-library (handle)
  "Close a foreign library."
  (clasp-ffi:%close-foreign-library handle))

(defun %foreign-symbol-pointer (name library)
  "Returns a pointer to a foreign symbol NAME."
  (clasp-ffi:%foreign-symbol-pointer name library))

(defun native-namestring (pathname)
  (namestring pathname))

;;;# Callbacks

(defmacro %defcallback (name rettype arg-names arg-types body
                        &key convention)
  `(clasp-ffi:%defcallback (,name ,@(when convention `(:convention ,convention)))
                           ,rettype ,arg-names ,arg-types ,body))

(defun %callback (name)
  (clasp-ffi:%get-callback name))

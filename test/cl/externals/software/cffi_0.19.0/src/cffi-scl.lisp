;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-scl.lisp --- CFFI-SYS implementation for the Scieneer Common Lisp.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2006-2007, Scieneer Pty Ltd.
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
  (:use #:common-lisp #:alien #:c-call)
  (:import-from #:alexandria #:once-only #:with-unique-names)
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
  (if (eq ext:*case-mode* :upper)
      (string-upcase name)
      (string-downcase name)))

;;;# Basic Pointer Operations

(deftype foreign-pointer ()
  'sys:system-area-pointer)

(declaim (inline pointerp))
(defun pointerp (ptr)
  "Return true if 'ptr is a foreign pointer."
  (sys:system-area-pointer-p ptr))

(declaim (inline pointer-eq))
(defun pointer-eq (ptr1 ptr2)
  "Return true if 'ptr1 and 'ptr2 point to the same address."
  (sys:sap= ptr1 ptr2))

(declaim (inline null-pointer))
(defun null-pointer ()
  "Construct and return a null pointer."
  (sys:int-sap 0))

(declaim (inline null-pointer-p))
(defun null-pointer-p (ptr)
  "Return true if 'ptr is a null pointer."
  (zerop (sys:sap-int ptr)))

(declaim (inline inc-pointer))
(defun inc-pointer (ptr offset)
  "Return a pointer pointing 'offset bytes past 'ptr."
  (sys:sap+ ptr offset))

(declaim (inline make-pointer))
(defun make-pointer (address)
  "Return a pointer pointing to 'address."
  (sys:int-sap address))

(declaim (inline pointer-address))
(defun pointer-address (ptr)
  "Return the address pointed to by 'ptr."
  (sys:sap-int ptr))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind 'var to 'size bytes of foreign memory during 'body.  The
  pointer in 'var is invalid beyond the dynamic extent of 'body, and
  may be stack-allocated if supported by the implementation.  If
  'size-var is supplied, it will be bound to 'size during 'body."
  (unless size-var
    (setf size-var (gensym (symbol-name '#:size))))
  ;; If the size is constant we can stack-allocate.
  (cond ((constantp size)
         (let ((alien-var (gensym (symbol-name '#:alien))))
           `(with-alien ((,alien-var (array (unsigned 8) ,(eval size))))
             (let ((,size-var ,size)
                   (,var (alien-sap ,alien-var)))
               (declare (ignorable ,size-var))
               ,@body))))
        (t
         `(let ((,size-var ,size))
            (alien:with-bytes (,var ,size-var)
              ,@body)))))

;;;# Allocation
;;;
;;; Functions and macros for allocating foreign memory on the stack and on the
;;; heap.  The main CFFI package defines macros that wrap 'foreign-alloc and
;;; 'foreign-free in 'unwind-protect for the common usage when the memory has
;;; dynamic extent.

(defun %foreign-alloc (size)
  "Allocate 'size bytes on the heap and return a pointer."
  (declare (type (unsigned-byte #-64bit 32 #+64bit 64) size))
  (alien-funcall (extern-alien "malloc"
                               (function system-area-pointer unsigned))
                 size))

(defun foreign-free (ptr)
  "Free a 'ptr allocated by 'foreign-alloc."
  (declare (type system-area-pointer ptr))
  (alien-funcall (extern-alien "free"
                               (function (values) system-area-pointer))
                 ptr))

;;;# Shareable Vectors

(defun make-shareable-byte-vector (size)
  "Create a Lisp vector of 'size bytes that can passed to
  'with-pointer-to-vector-data."
  (make-array size :element-type '(unsigned-byte 8)))

(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind 'ptr-var to a foreign pointer to the data in 'vector."
  (let ((vector-var (gensym (symbol-name '#:vector))))
    `(let ((,vector-var ,vector))
       (ext:with-pinned-object (,vector-var)
         (let ((,ptr-var (sys:vector-sap ,vector-var)))
           ,@body)))))

;;;# Dereferencing

;;; Define the %MEM-REF and %MEM-SET functions, as well as compiler
;;; macros that optimize the case where the type keyword is constant
;;; at compile-time.
(defmacro define-mem-accessors (&body pairs)
  `(progn
    (defun %mem-ref (ptr type &optional (offset 0))
      (ecase type
        ,@(loop for (keyword fn) in pairs
                collect `(,keyword (,fn ptr offset)))))
    (defun %mem-set (value ptr type &optional (offset 0))
      (ecase type
        ,@(loop for (keyword fn) in pairs
                collect `(,keyword (setf (,fn ptr offset) value)))))
    (define-compiler-macro %mem-ref
        (&whole form ptr type &optional (offset 0))
      (if (constantp type)
          (ecase (eval type)
            ,@(loop for (keyword fn) in pairs
                    collect `(,keyword `(,',fn ,ptr ,offset))))
          form))
    (define-compiler-macro %mem-set
        (&whole form value ptr type &optional (offset 0))
      (if (constantp type)
          (once-only (value)
            (ecase (eval type)
              ,@(loop for (keyword fn) in pairs
                      collect `(,keyword `(setf (,',fn ,ptr ,offset)
                                                ,value)))))
          form))))

(define-mem-accessors
  (:char sys:signed-sap-ref-8)
  (:unsigned-char sys:sap-ref-8)
  (:short sys:signed-sap-ref-16)
  (:unsigned-short sys:sap-ref-16)
  (:int sys:signed-sap-ref-32)
  (:unsigned-int sys:sap-ref-32)
  (:long #-64bit sys:signed-sap-ref-32 #+64bit sys:signed-sap-ref-64)
  (:unsigned-long #-64bit sys:sap-ref-32 #+64bit sys:sap-ref-64)
  (:long-long sys:signed-sap-ref-64)
  (:unsigned-long-long sys:sap-ref-64)
  (:float sys:sap-ref-single)
  (:double sys:sap-ref-double)
  #+long-float (:long-double sys:sap-ref-long)
  (:pointer sys:sap-ref-sap))

;;;# Calling Foreign Functions

(defun convert-foreign-type (type-keyword)
  "Convert a CFFI type keyword to an ALIEN type."
  (ecase type-keyword
    (:char               'char)
    (:unsigned-char      'unsigned-char)
    (:short              'short)
    (:unsigned-short     'unsigned-short)
    (:int                'int)
    (:unsigned-int       'unsigned-int)
    (:long               'long)
    (:unsigned-long      'unsigned-long)
    (:long-long          '(signed 64))
    (:unsigned-long-long '(unsigned 64))
    (:float              'single-float)
    (:double             'double-float)
    #+long-float
    (:long-double        'long-float)
    (:pointer            'system-area-pointer)
    (:void               'void)))

(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (values (truncate (alien-internals:alien-type-bits
                     (alien-internals:parse-alien-type
                      (convert-foreign-type type-keyword)))
                    8)))

(defun %foreign-type-alignment (type-keyword)
  "Return the alignment in bytes of a foreign type."
  (values (truncate (alien-internals:alien-type-alignment
                     (alien-internals:parse-alien-type
                      (convert-foreign-type type-keyword)))
                    8)))

(defun foreign-funcall-type-and-args (args)
  "Return an 'alien function type for 'args."
  (let ((return-type nil))
    (loop for (type arg) on args by #'cddr
          if arg collect (convert-foreign-type type) into types
          and collect arg into fargs
          else do (setf return-type (convert-foreign-type type))
          finally (return (values types fargs return-type)))))

(defmacro %%foreign-funcall (name types fargs rettype)
  "Internal guts of '%foreign-funcall."
  `(alien-funcall (extern-alien ,name (function ,rettype ,@types))
                  ,@fargs))

(defmacro %foreign-funcall (name args &key library convention)
  "Perform a foreign function call, document it more later."
  (declare (ignore library convention))
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    `(%%foreign-funcall ,name ,types ,fargs ,rettype)))

(defmacro %foreign-funcall-pointer (ptr args &key convention)
  "Funcall a pointer to a foreign function."
  (declare (ignore convention))
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    (with-unique-names (function)
      `(with-alien ((,function (* (function ,rettype ,@types)) ,ptr))
         (alien-funcall ,function ,@fargs)))))

;;; Callbacks

(defmacro %defcallback (name rettype arg-names arg-types body
                        &key convention)
  (declare (ignore convention))
   `(alien:defcallback ,name
       (,(convert-foreign-type rettype)
         ,@(mapcar (lambda (sym type)
                     (list sym (convert-foreign-type type)))
                   arg-names arg-types))
     ,body))

(declaim (inline %callback))
(defun %callback (name)
  (alien:callback-sap name))

;;;# Loading and Closing Foreign Libraries

(defun %load-foreign-library (name path)
  "Load the foreign library 'name."
  (declare (ignore name))
  (ext:load-dynamic-object path))

(defun %close-foreign-library (name)
  "Closes the foreign library 'name."
  (ext:close-dynamic-object name))

(defun native-namestring (pathname)
  (ext:unix-namestring pathname nil))

;;;# Foreign Globals

(defun %foreign-symbol-pointer (name library)
  "Returns a pointer to a foreign symbol 'name."
  (declare (ignore library))
  (let ((sap (sys:foreign-symbol-address name)))
    (if (zerop (sys:sap-int sap)) nil sap)))

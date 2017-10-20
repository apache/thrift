;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-clisp.lisp --- CFFI-SYS implementation for CLISP.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2005-2006, Joerg Hoehle  <hoehle@users.sourceforge.net>
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :ffi)
    (error "CFFI requires CLISP compiled with dynamic FFI support.")))

;;;# Symbol Case

(defun canonicalize-symbol-name-case (name)
  (declare (string name))
  (string-upcase name))

;;;# Built-In Foreign Types

(defun convert-foreign-type (type)
  "Convert a CFFI built-in type keyword to a CLisp FFI type."
  (ecase type
    (:char 'ffi:char)
    (:unsigned-char 'ffi:uchar)
    (:short 'ffi:short)
    (:unsigned-short 'ffi:ushort)
    (:int 'ffi:int)
    (:unsigned-int 'ffi:uint)
    (:long 'ffi:long)
    (:unsigned-long 'ffi:ulong)
    (:long-long 'ffi:sint64)
    (:unsigned-long-long 'ffi:uint64)
    (:float 'ffi:single-float)
    (:double 'ffi:double-float)
    ;; Clisp's FFI:C-POINTER converts NULL to NIL. For now
    ;; we have a workaround in the pointer operations...
    (:pointer 'ffi:c-pointer)
    (:void nil)))

(defun %foreign-type-size (type)
  "Return the size in bytes of objects having foreign type TYPE."
  (nth-value 0 (ffi:sizeof (convert-foreign-type type))))

;; Remind me to buy a beer for whoever made getting the alignment
;; of foreign types part of the public interface in CLisp. :-)
(defun %foreign-type-alignment (type)
  "Return the structure alignment in bytes of foreign TYPE."
  #+(and darwin ppc)
  (case type
    ((:double :long-long :unsigned-long-long)
     (return-from %foreign-type-alignment 8)))
  ;; Override not necessary for the remaining types...
  (nth-value 1 (ffi:sizeof (convert-foreign-type type))))

;;;# Basic Pointer Operations

(deftype foreign-pointer ()
  'ffi:foreign-address)

(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (typep ptr 'ffi:foreign-address))

(defun pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (eql (ffi:foreign-address-unsigned ptr1)
       (ffi:foreign-address-unsigned ptr2)))

(defun null-pointer ()
  "Return a null foreign pointer."
  (ffi:unsigned-foreign-address 0))

(defun null-pointer-p (ptr)
  "Return true if PTR is a null foreign pointer."
  (zerop (ffi:foreign-address-unsigned ptr)))

(defun inc-pointer (ptr offset)
  "Return a pointer pointing OFFSET bytes past PTR."
  (ffi:unsigned-foreign-address
   (+ offset (ffi:foreign-address-unsigned ptr))))

(defun make-pointer (address)
  "Return a pointer pointing to ADDRESS."
  (ffi:unsigned-foreign-address address))

(defun pointer-address (ptr)
  "Return the address pointed to by PTR."
  (ffi:foreign-address-unsigned ptr))

;;;# Foreign Memory Allocation

(defun %foreign-alloc (size)
  "Allocate SIZE bytes of foreign-addressable memory and return a
pointer to the allocated block.  An implementation-specific error
is signalled if the memory cannot be allocated."
  (ffi:foreign-address
   (ffi:allocate-shallow 'ffi:uint8 :count (if (zerop size) 1 size))))

(defun foreign-free (ptr)
  "Free a pointer PTR allocated by FOREIGN-ALLOC.  The results
are undefined if PTR is used after being freed."
  (ffi:foreign-free ptr))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind VAR to a pointer to SIZE bytes of foreign-addressable
memory during BODY.  Both PTR and the memory block pointed to
have dynamic extent and may be stack allocated if supported by
the implementation.  If SIZE-VAR is supplied, it will be bound to
SIZE during BODY."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  (let ((obj-var (gensym)))
    `(let ((,size-var ,size))
       (ffi:with-foreign-object
           (,obj-var `(ffi:c-array ffi:uint8 ,,size-var))
         (let ((,var (ffi:foreign-address ,obj-var)))
           ,@body)))))

;;;# Memory Access

;;; %MEM-REF and its compiler macro work around CLISP's FFI:C-POINTER
;;; type and convert NILs back to null pointers.
(defun %mem-ref (ptr type &optional (offset 0))
  "Dereference a pointer OFFSET bytes from PTR to an object of
built-in foreign TYPE.  Returns the object as a foreign pointer
or Lisp number."
  (let ((value (ffi:memory-as ptr (convert-foreign-type type) offset)))
    (if (eq type :pointer)
        (or value (null-pointer))
        value)))

(define-compiler-macro %mem-ref (&whole form ptr type &optional (offset 0))
  "Compiler macro to open-code when TYPE is constant."
  (if (constantp type)
      (let* ((ftype (convert-foreign-type (eval type)))
             (form `(ffi:memory-as ,ptr ',ftype ,offset)))
        (if (eq type :pointer)
            `(or ,form (null-pointer))
            form))
      form))

(defun %mem-set (value ptr type &optional (offset 0))
  "Set a pointer OFFSET bytes from PTR to an object of built-in
foreign TYPE to VALUE."
  (setf (ffi:memory-as ptr (convert-foreign-type type) offset) value))

(define-compiler-macro %mem-set
    (&whole form value ptr type &optional (offset 0))
  (if (constantp type)
      ;; (setf (ffi:memory-as) value) is exported, but not so nice
      ;; w.r.t. the left to right evaluation rule
      `(ffi::write-memory-as
        ,value ,ptr ',(convert-foreign-type (eval type)) ,offset)
      form))

;;;# Shareable Vectors
;;;
;;; This interface is very experimental.  WITH-POINTER-TO-VECTOR-DATA
;;; should be defined to perform a copy-in/copy-out if the Lisp
;;; implementation can't do this.

(declaim (inline make-shareable-byte-vector))
(defun make-shareable-byte-vector (size)
  "Create a Lisp vector of SIZE bytes can passed to
WITH-POINTER-TO-VECTOR-DATA."
  (make-array size :element-type '(unsigned-byte 8)))

(deftype shareable-byte-vector ()
  `(vector (unsigned-byte 8)))

(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
  (with-unique-names (vector-var size-var)
    `(let ((,vector-var ,vector))
       (check-type ,vector-var shareable-byte-vector)
       (with-foreign-pointer (,ptr-var (length ,vector-var) ,size-var)
         ;; copy-in
         (loop for i below ,size-var do
               (%mem-set (aref ,vector-var i) ,ptr-var :unsigned-char i))
         (unwind-protect (progn ,@body)
           ;; copy-out
           (loop for i below ,size-var do
                 (setf (aref ,vector-var i)
                       (%mem-ref ,ptr-var :unsigned-char i))))))))

;;;# Foreign Function Calling

(defun parse-foreign-funcall-args (args)
  "Return three values, a list of CLISP FFI types, a list of
values to pass to the function, and the CLISP FFI return type."
  (let ((return-type nil))
    (loop for (type arg) on args by #'cddr
          if arg collect (list (gensym) (convert-foreign-type type)) into types
          and collect arg into fargs
          else do (setf return-type (convert-foreign-type type))
          finally (return (values types fargs return-type)))))

(defun convert-calling-convention (convention)
  (ecase convention
    (:stdcall :stdc-stdcall)
    (:cdecl :stdc)))

(defun c-function-type (arg-types rettype convention)
  "Generate the apropriate CLISP foreign type specification. Also
takes care of converting the calling convention names."
  `(ffi:c-function (:arguments ,@arg-types)
                   (:return-type ,rettype)
                   (:language ,(convert-calling-convention convention))))

;;; Quick hack around the fact that the CFFI package is not yet
;;; defined when this file is loaded.  I suppose we could arrange for
;;; the CFFI package to be defined a bit earlier, though.
(defun library-handle-form (name)
  (flet ((find-cffi-symbol (symbol)
           (find-symbol (symbol-name symbol) '#:cffi)))
    `(,(find-cffi-symbol '#:foreign-library-handle)
       (,(find-cffi-symbol '#:get-foreign-library) ',name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; version 2.40 (CVS 2006-09-03, to be more precise) added a
  ;; PROPERTIES argument to FFI::FOREIGN-LIBRARY-FUNCTION.
  (defun post-2.40-ffi-interface-p ()
    (let ((f-l-f (find-symbol (string '#:foreign-library-function) '#:ffi)))
      (if (and f-l-f (= (length (ext:arglist f-l-f)) 5))
          '(:and)
          '(:or))))
  ;; FFI::FOREIGN-LIBRARY-FUNCTION and FFI::FOREIGN-LIBRARY-VARIABLE
  ;; were deprecated in 2.41 and removed in 2.45.
  (defun post-2.45-ffi-interface-p ()
    (if (find-symbol (string '#:foreign-library-function) '#:ffi)
        '(:or)
        '(:and))))

#+#.(cffi-sys::post-2.45-ffi-interface-p)
(defun %foreign-funcall-aux (name type library)
  `(ffi::find-foreign-function ,name ,type nil ,library nil nil))

#-#.(cffi-sys::post-2.45-ffi-interface-p)
(defun %foreign-funcall-aux (name type library)
  `(ffi::foreign-library-function
    ,name ,library nil
    #+#.(cffi-sys::post-2.40-ffi-interface-p)
    nil
    ,type))

(defmacro %foreign-funcall (name args &key library convention)
  "Invoke a foreign function called NAME, taking pairs of
foreign-type/value pairs from ARGS.  If a single element is left
over at the end of ARGS, it specifies the foreign return type of
the function call."
  (multiple-value-bind (types fargs rettype)
      (parse-foreign-funcall-args args)
    (let* ((fn (%foreign-funcall-aux
                name
                `(ffi:parse-c-type
                  ',(c-function-type types rettype convention))
                (if (eq library :default)
                    :default
                    (library-handle-form library))))
          (form `(funcall
                  (load-time-value
                   (handler-case ,fn
                     (error (err)
                       (warn "~A" err))))
                  ,@fargs)))
      (if (eq rettype 'ffi:c-pointer)
          `(or ,form (null-pointer))
          form))))

(defmacro %foreign-funcall-pointer (ptr args &key convention)
  "Similar to %foreign-funcall but takes a pointer instead of a string."
  (multiple-value-bind (types fargs rettype)
      (parse-foreign-funcall-args args)
    `(funcall (ffi:foreign-function
               ,ptr (load-time-value
                     (ffi:parse-c-type ',(c-function-type
                                          types rettype convention))))
              ,@fargs)))

;;;# Callbacks

;;; *CALLBACKS* contains the callbacks defined by the CFFI DEFCALLBACK
;;; macro.  The symbol naming the callback is the key, and the value
;;; is a list containing a Lisp function, the parsed CLISP FFI type of
;;; the callback, and a saved pointer that should not persist across
;;; saved images.
(defvar *callbacks* (make-hash-table))

;;; Return a CLISP FFI function type for a CFFI callback function
;;; given a return type and list of argument names and types.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun callback-type (rettype arg-names arg-types convention)
    (ffi:parse-c-type
     `(ffi:c-function
       (:arguments ,@(mapcar (lambda (sym type)
                               (list sym (convert-foreign-type type)))
                             arg-names arg-types))
       (:return-type ,(convert-foreign-type rettype))
       (:language ,(convert-calling-convention convention))))))

;;; Register and create a callback function.
(defun register-callback (name function parsed-type)
  (setf (gethash name *callbacks*)
        (list function parsed-type
              (ffi:with-foreign-object (ptr 'ffi:c-pointer)
                ;; Create callback by converting Lisp function to foreign
                (setf (ffi:memory-as ptr parsed-type) function)
                (ffi:foreign-value ptr)))))

;;; Restore all saved callback pointers when restarting the Lisp
;;; image.  This is pushed onto CUSTOM:*INIT-HOOKS*.
;;; Needs clisp > 2.35, bugfix 2005-09-29
(defun restore-callback-pointers ()
  (maphash
   (lambda (name list)
     (register-callback name (first list) (second list)))
   *callbacks*))

;;; Add RESTORE-CALLBACK-POINTERS to the lists of functions to run
;;; when an image is restarted.
(eval-when (:load-toplevel :execute)
  (pushnew 'restore-callback-pointers custom:*init-hooks*))

;;; Define a callback function NAME to run BODY with arguments
;;; ARG-NAMES translated according to ARG-TYPES and the return type
;;; translated according to RETTYPE.  Obtain a pointer that can be
;;; passed to C code for this callback by calling %CALLBACK.
(defmacro %defcallback (name rettype arg-names arg-types body
                        &key convention)
  `(register-callback
    ',name
    (lambda ,arg-names
      ;; Work around CLISP's FFI:C-POINTER type and convert NIL values
      ;; back into a null pointers.
      (let (,@(loop for name in arg-names
                    and type in arg-types
                    when (eq type :pointer)
                    collect `(,name (or ,name (null-pointer)))))
        ,body))
    ,(callback-type rettype arg-names arg-types convention)))

;;; Look up the name of a callback and return a pointer that can be
;;; passed to a C function.  Signals an error if no callback is
;;; defined called NAME.
(defun %callback (name)
  (multiple-value-bind (list winp) (gethash name *callbacks*)
    (unless winp
      (error "Undefined callback: ~S" name))
    (third list)))

;;;# Loading and Closing Foreign Libraries

(defun %load-foreign-library (name path)
  "Load a foreign library from PATH."
  (declare (ignore name))
  #+#.(cffi-sys::post-2.45-ffi-interface-p)
  (ffi:open-foreign-library path)
  #-#.(cffi-sys::post-2.45-ffi-interface-p)
  (ffi::foreign-library path))

(defun %close-foreign-library (handle)
  "Close a foreign library."
  (ffi:close-foreign-library handle))

(defun native-namestring (pathname)
  (namestring pathname))

;;;# Foreign Globals

(defun %foreign-symbol-pointer (name library)
  "Returns a pointer to a foreign symbol NAME."
  (prog1 (ignore-errors
           (ffi:foreign-address
            #+#.(cffi-sys::post-2.45-ffi-interface-p)
            (ffi::find-foreign-variable name nil library nil nil)
            #-#.(cffi-sys::post-2.45-ffi-interface-p)
            (ffi::foreign-library-variable name library nil nil)))))

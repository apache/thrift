;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-openmcl.lisp --- CFFI-SYS implementation for OpenMCL.
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
  (:use #:common-lisp #:ccl)
  (:import-from #:alexandria #:once-only #:if-let)
  (:export
   #:canonicalize-symbol-name-case
   #:foreign-pointer
   #:pointerp  ; ccl:pointerp
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
   #:%foreign-symbol-pointer
   #:%defcallback
   #:%callback))

(in-package #:cffi-sys)

;;;# Misfeatures

(pushnew 'flat-namespace *features*)

;;;# Symbol Case

(defun canonicalize-symbol-name-case (name)
  (declare (string name))
  (string-upcase name))

;;;# Allocation
;;;
;;; Functions and macros for allocating foreign memory on the stack
;;; and on the heap.  The main CFFI package defines macros that wrap
;;; FOREIGN-ALLOC and FOREIGN-FREE in UNWIND-PROTECT for the common
;;; usage when the memory has dynamic extent.

(defun %foreign-alloc (size)
  "Allocate SIZE bytes on the heap and return a pointer."
  (ccl::malloc size))

(defun foreign-free (ptr)
  "Free a PTR allocated by FOREIGN-ALLOC."
  ;; TODO: Should we make this a dead macptr?
  (ccl::free ptr))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  The
pointer in VAR is invalid beyond the dynamic extent of BODY, and
may be stack-allocated if supported by the implementation.  If
SIZE-VAR is supplied, it will be bound to SIZE during BODY."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  `(let ((,size-var ,size))
     (%stack-block ((,var ,size-var))
       ,@body)))

;;;# Misc. Pointer Operations

(deftype foreign-pointer ()
  'ccl:macptr)

(defun null-pointer ()
  "Construct and return a null pointer."
  (ccl:%null-ptr))

(defun null-pointer-p (ptr)
  "Return true if PTR is a null pointer."
  (ccl:%null-ptr-p ptr))

(defun inc-pointer (ptr offset)
  "Return a pointer OFFSET bytes past PTR."
  (ccl:%inc-ptr ptr offset))

(defun pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (ccl:%ptr-eql ptr1 ptr2))

(defun make-pointer (address)
  "Return a pointer pointing to ADDRESS."
  (ccl:%int-to-ptr address))

(defun pointer-address (ptr)
  "Return the address pointed to by PTR."
  (ccl:%ptr-to-int ptr))

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
  `(ccl:with-pointer-to-ivector (,ptr-var ,vector)
     ,@body))

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
  (:char %get-signed-byte)
  (:unsigned-char %get-unsigned-byte)
  (:short %get-signed-word)
  (:unsigned-short %get-unsigned-word)
  (:int %get-signed-long)
  (:unsigned-int %get-unsigned-long)
  #+(or 32-bit-target windows-target) (:long %get-signed-long)
  #+(and (not windows-target) 64-bit-target) (:long ccl::%%get-signed-longlong)
  #+(or 32-bit-target windows-target) (:unsigned-long %get-unsigned-long)
  #+(and 64-bit-target (not windows-target)) (:unsigned-long ccl::%%get-unsigned-longlong)
  (:long-long ccl::%get-signed-long-long)
  (:unsigned-long-long ccl::%get-unsigned-long-long)
  (:float %get-single-float)
  (:double %get-double-float)
  (:pointer %get-ptr))

;;;# Calling Foreign Functions

(defun convert-foreign-type (type-keyword)
  "Convert a CFFI type keyword to an OpenMCL type."
  (ecase type-keyword
    (:char                :signed-byte)
    (:unsigned-char       :unsigned-byte)
    (:short               :signed-short)
    (:unsigned-short      :unsigned-short)
    (:int                 :signed-int)
    (:unsigned-int        :unsigned-int)
    (:long                :signed-long)
    (:unsigned-long       :unsigned-long)
    (:long-long           :signed-doubleword)
    (:unsigned-long-long  :unsigned-doubleword)
    (:float               :single-float)
    (:double              :double-float)
    (:pointer             :address)
    (:void                :void)))

(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (/ (ccl::foreign-type-bits
      (ccl::parse-foreign-type
       (convert-foreign-type type-keyword)))
     8))

;; There be dragons here.  See the following thread for details:
;; http://clozure.com/pipermail/openmcl-devel/2005-June/002777.html
(defun %foreign-type-alignment (type-keyword)
  "Return the alignment in bytes of a foreign type."
  (/ (ccl::foreign-type-alignment
      (ccl::parse-foreign-type
       (convert-foreign-type type-keyword))) 8))

(defun convert-foreign-funcall-types (args)
  "Convert foreign types for a call to FOREIGN-FUNCALL."
  (loop for (type arg) on args by #'cddr
        collect (convert-foreign-type type)
        if arg collect arg))

(defun convert-external-name (name)
  "Add an underscore to NAME if necessary for the ABI."
  #+darwin (concatenate 'string "_" name)
  #-darwin name)

(defmacro %foreign-funcall (function-name args &key library convention)
  "Perform a foreign function call, document it more later."
  (declare (ignore library convention))
  `(external-call
    ,(convert-external-name function-name)
    ,@(convert-foreign-funcall-types args)))

(defmacro %foreign-funcall-pointer (ptr args &key convention)
  (declare (ignore convention))
  `(ff-call ,ptr ,@(convert-foreign-funcall-types args)))

;;;# Callbacks

;;; The *CALLBACKS* hash table maps CFFI callback names to OpenMCL "macptr"
;;; entry points.  It is safe to store the pointers directly because
;;; OpenMCL will update the address of these pointers when a saved image
;;; is loaded (see CCL::RESTORE-PASCAL-FUNCTIONS).
(defvar *callbacks* (make-hash-table))

;;; Create a package to contain the symbols for callback functions.  We
;;; want to redefine callbacks with the same symbol so the internal data
;;; structures are reused.
(defpackage #:cffi-callbacks
  (:use))

;;; Intern a symbol in the CFFI-CALLBACKS package used to name the internal
;;; callback for NAME.
(defun intern-callback (name)
  (intern (format nil "~A::~A"
                  (if-let (package (symbol-package name))
                    (package-name package)
                    "#")
                  (symbol-name name))
          '#:cffi-callbacks))

(defmacro %defcallback (name rettype arg-names arg-types body
                        &key convention)
  (let ((cb-name (intern-callback name)))
    `(progn
       (defcallback ,cb-name
           (,@(when (eq convention :stdcall)
                '(:discard-stack-args))
            ,@(mapcan (lambda (sym type)
                        (list (convert-foreign-type type) sym))
                      arg-names arg-types)
            ,(convert-foreign-type rettype))
         ,body)
       (setf (gethash ',name *callbacks*) (symbol-value ',cb-name)))))

(defun %callback (name)
  (or (gethash name *callbacks*)
      (error "Undefined callback: ~S" name)))

;;;# Loading Foreign Libraries

(defun %load-foreign-library (name path)
  "Load the foreign library NAME."
  (declare (ignore name))
  (open-shared-library path))

(defun %close-foreign-library (name)
  "Close the foreign library NAME."
  ;; C-S-L sometimes ends in an endless loop
  ;; with :COMPLETELY T
  (close-shared-library name :completely nil))

(defun native-namestring (pathname)
  (ccl::native-translated-namestring pathname))

;;;# Foreign Globals

(defun %foreign-symbol-pointer (name library)
  "Returns a pointer to a foreign symbol NAME."
  (declare (ignore library))
  (foreign-symbol-address (convert-external-name name)))

;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-allegro.lisp --- CFFI-SYS implementation for Allegro CL.
;;;
;;; Copyright (C) 2005-2009, Luis Oliveira  <loliveira(@)common-lisp.net>
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
  (:use #:common-lisp)
  (:import-from #:alexandria #:if-let #:with-unique-names #:once-only)
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
   #:defcfun-helper-forms
   #:%defcallback
   #:%callback))

(in-package #:cffi-sys)

;;;# Mis-features

#-64bit (pushnew 'no-long-long *features*)
(pushnew 'flat-namespace *features*)

;;;# Symbol Case

(defun canonicalize-symbol-name-case (name)
  (declare (string name))
  (if (eq excl:*current-case-mode* :case-sensitive-lower)
      (string-downcase name)
      (string-upcase name)))

;;;# Basic Pointer Operations

(deftype foreign-pointer ()
  'ff:foreign-address)

(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (ff:foreign-address-p ptr))

(defun pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (eql ptr1 ptr2))

(defun null-pointer ()
  "Return a null pointer."
  0)

(defun null-pointer-p (ptr)
  "Return true if PTR is a null pointer."
  (zerop ptr))

(defun inc-pointer (ptr offset)
  "Return a pointer pointing OFFSET bytes past PTR."
  (+ ptr offset))

(defun make-pointer (address)
  "Return a pointer pointing to ADDRESS."
  (check-type address ff:foreign-address)
  address)

(defun pointer-address (ptr)
  "Return the address pointed to by PTR."
  (check-type ptr ff:foreign-address)
  ptr)

;;;# Allocation
;;;
;;; Functions and macros for allocating foreign memory on the stack
;;; and on the heap.  The main CFFI package defines macros that wrap
;;; FOREIGN-ALLOC and FOREIGN-FREE in UNWIND-PROTECT for the common usage
;;; when the memory has dynamic extent.

(defun %foreign-alloc (size)
  "Allocate SIZE bytes on the heap and return a pointer."
  (ff:allocate-fobject :char :c size))

(defun foreign-free (ptr)
  "Free a PTR allocated by FOREIGN-ALLOC."
  (ff:free-fobject ptr))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  The
pointer in VAR is invalid beyond the dynamic extent of BODY, and
may be stack-allocated if supported by the implementation.  If
SIZE-VAR is supplied, it will be bound to SIZE during BODY."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  #+(version>= 8 1)
  (when (and (constantp size) (<= (eval size) ff:*max-stack-fobject-bytes*))
    (return-from with-foreign-pointer
      `(let ((,size-var ,(eval size)))
         (declare (ignorable ,size-var))
         (ff:with-static-fobject (,var '(:array :char ,(eval size))
                                       :allocation :foreign-static-gc)
           ;; (excl::stack-allocated-p var) => T
           (let ((,var (ff:fslot-address ,var)))
             ,@body)))))
  `(let* ((,size-var ,size)
          (,var (ff:allocate-fobject :char :c ,size-var)))
     (unwind-protect
          (progn ,@body)
       (ff:free-fobject ,var))))

;;;# Shareable Vectors
;;;
;;; This interface is very experimental.  WITH-POINTER-TO-VECTOR-DATA
;;; should be defined to perform a copy-in/copy-out if the Lisp
;;; implementation can't do this.

(defun make-shareable-byte-vector (size)
  "Create a Lisp vector of SIZE bytes can passed to
WITH-POINTER-TO-VECTOR-DATA."
  (make-array size :element-type '(unsigned-byte 8)
              :allocation :static-reclaimable))

(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
  ;; An array allocated in static-reclamable is a non-simple array in
  ;; the normal Lisp allocation area, pointing to a simple array in
  ;; the static-reclaimable allocation area. Therefore we have to get
  ;; out the simple-array to find the pointer to the actual contents.
  (with-unique-names (simple-vec)
    `(excl:with-underlying-simple-vector (,vector ,simple-vec)
       (let ((,ptr-var (ff:fslot-address-typed :unsigned-char :lisp
                                               ,simple-vec)))
         ,@body))))

;;;# Dereferencing

(defun convert-foreign-type (type-keyword)
  "Convert a CFFI type keyword to an Allegro type."
  (ecase type-keyword
    (:char             :char)
    (:unsigned-char    :unsigned-char)
    (:short            :short)
    (:unsigned-short   :unsigned-short)
    (:int              :int)
    (:unsigned-int     :unsigned-int)
    (:long             :long)
    (:unsigned-long    :unsigned-long)
    (:long-long
     #+64bit :nat
     #-64bit (error "this platform does not support :long-long."))
    (:unsigned-long-long
     #+64bit :unsigned-nat
     #-64bit (error "this platform does not support :unsigned-long-long"))
    (:float            :float)
    (:double           :double)
    (:pointer          :unsigned-nat)
    (:void             :void)))

(defun %mem-ref (ptr type &optional (offset 0))
  "Dereference an object of TYPE at OFFSET bytes from PTR."
  (unless (zerop offset)
    (setf ptr (inc-pointer ptr offset)))
  (ff:fslot-value-typed (convert-foreign-type type) :c ptr))

;;; Compiler macro to open-code the call to FSLOT-VALUE-TYPED when the
;;; CFFI type is constant.  Allegro does its own transformation on the
;;; call that results in efficient code.
(define-compiler-macro %mem-ref (&whole form ptr type &optional (off 0))
  (if (constantp type)
      (let ((ptr-form (if (eql off 0) ptr `(+ ,ptr ,off))))
        `(ff:fslot-value-typed ',(convert-foreign-type (eval type))
                               :c ,ptr-form))
      form))

(defun %mem-set (value ptr type &optional (offset 0))
  "Set the object of TYPE at OFFSET bytes from PTR."
  (unless (zerop offset)
    (setf ptr (inc-pointer ptr offset)))
  (setf (ff:fslot-value-typed (convert-foreign-type type) :c ptr) value))

;;; Compiler macro to open-code the call to (SETF FSLOT-VALUE-TYPED)
;;; when the CFFI type is constant.  Allegro does its own
;;; transformation on the call that results in efficient code.
(define-compiler-macro %mem-set (&whole form val ptr type &optional (off 0))
  (if (constantp type)
      (once-only (val)
        (let ((ptr-form (if (eql off 0) ptr `(+ ,ptr ,off))))
          `(setf (ff:fslot-value-typed ',(convert-foreign-type (eval type))
                                       :c ,ptr-form) ,val)))
      form))

;;;# Calling Foreign Functions

(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (ff:sizeof-fobject (convert-foreign-type type-keyword)))

(defun %foreign-type-alignment (type-keyword)
  "Returns the alignment in bytes of a foreign type."
  #+(and powerpc macosx32)
  (when (eq type-keyword :double)
    (return-from %foreign-type-alignment 8))
  ;; No override necessary for the remaining types....
  (ff::sized-ftype-prim-align
   (ff::iforeign-type-sftype
    (ff:get-foreign-type
     (convert-foreign-type type-keyword)))))

(defun foreign-funcall-type-and-args (args)
  "Returns a list of types, list of args and return type."
  (let ((return-type :void))
    (loop for (type arg) on args by #'cddr
          if arg collect type into types
          and collect arg into fargs
          else do (setf return-type type)
          finally (return (values types fargs return-type)))))

(defun convert-to-lisp-type (type)
  (ecase type
    ((:char :short :int :long :nat)
     `(signed-byte ,(* 8 (ff:sizeof-fobject type))))
    ((:unsigned-char :unsigned-short :unsigned-int :unsigned-long :unsigned-nat)
     `(unsigned-byte ,(* 8 (ff:sizeof-fobject type))))
    (:float 'single-float)
    (:double 'double-float)
    (:void 'null)))

(defun allegro-type-pair (cffi-type)
  ;; the :FOREIGN-ADDRESS pseudo-type accepts both pointers and
  ;; arrays. We need the latter for shareable byte vector support.
  (if (eq cffi-type :pointer)
      (list :foreign-address)
      (let ((ftype (convert-foreign-type cffi-type)))
        (list ftype (convert-to-lisp-type ftype)))))

#+ignore
(defun note-named-foreign-function (symbol name types rettype)
  "Give Allegro's compiler a hint to perform a direct call."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ',symbol 'system::direct-ff-call)
           (list '(,name :language :c)
                 t  ; callback
                 :c ; convention
                 ;; return type '(:c-type lisp-type)
                 ',(allegro-type-pair rettype)
                 ;; arg types '({(:c-type lisp-type)}*)
                 '(,@(mapcar #'allegro-type-pair types))
                 nil ; arg-checking
                 ff::ep-flag-never-release))))

(defmacro %foreign-funcall (name args &key convention library)
  (declare (ignore convention library))
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    `(system::ff-funcall
      (load-time-value (excl::determine-foreign-address
                        '(,name :language :c)
                        #-(version>= 8 1) ff::ep-flag-never-release
                        #+(version>= 8 1) ff::ep-flag-always-release
                        nil ; method-index
                        ))
      ;; arg types {'(:c-type lisp-type) argN}*
      ,@(mapcan (lambda (type arg)
                  `(',(allegro-type-pair type) ,arg))
                types fargs)
      ;; return type '(:c-type lisp-type)
      ',(allegro-type-pair rettype))))

(defun defcfun-helper-forms (name lisp-name rettype args types options)
  "Return 2 values for DEFCFUN. A prelude form and a caller form."
  (declare (ignore options))
  (let ((ff-name (intern (format nil "%cffi-foreign-function/~A" lisp-name))))
    (values
      `(ff:def-foreign-call (,ff-name ,name)
           ,(loop for type in types
                  collect (list* (gensym) (allegro-type-pair type)))
         :returning ,(allegro-type-pair rettype)
         ;; Don't use call-direct when there are no arguments.
         ,@(unless (null args) '(:call-direct t))
         :arg-checking nil
         :strings-convert nil
         #+(version>= 8 1) ,@'(:release-heap :when-ok
                               :release-heap-ignorable t)
         #+smp ,@'(:release-heap-implies-allow-gc t))
      `(,ff-name ,@args))))

;;; See doc/allegro-internals.txt for a clue about entry-vec.
(defmacro %foreign-funcall-pointer (ptr args &key convention)
  (declare (ignore convention))
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    (with-unique-names (entry-vec)
      `(let ((,entry-vec (excl::make-entry-vec-boa)))
         (setf (aref ,entry-vec 1) ,ptr) ; set jump address
         (system::ff-funcall
          ,entry-vec
          ;; arg types {'(:c-type lisp-type) argN}*
          ,@(mapcan (lambda (type arg)
                      `(',(allegro-type-pair type) ,arg))
                    types fargs)
          ;; return type '(:c-type lisp-type)
          ',(allegro-type-pair rettype))))))

;;;# Callbacks

;;; The *CALLBACKS* hash table contains information about a callback
;;; for the Allegro FFI.  The key is the name of the CFFI callback,
;;; and the value is a cons, the car containing the symbol the
;;; callback was defined on in the CFFI-CALLBACKS package, the cdr
;;; being an Allegro FFI pointer (a fixnum) that can be passed to C
;;; functions.
;;;
;;; These pointers must be restored when a saved Lisp image is loaded.
;;; The RESTORE-CALLBACKS function is added to *RESTART-ACTIONS* to
;;; re-register the callbacks during Lisp startup.
(defvar *callbacks* (make-hash-table))

;;; Register a callback in the *CALLBACKS* hash table.
(defun register-callback (cffi-name callback-name)
  (setf (gethash cffi-name *callbacks*)
        (cons callback-name (ff:register-foreign-callable
                             callback-name :reuse t))))

;;; Restore the saved pointers in *CALLBACKS* when loading an image.
(defun restore-callbacks ()
  (maphash (lambda (key value)
             (register-callback key (car value)))
           *callbacks*))

;;; Arrange for RESTORE-CALLBACKS to run when a saved image containing
;;; CFFI is restarted.
(eval-when (:load-toplevel :execute)
  (pushnew 'restore-callbacks excl:*restart-actions*))

;;; Create a package to contain the symbols for callback functions.
(defpackage #:cffi-callbacks
  (:use))

(defun intern-callback (name)
  (intern (format nil "~A::~A"
                  (if-let (package (symbol-package name))
                    (package-name package)
                    "#")
                  (symbol-name name))
          '#:cffi-callbacks))

(defun convert-calling-convention (convention)
  (ecase convention
    (:cdecl :c)
    (:stdcall :stdcall)))

(defmacro %defcallback (name rettype arg-names arg-types body
                        &key convention)
  (declare (ignore rettype))
  (let ((cb-name (intern-callback name)))
    `(progn
       (ff:defun-foreign-callable ,cb-name
           ,(mapcar (lambda (sym type) (list sym (convert-foreign-type type)))
                    arg-names arg-types)
         (declare (:convention ,(convert-calling-convention convention)))
         ,body)
       (register-callback ',name ',cb-name))))

;;; Return the saved Lisp callback pointer from *CALLBACKS* for the
;;; CFFI callback named NAME.
(defun %callback (name)
  (or (cdr (gethash name *callbacks*))
      (error "Undefined callback: ~S" name)))

;;;# Loading and Closing Foreign Libraries

(defun %load-foreign-library (name path)
  "Load a foreign library."
  ;; ACL 8.0 honors the :FOREIGN option and always tries to foreign load
  ;; the argument. However, previous versions do not and will only
  ;; foreign load the argument if its type is a member of the
  ;; EXCL::*LOAD-FOREIGN-TYPES* list. Therefore, we bind that special
  ;; to a list containing whatever type NAME has.
  (declare (ignore name))
  (let ((excl::*load-foreign-types*
         (list (pathname-type (parse-namestring path)))))
    (handler-case
        (progn
          #+(version>= 7) (load path :foreign t)
          #-(version>= 7) (load path))
      (file-error (fe)
        (error (change-class fe 'simple-error))))
    path))

(defun %close-foreign-library (name)
  "Close the foreign library NAME."
  (ff:unload-foreign-library name))

(defun native-namestring (pathname)
  (namestring pathname))

;;;# Foreign Globals

(defun convert-external-name (name)
  "Add an underscore to NAME if necessary for the ABI."
  #+macosx (concatenate 'string "_" name)
  #-macosx name)

(defun %foreign-symbol-pointer (name library)
  "Returns a pointer to a foreign symbol NAME."
  (declare (ignore library))
  (prog1 (ff:get-entry-point (convert-external-name name))))

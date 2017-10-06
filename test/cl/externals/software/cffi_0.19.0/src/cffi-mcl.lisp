;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-mcl.lisp --- CFFI-SYS implementation for Digitool MCL.
;;;
;;; Copyright 2010 james.anderson@setf.de
;;; Copyright 2005-2006, James Bielman  <jamesjb@jamesjb.com>
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

;;; this is a stop-gap emulation. (at least) three things are not right
;;; - integer vector arguments are copied
;;; - return values are not typed
;;; - a shared library must be packaged as a framework and statically loaded
;;; 
;;; on the topic of shared libraries, see
;;; http://developer.apple.com/library/mac/#documentation/DeveloperTools/Conceptual/MachOTopics/1-Articles/loading_code.html
;;; which describes how to package a shared library as a framework.
;;; once a framework exists, load it as, eg.
;;; (ccl::add-framework-bundle "fftw.framework" :pathname "ccl:frameworks;" )

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
  (#_newPtr size))

(defun foreign-free (ptr)
  "Free a PTR allocated by FOREIGN-ALLOC."
  ;; TODO: Should we make this a dead macptr?
  (#_disposePtr ptr))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  The
pointer in VAR is invalid beyond the dynamic extent of BODY, and
may be stack-allocated if supported by the implementation.  If
SIZE-VAR is supplied, it will be bound to SIZE during BODY."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  `(let ((,size-var ,size))
     (ccl:%stack-block ((,var ,size-var))
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

;;; from openmcl::macros.lisp

(defmacro with-pointer-to-vector-data ((ptr ivector) &body body)
  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
  (let* ((v (gensym))
         (l (gensym)))
    `(let* ((,v ,ivector)
            (,l (length ,v)))
       (unless (typep ,v 'ccl::ivector) (ccl::report-bad-arg ,v 'ccl::ivector))
       ;;;!!! this, unless it's possible to suppress gc
       (let ((,ptr (#_newPtr ,l)))
         (unwind-protect (progn (ccl::%copy-ivector-to-ptr ,v 0 ,ptr 0 ,l)
                                (mutliple-value-prog1
                                 (locally ,@body)
                                 (ccl::%copy-ptr-to-ivector ,ptr 0 ,v 0 ,l)))
           (#_disposePtr ,ptr))))))

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
  (:long %get-signed-long)
  (:unsigned-long %get-unsigned-long)
  (:long-long ccl::%get-signed-long-long)
  (:unsigned-long-long ccl::%get-unsigned-long-long)
  (:float %get-single-float)
  (:double %get-double-float)
  (:pointer %get-ptr))


(defun ccl::%get-unsigned-long-long (ptr offset)
  (let ((value 0) (bit 0))
    (dotimes (i 8)
      (setf (ldb (byte 8 (shiftf bit (+ bit 8))) value)
            (ccl:%get-unsigned-byte ptr (+ offset i))))
    value))

(setf (fdefinition 'ccl::%get-signed-long-long)
      (fdefinition 'ccl::%get-unsigned-long-long))

(defun (setf ccl::%get-unsigned-long-long) (value ptr offset)
  (let ((bit 0))
    (dotimes (i 8)
      (setf (ccl:%get-unsigned-byte ptr (+ offset i))
            (ldb (byte 8 (shiftf bit (+ bit 8))) value))))
  ptr)

(setf (fdefinition '(setf ccl::%get-signed-long-long))
      (fdefinition '(setf ccl::%get-unsigned-long-long)))


;;;# Calling Foreign Functions

(defun convert-foreign-type (type-keyword)
  "Convert a CFFI type keyword to a ppc-ff-call type."
  (ecase type-keyword
    (:char                :signed-byte)
    (:unsigned-char       :unsigned-byte)
    (:short               :signed-short)
    (:unsigned-short      :unsigned-short)
    (:int                 :signed-fullword)
    (:unsigned-int        :unsigned-fullword)
    (:long                :signed-fullword)
    (:unsigned-long       :unsigned-fullword)
    (:long-long           :signed-doubleword)
    (:unsigned-long-long  :unsigned-doubleword)
    (:float               :single-float)
    (:double              :double-float)
    (:pointer             :address)
    (:void                :void)))

(defun ppc-ff-call-type=>mactype-name (type-keyword)
  (ecase type-keyword
    (:signed-byte          :sint8)
    (:unsigned-byte        :uint8)
    (:signed-short         :sint16)
    (:unsigned-short       :uint16)
    (:signed-halfword      :sint16)
    (:unsigned-halfword    :uint16)
    (:signed-fullword      :sint32)
    (:unsigned-fullword    :uint32)
    ;(:signed-doubleword    :long-long)
    ;(:unsigned-doubleword  :unsigned-long-long)
    (:single-float         :single-float)
    (:double-float         :double-float)
    (:address              :pointer)
    (:void                 :void)))



(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (case type-keyword
    ((:long-long :unsigned-long-long) 8)
    (t (ccl::mactype-record-size
        (ccl::find-mactype
         (ppc-ff-call-type=>mactype-name (convert-foreign-type type-keyword)))))))

;; There be dragons here.  See the following thread for details:
;; http://clozure.com/pipermail/openmcl-devel/2005-June/002777.html
(defun %foreign-type-alignment (type-keyword)
  "Return the alignment in bytes of a foreign type."
  (case type-keyword
    ((:long-long :unsigned-long-long) 4)
    (t (ccl::mactype-record-size
        (ccl::find-mactype
         (ppc-ff-call-type=>mactype-name (convert-foreign-type type-keyword)))))))

(defun convert-foreign-funcall-types (args)
  "Convert foreign types for a call to FOREIGN-FUNCALL."
  (loop for (type arg) on args by #'cddr
        collect (convert-foreign-type type)
        if arg collect arg))

(defun convert-external-name (name)
  "no '_' is necessary here, the internal lookup operators handle it"
  name)

(defmacro %foreign-funcall (function-name args &key library convention)
  "Perform a foreign function call, document it more later."
  (declare (ignore library convention))
  `(ccl::ppc-ff-call
    (ccl::macho-address ,(ccl::get-macho-entry-point (convert-external-name function-name)))
    ,@(convert-foreign-funcall-types args)))

(defmacro %foreign-funcall-pointer (ptr args &key convention)
  (declare (ignore convention))
  `(ccl::ppc-ff-call ,ptr ,@(convert-foreign-funcall-types args)))

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
  (declare (ignore convention))
  (let ((cb-name (intern-callback name)))
    `(progn
       (ccl::ppc-defpascal ,cb-name
           (;; ? ,@(when (eq convention :stdcall) '(:discard-stack-args))
            ,@(mapcan (lambda (sym type)
                        (list (ppc-ff-call-type=>mactype-name (convert-foreign-type type)) sym))
                      arg-names arg-types)
            ,(ppc-ff-call-type=>mactype-name (convert-foreign-type rettype)))
         ,body)
       (setf (gethash ',name *callbacks*) (symbol-value ',cb-name)))))

(defun %callback (name)
  (or (gethash name *callbacks*)
      (error "Undefined callback: ~S" name)))

;;;# Loading Foreign Libraries

(defun %load-foreign-library (name path)
  "Load the foreign library NAME."
  (declare (ignore path))
  (setf name (string name))
  ;; for mcl emulate this wrt frameworks
  (unless (and (> (length name) 10)
               (string-equal name ".framework" :start1 (- (length name) 10)))
    (setf name (concatenate 'string name ".framework")))
  ;; if the framework was not registered, add it
  (unless (gethash name ccl::*framework-descriptors*)
    (ccl::add-framework-bundle name :pathname "ccl:frameworks;" ))
  (ccl::load-framework-bundle name))

(defun %close-foreign-library (name)
  "Close the foreign library NAME."
  ;; for mcl do nothing
  (declare (ignore name))
  nil)

(defun native-namestring (pathname)
  (ccl::posix-namestring (ccl:full-pathname pathname)))


;;;# Foreign Globals

(deftrap-inline "_findsymbol"
    ((map :pointer)
     (name :pointer))
    :pointer
    ())


(defun %foreign-symbol-pointer (name library)
  "Returns a pointer to a foreign symbol NAME."
  (declare (ignore library))
  (ccl::macho-address
   (ccl::get-macho-entry-point (convert-external-name name))))

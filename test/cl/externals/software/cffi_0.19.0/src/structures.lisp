;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; structures.lisp --- Methods for translating foreign structures.
;;;
;;; Copyright (C) 2011, Liam M. Healy  <lhealy@common-lisp.net>
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

(in-package #:cffi)

;;; Definitions for conversion of foreign structures.

(defmethod translate-into-foreign-memory ((object list)
                                          (type foreign-struct-type)
                                          p)
  (unless (bare-struct-type-p type)
    (loop for (name value) on object by #'cddr
          do (setf (foreign-slot-value p (unparse-type type) name)
                   (let ((slot (gethash name (structure-slots type))))
                     (convert-to-foreign value (slot-type slot)))))))

(defmethod translate-to-foreign (value (type foreign-struct-type))
  (let ((ptr (foreign-alloc type)))
    (translate-into-foreign-memory value type ptr)
    ptr))

(defmethod translate-from-foreign (p (type foreign-struct-type))
  ;; Iterate over slots, make plist
  (if (bare-struct-type-p type)
      p
      (let ((plist (list)))
        (loop for slot being the hash-value of (structure-slots type)
              for name = (slot-name slot)
              do (setf (getf plist name)
                       (foreign-struct-slot-value p slot)))
        plist)))

(defmethod free-translated-object (ptr (type foreign-struct-type) freep)
  (unless (bare-struct-type-p type)
    ;; Look for any pointer slots and free them first
    (loop for slot being the hash-value of (structure-slots type)
          when (and (listp (slot-type slot)) (eq (first (slot-type slot)) :pointer))
            do
               ;; Free if the pointer is to a specific type, not generic :pointer
               (free-translated-object
                (foreign-slot-value ptr type (slot-name slot))
                (rest (slot-type slot))
                freep))
    (foreign-free ptr)))

(defmacro define-translation-method ((object type method) &body body)
  "Define a translation method for the foreign structure type; 'method is one of :into, :from, or :to, meaning relation to foreign memory.  If :into, the variable 'pointer is the foreign pointer.  Note: type must be defined and loaded before this macro is expanded, and just the bare name (without :struct) should be specified."
  (let ((tclass (class-name (class-of (cffi::parse-type `(:struct ,type))))))
    (when (eq tclass 'foreign-struct-type)
      (error "Won't replace existing translation method for foreign-struct-type"))
    `(defmethod
         ,(case method
            (:into 'translate-into-foreign-memory)
            (:from 'translate-from-foreign)
            (:to 'translate-to-foreign))
       ;; Arguments to the method
       (,object
        (type ,tclass)
        ,@(when (eq method :into) '(pointer))) ; is intentional variable capture a good idea?
       ;; The body
       (declare (ignorable type)) ; I can't think of a reason why you'd want to use this
       ,@body)))

(defmacro translation-forms-for-class (class type-class)
  "Make forms for translation of foreign structures to and from a standard class.  The class slots are assumed to have the same name as the foreign structure."
  ;; Possible improvement: optional argument to map structure slot names to/from class slot names.
  `(progn
     (defmethod translate-from-foreign (pointer (type ,type-class))
       ;; Make the instance from the plist
       (apply 'make-instance ',class (call-next-method)))
     (defmethod translate-into-foreign-memory ((object ,class) (type ,type-class) pointer)
       (call-next-method
        ;; Translate into a plist and call the general method
        (loop for slot being the hash-value of (structure-slots type)
              for name = (slot-name slot)
              append (list slot-name (slot-value object slot-name)))
        type
        pointer))))

;;; For a class already defined and loaded, and a defcstruct already defined, use
;;; (translation-forms-for-class class type-class)
;;; to connnect the two.  It would be nice to have a macro to do all three simultaneously.
;;; (defmacro define-foreign-structure (class ))

#|
(defmacro define-structure-conversion
    (value-symbol type lisp-class slot-names to-form from-form &optional (struct-name type))
  "Define the functions necessary to convert to and from a foreign structure.  The to-form sets each of the foreign slots in succession, assume the foreign object exists.  The from-form creates the Lisp object, making it with the correct value by reference to foreign slots."
  `(flet ((map-slots (fn val)
            (maphash
             (lambda (name slot-struct)
               (funcall fn (foreign-slot-value val ',type name) (slot-type slot-struct)))
             (slots (follow-typedefs (parse-type ',type))))))
     ;; Convert this to a separate function so it doesn't have to be recomputed on the fly each time.
     (defmethod translate-to-foreign ((,value-symbol ,lisp-class) (type ,type))
       (let ((p (foreign-alloc ',struct-name)))
         ;;(map-slots #'translate-to-foreign ,value-symbol) ; recursive translation of slots
         (with-foreign-slots (,slot-names p ,struct-name)
           ,to-form)
         (values p t))) ; second value is passed to FREE-TRANSLATED-OBJECT
     (defmethod free-translated-object (,value-symbol (p ,type) freep)
       (when freep
         ;; Is this redundant?
         (map-slots #'free-translated-object value) ; recursively free slots
         (foreign-free ,value-symbol)))
     (defmethod translate-from-foreign (,value-symbol (type ,type))
       (with-foreign-slots (,slot-names ,value-symbol ,struct-name)
         ,from-form))))
|#

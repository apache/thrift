;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; funcall.lisp -- FOREIGN-FUNCALL implementation using libffi
;;;
;;; Copyright (C) 2009, 2010, 2011 Liam M. Healy  <lhealy@common-lisp.net>
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

(define-condition libffi-error (cffi-error)
  ((function-name
    :initarg :function-name :reader function-name)))

(define-condition simple-libffi-error (simple-error libffi-error)
  ())

(defun libffi-error (function-name format-control &rest format-arguments)
  (error 'simple-libffi-error
         :function-name function-name
         :format-control format-control
         :format-arguments format-arguments))

(defun make-libffi-cif (function-name return-type argument-types
                        &optional (abi :default-abi))
  "Generate or retrieve the Call InterFace needed to call the function through libffi."
  (let* ((argument-count (length argument-types))
         (cif (foreign-alloc '(:struct ffi-cif)))
         (ffi-argtypes (foreign-alloc :pointer :count argument-count)))
    (loop
      :for type :in argument-types
      :for index :from 0
      :do (setf (mem-aref ffi-argtypes :pointer index)
                (make-libffi-type-descriptor (parse-type type))))
    (unless (eql :ok (libffi/prep-cif cif abi argument-count
                                      (make-libffi-type-descriptor (parse-type return-type))
                                      ffi-argtypes))
      (libffi-error function-name
                    "The 'ffi_prep_cif' libffi call failed for function ~S."
                    function-name))
    cif))

(defun free-libffi-cif (ptr)
  (foreign-free (foreign-slot-value ptr '(:struct ffi-cif) 'argument-types))
  (foreign-free ptr))

(defun translate-objects-ret (symbols function-arguments types return-type call-form)
  (translate-objects
   symbols
   function-arguments
   types
   return-type
   (if (or (eql return-type :void)
           (typep (parse-type return-type) 'translatable-foreign-type))
       call-form
       ;; built-in types won't be translated by
       ;; expand-from-foreign, we have to do it here
       `(mem-ref
         ,call-form
         ',(canonicalize-foreign-type return-type)))
   t))

(defun foreign-funcall-form/fsbv-with-libffi (function function-arguments symbols types
                                              return-type argument-types
                                              &optional pointerp (abi :default-abi))
  "A body of foreign-funcall calling the libffi function #'call (ffi_call)."
  (let ((argument-count (length argument-types)))
    `(with-foreign-objects ((argument-values :pointer ,argument-count)
                            ,@(unless (eql return-type :void)
                                `((result ',return-type))))
       ,(translate-objects-ret
         symbols function-arguments types return-type
         ;; NOTE: We must delay the cif creation until the first call
         ;; because it's FOREIGN-ALLOC'd, i.e. it gets corrupted by an
         ;; image save/restore cycle. This way a lib will remain usable
         ;; through a save/restore cycle if the save happens before any
         ;; FFI calls will have been made, i.e. nothing is malloc'd yet.
         `(progn
            (loop
              :for arg :in (list ,@symbols)
              :for count :from 0
              :do (setf (mem-aref argument-values :pointer count) arg))
            (let* ((libffi-cif-cache (load-time-value (cons 'libffi-cif-cache nil)))
                   (libffi-cif (or (cdr libffi-cif-cache)
                                   (setf (cdr libffi-cif-cache)
                                         ;; FIXME ideally we should install a finalizer on the cons
                                         ;; that calls FREE-LIBFFI-CIF on the cif (when the function
                                         ;; gets redefined, and the cif becomes unreachable). but a
                                         ;; finite world is full of compromises... - attila
                                         (make-libffi-cif ,function ',return-type
                                                          ',argument-types ',abi)))))
              (libffi/call libffi-cif
                           ,(if pointerp
                                function
                                `(foreign-symbol-pointer ,function))
                           ,(if (eql return-type :void) '(null-pointer) 'result)
                           argument-values)
              ,(if (eql return-type :void)
                   '(values)
                   'result)))))))

(setf *foreign-structures-by-value* 'foreign-funcall-form/fsbv-with-libffi)

;; DEPRECATED Its presence encourages the use of #+fsbv which may lead to the
;; situation where a fasl was produced by an image that has fsbv feature
;; and then ends up being loaded into an image later that has no fsbv support
;; loaded. Use explicit ASDF dependencies instead and assume the presence
;; of the feature accordingly.
(pushnew :fsbv *features*)

;; DEPRECATED This is here only for backwards compatibility until its fate is
;; decided. See the mailing list discussion for details.
(defctype :sizet size-t)

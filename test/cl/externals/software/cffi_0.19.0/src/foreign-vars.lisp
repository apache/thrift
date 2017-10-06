;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; foreign-vars.lisp --- High-level interface to foreign globals.
;;;
;;; Copyright (C) 2005-2008, Luis Oliveira  <loliveira(@)common-lisp.net>
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

;;;# Accessing Foreign Globals

;;; Called by FOREIGN-OPTIONS in functions.lisp.
(defun parse-defcvar-options (options)
  (destructuring-bind (&key (library :default) read-only) options
    (list :library library :read-only read-only)))

(defun get-var-pointer (symbol)
  "Return a pointer to the foreign global variable relative to SYMBOL."
  (foreign-symbol-pointer (get symbol 'foreign-var-name)
                          :library (get symbol 'foreign-var-library)))

;;; Note: this will lookup not only variables but also functions.
(defun foreign-symbol-pointer (name &key (library :default))
  (check-type name string)
  (%foreign-symbol-pointer
   name (if (eq library :default)
            :default
            (foreign-library-handle
             (get-foreign-library library)))))

(defun fs-pointer-or-lose (foreign-name library)
  "Like foreign-symbol-ptr but throws an error instead of
returning nil when foreign-name is not found."
  (or (foreign-symbol-pointer foreign-name :library library)
      (error "Trying to access undefined foreign variable ~S." foreign-name)))

(defmacro defcvar (name-and-options type &optional documentation)
  "Define a foreign global variable."
  (multiple-value-bind (lisp-name foreign-name options)
      (parse-name-and-options name-and-options t)
    (let ((fn (symbolicate '#:%var-accessor- lisp-name))
          (read-only (getf options :read-only))
          (library (getf options :library)))
      ;; We can't really setf an aggregate type.
      (when (aggregatep (parse-type type))
        (setq read-only t))
      `(progn
         (setf (documentation ',lisp-name 'variable) ,documentation)
         ;; Save foreign-name and library for posterior access by
         ;; GET-VAR-POINTER.
         (setf (get ',lisp-name 'foreign-var-name) ,foreign-name)
         (setf (get ',lisp-name 'foreign-var-library) ',library)
         ;; Getter
         (defun ,fn ()
           (mem-ref (fs-pointer-or-lose ,foreign-name ',library) ',type))
         ;; Setter
         (defun (setf ,fn) (value)
           ,(if read-only '(declare (ignore value)) (values))
           ,(if read-only
                `(error ,(format nil
                                 "Trying to modify read-only foreign var: ~A."
                                 lisp-name))
                `(setf (mem-ref (fs-pointer-or-lose ,foreign-name ',library)
                                ',type)
                       value)))
         ;; While most Lisps already expand DEFINE-SYMBOL-MACRO to an
         ;; EVAL-WHEN form like this, that is not required by the
         ;; standard so we do it ourselves.
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (define-symbol-macro ,lisp-name (,fn)))))))

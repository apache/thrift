;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-tests.asd --- ASDF system definition for CFFI unit tests.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2005-2011, Luis Oliveira  <loliveira@common-lisp.net>
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

(load-systems "trivial-features" "cffi-grovel")

(defclass c-test-lib (c-source-file)
  ())

(defmethod perform ((o load-op) (c c-test-lib))
  nil)

(defmethod perform ((o load-source-op) (c c-test-lib))
  nil)

(defmethod output-files ((o compile-op) (c c-test-lib))
  (let ((p (component-pathname c)))
    (values
     (list (make-pathname :defaults p :type (asdf/bundle:bundle-pathname-type :object))
           (make-pathname :defaults p :type (asdf/bundle:bundle-pathname-type :shared-library)))
     t)))

(defmethod perform ((o compile-op) (c c-test-lib))
  (let ((cffi-toolchain:*cc-flags* `(,@cffi-toolchain:*cc-flags* "-Wall" "-std=c99" "-pedantic")))
    (destructuring-bind (obj dll) (output-files o c)
      (cffi-toolchain:cc-compile obj (input-files o c))
      (cffi-toolchain:link-shared-library dll (list obj)))))

(defsystem "cffi-tests"
  :description "Unit tests for CFFI."
  :depends-on ("cffi-grovel" "cffi-libffi" "bordeaux-threads" #-ecl "rt" #+ecl (:require "rt"))
  :components
  ((:module "tests"
    :components
    ((:c-test-lib "libtest")
     (:c-test-lib "libtest2")
     (:c-test-lib "libfsbv")
     (:file "package")
     (:file "bindings" :depends-on ("package" "libtest" "libtest2" "libfsbv"))
     (:file "funcall" :depends-on ("bindings"))
     (:file "defcfun" :depends-on ("bindings"))
     (:file "callbacks" :depends-on ("bindings"))
     (:file "foreign-globals" :depends-on ("package"))
     (:file "memory" :depends-on ("package"))
     (:file "strings" :depends-on ("package"))
     (:file "arrays" :depends-on ("package"))
     (:file "struct" :depends-on ("package"))
     (:file "union" :depends-on ("package"))
     (:file "enum" :depends-on ("package"))
     (:file "fsbv" :depends-on ("bindings" "enum"))
     (:file "misc-types" :depends-on ("bindings"))
     (:file "misc" :depends-on ("bindings"))
     (:file "test-asdf" :depends-on ("package"))
     (:file "grovel" :depends-on ("package")))))
  :perform (test-op (o c) (symbol-call :cffi-tests '#:run-all-cffi-tests)))

(defsystem "cffi-tests/example"
  :defsystem-depends-on ("cffi-grovel")
  :entry-point "cffi-example::entry-point"
  :components
  ((:module "examples" :components
     ((:file "package")
      (:cffi-wrapper-file "wrapper-example" :depends-on ("package"))
      (:cffi-grovel-file "grovel-example" :depends-on ("package"))
      (:file "main-example" :depends-on ("package"))))))

;;; vim: ft=lisp et

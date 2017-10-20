;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; libtest.lisp --- Setup CFFI bindings for libtest.
;;;
;;; Copyright (C) 2005-2007, Luis Oliveira  <loliveira(@)common-lisp.net>
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

(in-package #:cffi-tests)

(define-foreign-library (libtest :type :test)
  (:darwin (:or "libtest.dylib" "libtest32.dylib"))
  (:unix (:or "libtest.so" "libtest32.so"))
  (:windows "libtest.dll")
  (t (:default "libtest")))

(define-foreign-library (libtest2 :type :test)
  (:darwin (:or "libtest2.dylib" "libtest2_32.dylib"))
  (:unix (:or "libtest2.so" "libtest2_32.so"))
  (t (:default "libtest2")))

(define-foreign-library (libfsbv :type :test)
  (:darwin (:or "libfsbv.dylib" "libfsbv32.dylib"))
  (:unix (:or "libfsbv.so" "libfsbv_32.so"))
  (:windows "libfsbv.dll")
  (t (:default "libfsbv")))

(define-foreign-library libc
  (:windows "msvcrt.dll"))

(define-foreign-library libm
  #+(and lispworks darwin) ; not sure why the full path is necessary
  (:darwin "/usr/lib/libm.dylib")
  (t (:default "libm")))

(defmacro deftest (name &rest body)
  (destructuring-bind (name &key expected-to-fail)
      (alexandria:ensure-list name)
    (let ((result `(rt:deftest ,name ,@body)))
      (when expected-to-fail
        (setf result `(progn
                        (when ,expected-to-fail
                          (pushnew ',name rt::*expected-failures*))
                        ,result)))
      result)))

(defun call-within-new-thread (fn &rest args)
  (let (result
        error
        (cv (bordeaux-threads:make-condition-variable))
        (lock (bordeaux-threads:make-lock)))
    (bordeaux-threads:with-lock-held (lock)
      (bordeaux-threads:make-thread
       (lambda ()
         (multiple-value-setq (result error)
           (ignore-errors (apply fn args)))
         (bordeaux-threads:with-lock-held (lock)
           (bordeaux-threads:condition-notify cv))))
      (bordeaux-threads:condition-wait cv lock)
      (values result error))))

;;; As of OSX 10.6.6, loading CoreFoundation on something other than
;;; the initial thread results in a crash.
(deftest load-core-foundation
    (progn
      #+bordeaux-threads
      (call-within-new-thread 'load-foreign-library
                              '(:framework "CoreFoundation"))
      t)
  t)

;;; Return the directory containing the source when compiling or
;;; loading this file.  We don't use *LOAD-TRUENAME* because the fasl
;;; file may be in a different directory than the source with certain
;;; ASDF extensions loaded.
(defun load-directory ()
  (let ((here #.(or *compile-file-truename* *load-truename*)))
    (make-pathname :name nil :type nil :version nil
                   :defaults here)))

(defun load-test-libraries ()
  (let ((*foreign-library-directories* (list (load-directory))))
    (load-foreign-library 'libtest)
    (load-foreign-library 'libtest2)
    (load-foreign-library 'libfsbv)
    (load-foreign-library 'libc)
    #+(or abcl lispworks) (load-foreign-library 'libm)))

#-(:and :ecl (:not :dffi))
(load-test-libraries)

#+(:and :ecl (:not :dffi))
(ffi:load-foreign-library
 #.(make-pathname :name "libtest" :type "so"
                  :defaults (or *compile-file-truename* *load-truename*)))

;;; check libtest version
(defparameter *required-dll-version* "20120107")

(defcvar "dll_version" :string)

(unless (string= *dll-version* *required-dll-version*)
  (error "version check failed: expected ~s but libtest reports ~s"
         *required-dll-version*
         *dll-version*))

;;; The maximum and minimum values for single and double precision C
;;; floating point values, which may be quite different from the
;;; corresponding Lisp versions.
(defcvar "float_max" :float)
(defcvar "float_min" :float)
(defcvar "double_max" :double)
(defcvar "double_min" :double)

(defun run-cffi-tests (&key (compiled nil))
  (let ((regression-test::*compile-tests* compiled)
        (*package* (find-package '#:cffi-tests)))
    (format t "~&;;; running tests (~Acompiled)" (if compiled "" "un"))
    (do-tests)
    (set-difference (regression-test:pending-tests)
                    regression-test::*expected-failures*)))

(defun run-all-cffi-tests ()
  (append (run-cffi-tests :compiled nil)
          (run-cffi-tests :compiled t)))

(defmacro expecting-error (&body body)
  `(handler-case (progn ,@body :no-error)
     (error () :error)))

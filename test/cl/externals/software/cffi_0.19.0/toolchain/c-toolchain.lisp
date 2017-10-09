;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; c-toolchain.lisp --- Generic support compiling and linking C code.
;;;
;;; Copyright (C) 2005-2006, Dan Knap <dankna@accela.net>
;;; Copyright (C) 2005-2006, Emily Backes <lucca@accela.net>
;;; Copyright (C) 2007, Stelian Ionescu <sionescu@cddr.org>
;;; Copyright (C) 2007, Luis Oliveira <loliveira@common-lisp.net>
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

(in-package #:cffi-toolchain)

;;; Utils

(defun parse-command-flags (flags)
  (let ((separators '(#\Space #\Tab #\Newline #\Return)))
    (remove-if 'emptyp (split-string flags :separator separators))))

(defun parse-command-flags-list (strings)
  (loop for flags in strings append (parse-command-flags flags)))

(defun program-argument (x)
  (etypecase x
    (string x)
    (pathname (native-namestring x))))

(defun invoke (command &rest args)
  (when (pathnamep command)
    (setf command (native-namestring command))
    #+os-unix
    (unless (absolute-pathname-p command)
      (setf command (strcat "./" command))))
  (let ((cmd (cons command (mapcar 'program-argument args))))
    (safe-format! *debug-io* "; ~A~%" (escape-command cmd))
    (run-program cmd :output :interactive :error-output :interactive)))


;;; C support

(defparameter *cc* nil "C compiler")
(defparameter *cc-flags* nil "flags for the C compiler")
(defparameter *ld* nil "object linker") ;; NB: can actually be the same as *cc*
(defparameter *ld-exe-flags* nil "flags for linking executables via *ld*")
(defparameter *ld-dll-flags* nil "flags for linking shared library via *ld*")
(defparameter *linkkit-start* nil "flags for the implementation linkkit, start")
(defparameter *linkkit-end* nil "flags for the implementation linkkit, end")

(defun link-all-library (lib)
  ;; Flags to provide to cc to link a whole library into an executable
  (when lib
    (if (featurep :darwin) ;; actually, LLVM ld vs GNU ld
        `("-Wl,-force_load" ,lib)
        `("-Wl,--whole-archive" ,lib "-Wl,--no-whole-archive"))))

(defun normalize-flags (directory flags)
  (loop for val in (parse-command-flags flags) collect
        (cond
          ((find (first-char val) "-+/") val)
          ((probe-file* (subpathname directory val)))
          (t val))))

(defun implementation-file (file &optional type)
  (subpathname (lisp-implementation-directory) file
               :type (bundle-pathname-type type)))

;; TODO: on CCL, extract data from
;; (pathname (strcat "ccl:lisp-kernel/" (ccl::kernel-build-directory) "/Makefile")) ?

#+clisp
(progn
  (defparameter *clisp-toolchain-parameters*
    '(("CC" *cc*)
      ("CFLAGS" *cc-flags* t)
      ("CLFLAGS" *cc-exe-flags* t)
      ("LIBS" *linkkit-start* t)
      ("X_LIBS" *linkkit-end* t)))
  (defun clisp-toolchain-parameters (&optional linkset)
    (nest
     (let* ((linkset (ensure-pathname
                      (or linkset "base")
                      :defaults (lisp-implementation-directory)
                      :ensure-absolute t
                      :ensure-directory t
                      :want-existing t))
            (makevars (subpathname linkset "makevars"))))
     (with-input-file (params makevars :if-does-not-exist nil))
     (when params)
     (loop for l = (read-line params nil nil) while l
           finally (appendf *linkkit-start* (normalize-flags linkset "modules.o")) do)
     (if-let (p (position #\= l)))
     (let ((var (subseq l 0 p))
           ;; strip the start and end quote characters
           (val (subseq l (+ p 2) (- (length l) 1)))))
     (if-let (param (cdr (assoc var *clisp-toolchain-parameters* :test 'equal))))
     (destructuring-bind (sym &optional normalizep) param
       (setf (symbol-value sym)
             (if normalizep (normalize-flags linkset val) val))))
    (setf *ld* *cc*
          *ld-exe-flags* `(,@*cc-flags* #-darwin "-Wl,--export-dynamic")
          *ld-dll-flags* (list* #+darwin "-dynamiclib" ;; -bundle ?
                                #-darwin "-shared"
                                *cc-flags*))))

;; TODO: for CMUCL, see whatever uses its linker.sh,
;; and teach it to accept additional objects / libraries
;; as it links a runtime plus a core into an executable

#+ecl
(defun ecl-toolchain-parameters ()
  (setf *cc* c:*cc*
        *cc-flags* `(,@(parse-command-flags c::*cc-flags*)
                     ,@(parse-command-flags c:*user-cc-flags*))
        ;; For the below, we just use c::build-FOO
        *ld* *cc*
        *ld-exe-flags* *cc-flags*
        *ld-dll-flags* *cc-flags*
        *linkkit-start* nil
        *linkkit-end* nil))

#+mkcl
(defun mkcl-toolchain-parameters ()
  (setf *cc* compiler::*cc*
        *cc-flags* (parse-command-flags compiler::*cc-flags*)
        ;; For the below, we just use compiler::build-FOO
        *ld* *cc*
        *ld-exe-flags* *cc-flags*
        *ld-dll-flags* *cc-flags*
        *linkkit-start* nil
        *linkkit-end* nil))

#+sbcl
(progn
  (defparameter *sbcl-toolchain-parameters*
    '(("CC" *cc*)
      ("CFLAGS" *cc-flags* t)
      ("LINKFLAGS" *ld-exe-flags* t)
      ("USE_LIBSBCL" *linkkit-start* t)
      ("LIBS" *linkkit-end* t)))
  (defun sbcl-toolchain-parameters ()
    (nest
     (let* ((sbcl-home (lisp-implementation-directory))
            (sbcl.mk (subpathname sbcl-home "sbcl.mk"))))
     (with-input-file (params sbcl.mk :if-does-not-exist nil))
     (when params)
     (loop for l = (read-line params nil nil) while l
           finally (appendf *linkkit-end* '("-lm")) do)
     (if-let (p (position #\= l)))
     (let ((var (subseq l 0 p))
           (val (subseq l (1+ p)))))
     (if-let (param (cdr (assoc var *sbcl-toolchain-parameters* :test 'equal))))
     (destructuring-bind (sym &optional normalizep) param
       (setf (symbol-value sym)
             (if normalizep (normalize-flags sbcl-home val) val))))
    (unless (featurep :sb-linkable-runtime)
      (setf *linkkit-start* nil *linkkit-end* nil))
    (setf *ld* *cc* ;; !
          *ld-dll-flags* (list* #+darwin "-dynamiclib" #-darwin "-shared"
                                *cc-flags*))))

(defun default-toolchain-parameters ()
  ;; The values below are legacy guesses from previous versions of CFFI.
  ;; It would be nice to clean them up, remove unneeded guesses,
  ;; annotate every guess with some comment explaining the context.
  ;; TODO: have proper implementation-provided linkkit parameters
  ;; for all implementations as above, and delete the below altogether.
  (let ((arch-flags
         ;; Former *cpu-word-size-flags*
         #+arm '("-marm")
         #+arm64 '()
         #-(or arm arm64)
         (ecase (cffi:foreign-type-size :pointer)
                (4 '("-m32"))
                (8 '("-m64")))))
    (setf *cc*
          (or (getenvp "CC")
              #+(or cygwin (not windows)) "cc"
              "gcc")
          *cc-flags*
          (or (getenv "CFLAGS")
              (append
               arch-flags
               ;; For MacPorts
               #+darwin (list "-I" "/opt/local/include/")
               ;; ECL internal flags
               #+ecl (parse-command-flags c::*cc-flags*)
               ;; FreeBSD non-base header files
               #+freebsd (list "-I" "/usr/local/include/")))
          *ld* *cc*
          *ld-exe-flags* `(,@arch-flags #-darwin "-Wl,--export-dynamic")
          *ld-dll-flags* (list* #+darwin "-dynamiclib" ;; -bundle ?
                                #-darwin "-shared"
                                *cc-flags*)
          *linkkit-start* nil
          *linkkit-end* nil)))

(defun ensure-toolchain-parameters ()
  #+clisp (unless *cc* (clisp-toolchain-parameters))
  #+ecl (unless *cc* (ecl-toolchain-parameters))
  #+mkcl (unless *cc* (mkcl-toolchain-parameters))
  #+sbcl (unless *cc* (sbcl-toolchain-parameters))
  (unless *cc* (default-toolchain-parameters)))

;; Actually initialize toolchain parameters
(ignore-errors (ensure-toolchain-parameters))


(defun call-with-temporary-output (output-file fun)
  (let ((output-file (ensure-pathname output-file :want-file t :ensure-absolute t :truenamize t)))
    (with-temporary-file
        (:pathname tmp :direction :output
         :prefix (strcat (native-namestring (pathname-directory-pathname output-file))
                         (pathname-name output-file) "-tmp")
         :suffix ""
         :type (pathname-type output-file))
      (funcall fun tmp)
      (rename-file-overwriting-target tmp output-file))))

(defmacro with-temporary-output ((output-file-var &optional (output-file-val output-file-var))
                                 &body body)
  "Create an output file atomically, by executing the BODY while OUTPUT-FILE-VAR
is bound to a temporary file name, then atomically renaming that temporary file to OUTPUT-FILE-VAL."
  `(call-with-temporary-output ,output-file-val (lambda (,output-file-var) ,@body)))

(defun invoke-builder (builder output-file &rest args)
  "Invoke the C Compiler with given OUTPUT-FILE and arguments ARGS"
  (with-temporary-output (output-file)
    (apply 'invoke `(,@builder ,output-file ,@args))))

(defun cc-compile (output-file inputs)
  (apply 'invoke-builder (list *cc* "-o") output-file
         "-c" (append *cc-flags* #-windows '("-fPIC") inputs)))

(defun link-executable (output-file inputs)
  (apply 'invoke-builder (list *ld* "-o") output-file
         (append *ld-exe-flags* inputs)))

(defun link-lisp-executable (output-file inputs)
  #+ecl
  (let ((c::*ld-flags*
         (format nil "-Wl,--export-dynamic ~@[ ~A~]"
                 c::*ld-flags*)))
    (c::build-program output-file :lisp-files inputs))
  #+mkcl (compiler::build-program
          output-file :lisp-object-files (mapcar 'program-argument inputs)
          :on-missing-lisp-object-initializer nil)
  #-(or ecl mkcl)
  (link-executable output-file `(,@*linkkit-start* ,@inputs ,@*linkkit-end*)))

(defun link-static-library (output-file inputs)
  #+ecl (c::build-static-library output-file :lisp-files inputs)
  #+mkcl (compiler::build-static-library
          output-file :lisp-object-files (mapcar 'program-argument inputs)
          :on-missing-lisp-object-initializer nil)
  #-(or ecl mkcl)
  (with-temporary-output (output-file)
    (delete-file-if-exists output-file)
    #+(or bsd linux windows)
    (apply 'invoke
           `(;; TODO: make it portable to BSD.
             ;; ar D is also on FreeBSD, but not on OpenBSD or Darwin, dunno about NetBSD;
             ;; ar T seems to only be on Linux (means something different on Darwin). Sigh.
             ;; A MRI script might be more portable... not, only supported by GNU binutils.
             ;; I couldn't get libtool to work, and it's not ubiquitous anyway.
             ;; ,@`("libtool" "--mode=link" ,*cc* ,@*cc-flags* "-static" "-o" ,output-file)
             ;; "Solution": never link .a's into further .a's, only link .o's into .a's,
             ;; which implied changes that are now the case in ASDF 3.2.0.
             #+bsd ,@`("ar" "rcs" ,output-file) ;; NB: includes darwin
             #+linux ,@`("ar" "rcsDT" ,output-file)
             #+windows ,@`("lib" "-nologo" ,(strcat "-out:" (native-namestring output-file)))
             ,@inputs))
    #-(or bsd linux windows)
    (error "Not implemented on your system")))

(defun link-shared-library (output-file inputs)
  ;; remove the library so we won't possibly be overwriting
  ;; the code of any existing process
  (delete-file-if-exists output-file)
  #+ecl (c::build-shared-library output-file :lisp-files inputs)
  #+mkcl (compiler::build-shared-library
          output-file :lisp-object-files (mapcar 'program-argument inputs)
          :on-missing-lisp-object-initializer nil)
  #-(or ecl mkcl)
  ;; Don't use a temporary file, because linking is sensitive to the output file name :-/ (or put it in a temporary directory?)
  (apply 'invoke *ld* "-o" output-file
         (append *ld-dll-flags* inputs)))


;;; Computing file names

(defun make-c-file-name (output-defaults &optional suffix)
  (make-pathname :type "c"
                 :name (strcat (pathname-name output-defaults) suffix)
                 :defaults output-defaults))

(defun make-o-file-name (output-defaults &optional suffix)
  (make-pathname :type (bundle-pathname-type :object)
                 :name (format nil "~A~@[~A~]" (pathname-name output-defaults) suffix)
                 :defaults output-defaults))

(defun make-so-file-name (defaults)
  (make-pathname :type (bundle-pathname-type :shared-library)
                 :defaults defaults))

(defun make-exe-file-name (defaults)
  (make-pathname :type (bundle-pathname-type :program)
                 :defaults defaults))


;;; Implement link-op on image-based platforms.
#-(or clasp ecl mkcl)
(defmethod perform ((o link-op) (c system))
  (let* ((inputs (input-files o c))
         (output (first (output-files o c)))
         (kind (bundle-type o)))
    (when output ;; some operations skip any output when there is no input
      (ecase kind
        (:program (link-executable output inputs))
        ((:lib :static-library) (link-static-library output inputs))
        ((:dll :shared-library) (link-shared-library output inputs))))))

(defclass c-file (source-file)
  ((cflags :initarg :cflags :initform :default)
   (type :initform "c")))

(defmethod output-files ((o compile-op) (c c-file))
  (let* ((i (first (input-files o c)))
         (base (format nil "~(~{~a~^__~}~)"
                       (mapcar (lambda (x) (substitute-if #\_ (complement #'alphanumericp) x))
                               (component-find-path c))))
         (path (make-pathname :defaults i :name base)))
    (list (make-o-file-name path)
          (make-so-file-name path))))

(defmethod perform ((o compile-op) (c c-file))
  (let ((i (first (input-files o c))))
    (destructuring-bind (.o .so) (output-files o c)
      (cc-compile .o (list i))
      (link-shared-library .so (list .o)))))

(defmethod perform ((o load-op) (c c-file))
  (let ((o (second (input-files o c))))
    (cffi:load-foreign-library (file-namestring o) :search-path (list (pathname-directory-pathname o)))))

(setf (find-class 'asdf::c-file) (find-class 'c-file))

(defclass o-file (source-file)
  ((cflags :initarg :cflags :initform :default)
   (type :initform (bundle-pathname-type :object)))
  (:documentation "class for pre-compile object components"))

(defmethod output-files ((op compile-op) (c o-file))
  (let* ((o (first (input-files op c)))
         (so (apply-output-translations (make-so-file-name o))))
    (values (list o so) t)))

(defmethod perform ((o load-op) (c o-file))
  (let ((so (second (input-files o c))))
    (cffi:load-foreign-library (file-namestring so) :search-path (list (pathname-directory-pathname so)))))

(setf (find-class 'asdf::o-file) (find-class 'o-file))


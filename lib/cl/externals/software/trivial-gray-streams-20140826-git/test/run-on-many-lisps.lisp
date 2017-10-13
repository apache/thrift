(ql:quickload :trivial-gray-streams)
(ql:quickload :test-grid-agent)
(ql:quickload :cl-fad)
(in-package :cl-user)

(defparameter *abcl* (make-instance 'lisp-exe:abcl
                                    :java-exe-path "C:\\Program Files\\Java\\jdk1.6.0_26\\bin\\java"
                                    :abcl-jar-path "C:\\Users\\anton\\unpacked\\abcl\\abcl-bin-1.1.0\\abcl.jar"))
(defparameter *clisp* (make-instance 'lisp-exe:clisp :exe-path "clisp"))
(defparameter *ccl-1.8-x86* (make-instance 'lisp-exe:ccl
                                           :exe-path "C:\\Users\\anton\\unpacked\\ccl\\ccl-1.8-windows\\wx86cl.exe"))
(defparameter *ccl-1.8-x86-64* (make-instance 'lisp-exe:ccl
                                              :exe-path "C:\\Users\\anton\\unpacked\\ccl\\ccl-1.8-windows\\wx86cl64.exe"))
(defparameter *sbcl-1.1.0.45* (make-instance 'lisp-exe:sbcl :exe-path "C:\\Program Files (x86)\\Steel Bank Common Lisp\\1.1.0.45\\run.bat"))
(defparameter *sbcl-win-branch-64* (make-instance 'lisp-exe:sbcl :exe-path "C:\\Program Files\\Steel Bank Common Lisp\\1.1.0.36.mswinmt.1201-284e340\\run.bat"))
(defparameter *sbcl-win-branch-32* (make-instance 'lisp-exe:sbcl :exe-path "C:\\Program Files (x86)\\Steel Bank Common Lisp\\1.1.0.36.mswinmt.1201-284e340\\run.bat"))
(defparameter *ecl-bytecode* (make-instance 'lisp-exe:ecl
                                            :exe-path "C:\\Users\\anton\\projects\\ecl\\bin\\ecl.exe"
                                            :compiler :bytecode))
(defparameter *ecl-lisp-to-c* (make-instance 'lisp-exe:ecl
                                             :exe-path "C:\\Users\\anton\\projects\\ecl\\bin\\ecl.exe"
                                             :compiler :lisp-to-c))
(defparameter *acl* (make-instance 'lisp-exe:acl :exe-path "C:\\Program Files (x86)\\acl90express\\alisp.exe"))

(defun run-on-many-lisps (run-description test-run-dir quicklisp-dir lisps)
  (ensure-directories-exist test-run-dir)
  (let ((fasl-root (merge-pathnames "fasl/" test-run-dir)))
    (labels ((log-name (lisp)
               (substitute #\- #\.
                           ;; Substitute dots by hypens if our main process is CCL, it 
                           ;; prepends the > symbol before dots;
                           ;; for example: 1.1.0.36.mswinmt.1201-284e340 => 1>.1>.0>.36>.mswinmt.1201-284e340
                           ;; When we pass such a pathname to another lisps, they can't handle it.
                           (string-downcase (tg-agent::implementation-identifier lisp))))
             (fasl-dir (lisp)
               (merge-pathnames (format nil "~A/" (log-name lisp))
                                fasl-root))
             (run (lisp)
               (let* ((lib-result (tg-agent::proc-run-libtest lisp
                                                              :trivial-gray-streams
                                                              run-description
                                                              (merge-pathnames (log-name lisp) test-run-dir)
                                                              quicklisp-dir
                                                              (fasl-dir lisp)))
                      (status (getf lib-result :status)))
                 (if (listp status)
                     (getf status :failed-tests)
                     status))))
      (let ((results (mapcar (lambda (lisp)
                               (list (tg-agent::implementation-identifier lisp)
                                     (run lisp)))
                             lisps)))
        (tg-utils::write-to-file results (merge-pathnames "resutls.lisp" test-run-dir))
        (cl-fad:delete-directory-and-files fasl-root)
        results))))

(run-on-many-lisps '(:lib-world "quicklisp 2013-02-17 + trivial-gray-streams.head"
                     :contact-email "avodonosov@yandex.ru")
                   "C:\\Users\\anton\\projects\\trivial-gray-streams\\test\\"
                   (merge-pathnames "quicklisp/" (user-homedir-pathname))
                   (list *sbcl-1.1.0.45* *sbcl-win-branch-64* *sbcl-win-branch-32*
                         *abcl*
                         *clisp*
                         *ccl-1.8-x86* *ccl-1.8-x86-64*                         
                         *ecl-bytecode* *ecl-lisp-to-c*
                         *acl*))

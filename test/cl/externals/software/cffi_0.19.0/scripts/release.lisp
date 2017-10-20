#!/usr/bin/env clisp
;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(defpackage :release-script (:use #:cl #:regexp))
(in-package :release-script)

;;;; Configuration ------------------------------------------------------------

(defparameter *project-name* "cffi")
(defparameter *asdf-file* (format nil "~A.asd" *project-name*))

(defparameter *host* "common-lisp.net")
(defparameter *release-dir*
  (format nil "/project/~A/public_html/releases" *project-name*))

(defparameter *version-file* "VERSION")
(defparameter *version-file-dir*
  (format nil "/project/~A/public_html" *project-name*))

;;;; --------------------------------------------------------------------------

;;;; Utilities

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defmacro string-case (expression &body clauses)
  `(let ((it ,expression)) ; yes, anaphoric, deal with it.
     (cond
       ,@(loop for clause in clauses collect
               `((or ,@(loop for alternative in (ensure-list (first clause))
                             collect (or (eq t alternative)
                                         `(string= it ,alternative))))
                 ,@(rest clause))))))

(defparameter *development-mode* nil)

(defun die (format-control &rest format-args)
  (format *error-output* "~?" format-control format-args)
  (if *development-mode*
      (cerror "continue" "die")
      (ext:quit 1)))

(defun numeric-split (string)
  (if (digit-char-p (char string 0))
      (multiple-value-bind (number next-position)
          (parse-integer string :junk-allowed t)
        (cons number (when (< next-position (length string))
                       (numeric-split (subseq string next-position)))))
      (let ((next-digit-position (position-if #'digit-char-p string)))
        (if next-digit-position
            (cons (subseq string 0 next-digit-position)
                  (numeric-split (subseq string next-digit-position)))
            (list string)))))

(defun natural-string-< (s1 s2)
  (labels ((aux< (l1 l2)
             (cond ((null l1) (not (null l2)))
                   ((null l2) nil)
                   (t (destructuring-bind (x . xs) l1
                        (destructuring-bind (y . ys) l2
                          (cond ((and (numberp x) (stringp y))
                                 t)
                                ((and (numberp y) (stringp x))
                                 nil)
                                ((and (numberp x) (numberp y))
                                 (or (< x y) (and (= x y) (aux< xs ys))))
                                (t
                                 (or (string-lessp x y)
                                     (and (string-equal x y)
                                          (aux< xs ys)))))))))))
    (aux< (numeric-split s1)
          (numeric-split s2))))

;;;; Running commands

(defparameter *dry-run* nil)

(defun cmd? (format-control &rest format-args)
  (let ((cmd (format nil "~?" format-control format-args)))
    (with-open-stream (s1 (ext:run-shell-command cmd :output :stream))
      (loop for line = (read-line s1 nil nil)
            while line
            collect line))))

;; XXX: quote arguments.
(defun cmd (format-control &rest format-args)
  (when *development-mode*
    (format *debug-io* "CMD: ~?~%" format-control format-args))
  (let ((ret (ext:run-shell-command (format nil "~?" format-control format-args))))
    (or (null ret)
        (zerop ret))))

(defun cmd! (format-control &rest format-args)
  (or (apply #'cmd format-control format-args)
      (die "cmd '~?' failed." format-control format-args)))

(defun maybe-cmd! (format-control &rest format-args)
  (if *dry-run*
      (format t "SUPPRESSING: ~?~%" format-control format-args)
      (apply #'cmd! format-control format-args)))

;;;;

(defun find-current-version ()
  (subseq (reduce (lambda (x y) (if (natural-string-< x y) y x))
                  (or (cmd? "git tag -l v\\*")
                      (die "no version tags found. Please specify initial version.")))
          1))

(defun parse-version (string)
  (mapcar (lambda (x)
            (parse-integer x :junk-allowed t))
          (loop repeat 3 ; XXX: parameterize
                for el in (regexp-split "\\." (find-current-version))
                collect el)))

(defun check-for-unrecorded-changes (&optional force)
  (unless (cmd "git diff --exit-code")
    (write-line "Unrecorded changes.")
    (if force
        (write-line "Continuing anyway.")
        (die "Aborting.~@
              Use -f or --force if you want to make a release anyway."))))

(defun new-version-number-candidates (current-version)
  (labels ((alternatives (before after)
             (when after
               (cons (append before
                             (list (1+ (first after)))
                             (mapcar (constantly 0) (rest after)))
                     (alternatives (append before (list (first after)))
                                   (rest after))))))
    (loop for alt in (alternatives nil (parse-version current-version))
          collect (format nil "~{~d~^.~}" alt))))

(defun ask-user-for-version (current-version next-versions)
  (format *query-io* "Current version is ~A. Which will be the next one?~%"
          current-version)
  (loop for i from 1 and version in next-versions
        do (format *query-io* "~T~A) ~A~%" i version))
  (format *query-io* "? ")
  (finish-output *query-io*)
  (nth (1- (parse-integer (read-line) :junk-allowed t))
       next-versions))

(defun git-tag-tree (version)
  (write-line "Tagging the tree...")
  (maybe-cmd! "git tag \"v~A\"" version))

(defun add-version-to-system-file (version path-in path-out)
  (let ((defsystem-line (format nil "(defsystem :~A" *project-name*)))
    (with-open-file (in path-in :direction :input)
      (with-open-file (out path-out :direction :output)
        (loop for line = (read-line in nil nil) while line
              do (write-line line out)
              when (string= defsystem-line line)
                do (format out "  :version ~s~%" version))))))

(defun create-dist (version distname)
  (write-line "Creating distribution...")
  (cmd! "mkdir \"~a\"" distname)
  (cmd! "git archive master | tar xC \"~A\"" distname)
  (format t "Updating ~A with new version: ~A~%" *asdf-file* version)
  (let* ((asdf-file-path (format nil "~A/~A" distname *asdf-file*))
         (tmp-asdf-file-path (format nil "~a.tmp" asdf-file-path)))
    (add-version-to-system-file version asdf-file-path tmp-asdf-file-path)
    (cmd! "mv \"~a\" \"~a\"" tmp-asdf-file-path asdf-file-path)))

(defun tar-and-sign (distname tarball)
  (write-line "Creating and signing tarball...")
  (cmd! "tar czf \"~a\" \"~a\"" tarball distname)
  (cmd! "gpg -b -a \"~a\"" tarball))

(defparameter *remote-directory* (format nil "~A:~A" *host* *release-dir*))

(defun upload-tarball (tarball signature remote-directory)
  (write-line "Copying tarball to web server...")
  (maybe-cmd! "scp \"~A\" \"~A\" \"~A\"" tarball signature remote-directory)
  (format t "Uploaded ~A and ~A.~%" tarball signature))

(defun update-remote-links (tarball signature host release-dir project-name)
  (format t "Updating ~A_latest links...~%" project-name)
  (maybe-cmd! "ssh \"~A\" ln -sf \"~A\" \"~A/~A_latest.tar.gz\""
              host tarball release-dir project-name)
  (maybe-cmd! "ssh \"~A\" ln -sf \"~A\" \"~A/~A_latest.tar.gz.asc\""
              host signature release-dir project-name))

(defun upload-version-file (version version-file host version-file-dir)
  (format t "Uploading ~A...~%" version-file)
  (with-open-file (out version-file :direction :output)
    (write-string version out))
  (maybe-cmd! "scp \"~A\" \"~A\":\"~A\"" version-file host version-file-dir)
  (maybe-cmd! "rm \"~A\"" version-file))

(defun maybe-clean-things-up (tarball signature)
  (when (y-or-n-p "Clean local tarball and signature?")
    (cmd! "rm \"~A\" \"~A\"" tarball signature)))

(defun run (force version)
  (check-for-unrecorded-changes force)
  ;; figure out what version we'll be preparing.
  (unless version
    (let* ((current-version (find-current-version))
           (next-versions (new-version-number-candidates current-version)))
      (setf version (or (ask-user-for-version current-version next-versions)
                        (die "invalid selection.")))))
  (git-tag-tree version)
  (let* ((distname (format nil "~A_~A" *project-name* version))
         (tarball (format nil "~A.tar.gz" distname))
         (signature (format nil "~A.asc" tarball)))
    ;; package things up.
    (create-dist version distname)
    (tar-and-sign distname tarball)
    ;; upload.
    (upload-tarball tarball signature *remote-directory*)
    (update-remote-links tarball signature *host* *release-dir* *project-name*)
    (when *version-file*
      (upload-version-file version *version-file* *host* *version-file-dir*))
    ;; clean up.
    (maybe-clean-things-up tarball signature)
    ;; documentation.
    (write-line "Building and uploading documentation...")
    (maybe-cmd! "make -C doc upload-docs")
    ;; push tags and any outstanding changes.
    (write-line "Pushing tags and changes...")
    (maybe-cmd! "git push --tags origin master")))


;;;; Do it to it

(let ((force nil)
      (version nil)
      (args ext:*args*))
  (loop while args
        do (string-case (pop args)
             (("-h" "--help")
              (write-line "No help, sorry. Read the source.")
              (ext:quit 0))
             (("-f" "--force")
              (setf force t))
             (("-v" "--version")
              (setf version (pop args)))
             (("-n" "--dry-run")
              (setf *dry-run* t))
             (t
              (die "Unrecognized argument '~a'" it))))
  (run force version))

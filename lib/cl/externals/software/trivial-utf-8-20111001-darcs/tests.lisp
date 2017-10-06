(defpackage :trivial-utf-8-tests
  (:use :common-lisp :trivial-utf-8))

(in-package :trivial-utf-8-tests)

;; TODO this could be a lot nicer and more exhaustive.

(defparameter *my-path* (asdf:component-pathname (asdf:find-component nil :trivial-utf-8)))
(defparameter *test-file* (merge-pathnames #P"test.txt" *my-path*))
(defparameter *output-file* (merge-pathnames #P"test-out.txt" *my-path*))

;; Read the content of a file.
(let ((content (with-open-file (input *test-file* :direction :input
				      :element-type '(unsigned-byte 8))
		 (read-utf-8-string input :stop-at-eof t))))
  ;; Is the amount of characters correct?
  (assert (= (length content) 616))
  ;; See if encoding and decoding the string leaves it intact.
  (assert (string= (utf-8-bytes-to-string (string-to-utf-8-bytes content)) content))
  (with-open-file (output *output-file* :direction :output :element-type '(unsigned-byte 8)
			  :if-exists :supersede)
    ;; Write it to another file.
    (write-utf-8-bytes content output)))

;; Check whether the files are really the same.
(with-open-file (original *test-file* :direction :input :element-type '(unsigned-byte 8))
  (with-open-file (new *output-file* :direction :input :element-type '(unsigned-byte 8))
    (loop :for byte1 = (read-byte original nil :eof)
	  :for byte2 = (read-byte new nil :eof)
	  :do (assert (equal byte1 byte2))
	  :while (not (eq byte1 :eof)))))

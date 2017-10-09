;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; strings.lisp --- Tests for foreign string conversion.
;;;
;;; Copyright (C) 2005, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
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

;;;# Foreign String Conversion Tests
;;;
;;; With the implementation of encoding support, there are a lot of
;;; things that can go wrong with foreign string conversions.  This is
;;; a start at defining tests for strings and encoding conversion, but
;;; there needs to be a lot more.

(babel:enable-sharp-backslash-syntax)

;;; *ASCII-TEST-STRING* contains the characters in the ASCII character
;;; set that we will convert to a foreign string and check against
;;; *ASCII-TEST-BYTES*.  We don't bother with control characters.
;;;
;;; FIXME: It would probably be good to move these tables into files
;;; in "tests/", especially if we ever want to get fancier and have
;;; tests for more encodings.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ascii-test-string*
    (concatenate 'string " !\"#$%&'()*+,-./0123456789:;"
                 "<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]"
                 "^_`abcdefghijklmnopqrstuvwxyz{|}~")))

;;; *ASCII-TEST-BYTES* contains the expected ASCII encoded values
;;; for each character in *ASCII-TEST-STRING*.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ascii-test-bytes*
    (let ((vector (make-array 95 :element-type '(unsigned-byte 8))))
      (loop for i from 0
            for code from 32 below 127
            do (setf (aref vector i) code)
            finally (return vector)))))

;;; Test basic consistency converting a string to and from Lisp using
;;; the default encoding.
(deftest string.conversion.basic
    (with-foreign-string (s *ascii-test-string*)
      (foreign-string-to-lisp s))
  #.*ascii-test-string* 95)

(deftest string.conversion.basic.2
    (with-foreign-string ((ptr size) "123" :null-terminated-p nil)
      (values (foreign-string-to-lisp ptr :count 3) size))
  "123" 3)

;;; Ensure that conversion of *ASCII-TEST-STRING* to a foreign buffer
;;; and back preserves ASCII encoding.
(deftest string.encoding.ascii
    (with-foreign-string (s *ascii-test-string* :encoding :ascii)
      (let ((vector (make-array 95 :element-type '(unsigned-byte 8))))
        (loop for i from 0 below (length vector)
              do (setf (aref vector i) (mem-ref s :unsigned-char i)))
        vector))
  #.*ascii-test-bytes*)

;;; FIXME: bogus test. We need support for BOM or UTF-16{BE,LE}.
(pushnew 'string.encoding.utf-16.basic rtest::*expected-failures*)

;;; Test UTF-16 conversion of a string back and forth.  Tests proper
;;; null terminator handling for wide character strings and ensures no
;;; byte order marks are added.  (Why no BOM? --luis)
;;;
;;; FIXME: an identical test using :UTF-16 wouldn't work because on
;;; little-endian architectures, :UTF-16 defaults to little-endian
;;; when writing and big-endian on reading because the BOM is
;;; suppressed.
#-babel::8-bit-chars
(progn
  (deftest string.encoding.utf-16le.basic
      (with-foreign-string (s *ascii-test-string* :encoding :utf-16le)
        (foreign-string-to-lisp s :encoding :utf-16le))
    #.*ascii-test-string* 190)

  (deftest string.encoding.utf-16be.basic
      (with-foreign-string (s *ascii-test-string* :encoding :utf-16be)
        (foreign-string-to-lisp s :encoding :utf-16be))
    #.*ascii-test-string* 190))

;;; Ensure that writing a long string into a short buffer does not
;;; attempt to write beyond the edge of the buffer, and that the
;;; resulting string is still null terminated.
(deftest string.short-write.1
    (with-foreign-pointer (buf 6)
      (setf (mem-ref buf :unsigned-char 5) 70)
      (lisp-string-to-foreign "ABCDE" buf 5 :encoding :ascii)
      (values (mem-ref buf :unsigned-char 4)
              (mem-ref buf :unsigned-char 5)))
  0 70)

#-babel::8-bit-chars
(deftest string.encoding.utf-8.basic
    (with-foreign-pointer (buf 7 size)
      (let ((string (concatenate 'babel:unicode-string
                                 '(#\u03bb #\u00e3 #\u03bb))))
        (lisp-string-to-foreign string buf size :encoding :utf-8)
        (loop for i from 0 below size
              collect (mem-ref buf :unsigned-char i))))
  (206 187 195 163 206 187 0))

(defparameter *basic-latin-alphabet* "abcdefghijklmnopqrstuvwxyz")

(deftest string.encodings.all.basic
    (let (failed)
      ;;; FIXME: UTF-{32,16} and friends fail due to lack of BOM. See
      ;;; STRING.ENCODING.UTF-16.BASIC for more details.
      (dolist (encoding (remove-if (lambda (x)
                                     (member x '(:utf-32 :utf-16 :ucs-2)))
                                   (babel:list-character-encodings)))
        ;; (format t "Testing ~S~%" encoding)
        (with-foreign-string (ptr *basic-latin-alphabet* :encoding encoding)
          (let ((string (foreign-string-to-lisp ptr :encoding encoding)))
            ;; (format t "  got ~S~%" string)
            (unless (string= *basic-latin-alphabet* string)
              (push encoding failed)))))
      failed)
  nil)

;;; rt: make sure *default-foreign-enconding* binds to a keyword
(deftest string.encodings.default
    (keywordp *default-foreign-encoding*)
  t)

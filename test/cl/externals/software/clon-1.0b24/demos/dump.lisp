;;; dump.lisp --- ECL demos dumping code

;; Copyright (C) 2010-2012, 2015 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Clon.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(require :asdf)

(defconstant +executable+
  (second (member "--" (si:command-args) :test #'string=)))
(defconstant +source+ (concatenate 'string +executable+ ".lisp"))
(defconstant +object+ (concatenate 'string +executable+ ".o"))

(asdf:load-system :net.didierverna.clon)

(compile-file +source+ :output-file +object+ :system-p t)

;; #### NOTE: this only dumps an executable without Clon in the image. Clon
;; will still be loaded dynamically by ASDF every time the program is run.
(c:build-program +executable+ :lisp-files (list +object+))

(si:exit 0)

;;; dump.lisp ends here

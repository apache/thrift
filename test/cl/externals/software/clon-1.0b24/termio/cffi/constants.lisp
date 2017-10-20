;;; constants.lisp --- CFFI operating system interface

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

(in-package :net.didierverna.clon)

(define "winsize_t" "struct winsize")

(include "sys/ioctl.h")
(include "errno.h")

(constant (+tiocgwinsz+ "TIOCGWINSZ") :type integer)
(constant (+enotty+     "ENOTTY")     :type integer)

(cvar ("errno" +errno+ :read-only t) :int)

(cstruct winsize "winsize_t"
  (ws-row "ws_row" :type :unsigned-short)
  (ws-col "ws_col" :type :unsigned-short)
  (ws-xpixel "ws_xpixel" :type :unsigned-short)
  (ws-ypixel "ws_ypixel" :type :unsigned-short))


;;; constants.lisp ends here

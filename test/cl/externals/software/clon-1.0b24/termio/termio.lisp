;;; termio.lisp --- Terminal-related utilities

;; Copyright (C) 2012, 2015 Didier Verna

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

;; Basically, this whole stream manipulation stuff is a big non-standard mess.
;; Terminal streams are sometimes implemented on top of file streams,
;; sometimes not. Functions that retreive file descriptors sometimes just
;; return nil when it's not possible, or something else, or throw errors. It
;; is not always possible to know whether a stream has file descriptors or
;; not. Finally, there are cases where streams have an implicit hard-wired
;; file descriptor of 1, without any function to get it...

;; Here's the information that I've gathered so far. This information is used
;; in the implementation of STREAM-IOCTL-OUTPUT-HANDLE below.

;;    SBCL:  SB-SYS:FD-STREAM for both terminals and files.
;;    CMUCL: SYSTEM:FD-STREAM for both terminals and files.
;;    CCL:   CCL::BASIC-CHARACTER-[INPUT|OUTPUT]-STREAM for terminals,
;;           CCL::BASIC-FILE-CHARACTER-[INPUT|OUTPUT]-STREAM for files, with
;;           the above somewhere in the superclasses. I'm not sure if any
;;           class guarantees the existence of file descriptors, but it
;;           doesn't matter because CCL:STREAM-DEVICE returns nil if there are
;;           none, so we can safely use it on all classes.
;;    ECL:   FILE-STREAM for both terminals and files.
;;    CLISP: STREAM for terminals, FILE-STREAM for files. It is impossible (at
;;           the Lisp level) to detect whether a stream has file descriptors
;;           or not. The only workaround available right now is to catch a
;;           STREAM-ERROR potentially thrown by EXT:STREAM-HANDLES.
;;    ACL:   EXCL:TERMINAL-SIMPLE-STREAM for terminals,
;;           EXCL:FILE-SIMPLE-STREAM for files. I'm not sure if any class
;;           guarantees the existence of file descriptors, but it doesn't
;;           matter because EXCL::STREAM-[INPUT|OUTPUT]-HANDLE returns
;;           something which is not a number when there are none. The return
;;           value seems to be either NIL or some modified version of the
;;           original stream...
;;    LW:    SYSTEM::TERMINAL-STREAM for terminals, STREAM::[ENC]-FILE-STREAM
;;           for files. Terminal streams always have file descriptors 0
;;           (input) and 1 (output) hard wired. File streams are subclasses of
;;           STREAM::OS-FILE-HANDLE-STREAM on which we can retrieve file
;;           handles.


;;; Code:

(in-package :net.didierverna.clon)
(in-readtable :net.didierverna.clon)


;; Preamble C code needed for ECL's FD-LINE-WIDTH function.
#+ecl (ffi:clines "
#include <stdio.h>
#include <errno.h>
#include <sys/ioctl.h>")


(defgeneric stream-ioctl-output-handle (stream)
  (:documentation "Return STREAM's ioctl output handle or NIL.")
  ;; Standard canonicalization methods:
  (:method ((stream synonym-stream))
    (stream-ioctl-output-handle (symbol-value (synonym-stream-symbol stream))))
  (:method ((stream two-way-stream))
    (stream-ioctl-output-handle (two-way-stream-output-stream stream)))
  ;; #### PORTME.
  ;; Compiler-specific implementations:
  #+sbcl
  (:method ((stream sb-sys:fd-stream))
    ;; sb-posix's IOCTL function takes a stream directly
    stream)
  #+cmu
  (:method ((stream system:fd-stream))
    (system:fd-stream-fd stream))
  #+ecl
  (:method ((stream file-stream))
    (ext:file-stream-fd stream))
  #+lispworks
  (:method ((stream system::terminal-stream))
    1)
  #+lispworks
  (:method ((stream stream::os-file-handle-stream))
    (stream::os-file-handle-stream-file-handle stream))
  (:method (stream)
    #+ecl (declare (ignore stream))
    #+ccl
    (ccl:stream-device stream :output)
    #+clisp
    (multiple-value-bind (input-handle output-handle)
	(when (or (sys::built-in-stream-p stream)
		  (eq (type-of stream) 'socket:socket-server))
	  (handler-case (ext:stream-handles stream)
	    (stream-error ()
	      nil)))
      (declare (ignore input-handle))
      output-handle)
    #+allegro
    (let ((handle (excl::stream-output-handle stream)))
      (when (numberp handle)
	handle))
    #-(or ccl clisp allegro)
    nil))

#+ecl
(defun fd-line-width (fd)
  "Get the line width for FD (file descriptor).
Return two values:
- the line width, or -1 if it can't be computed
  (typically when FD does not denote a tty),
- an error message if the operation failed."
  (ffi:c-inline (fd) (:int) (values :int :cstring) "{
    int fd = #0;

    int cols = -1;
    char *msg = NULL;

    struct winsize window;
    if (ioctl (fd, TIOCGWINSZ, &window) == -1)
      {
	if (errno != ENOTTY)
	  msg = strerror (errno);
      }
    else
      cols = (int) window.ws_col;

    @(return 0) = cols;
    @(return 1) = msg;
}"))

;; #### NOTE: ABCL doesn't appear below because this module (termio) is never
;; loaded with it.
(defun stream-line-width
    (stream &aux (handle (stream-ioctl-output-handle stream)))
  "Get STREAM's line width.
Return two values:
- the stream's line width, or nil if it can't be computed
  (typically when the stream does not denote a tty),
- an error message if the operation failed."
  (when handle
    ;; #### NOTE: doing a TIOCGWINSZ ioctl here is a convenient way to both
    ;; know whether we're connected to a tty, and getting the terminal width
    ;; at the same time. In case the ioctl fails, we need to distinguish
    ;; between and ENOTTY error, which simply means that we're not connected
    ;; to a terminal, and the other which are real errors and need to be
    ;; reported.
    ;; #### PORTME.
    #+sbcl
    (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (handler-case
	  (with-winsize winsize ()
	    (sb-posix:ioctl handle +tiocgwinsz+ winsize)
	    (winsize-ws-col winsize))
	(sb-posix:syscall-error (error)
	  (unless (= (sb-posix:syscall-errno error) sb-posix:enotty)
	    (values nil error)))))
    #+cmu
    (locally (declare (optimize (ext:inhibit-warnings 3)))
      (alien:with-alien ((winsize (alien:struct unix:winsize)))
	(multiple-value-bind (success error-number)
	    (unix:unix-ioctl handle unix:tiocgwinsz winsize)
	  (if success
	      (alien:slot winsize 'unix:ws-col)
	    (unless (= error-number unix:enotty)
	      (values nil (unix:get-unix-error-msg error-number)))))))
    #+ccl
    (ccl:rlet ((winsize :winsize))
      (let ((result (ccl::int-errno-call
		     (#_ioctl handle #$TIOCGWINSZ :address winsize))))
	(if (zerop result)
	    (ccl:pref winsize :winsize.ws_col)
	  (unless (= result (- #$ENOTTY))
	    (values nil (ccl::%strerror (- result)))))))
    #+ecl
    (multiple-value-bind (cols msg)
	(fd-line-width handle)
      (values (unless (= cols -1) cols) msg))
    #+(or clisp allegro lispworks)
    (cffi:with-foreign-object (winsize 'winsize)
      (let ((result (cffi:foreign-funcall "ioctl"
		      :int handle
		      :int +tiocgwinsz+
		      :pointer winsize
		      :int)))
	(if (= result -1)
	    (unless (= +errno+ +enotty+)
	      (values nil
		      (cffi:foreign-funcall "strerror"
			:int +errno+ :string)))
	  (cffi:with-foreign-slots ((ws-col) winsize winsize)
	    ws-col))))))

;;; termio.lisp ends here

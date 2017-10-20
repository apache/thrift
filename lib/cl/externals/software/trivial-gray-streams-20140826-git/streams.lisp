#+xcvb (module (:depends-on ("package")))

(in-package :trivial-gray-streams)

(defclass fundamental-stream (impl-specific-gray:fundamental-stream) ())
(defclass fundamental-input-stream
    (fundamental-stream impl-specific-gray:fundamental-input-stream) ())
(defclass fundamental-output-stream
    (fundamental-stream impl-specific-gray:fundamental-output-stream) ())
(defclass fundamental-character-stream
    (fundamental-stream impl-specific-gray:fundamental-character-stream) ())
(defclass fundamental-binary-stream
    (fundamental-stream impl-specific-gray:fundamental-binary-stream) ())
(defclass fundamental-character-input-stream
    (fundamental-input-stream fundamental-character-stream
     impl-specific-gray:fundamental-character-input-stream) ())
(defclass fundamental-character-output-stream
    (fundamental-output-stream fundamental-character-stream
     impl-specific-gray:fundamental-character-output-stream) ())
(defclass fundamental-binary-input-stream
    (fundamental-input-stream fundamental-binary-stream
     impl-specific-gray:fundamental-binary-input-stream) ())
(defclass fundamental-binary-output-stream
    (fundamental-output-stream fundamental-binary-stream
     impl-specific-gray:fundamental-binary-output-stream) ())

(defgeneric stream-read-sequence
    (stream sequence start end &key &allow-other-keys))
(defgeneric stream-write-sequence
    (stream sequence start end &key &allow-other-keys))

(defgeneric stream-file-position (stream))
(defgeneric (setf stream-file-position) (newval stream))

;;; Default methods for stream-read/write-sequence.
;;;
;;; It would be nice to implement default methods
;;; in trivial gray streams, maybe borrowing the code
;;; from some of CL implementations. But now, for
;;; simplicity we will fallback to default implementation
;;; of the implementation-specific analogue function which calls us.

(defmethod stream-read-sequence ((stream fundamental-input-stream) seq start end &key)
  (declare (ignore seq start end))
  'fallback)

(defmethod stream-write-sequence ((stream fundamental-output-stream) seq start end &key)
  (declare (ignore seq start end))
  'fallback)

(defmacro or-fallback (&body body)
  `(let ((result ,@body))
     (if (eq result (quote fallback))
         (call-next-method)
         result)))

;; Implementations should provide this default method, I believe, but
;; at least sbcl and allegro don't.
(defmethod stream-terpri ((stream fundamental-output-stream))
  (write-char #\newline stream))

;; stream-file-position could be specialized to
;; fundamental-stream, but to support backward
;; compatibility with flexi-streams, we specialize
;; it on T. The reason: flexi-streams calls stream-file-position
;; for non-gray stream:
;; https://github.com/edicl/flexi-streams/issues/4
(defmethod stream-file-position ((stream t))
  nil)

(defmethod (setf stream-file-position) (newval (stream t))
  (declare (ignore newval))
  nil)

#+abcl
(progn
  (defmethod gray-streams:stream-read-sequence 
      ((s fundamental-input-stream) seq &optional start end)
    (or-fallback (stream-read-sequence s seq (or start 0) (or end (length seq)))))
  
  (defmethod gray-streams:stream-write-sequence 
      ((s fundamental-output-stream) seq &optional start end)
    (or-fallback (stream-write-sequence s seq (or start 0) (or end (length seq)))))
  
  (defmethod gray-streams:stream-write-string 
      ((stream xp::xp-structure) string &optional (start 0) (end (length string)))
    (xp::write-string+ string stream start end))

  #+#.(cl:if (cl:and (cl:find-package :gray-streams)
		     (cl:find-symbol "STREAM-FILE-POSITION" :gray-streams))
	     '(:and)
	     '(:or))
  (defmethod gray-streams:stream-file-position
      ((s fundamental-stream) &optional position)
    (if position
        (setf (stream-file-position s) position)
        (stream-file-position s))))

#+allegro
(progn
  (defmethod excl:stream-read-sequence
      ((s fundamental-input-stream) seq &optional start end)
    (or-fallback (stream-read-sequence s seq (or start 0) (or end (length seq)))))

  (defmethod excl:stream-write-sequence
      ((s fundamental-output-stream) seq &optional start end)
    (or-fallback (stream-write-sequence s seq (or start 0) (or end (length seq)))))

  (defmethod excl::stream-file-position
       ((stream fundamental-stream) &optional position)
     (if position
         (setf (stream-file-position stream) position)
         (stream-file-position stream))))

;; Untill 2014-08-09 CMUCL did not have stream-file-position:
;; http://trac.common-lisp.net/cmucl/ticket/100
#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol (string '#:stream-file-position) '#:ext)
    (pushnew :cmu-has-stream-file-position *features*)))

#+cmu
(progn
  (defmethod ext:stream-read-sequence
      ((s fundamental-input-stream) seq &optional start end)
    (or-fallback (stream-read-sequence s seq (or start 0) (or end (length seq)))))
  (defmethod ext:stream-write-sequence
      ((s fundamental-output-stream) seq &optional start end)
    (or-fallback (stream-write-sequence s seq (or start 0) (or end (length seq)))))

  #+cmu-has-stream-file-position
  (defmethod ext:stream-file-position ((stream fundamental-stream))
    (stream-file-position stream))

  #+cmu-has-stream-file-position
  (defmethod (setf ext:stream-file-position) (position (stream fundamental-stream))
    (setf (stream-file-position stream) position)))

#+lispworks
(progn
  (defmethod stream:stream-read-sequence
      ((s fundamental-input-stream) seq start end)
    (or-fallback (stream-read-sequence s seq start end)))
  (defmethod stream:stream-write-sequence
      ((s fundamental-output-stream) seq start end)
    (or-fallback (stream-write-sequence s seq start end)))

  (defmethod stream:stream-file-position ((stream fundamental-stream))
    (stream-file-position stream))
  (defmethod (setf stream:stream-file-position)
      (newval (stream fundamental-stream))
    (setf (stream-file-position stream) newval)))

#+openmcl
(progn
  (defmethod ccl:stream-read-vector
      ((s fundamental-input-stream) seq start end)
    (or-fallback (stream-read-sequence s seq start end)))
  (defmethod ccl:stream-write-vector
      ((s fundamental-output-stream) seq start end)
    (or-fallback (stream-write-sequence s seq start end)))

  (defmethod ccl:stream-read-list ((s fundamental-input-stream) list count)
    (or-fallback (stream-read-sequence s list 0 count)))
  (defmethod ccl:stream-write-list ((s fundamental-output-stream) list count)
    (or-fallback (stream-write-sequence s list 0 count)))

  (defmethod ccl::stream-position ((stream fundamental-stream) &optional new-position)
    (if new-position
	(setf (stream-file-position stream) new-position)
	(stream-file-position stream))))

;; up to version 2.43 there were no
;; stream-read-sequence, stream-write-sequence
;; functions in CLISP
#+clisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol (string '#:stream-read-sequence) '#:gray)
    (pushnew :clisp-has-stream-read/write-sequence *features*)))

#+clisp
(progn

  #+clisp-has-stream-read/write-sequence
  (defmethod gray:stream-read-sequence
      (seq (s fundamental-input-stream) &key start end)
    (or-fallback (stream-read-sequence s seq (or start 0) (or end (length seq)))))

  #+clisp-has-stream-read/write-sequence
  (defmethod gray:stream-write-sequence
      (seq (s fundamental-output-stream) &key start end)
    (or-fallback (stream-write-sequence s seq (or start 0) (or end (length seq)))))

  ;;; for old CLISP
  (defmethod gray:stream-read-byte-sequence
      ((s fundamental-input-stream)
       seq
       &optional start end no-hang interactive)
    (when no-hang
      (error "this stream does not support the NO-HANG argument"))
    (when interactive
      (error "this stream does not support the INTERACTIVE argument"))
    (or-fallback (stream-read-sequence s seq start end)))

  (defmethod gray:stream-write-byte-sequence
      ((s fundamental-output-stream)
       seq
       &optional start end no-hang interactive)
    (when no-hang
      (error "this stream does not support the NO-HANG argument"))
    (when interactive
      (error "this stream does not support the INTERACTIVE argument"))
    (or-fallback (stream-write-sequence s seq start end)))

  (defmethod gray:stream-read-char-sequence
      ((s fundamental-input-stream) seq &optional start end)
    (or-fallback (stream-read-sequence s seq start end)))

  (defmethod gray:stream-write-char-sequence
      ((s fundamental-output-stream) seq &optional start end)
    (or-fallback (stream-write-sequence s seq start end)))

  ;;; end of old CLISP read/write-sequence support

  (defmethod gray:stream-position ((stream fundamental-stream) position)
    (if position
        (setf (stream-file-position stream) position)
        (stream-file-position stream))))

#+sbcl
(progn
  (defmethod sb-gray:stream-read-sequence
      ((s fundamental-input-stream) seq &optional start end)
    (or-fallback (stream-read-sequence s seq (or start 0) (or end (length seq)))))
  (defmethod sb-gray:stream-write-sequence
      ((s fundamental-output-stream) seq &optional start end)
    (or-fallback (stream-write-sequence s seq (or start 0) (or end (length seq)))))
  (defmethod sb-gray:stream-file-position 
      ((stream fundamental-stream) &optional position)
    (if position
        (setf (stream-file-position stream) position)
        (stream-file-position stream)))
  ;; SBCL extension:
  (defmethod sb-gray:stream-line-length ((stream fundamental-stream))
    80))

#+ecl
(progn
  (defmethod gray::stream-file-position 
    ((stream fundamental-stream) &optional position)
    (if position
      (setf (stream-file-position stream) position)
      (stream-file-position stream)))
  (defmethod gray:stream-read-sequence
    ((s fundamental-input-stream) seq &optional start end)
    (or-fallback (stream-read-sequence s seq (or start 0) (or end (length seq)))))
  (defmethod gray:stream-write-sequence
    ((s fundamental-output-stream) seq &optional start end)
    (or-fallback (stream-write-sequence s seq (or start 0) (or end (length seq))))))

#+mocl
(progn
  (defmethod gray:stream-read-sequence
      ((s fundamental-input-stream) seq &optional start end)
    (or-fallback (stream-read-sequence s seq (or start 0) (or end (length seq)))))
  (defmethod gray:stream-write-sequence
      ((s fundamental-output-stream) seq &optional start end)
    (or-fallback (stream-write-sequence s seq (or start 0) (or end (length seq)))))
  (defmethod gray:stream-file-position
      ((stream fundamental-stream) &optional position)
    (if position
	(setf (stream-file-position stream) position)
	(stream-file-position stream))))

;; deprecated
(defclass trivial-gray-stream-mixin () ())


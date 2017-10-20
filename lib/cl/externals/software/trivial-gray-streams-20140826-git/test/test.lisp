(in-package :trivial-gray-streams-test)

;;; assert-invoked - a tool to check that specified method with parameters has
;;; been invoked during execution of a code body

(define-condition invoked ()
  ((method :type (or symbol cons) ;; cons is for (setf method)
           :accessor method
           :initarg :method
           :initform (error ":method is required"))
   (args :type list
         :accessor args
         :initarg :args
         :initform nil)))

(defun assert-invoked-impl (method args body-fn)
  (let ((expected-invocation (cons method args))
        (actual-invocations nil))
    (handler-bind ((invoked (lambda (i)
                              (let ((invocation (cons (method i) (args i))))
                                (when (equalp invocation expected-invocation)
                                  (return-from assert-invoked-impl nil))
                                (push invocation actual-invocations)))))
      (funcall body-fn))
    (let ((*package* (find-package :keyword))) ; ensures package prefixes are printed
      (error "expected invocation: ~(~S~) actual: ~{~(~S~)~^, ~}"
             expected-invocation (reverse actual-invocations)))))

(defmacro assert-invoked ((method &rest args) &body body)
  "If during execution of the BODY the specified METHOD with ARGS
hasn't been invoked, signals an ERROR."
  `(assert-invoked-impl (quote ,method) (list ,@args) (lambda () ,@body)))

(defun invoked (method &rest args)
  (signal 'invoked :method method :args args))

;;; The tests.

#|
  We will define a gray stream class, specialise 
  the gray generic function methods on it and test that the methods
  are invoked when we call functions from common-lisp package
  on that stream.

  Some of the gray generic functions are only invoked by default
  methods of other generic functions:

      cl:format ~t or cl:pprint -> stream-advance-to-column -> stream-line-column
                                                               stream-write-char
      cl:fresh-line -> stream-fresh-line -> stream-start-line-p -> stream-line-column
                                            stream-terpri


  If we define our methods for stream-advance-to-column and stream-fresh-line,
  then stream-start-line-p, stream-terpri, stram-line-column are not invoked.

  Therefore we define another gray stream class. The first class is used
  for all lower level functions (stream-terpri). The second class
  is used to test methods for higher level functions (stream-fresh-line).
|#

(defclass test-stream (fundamental-binary-input-stream
                       fundamental-binary-output-stream
                       fundamental-character-input-stream
                       fundamental-character-output-stream)
  ())

(defclass test-stream2 (test-stream) ())

(defmethod stream-read-char ((stream test-stream))
  (invoked 'stream-read-char stream))

(defmethod stream-unread-char ((stream test-stream) char)
  (invoked 'stream-unread-char stream char))

(defmethod stream-read-char-no-hang ((stream test-stream))
  (invoked 'stream-read-char-no-hang stream))

(defmethod stream-peek-char ((stream test-stream))
  (invoked 'stream-peek-char stream))

(defmethod stream-listen ((stream test-stream))
  (invoked 'stream-listen stream))

(defmethod stream-read-line ((stream test-stream))
  (invoked 'stream-read-line stream))

(defmethod stream-clear-input ((stream test-stream))
  (invoked 'stream-clear-input stream))

(defmethod stream-write-char ((stream test-stream) char)
  (invoked 'stream-write-char stream char))

(defmethod stream-line-column ((stream test-stream))
  (invoked 'stream-line-column stream))

(defmethod stream-start-line-p ((stream test-stream))
  (invoked 'stream-start-line-p stream))

(defmethod stream-write-string ((stream test-stream) string &optional start end)
  (invoked 'stream-write-string stream string start end))

(defmethod stream-terpri ((stream test-stream))
  (invoked 'stream-terpri stream))

(defmethod stream-fresh-line ((stream test-stream2))
  (invoked 'stream-fresh-line stream))

(defmethod stream-finish-output ((stream test-stream))
  (invoked 'stream-finish-output stream))

(defmethod stream-force-output ((stream test-stream))
  (invoked 'stream-force-output stream))

(defmethod stream-clear-output ((stream test-stream))
  (invoked 'stream-clear-output stream))

(defmethod stream-advance-to-column ((stream test-stream2) column)
  (invoked 'stream-advance-to-column stream column))

(defmethod stream-read-byte ((stream test-stream))
  (invoked 'stream-read-byte stream))

(defmethod stream-write-byte ((stream test-stream) byte)
  (invoked 'stream-write-byte stream byte))

(defmethod stream-read-sequence ((s test-stream) seq start end &key)
  (invoked 'stream-read-sequence s seq :start start :end end))

(defmethod stream-write-sequence ((s test-stream) seq start end &key)
  (invoked 'stream-write-sequence s seq :start start :end end))

(defmethod stream-file-position ((s test-stream))
  (invoked 'stream-file-position s))

(defmethod (setf stream-file-position) (newval (s test-stream))
  (invoked '(setf stream-file-position) newval s))

;; Convinience macro, used when we want to name
;; the test case with the same name as of the gray streams method we test.
(defmacro test-invoked ((method &rest args) &body body)
  `(test (,method)
     (assert-invoked (,method ,@args)
       ,@body)))

(defun run-tests ()
  (let ((s (make-instance 'test-stream))
        (s2 (make-instance 'test-stream2)))
    (list
     (test-invoked (stream-read-char s)
       (read-char s))
     (test-invoked (stream-unread-char s #\a)
       (unread-char #\a s))
     (test-invoked (stream-read-char-no-hang s)
       (read-char-no-hang s))
     (test-invoked (stream-peek-char s)
       (peek-char nil s))
     (test-invoked (stream-listen s)
       (listen s))
     (test-invoked (stream-read-line s)
       (read-line s))
     (test-invoked (stream-clear-input s)
       (clear-input s))
     (test-invoked (stream-write-char s #\b)
       (write-char #\b s))
     (test-invoked (stream-line-column s)
       (format s "~10,t"))
     (test-invoked (stream-start-line-p s)
       (fresh-line s))
     (test-invoked (stream-write-string s "hello" 1 4)
       (write-string "hello" s :start 1 :end 4))
     (test-invoked (stream-terpri s)
       (fresh-line s))
     (test-invoked (stream-fresh-line s2)
       (fresh-line s2))
     (test-invoked (stream-finish-output s)
       (finish-output s))
     (test-invoked (stream-force-output s)
       (force-output s))
     (test-invoked (stream-clear-output s)
       (clear-output s))
     (test-invoked (stream-advance-to-column s2 10)
        (format s2 "~10,t"))
     (test-invoked (stream-read-byte s)
       (read-byte s))
     (test-invoked (stream-write-byte s 1)
       (write-byte 1 s))
     ;;; extensions
     (test-invoked (stream-read-sequence s #(1 2) :start 0 :end 1)
       (read-sequence #(1 2) s :start 0 :end 1))
     (test-invoked (stream-write-sequence s #(1 2) :start 0 :end 1)
       (write-sequence #(1 2) s :start 0 :end 1))
     (test-invoked (stream-file-position s)
       (file-position s))
     (test (setf-stream-file-position)
       (assert-invoked ((setf stream-file-position) 9 s)
         (file-position s 9))))))

(defun failed-tests (results)
  (remove-if-not #'failed-p results))

(defun failed-test-names (results)
  (mapcar (lambda (result)
            (string-downcase (name result)))
          (failed-tests results)))
               
#|
(failed-test-names (run-tests))

(setf *allow-debugger* nil))

|#
(in-package :thrift.test-implementation)

(defun test-void ()
  (format t "testVoid()~%"))

(defun test-string (thing)
  (format t "testString(~a)~%" thing)
  thing)

(defun test-bool (thing)
  (format t "testBool(~a)~%" (if thing "true" "false"))
  thing)

(defun test-byte (thing)
  (format t "testByte(~a)~%" thing)
  thing)

(defun test-i32 (thing)
  (format t "testI32(~a)~%" thing)
  thing)

(defun test-i64 (thing)
  (format t "testI64(~a)~%" thing)
  thing)

(defun test-double (thing)
  (format t "testDouble(~a)~%" thing)
  thing)

(defun test-binary (thing)
  (format t "testBinary(~a)~%" thing)
  thing)

(defun test-struct (thing)
  (format t "testStruct(~a)~%" thing)
  thing)

(defun test-nest (thing)
  (format t "testNest(~a)~%" thing)
  thing)

(defun test-map (thing)
  (format t "testMap(~a)~%" thing)
  thing)

(defun test-string-map (thing)
  (format t "testStringMap(~a)~%" thing)
  thing)

(defun test-set (thing)
  (format t "testSet(~a)~%" thing)
  thing)

(defun test-list (thing)
  (format t "testList(~a)~%" thing)
  thing)

(defun test-enum (thing)
  (format t "testEnum(~a)~%" thing)
  thing)

(defun test-typedef (thing)
  (format t "testTypedef(~a)~%" thing)
  thing)

(defun test-map-map (hello)
  (format t "testMapMap(~a)~%" hello)
  '((-4 . ((-4 . -4) (-3 . -3) (-2 . -2) (-1 . -1))) (4 . ((1 . 1) (2 . 2) (3 . 3) (4 . 4)))))

(defun test-insanity (argument)
  (let ((result `((1 . ((2 . ,argument) (3 . ,argument)))
		  (2 . ((6 . ,(thrift.test::make-insanity :user-map nil :xtructs nil)))))))
    (format t "~a~%" result)
    result))

(defun test-multi (arg0 arg1 arg2 arg3 arg4 arg5)
  (declare (ignorable arg3 arg4 arg5))
  (format t "testMulti()~%")
  (thrift.test:make-xtruct :string-thing "Hello"
			   :byte-thing arg0
			   :i32-thing arg1
			   :i64-thing arg2))

(defun test-exception (arg)
  (format t "testException(~a)~%" arg)
  (cond
    ((string= arg "Xception") (error 'thrift.test:xception
				     :error-code 1001
				     :message arg))
    ((string= arg "TException") (print "TODO!"))))

(defun test-multi-exception (arg0 arg1)
  (format t "testMultiException(~a, ~a)~%" arg0 arg1)
  (cond
    ((string= arg0 "Xception") (error 'thrift.test:xception
				     :error-code 1001
				     :message "This is an Xception"))
    ((string= arg0 "Xception2") (error 'thrift.test:xception2
				     :error-code 2002
				     :struct-thing (thrift.test:make-xtruct :string-thing "This is an Xception2"
									    :byte-thing 0
									    :i32-thing 0
									    :i64-thing 0))))
  (thrift.test:make-xtruct :string-thing arg1
			   :byte-thing 0
			   :i32-thing 0
			   :i64-thing 0))

(defun test-oneway (seconds)
  (format t "testOneway(~a): Sleeping...~%" seconds)
  (sleep seconds)
  (format t "testOneway(~a): done sleeping!~%" seconds))

(defun blah-blah ()
  )

(defun secondtest-string (thing)
  (format t "testString(~a)~%" thing)
  thing)

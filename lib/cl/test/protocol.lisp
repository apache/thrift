(in-package :thrift-test)

;;; tests for transport operations
;;; (run-tests "protocol.*")


(defvar *string-w/euro* (cl:map 'string #'code-char '(48 46 57 57 57 8364)))

(test protocol.open-stream-p
  (open-stream-p (make-test-transport)))

(defun test-read-write-equivalence (protocol reader writer &rest values)
  (let ((transport (protocol-output-transport protocol)))
    (dolist (value values t)
      (reset protocol)
      (funcall writer protocol value)
      (rewind protocol)
      (let ((read (funcall reader protocol)))
        (unless (equalp read value)
	  (format *trace-output*
		  "failed: ~a/~a ~s ~s ~s"
		  reader
		  writer
		  value
		  read
		  (subseq (thrift.implementation::get-vector-stream-vector transport)
			  0
			  (thrift.implementation::stream-position transport)))
        (return nil))))))


;;;

(test protocol.stream-read/write-integer
  (let ((stream (make-test-protocol)))
    (every #'(lambda (entry)
               (apply #'test-read-write-equivalence stream entry))
           `((stream-read-bool stream-write-bool t nil)
             ;; `thfit:byte' is encoded the same in the protocol as i8,
             ;; so it will be read back as i8. There is no good way
             ;; around that, but these types are equivalent according
             ;; to spec and `thrift:byte' is deprecated.
             (stream-read-type stream-write-type ;thrift:byte
                               thrift:map thrift:list thrift:set struct)
             (stream-read-message-type stream-write-message-type call)
             (stream-read-i8 stream-write-i8 ,(- (expt 2 7))  -1 0 1 ,(1- (expt 2 7)))
             (stream-read-i16 stream-write-i16 ,(- (expt 2 15))  -1 0 1 ,(1- #x70f0) ,(1- (expt 2 15)))
             (stream-read-i32 stream-write-i32 ,(- (expt 2 31))  -1 0 1 ,(1- #x7700ff00) ,(1- (expt 2 31)))
             (stream-read-i64 stream-write-i64 ,(- (expt 2 63))  -1 0 1 ,(1- #x77770000ffff0000) ,(1- (expt 2 63)))))))


(test protocol.stream-read/write-double
  (let ((stream (make-test-protocol)))
    (every #'(lambda (entry)
               (apply #'test-read-write-equivalence stream entry))
           `((stream-read-double stream-write-double
              ,most-negative-double-float ,least-negative-double-float
              ,most-positive-double-float ,least-positive-double-float
              0.0d0 1.0d0 -1.0d0)))))


(test protocol.stream-read/write-string
  (let ((stream (make-test-protocol)))
    (every #'(lambda (entry)
               (apply #'test-read-write-equivalence stream entry))
           `((stream-read-string stream-write-string "a" "0123456789" ,*string-w/euro*)))))


(test protocol.stream-read/write-binary
  (let ((stream (make-test-protocol)))
    (every #'(lambda (entry)
               (apply #'test-read-write-equivalence stream entry))
           ;; presuming (unsigned-bte 8)
           `((stream-read-binary stream-write-binary #( 0 1 255))))))


(test protocol.stream-read/write-message
  (let ((struct (make-instance 'test-struct :field1 "one" :field2 2))
        (stream (make-test-protocol)))
    (stream-write-message stream struct 'call)
    (rewind stream) 
    (multiple-value-bind (name type sequence response)
                         (stream-read-message stream)
      (and (equal name 'test-struct)
           (eq type 'call)
           (eql sequence 1)
           (typep response 'test-struct)
           (equal (test-struct-field1 response) "one")
           (equal (test-struct-field2 response) 2)))))


(test protocol.stream-read/write-struct
  (let ((struct (make-instance 'test-struct :field1 "one" :field2 2))
        (stream (make-test-protocol)))
    (stream-write-struct stream struct)
    (rewind stream)
    (let* ((type 'test-struct)
           (result (stream-read-struct stream type)))
      (and (typep result 'test-struct)
           (equal (test-struct-field1 result) "one")
           (equal (test-struct-field2 result) 2)))))
;;; (run-tests "protocol.stream-read/write-struct")

(test protocol.stream-read/write-struct.inline
  (let ((struct (make-instance 'test-struct :field1 "one" :field2 2))
        (stream (make-test-protocol)))
    (stream-write-struct stream struct 'test-struct)
    (rewind stream)
    (let ((result (stream-read-struct stream 'test-struct)))
      (and (typep result 'test-struct)
           (equal (test-struct-field1 result) "one")
           (equal (test-struct-field2 result) 2)))))
;;; (run-tests "protocol.stream-read/write-struct.inline")


(test protocol.stream-read/write-struct.optional
  (let ((struct (make-instance 'test-large-struct :field1 1 :field2 2))
        (stream (make-test-protocol)))
    (assert (not (slot-boundp struct 'field3)))
    (stream-write-struct stream struct 'test-large-struct)
    (rewind stream)
    (let ((result (stream-read-struct stream 'test-large-struct)))
      (and (typep result 'test-large-struct)
           (not (slot-boundp result 'field3))
           (equal (test-struct-field1 result) 1)
           (equal (test-struct-field2 result) 2)))))


(test protocol.stream-read/write-field
  (let ((stream (make-test-protocol)))
    (every #'(lambda (entry)
               (apply #'test-read-write-equivalence stream entry))
           `((,(lambda (p) (multiple-value-bind (value name id)
                                                (stream-read-field p)
                             (when (ecase (protocol-field-id-mode stream)
                                     (:identifier-name (and (equal name "test") (null id)))
                                     (:identifier-number (and (null name) (equal id 10))))
                               value)))
              ,(lambda (p v) (stream-write-field p v :identifier-name "test" :identifier-number 10))
              "a" "0123456789" ,*string-w/euro*)))))


(test protocol.stream-read/write-map
  (let ((stream (make-test-protocol)))
    (every #'(lambda (entry)
               (apply #'test-read-write-equivalence stream entry))
           `((stream-read-map stream-write-map ,(thrift:map 1 "a" 2 "b"))))))
;;; (run-tests "protocol.stream-read/write-map")



(test protocol.stream-read/write-list
  (let ((stream (make-test-protocol)))
    (every #'(lambda (entry)
               (apply #'test-read-write-equivalence stream entry))
           `((stream-read-list stream-write-list
                               (t nil) (1 2 3) (32767 1 -1 -32768)
                               (,(expt 2 33) ,(- (expt 2 33)))
                               ("asdf" ,*string-w/euro*)
                               ;; no test for binary ! there is no type code
                               ;; and the java and cpp versions just send it as a string
                               ;; (#(1 2 3) #(4 5 6))
                               (1.0d0 -1.0d0)
                               (,(thrift:map 1 "a" 2 "b")))))))


(test protocol.stream-read/write-set
  (let ((stream (make-test-protocol)))
    (every #'(lambda (entry)
               (apply #'test-read-write-equivalence stream entry))
           `((stream-read-set stream-write-set
                               (t nil) (1 2 3) (32767 1 -1 -32768))))))


#+(or ccl sbcl)
(defun time-struct-io (&optional (count 1024))
  (let ((initargs '(:field1 1 :field2 2 :field3 3 :field4 4 :field5 5
                    :field6 6 :field7 7 :field8 8 :field9 9 :field10 10))
        (stream (make-test-protocol))
        (bound-count 0)
        (gctime nil)
        (slot-count 10))
    (flet ((gctime () #+ccl (ccl::gctime) #+sbcl sb-ext:*GC-RUN-TIME*)
           (gcbytes () #+ccl (ccl::total-bytes-allocated)
                       #+sbcl (nth-value 3 (sb-impl::time-get-sys-info))))
      (format *trace-output* "~&slots,~10Tbound,~20Tdynamic-ms,~36Tstatic-ms,~52Tstatic/w-ms,~68Tdynamic-kb,~84Tstatic-kb,~100Tstatic/w-kb")
      (loop (when (eql gctime (setf gctime (gctime)))
              (incf bound-count)
              (when (> bound-count 10) (return)))
            (let ((struct (apply #'make-instance 'test-large-struct
                                 (subseq initargs 0 (* bound-count 2))))
                  (result (apply #'make-instance 'test-large-struct nil))
                  (dynamic-time 0)
                  (static-time 0)
                  (static-with-time 0)
                  (dynamic-bytes 0)
                  (static-bytes 0)
                  (static-with-bytes 0))
              (let ((time (get-internal-run-time))
                    (bytes (gcbytes)))
                (dotimes (i count)
                  (rewind stream)
                  (stream-write-struct stream struct)
                  (rewind stream)
                  (stream-read-struct stream))
                (setf dynamic-time (- (get-internal-run-time) time)
                      dynamic-bytes (- (gcbytes) bytes)))
              (let ((time (get-internal-run-time))
                    (bytes (gcbytes)))
                (dotimes (i count)
                  (rewind stream)
                  (stream-write-struct stream struct 'test-large-struct)
                  (rewind stream)
                  (stream-read-struct stream 'test-large-struct))
                (setf static-time (- (get-internal-run-time) time)
                      static-bytes (- (gcbytes) bytes)))
              (let ((time (get-internal-run-time))
                    (bytes (gcbytes)))
                (dotimes (i count)
                  (rewind stream)
                  (stream-write-struct stream struct 'test-large-struct)
                  (rewind stream)
                  (stream-read-struct stream 'test-large-struct result))
                (setf static-with-time (- (get-internal-run-time) time)
                      static-with-bytes (- (gcbytes) bytes)))
              (format *trace-output* "~%~d,~10T~d,~20T~d,~36T~d,~52T~d,~68T~d,~84T~d,~100T~d"
                      slot-count bound-count
                      dynamic-time static-time static-with-time
                      dynamic-bytes static-bytes static-with-bytes))))))
;;; (time-struct-io)

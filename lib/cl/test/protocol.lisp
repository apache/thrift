;;;; Copyright 2010 James Anderson <james.anderson@setf.de>
;;;;
;;;; Licensed under the Apache License, Version 2.0 (the "License");
;;;; you may not use this file except in compliance with the License.
;;;; You may obtain a copy of the License at
;;;;
;;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing, software
;;;; distributed under the License is distributed on an "AS IS" BASIS,
;;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;; See the License for the specific language governing permissions and
;;;; limitations under the License.

;;;; Tests for transport operations.

(fiasco:define-test-package (#:protocol-tests :in thrift-test:thrift-self-tests)
  (:use #:thrift-test-utils))

(in-package #:protocol-tests)

(defvar *string-w/euro* (cl:map 'string #'code-char '(48 46 57 57 57 8364)))

(deftest open-stream-p-test ()
  (is (open-stream-p (make-test-transport))))

(defun test-read-write-equivalence (protocol reader writer &rest values)
  (let ((transport (thrift:protocol-output-transport protocol)))
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

(deftest write-integer-test ()
  (let ((stream (make-test-protocol)))
    (is (every #'(lambda (entry)
                   (apply #'test-read-write-equivalence stream entry))
               `((thrift:stream-read-bool thrift:stream-write-bool t nil)
                 ;; `thift:byte' is encoded the same in the protocol as i8,
                 ;; so it will be read back as i8. There is no good way
                 ;; around that, but these types are equivalent according
                 ;; to spec and `thrift:byte' is deprecated.
                 (thrift:stream-read-type thrift:stream-write-type ;thrift:byte
                                          thrift:map thrift:list thrift:set thrift:struct)
                 (thrift:stream-read-message-type thrift:stream-write-message-type thrift:call)
                 (thrift:stream-read-i8 thrift:stream-write-i8 ,(- (expt 2 7))  -1 0 1 ,(1- (expt 2 7)))
                 (thrift:stream-read-i16 thrift:stream-write-i16 ,(- (expt 2 15))  -1 0 1 ,(1- #x70f0) ,(1- (expt 2 15)))
                 (thrift:stream-read-i32 thrift:stream-write-i32 ,(- (expt 2 31))  -1 0 1 ,(1- #x7700ff00) ,(1- (expt 2 31)))
                 (thrift:stream-read-i64 thrift:stream-write-i64 ,(- (expt 2 63))  -1 0 1 ,(1- #x77770000ffff0000) ,(1- (expt 2 63))))))))

(deftest write-double-test ()
  (let ((stream (make-test-protocol)))
    (is (every #'(lambda (entry)
                   (apply #'test-read-write-equivalence stream entry))
               `((thrift:stream-read-double thrift:stream-write-double
                                            ,most-negative-double-float ,least-negative-double-float
                                            ,most-positive-double-float ,least-positive-double-float
                                            0.0d0 1.0d0 -1.0d0))))))

(deftest write-string-test ()
  (let ((stream (make-test-protocol)))
    (is (every #'(lambda (entry)
                   (apply #'test-read-write-equivalence stream entry))
               `((thrift:stream-read-string thrift:stream-write-string "a" "0123456789" ,*string-w/euro*))))))

(deftest write-binary-test ()
  (let ((stream (make-test-protocol)))
    (is (every #'(lambda (entry)
                   (apply #'test-read-write-equivalence stream entry))
               ;; presuming (unsigned-byte 8)
               `((thrift:stream-read-binary thrift:stream-write-binary #( 0 1 255)))))))

(deftest write-message-test ()
  (let ((struct (make-test-struct :field-one "one" :field-two 2))
        (stream (make-test-protocol)))
    (thrift:stream-write-message stream struct 'thrift:call)
    (rewind stream)
    (multiple-value-bind (name type sequence)
        (thrift:stream-read-message-begin stream)
      (let ((response (thrift:stream-read-struct stream 'test-struct)))
        (is (string= name "TestStruct"))
        (is (eq type 'thrift:call))
        (is (eql sequence 1))
        (is (typep response 'test-struct))
        (is (equal (test-struct-field-one response) "one"))
        (is (equal (test-struct-field-two response) 2))))))

(deftest write-struct-test ()
  (let ((struct (make-test-struct :field-one "one" :field-two 2))
        (stream (make-test-protocol)))
    (thrift:stream-write-struct stream struct)
    (rewind stream)
    (let* ((type 'test-struct)
           (result (thrift:stream-read-struct stream type)))
      (is (typep result 'test-struct))
      (is (equal (test-struct-field-one result) "one"))
      (is (equal (test-struct-field-two result) 2)))))

(deftest write-struct.inline ()
  (let ((struct (make-test-struct :field-one "one" :field-two 2))
        (stream (make-test-protocol)))
    (thrift:stream-write-struct stream struct 'test-struct)
    (rewind stream)
    (let ((result (thrift:stream-read-struct stream 'test-struct)))
      (is (typep result 'test-struct))
      (is (equal (test-struct-field-one result) "one"))
      (is (equal (test-struct-field-two result) 2)))))

(deftest write-struct.optional ()
  (let ((struct (make-instance 'test-large-struct :field-one 1 :field-two 2))
        (stream (make-test-protocol)))
    (assert (not (slot-boundp struct 'field-three)))
    (thrift:stream-write-struct stream struct 'test-large-struct)
    (rewind stream)
    (let ((result (thrift:stream-read-struct stream 'test-large-struct)))
      (is (typep result 'test-large-struct))
      (is (not (slot-boundp result 'field-three)))
      (is (equal (test-large-struct-field-one result) 1))
      (is (equal (test-large-struct-field-two result) 2)))))

(deftest write-field-test ()
  (let ((stream (make-test-protocol)))
    (is (every #'(lambda (entry)
                   (apply #'test-read-write-equivalence stream entry))
               `((,(lambda (p) (multiple-value-bind (value name id)
                                   (thrift:stream-read-field p)
                                 (when (ecase (thrift:protocol-field-id-mode stream)
                                         (:identifier-name (and (equal name "test") (null id)))
                                         (:identifier-number (and (null name) (equal id 10))))
                                   value)))
                   ,(lambda (p v) (thrift:stream-write-field p v :identifier-name "test" :identifier-number 10))
                   "a" "0123456789" ,*string-w/euro*))))))

(deftest write-map-test ()
  (let ((stream (make-test-protocol)))
    (is (every #'(lambda (entry)
                   (apply #'test-read-write-equivalence stream entry))
               `((thrift:stream-read-map thrift:stream-write-map ,(thrift:map 1 "a" 2 "b")))))))

(deftest write-list-test ()
  (let ((stream (make-test-protocol)))
    (is (every #'(lambda (entry)
                   (apply #'test-read-write-equivalence stream entry))
               `((thrift:stream-read-list thrift:stream-write-list
                                          (t nil) (1 2 3) (32767 1 -1 -32768)
                                          (,(expt 2 33) ,(- (expt 2 33)))
                                          ("asdf" ,*string-w/euro*)
                                          ;; no test for binary ! there is no type code
                                          ;; and the java and cpp versions just send it as a string
                                          ;; (#(1 2 3) #(4 5 6))
                                          (1.0d0 -1.0d0)
                                          (,(thrift:map 1 "a" 2 "b"))))))))

(deftest write-set-test ()
  (let ((stream (make-test-protocol)))
    (is (every #'(lambda (entry)
                   (apply #'test-read-write-equivalence stream entry))
               `((thrift:stream-read-set thrift:stream-write-set
                                         (t nil) (1 2 3) (32767 1 -1 -32768)))))))

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
                  (thrift:stream-write-struct stream struct)
                  (rewind stream)
                  (thrift:stream-read-struct stream))
                (setf dynamic-time (- (get-internal-run-time) time)
                      dynamic-bytes (- (gcbytes) bytes)))
              (let ((time (get-internal-run-time))
                    (bytes (gcbytes)))
                (dotimes (i count)
                  (rewind stream)
                  (thrift:stream-write-struct stream struct 'test-large-struct)
                  (rewind stream)
                  (thrift:stream-read-struct stream 'test-large-struct))
                (setf static-time (- (get-internal-run-time) time)
                      static-bytes (- (gcbytes) bytes)))
              (let ((time (get-internal-run-time))
                    (bytes (gcbytes)))
                (dotimes (i count)
                  (rewind stream)
                  (thrift:stream-write-struct stream struct 'test-large-struct)
                  (rewind stream)
                  (thrift:stream-read-struct stream 'test-large-struct result))
                (setf static-with-time (- (get-internal-run-time) time)
                      static-with-bytes (- (gcbytes) bytes)))
              (format *trace-output* "~%~d,~10T~d,~20T~d,~36T~d,~52T~d,~68T~d,~84T~d,~100T~d"
                      slot-count bound-count
                      dynamic-time static-time static-with-time
                      dynamic-bytes static-bytes static-with-bytes))))))

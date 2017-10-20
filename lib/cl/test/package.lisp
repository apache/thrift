(in-package #:common-lisp-user)


(defpackage #:thrift-test
  (:shadowing-import-from #:thrift #:byte #:set #:list #:map #:type-of #:float)
  (:use #:common-lisp #:thrift)
  (:import-from #:trivial-gray-streams
                #:stream-write-byte
                #:stream-read-byte
                #:stream-read-sequence
                #:stream-write-sequence
                #:stream-force-output
                #:stream-finish-output)
  #+ccl
  (:import-from #:ccl
                #:stream-tyo
                #:stream-tyi
                #:stream-reader
                #:stream-writer
                #:stream-position)
  #+sbcl
  (:export #:test
           #:with-test-services
           #:*test-location*))

(defpackage #:thrift-test-request (:use))

(defpackage #:thrift-test-response (:use))

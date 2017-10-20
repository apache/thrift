(in-package :thrift-test)

;;; this file defines tests for exception classes.
;;; (run-tests "conditions/.*")


(test conditions/thrift-error
      (stringp (princ-to-string (make-condition 'thrift-error))))


(test conditions/application-error 
      (stringp (princ-to-string (make-condition 'application-error
                                                :condition (nth-value 1 (ignore-errors (error "testing errors")))))))

(test conditions/protocol-error
      (stringp (princ-to-string (make-condition 'protocol-error
                                                :protocol (make-test-protocol)))))

(test conditions/transport-error
      (stringp (princ-to-string (make-condition 'transport-error))))

(test conditions/class-not-found-error
      (and (stringp (princ-to-string (make-condition 'class-not-found-error
                                                     :protocol (make-test-protocol)
                                                     :identifier "UnknownClass")))
           (typep (nth-value 1 (ignore-errors (class-not-found (make-test-protocol) "UnknownClass")))
                  'class-not-found-error)))

(test conditions/protocol-version-error
      (and (stringp (princ-to-string (make-condition 'protocol-version-error
                                                     :protocol (make-test-protocol)
                                                     :datum '(0 . 0) :expected-type '(1 . 1))))
           (typep (nth-value 1 (ignore-errors (invalid-protocol-version (make-test-protocol) 0 0)))
                  'protocol-version-error)))

(test conditions/element-type-error
      (and (stringp (princ-to-string (make-condition 'element-type-error
                                                     :protocol (make-test-protocol)
                                                     :container-type 'list :expected-type 'bool :element-type 'i16)))
           (typep (nth-value 1 (ignore-errors (invalid-element-type (make-test-protocol) 'list 'bool 'i16)))
                  'element-type-error)))

(test conditions/enum-type-error
      (and (stringp (princ-to-string (make-condition 'enum-type-error
                                                     :protocol (make-test-protocol)
                                                     :datum 3 :expected-type '(enum "x"))))
           (typep (nth-value 1 (ignore-errors (invalid-enum (make-test-protocol) '(enum "x") 3)))
                  'enum-type-error)))

(test conditions/field-size-error
      (and (stringp (princ-to-string (make-condition 'field-size-error
                                                     :protocol (make-test-protocol)
                                                     :name "fieldex" :number -1
                                                     :datum most-negative-fixnum :expected-type `(integer 0 ,most-positive-fixnum))))
           (typep (nth-value 1 (ignore-errors (invalid-field-size (make-test-protocol) -1 "fieldex" `(integer 0 ,most-positive-fixnum) most-negative-fixnum)))
                  'field-size-error)))

(test conditions/field-type-error
      (and (stringp (princ-to-string (make-condition 'field-type-error
                                                     :protocol (make-test-protocol)
                                                     :structure-type 'test-struct :name "fieldex" :number 17
                                                     :expected-type 'bool :datum 12345)))
           (typep (nth-value 1 (ignore-errors (invalid-field-type (make-test-protocol) 'test-struct 17 "fieldex" 'bool 12345)))
                  'field-type-error)))

(test conditions/unknown-field-error
      (and (stringp (princ-to-string (make-condition 'unknown-field-error
                                                     :protocol (make-test-protocol)
                                                     :structure-type 'test-struct :name "fieldex" :number 17 :datum 12345)))
           (typep (nth-value 1 (ignore-errors (unknown-field (make-test-protocol) 17 "fieldex" 'i16 12345)))
                  'null)))

(test conditions/unknown-method-error
      (and (stringp (princ-to-string (make-condition 'unknown-method-error
                                                     :protocol (make-test-protocol)
                                                     :identifier "methodex" :request t)))
           (typep (nth-value 1 (ignore-errors (unknown-method (make-test-protocol) "methodex" 12345 t)))
                  'unknown-method-error)))


(test conditions/struct-type-error
      (and (stringp (princ-to-string (make-condition 'struct-type-error
                                                     :protocol (make-test-protocol)
                                                     :expected-type 'test-struct :datum t)))
           (typep (nth-value 1 (ignore-errors (invalid-struct-type (make-test-protocol) 'test-struct t)))
                  'struct-type-error)))

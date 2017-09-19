;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: thrift-test; -*-

(in-package :thrift-test)

;;; tests for definition operators
;;; (run-tests "def-.*")

(test def-package.1
  (progn (def-package :test-package)
         (prog1 (and (find-package :test-package)
                     (find-package :test-package-implementation)
                     (find-package :test-package-response))
           (delete-package :test-package)
           (delete-package :test-package-implementation)
           (delete-package :test-package-response))))

(test def-package.2
  ;; redfinition should succeed
  (progn (def-package :test-package)
         (def-package :test-package)
         (prog1 (and (find-package :test-package)
                     (find-package :test-package-implementation)
                     (find-package :test-package-response))
           (delete-package :test-package)
           (delete-package :test-package-implementation)
           (delete-package :test-package-response))))
;;; (run-tests "def-package.*")

(test def-enum
  (progn (def-enum "TestEnum" ((first . 1) (second . 2)))
         (prog1 (and (eql (symbol-value 'test-enum.first) 1)
                     (eql (symbol-value 'test-enum.second) 2)))))
;;; (run-tests "def-enum")

(test def-constant
  (progn (def-constant "aConstant" 1)
         (prog1 (eql (symbol-value 'a-constant) 1)
           (unintern 'a-constant))))


(defgeneric test-struct-too-field1 (struct))
(defgeneric test-struct-too-field2 (struct))
(defgeneric test-struct-too-field3 (struct))
(defgeneric (setf test-struct-too-field2) (value struct))

(test def-struct
  (progn
    (def-struct "testStructToo" ())
    (def-struct "testStructToo"
      (("field1" 0 :type i32 :id 1)
       ("field2" nil :type i16 :id 2 :optional t)
       ("field3" "string value" :type string :id 3)))
    (let ((struct (make-instance 'test-struct-too :field1 -1)))
      (prog1 (and (equal (test-struct-too-field3 struct) "string value")
                  (not (slot-boundp struct 'field2))
                  (equal (test-struct-too-field1 struct) -1)
                  (typep (nth-value 1 (ignore-errors (setf (test-struct-too-field2 struct) 1.1)))
                         ;; some implementation may not constrain
                         ;; some signal a type error
                         #+ccl 'type-error
                         #+sbcl 'null))        ; how to enable slot type checks?
        (mapc #'(lambda (method) (remove-method (c2mop:method-generic-function method) method))
              (c2mop:specializer-direct-methods (find-class 'test-struct-too)))
        (setf (find-class 'test-struct-too) nil)))))
;;; (run-tests "def-struct")

(defgeneric test-exception-reason (exception))

(test def-exception
  (progn
    (eval '(def-exception "testException" (("reason" nil :type string :id 1))))
    (let ((ex (make-condition 'test-exception :reason "testing")))
      (prog1 (and (equal (test-exception-reason ex) "testing")
                  (eq (cl:type-of (nth-value 1 (ignore-errors (error ex))))
                      'test-exception)
                  (stringp (princ-to-string ex)))
        (mapc #'(lambda (method) (remove-method (c2mop:method-generic-function method) method))
              (c2mop:specializer-direct-methods (find-class 'test-exception)))
        (mapc #'(lambda (method) (remove-method (c2mop:method-generic-function method) method))
              (c2mop:specializer-direct-methods (find-class 'test-exception-exception-class)))
        (setf (find-class 'test-exception) nil)
        (setf (find-class 'test-exception-exception-class) nil)))))



(test def-service
  (progn (defun thrift-test-implementation::test-method (arg1 arg2) (format nil "~a ~a" arg1 arg2))
         (eval '(def-service "TestService" nil
                  (:method "testMethod" ((("arg1" i32 1) ("arg2" string 2)) string))))
         (let (request-protocol
               response-protocol
               (run-response-result nil))
           (flet ((run-response (request-stream)
                    (rewind request-stream)
                    (multiple-value-bind (identifier type seq)
                                         (stream-read-message-begin response-protocol)
                      (cond ((and (equal identifier "testMethod") (eq type 'call))
                             (setf run-response-result
                                   (funcall 'thrift-test-response::test-method t seq response-protocol)))
                            (t
                             (unknown-method response-protocol identifier seq
                                             (prog1 (stream-read-struct response-protocol)
                                               (stream-read-message-end response-protocol))))))))
             
             (multiple-value-setq (request-protocol response-protocol)
               (make-test-protocol-peers :request-hook #'run-response))
             
             (prog1 (and (equal (funcall 'thrift-test::test-method request-protocol 1 "testing")
                                "1 testing")
                         ;; if the first test succeed, this should also be true
                         (equal run-response-result "1 testing"))
               (fmakunbound 'thrift-test-implementation::test-method)
               (fmakunbound 'thrift-test::test-method)
               (fmakunbound 'thrift-test-response::test-method)
               )))))
;;; (run-tests "def-service")


(in-package :trivial-gray-streams-test)

;;; test framework

#|
  Used like this:

  (list (test (add) (assert (= 5 (+ 2 2))))
        (test (mul) (assert (= 4 (* 2 2))))
        (test (subst) (assert (= 3 (- 4 2)))))

  => ;; list of test results, 2 failed 1 passed
     (#<TEST-RESULT ADD :FAIL The assertion (= 5 (+ 2 2)) failed.>
      #<TEST-RESULT MUL :OK>
      #<TEST-RESULT SUBST :FAIL The assertion (= 3 (- 4 2)) failed.>)

|#

(defclass test-result ()
  ((name :type symbol
         :initarg :name
         :initform (error ":name is requierd")
         :accessor name)
   (status :type (or (eql :ok) (eql :fail))
           :initform :ok
           :initarg :status
           :accessor status)
   (cause :type (or null condition)
          :initform nil
          :initarg :cause
          :accessor cause)))

(defun failed-p (test-result)
  (eq (status test-result) :fail))

(defmethod print-object ((r test-result) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~S ~S~@[ ~A~]" (name r) (status r) (cause r))))

(defparameter *allow-debugger* nil)

(defun test-impl (name body-fn)
  (flet ((make-result (status &optional cause)
           (make-instance 'test-result :name name :status status :cause cause)))
    (handler-bind ((serious-condition
                    (lambda (c)
                      (unless *allow-debugger*
                        (format t "FAIL: ~A~%" c)
                        (let ((result (make-result :fail c)))
                          (return-from test-impl result))))))
      (format t "Running test ~S... " name)
      (funcall body-fn)
      (format t "OK~%")
      (make-result :ok))))

(defmacro test ((name) &body body)
  "If the BODY signals a SERIOUS-CONDITION
this macro returns a failed TEST-RESULT; otherwise
returns a successfull TEST-RESULT."
  `(test-impl (quote ,name) (lambda () ,@body)))

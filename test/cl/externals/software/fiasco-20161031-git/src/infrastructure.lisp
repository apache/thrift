;;; -*- mode: Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :fiasco)


;;; Special variables
;;;
;; Warning: setf-ing these variables in not a smart idea because other
;; systems may rely on their default value.  It's smarter to rebind
;; them in an :around method from your .asd or shadow fiasco:deftest
;; with your own that sets their keyword counterparts.
(defvar *suite*)
(defvar *root-suite*)
(defvar *package-bound-suites* (make-hash-table))
(defvar *print-test-run-progress* t)
(defvar *test-progress-print-right-margin* 80)
(defvar *debug-on-unexpected-error* t)
(defvar *debug-on-assertion-failure* t)
(defvar *test-result-history* '())
(defvar *last-test-result* nil)
(defvar *failures-and-errors-are-expected* nil)
(defvar *always-show-failed-sexp* nil)
(defvar *warn-about-test-redefinitions* nil)

;; TODO introduce *progress-output*
(defvar *test-run-standard-output* '*standard-output*
  "*STANDARD-OUTPUT* is bound to (eval *test-run-standard-output*) at
the toplevel entry point to any test.")

(defvar *tests* (make-hash-table :test 'eql)) ; this is not thread-safe, but...

(defmacro without-debugging (&body body)
  `(let* ((*debug-on-unexpected-error* nil)
          (*debug-on-assertion-failure* nil))
    ,@body))


;;; Testable class
;;; 
(defclass testable ()
  ((name :accessor name-of :initarg :name :type symbol)
   (parent :initform nil :accessor parent-of :type (or null testable))
   (children :initform (make-hash-table) :accessor children-of
             :initarg :children
             :documentation "A mapping from testable names to testables")
   (auto-call :initform t :accessor auto-call? :initarg :auto-call :type boolean
              :documentation "Controls whether to automatically call
this test when its parent suite is invoked. Enabled by default.")))

(defmethod print-object ((self testable) s)
  (print-unreadable-object (self s :type nil :identity nil)
    (format s "test ~s" (name-of self))
    (let* ((children (count-tests self)))
      (unless (zerop children) (format s " :tests ~s" children))))
  self)

(defvar *ignore-package-suite-mismatch* nil)

(defmethod shared-initialize :after
    ((self testable) slot-names &key (in (or (parent-of self)
                                             (find-suite-for-package *package*)
                                             (and (boundp '*suite*)
                                                  *suite*))
                                         in-supplied-p))
  (declare (ignore slot-names))
  (assert (name-of self))
  (setf (find-test (name-of self)) self)
  ;; make sure the specialized writer below is triggered
  (let ((*ignore-package-suite-mismatch* in-supplied-p))
    (setf (parent-of self) in)))

(defmethod (setf parent-of) :around (new-parent (self testable))
  (assert (typep new-parent '(or null testable)))
  (when (and new-parent
             (symbol-package (name-of self)) ; leave alone tests named
                                        ; by uninterned symbols
             (not (eq new-parent *root-suite*))
             (not (eq (symbol-package (name-of new-parent))
                      (symbol-package (name-of self))))
             (not *ignore-package-suite-mismatch*)
             (not (gethash (package-of self) *package-bound-suites*)))
    (warn 'test-style-warning
          :test self
          :format-control "Adding test under parent ~S which is in a~
different package (parent: ~A, child: ~A). Maybe a~
missing (in-root-suite)?"
          :format-arguments (list new-parent (symbol-package
                                              (name-of new-parent))
                                  (symbol-package (name-of self)))))
  (let* ((old-parent (parent-of self)))
    (when old-parent
      (remhash (name-of self) (children-of old-parent)))
    (prog1
        (call-next-method)
      (when new-parent
        (setf (gethash (name-of self) (children-of new-parent)) self)))))

(defgeneric count-tests (testable)
  (:method ((self testable))
           (+ (hash-table-count (children-of self))
              (loop
                :for child :being :the :hash-values :of (children-of self)
                :summing (count-tests child)))))


;;; The object that represents a particular test run.
;;;
;;; Curiously called a "context"
;;;
(defvar *context* nil
  "Status and progress info for a particular test run.")


(defvar *current-test* nil
  "Current singleton instance of TEST executing its associated DEFTEST lambda.")

(defmacro check-required (sym) `(error "Must provide ~a" ,sym))

(defclass context ()
  ((test :accessor test-of :initarg :test)
   (internal-realtime-spent-with-test
    :initform nil
    :accessor internal-realtime-spent-with-test-of
    :initarg :internal-realtime-spent-with-test)
   (actual-test-arguments :accessor actual-test-arguments-of
                          :initarg :actual-test-arguments
                          :initform (check-required 'actual-test-arguments))
   ;; recording
   ;; 
   (self-failures :initform nil)
   (self-assertions :initform nil)
   ;; tree structure
   ;; 
   (parent-context
    :initarg :parent-context :initform nil :accessor parent-context-of)
   (children-contexts
    :initform nil :accessor children-contexts-of)))

(defgeneric failures-of (context)
  (:method ((context context))
    (reduce #'append (mapcar (alexandria:rcurry #'slot-value 'self-failures)
                             (all-test-runs-of context)))))

(defgeneric assertions-of (context)
  (:method ((context context))
    (reduce #'append (mapcar (alexandria:rcurry #'slot-value 'self-assertions)
                             (all-test-runs-of context)))))

(defmethod initialize-instance :after ((obj context) &key parent-context &allow-other-keys)
  (setf (parent-context-of obj) parent-context))

(defmethod (setf parent-context-of) :before (new-parent (obj context))
  (declare (ignore new-parent))
  (let ((ex-parent (parent-context-of obj)))
    (when ex-parent
      (setf (children-contexts-of ex-parent)
            (remove obj (children-contexts-of ex-parent))))))

(defmethod (setf parent-context-of) :after (new-parent (obj context))
  (when new-parent
    (push obj (children-contexts-of new-parent))))

(defmethod shared-initialize ((obj context) slots &rest args)
  (declare (ignore slots args))
  (call-next-method)
  (with-slots (self-failures self-assertions children-contexts) obj
    (setq self-failures nil self-assertions nil children-contexts nil)))

(defgeneric real-time-spent-in-seconds (context)
  (:method ((self context))
    (let* ((time-spent (internal-realtime-spent-with-test-of self)))
      (when time-spent
        (coerce (/ time-spent
                   internal-time-units-per-second)
                'float)))))


;;; Conditions
;;;

(define-condition test-assertion (warning)
  ()
  (:documentation "Signalled when an assertion such as IS is encountered"))

(define-condition is-assertion (test-assertion)
  ((form :initarg :form
         :initform (check-required 'form)
         :accessor form-of)
   (message :initarg :message
            :initform (check-required 'message)
            :accessor message-of)
   (message-args :initarg :message-args
                 :initform (check-required 'message-args)
                 :accessor message-args-of)))

(define-condition signals-assertion (test-assertion)
  ((expected-condition-type :initarg :expected-condition-type
                            :accessor expected-condition-type-of)))

(define-condition not-signals-assertion (test-assertion)
  ((expected-condition-type :initarg :expected-condition-type
                            :accessor expected-condition-type-of)))

(define-condition finishes-assertion (test-assertion) ())

(define-condition test-related-condition ()
  ((test :initform (check-required 'test) :accessor test-of :initarg :test)))

(define-condition test-started (test-related-condition) ())

(define-condition test-style-warning (style-warning test-related-condition
                                      simple-warning)
  ())

(define-condition failure ()
  ((context :initform *context* :accessor context-of
            :documentation "Might perfectly well be NIL") 
   (progress-char :initform #\X :accessor progress-char-of
                  :initarg :progress-char :allocation :class)
   (expected :initarg :expected :initform *failures-and-errors-are-expected*
             :accessor expected-p)))

(define-condition failed-assertion (failure)
  ((form :accessor form-of :initarg :form)
   (format-control :accessor format-control-of :initarg :format-control)
   (format-arguments :initform nil :accessor format-arguments-of
                     :initarg :format-arguments))
  (:report (lambda (c stream)
             (if (context-of c)
                 (format stream "Test assertion failed when running ~a:~%~%"
                         (name-of (test-of (context-of c))))
                 (format stream "Test assertion failed:~%~%"))
             (describe c stream))))

(defmethod describe-object ((self failed-assertion) stream)
  (let ((*print-circle* nil))
    ;; (format stream "Form ~S inside test chain: ~{~A~^,~}"
    ;;         (form-of self)
    ;;         (mapcar (compose #'name-of #'test-of)
    ;;                 (loop for context = (context-of self) then (parent-context-of context)
    ;;                       while context collect context)))
    (apply #'format stream (format-control-of self)
           (format-arguments-of self))))

(define-condition missing-condition (failure)
  ((expected-condition-type :initarg :expected-condition-type
                            :accessor expected-condition-type-of)))

(defmethod describe-object ((self missing-condition) stream)
  (let ((*print-circle* nil))
    (format stream "~S failed to signal a condition of type ~S" (form-of self)
            (expected-condition-type-of self))))

(define-condition unwanted-condition (failure)
  ((expected-condition-type :initarg :expected-condition-type :accessor expected-condition-type-of)
   (observed-condition :initarg :observed-condition :accessor observed-condition-of)))

(defmethod describe-object ((self unwanted-condition) stream)
  (let ((*print-circle* nil))
    (format stream "~S signaled an unwanted condition ~S"
            (form-of self) (observed-condition-of self))))

(define-condition unexpected-error (failure)
  ((error :accessor error-of :initform (error "Must provide ~S" 'error)
          :initarg :error)
   (progress-char :initform #\E :accessor progress-char-of
                  :initarg :progress-char :allocation :class))
  (:report (lambda (c stream)
             (if (context-of c)
                 (format stream "Unexpected error when running ~a:~%~%"
                         (name-of (test-of (context-of c))))
                 (format stream "Unexpected error:~%~%"))
             (describe c stream))))

(defmethod describe-object ((self unexpected-error) stream)
  (format stream "~a" (error-of self)))

(defmethod print-object ((self unexpected-error) stream)
  (print-unreadable-object (self stream :identity nil :type nil)
    (format stream "error ~{~A~^,~}: ~S"
            (mapcar (compose #'name-of #'test-of)
                    (loop for context = (context-of self) then (parent-context-of context)
                          while context collect context))
            (error-of self))))


;;; Test repository
;;; 
(defun find-test (name &key (otherwise :error))
  (multiple-value-bind (test found-p)
      (if (typep name 'testable)
          (values name t)
          (gethash name *tests*))
    (when (and (not found-p)
               otherwise)
      (typecase otherwise
        (symbol (ecase otherwise
                  (:error (error "Testable called ~A was not found" name))))
        (function (funcall otherwise))
        (t (setf test otherwise))))
    (values test found-p)))

(defun (setf find-test) (new-value key)
  (if new-value
      (progn
        (when (and *warn-about-test-redefinitions*
                   (gethash key *tests*))
          (warn 'test-style-warning
                :format-control "redefining test ~A"
                :format-arguments (list
                                   (let ((*package* #.(find-package "KEYWORD")))
                                          (format nil "~S" key)))))
        (setf (gethash key *tests*) new-value))
      (delete-test key)))

(defun delete-test (name &rest args)
  (let* ((test (apply #'find-test name args))
         (name (name-of test))
         (parent (when test
                   (parent-of test))))
    (when test
      (assert (or (not (eq *suite* test))
                  (parent-of test))
              () "You can not remove a test which is the current suite~
and has no parent")
      (remhash name *tests*)
      (setf (parent-of test) nil)
      (fmakunbound (name-of test))
      (loop
        :for subtest :being :the :hash-values :of (children-of test)
        :do (delete-test (name-of subtest)))
      (when (eq *suite* test)
        (setf *suite* parent)))
    test))


;;;;;;
;;; the real thing

(defun all-test-runs-of (context)
  (cons context
        (loop for context in (children-contexts-of context)
                append (all-test-runs-of context))))

(defun extract-test-run-statistics (context)
  (let* ((failures (failures-of context))
         (failed-assertion-count (count-if (of-type '(or
                                                      failed-assertion
                                                      missing-condition
                                                      unwanted-condition))
                                           failures))
         (unexpected-error-count (count-if (of-type 'unexpected-error)
                                           failures))
         (expected-count (count-if 'expected-p failures)))
    (list :number-of-tests-run (length (all-test-runs-of context))
          :number-of-assertions (length (assertions-of context))
          :number-of-failures (length failures)
          :number-of-expected-failures expected-count
          :number-of-failed-assertions failed-assertion-count
          :number-of-unexpected-errors unexpected-error-count)))

(defmethod print-object ((self context) stream)
  (print-unreadable-object (self stream :identity nil :type nil)
    (destructuring-bind (&key number-of-tests-run
                           number-of-assertions
                           number-of-failures
                           number-of-failed-assertions
                           number-of-unexpected-errors
                           number-of-expected-failures
                         &allow-other-keys)
        (extract-test-run-statistics self)
      (format stream "test-run of ~a: ~A test~:P, ~A assertion~:P, ~A failure~:P in ~
~A sec~[~:; (~A failed assertion~:P, ~A error~:P, ~A expected)~]"
              (name-of (test-of self))
              number-of-tests-run
              number-of-assertions
              number-of-failures
              (real-time-spent-in-seconds self)
              number-of-failures ; index in the ~[] conditional
              number-of-failed-assertions
              number-of-unexpected-errors
              (cond ((= number-of-expected-failures number-of-failures)
                     "all")
                    ((zerop number-of-expected-failures)
                     "none")
                    (t number-of-expected-failures))))))

(defmacro without-test-progress-printing (&body body)
  (with-unique-names (old-state)
    `(let ((,old-state *print-test-run-progress*))
      (unwind-protect
           (progn
             (setf *print-test-run-progress* nil)
             ,@body)
        (setf *print-test-run-progress* ,old-state)))))

(defmacro with-toplevel-restarts (&body body)
  `(block restart-wrapper
     (restart-bind
         ((continue-without-debugging
            (lambda ()
              (setf *debug-on-unexpected-error* nil
                    *debug-on-assertion-failure* nil)
              (continue))
            :report-function (lambda (stream)
                               (format stream "~
~@<Turn off debugging for this test session and invoke the first ~
CONTINUE restart~@:>")))
          (continue-without-debugging-errors
            (lambda ()
              (setf *debug-on-unexpected-error* nil)
              (continue))
            :report-function (lambda (stream)
                               (format stream "~
~@<Do not stop at unexpected errors for the rest of this test session ~
and continue by invoking the first CONTINUE restart~@:>")))
          (continue-without-debugging-assertions
            (lambda ()
              (setf *debug-on-assertion-failure* nil)
              (continue))
            :report-function (lambda (stream)
                               (format stream "~
~@<Do not stop at failed assertions for the rest of this test session ~
and continue by invoking the first CONTINUE restart~@:>")))
          (abort-testing
            (lambda ()
              (return-from restart-wrapper))
            :report-function (lambda (stream)
                               (format stream "~@<Abort the entire ~
test session~@:>"))))
       ,@body)))

(defun run-failed-tests (&optional (test-run *last-test-result*))
  (warn "Re-running failed tests without considering their dynamic
environment, which may affect their behaviour!")
  (with-toplevel-restarts
    (loop
     :for failure in (failures-of test-run)
     :do (apply (name-of (test-of (context-of failure)))
                (actual-test-arguments-of (context-of failure))))
    (when *print-test-run-progress*
      (terpri *debug-io*))))

(defmacro with-expected-failures* (&whole whole condition &body body)
  "Run BODY and registering failure conditions as expected failure iff
CONDITION."
  (with-unique-names (with-expected-failures-block starting-failure-count)
    `(let* ((*failures-and-errors-are-expected* ,condition)
            (,starting-failure-count
              (length (failures-of *context*))))
       (block ,with-expected-failures-block
         (restart-case
             (handler-bind ((serious-condition
                             ;; TODO comment on why it's needed here...
                             (lambda (error)
                               (record-failure 'unexpected-error :error error)
                               (return-from ,with-expected-failures-block
                                 (values)))))
               (multiple-value-prog1
                   (progn ,@body)
                 (unless (< ,starting-failure-count
                            (length (failures-of *context*)))
                   (warn "The following ~S block ran without any failures: ~S"
                         'with-expected-failures* ',whole))))
           (continue ()
             :report (lambda (stream)
                       (format stream "~
~@<Skip the rest of the innermost WITH-EXPECTED-FAILURES body and ~
continue by returning (values)~@:>"))
             (values)))))))

(defmacro with-expected-failures (&body body)
  "Run BODY registering failured conditions as expected failures."
  `(with-expected-failures* t ,@body))


;;;;;;
;;; some utils

(define-condition illegal-lambda-list (error)
  ((lambda-list :accessor lambda-list-of :initarg :lambda-list)))

(defun illegal-lambda-list (lambda-list)
  (error 'illegal-lambda-list :lambda-list lambda-list))

(defun parse-lambda-list (lambda-list visitor &key macro)
  ;; TODO delme, and use alexandria:parse-ordinary-lambda-list
  (declare #+nil (optimize (speed 3))
           (type list lambda-list)
           (type (or symbol function) visitor))
  (let ((args lambda-list))
    (labels
        ((fail ()
           (illegal-lambda-list lambda-list))
         (process-&whole ()
           (assert (eq (first args) '&whole))
           (pop args)
           (unless macro
             (fail))
           (let ((whole (pop args)))
             (unless whole
               (fail))
             (funcall visitor '&whole whole whole))
           (case (first args)
             (&key          (entering-&key))
             (&rest         (process-&rest))
             (&optional     (entering-&optional))
             (&body         (process-&body))
             (&environment  (process-&environment))
             ((&whole &aux &allow-other-keys) (fail))
             (t             (process-required))))
         (process-&body ()
           (assert (eq (first args) '&body))
           (pop args)
           (unless macro
             (fail))
           (let ((body (pop args)))
             (unless (null args)
               (fail))
             (unless body
               (fail))
             (funcall visitor '&body body body)))
         (process-&environment ()
           (assert (eq (first args) '&environment))
           (pop args)
           (unless macro
             (fail))
           (let ((env (pop args)))
             (unless env
               (fail))
             (funcall visitor '&environment env env))
           (case (first args)
             (&key          (entering-&key))
             (&rest         (process-&rest))
             (&optional     (entering-&optional))
             (&body         (process-&body))
             (&aux          (process-&aux))
             ((&whole &environment &allow-other-keys) (fail))
             (t             (process-required))))
         (process-required ()
           (unless args
             (done))
           (case (first args)
             (&key          (entering-&key))
             (&rest         (process-&rest))
             (&optional     (entering-&optional))
             (&body         (process-&body))
             (&environment  (process-&environment))
             ((&whole &allow-other-keys) (fail))
             (&aux          (entering-&aux))
             (t
              (let ((arg (pop args)))
                (funcall visitor nil arg arg))
              (process-required))))
         (process-&rest ()
           (assert (eq (first args) '&rest))
           (pop args)
           (let ((rest (pop args)))
             (unless rest
               (fail))
             (funcall visitor '&rest rest rest))
           (unless args
             (done))
           (case (first args)
             (&key               (entering-&key))
             (&environment       (process-&environment))
             ((&whole &optional &rest &body &allow-other-keys) (fail))
             (&aux               (entering-&aux))
             (t                  (fail))))
         (entering-&optional ()
           (assert (eq (first args) '&optional))
           (pop args)
           (process-&optional))
         (process-&optional ()
           (unless args
             (done))
           (case (first args)
             (&key               (entering-&key))
             (&rest              (process-&rest))
             (&body              (process-&body))
             ((&whole &optional &environment &allow-other-keys) (fail))
             (&aux               (entering-&aux))
             (t
              (let* ((arg (ensure-list (pop args)))
                     (name (first arg))
                     (default (second arg)))
                (funcall visitor '&optional name arg nil default))
              (process-&optional))))
         (entering-&key ()
           (assert (eq (first args) '&key))
           (pop args)
           (process-&key))
         (process-&key ()
           (unless args
             (done))
           (case (first args)
             (&allow-other-keys (funcall visitor '&allow-other-keys nil nil))
             ((&key &optional &whole &environment &body) (fail))
             (&aux                    (entering-&aux))
             (t
              (let* ((arg (ensure-list (pop args)))
                     (name-part (first arg))
                     (default (second arg))
                     (external-name (if (consp name-part)
                                        (progn
                                          (unless (= (length name-part) 2)
                                            (illegal-lambda-list lambda-list))
                                          (first name-part))
                                        (intern (symbol-name name-part)
                                                #.(find-package "KEYWORD"))))
                     (local-name (if (consp name-part)
                                     (second name-part)
                                     name-part)))
                (funcall visitor '&key local-name arg external-name default))
              (process-&key))))
         (entering-&aux ()
           (assert (eq (first args) '&aux))
           (pop args)
           (process-&aux))
         (process-&aux ()
           (unless args
             (done))
           (case (first args)
             ((&whole &optional &key &environment
                      &allow-other-keys &aux &body) (fail))
             (t
              (let ((arg (ensure-list (pop args))))
                (funcall visitor '&aux (first arg) arg))
              (process-&aux))))
         (done ()
           (return-from parse-lambda-list (values))))
      (when args
        (case (first args)
          (&whole (process-&whole))
          (t      (process-required)))))))

(defun lambda-list-to-funcall-list (args)
  (multiple-value-bind (requireds optionals rest keywords)
      (parse-ordinary-lambda-list args)
    (values (append requireds
                    (loop
                      :for entry :in optionals
                      :collect (first entry))
                    (loop
                      :for entry :in keywords
                      :appending (list (first (first entry))
                                       (second (first entry)))))
            rest)))

(defun lambda-list-to-funcall-expression (function args)
  (multiple-value-bind (arg-list rest-variable)
      (lambda-list-to-funcall-list args)
    (if rest-variable
        `(apply ,function ,@arg-list ,rest-variable)
        `(funcall ,function ,@arg-list))))

(defun lambda-list-to-value-list-expression (args)
  ;; TODO use alexandria:parse-ordinary-lambda-list
  ;; JT@15/08/14: Seconded
  `(list ,@(let ((result (list)))
             (parse-lambda-list args
                                (lambda (kind name entry
                                         &optional external-name default)
                                  (declare (ignore entry external-name default))
                                  (case kind
                                    (&allow-other-keys)
                                    (t (push `(cons ',name ,name) result)))))
             (nreverse result))))

(defun lambda-list-to-variable-name-list (args &key macro include-specials)
  ;; TODO use alexandria:parse-ordinary-lambda-list
  (let ((result (list))
        (rest-variable-name nil)
        (whole-variable-name nil)
        (env-variable-name nil))
    (parse-lambda-list args
                       (lambda (kind name entry &optional external-name default)
                         (declare (ignore entry external-name default))
                         (case kind
                           (&allow-other-keys )
                           (&environment      (setf env-variable-name name)
                                              (when include-specials
                                                (push name result)))
                           (&whole            (setf whole-variable-name name)
                                              (when include-specials
                                                (push name result)))
                           ((&rest &body)     (setf rest-variable-name name)
                                              (when include-specials
                                                (push name result)))
                           (t                 (push name result))))
                       :macro macro)
    (values (nreverse result)
            rest-variable-name
            whole-variable-name
            env-variable-name)))

(defun funcall-test-with-feedback-message (test-function &rest args)
  "Run TEST non-interactively and print results to *STANDARD-OUTPUT*.
This function is ideal for ASDF:TEST-OP's."
  (let* ((*test-run-standard-output* (make-broadcast-stream))
         (result (without-debugging (apply test-function args)))
         (*package* (find-package :common-lisp)))
    (format *standard-output*
"The result of ~S is:

  ~A

For more details run it from the REPL."
            test-function result)
    result))

;; Local Variables:
;; coding: utf-8-unix
;; End:

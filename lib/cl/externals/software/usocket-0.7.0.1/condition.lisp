;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

;; Condition signalled by operations with unsupported arguments
;; For trivial-sockets compatibility.

(define-condition insufficient-implementation (error)
  ((feature :initarg :feature :reader feature)
   (context :initarg :context :reader context
    :documentation "String designator of the public API function which
the feature belongs to."))
  (:documentation "The ancestor of all errors usocket may generate
because of insufficient support from the underlying implementation
with respect to the arguments given to `function'.

One call may signal several errors, if the caller allows processing
to continue.
"))

(define-condition unsupported (insufficient-implementation)
  ((minimum :initarg :minimum :reader minimum
            :documentation "Indicates the minimal version of the
implementation required to support the requested feature."))
  (:report (lambda (c stream)
	     (format stream "~A in ~A is unsupported."
		     (feature c) (context c))
	     (when (minimum c)
	       (format stream " Minimum version (~A) is required."
		       (minimum c)))))
  (:documentation "Signalled when the underlying implementation
doesn't allow supporting the requested feature.

When you see this error, go bug your vendor/implementation developer!"))

(define-condition unimplemented (insufficient-implementation)
  ()
  (:report (lambda (c stream)
	     (format stream "~A in ~A is unimplemented."
		     (feature c) (context c))))
  (:documentation "Signalled if a certain feature might be implemented,
based on the features of the underlying implementation, but hasn't
been implemented yet."))

;; Conditions raised by sockets operations

(define-condition socket-condition (condition)
  ((socket :initarg :socket
           :accessor usocket-socket))
  ;;###FIXME: no slots (yet); should at least be the affected usocket...
  (:documentation "Parent condition for all socket related conditions."))

(define-condition socket-error (socket-condition error)
  () ;; no slots (yet)
  (:documentation "Parent error for all socket related errors"))

(define-condition ns-condition (condition)
  ((host-or-ip :initarg :host-or-ip
               :accessor host-or-ip))
  (:documentation "Parent condition for all name resolution conditions."))

(define-condition ns-error (ns-condition error)
  ()
  (:documentation "Parent error for all name resolution errors."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun define-usocket-condition-class (class &rest parents)
    `(progn
       (define-condition ,class ,parents ())
       (export ',class))))

(defmacro define-usocket-condition-classes (class-list parents)
  `(progn ,@(mapcar #'(lambda (x)
                        (apply #'define-usocket-condition-class
                               x parents))
                    class-list)))

;; Mass define and export our conditions
(define-usocket-condition-classes
  (interrupted-condition)
  (socket-condition))

(define-condition unknown-condition (socket-condition)
  ((real-condition :initarg :real-condition
                   :accessor usocket-real-condition))
  (:documentation "Condition raised when there's no other - more applicable -
condition available."))


;; Mass define and export our errors
(define-usocket-condition-classes
  (address-in-use-error
   address-not-available-error
   bad-file-descriptor-error
   connection-refused-error
   connection-aborted-error
   connection-reset-error
   invalid-argument-error
   no-buffers-error
   operation-not-supported-error
   operation-not-permitted-error
   protocol-not-supported-error
   socket-type-not-supported-error
   network-unreachable-error
   network-down-error
   network-reset-error
   host-down-error
   host-unreachable-error
   shutdown-error
   timeout-error
   deadline-timeout-error
   invalid-socket-error
   invalid-socket-stream-error)
  (socket-error))

(define-condition unknown-error (socket-error)
  ((real-error :initarg :real-error
               :accessor usocket-real-error
               :initform nil)
   (errno      :initarg :errno
               :reader usocket-errno
               :initform 0))
  (:report (lambda (c stream)
             (typecase c
               (simple-condition
                (format stream
                        (simple-condition-format-control (usocket-real-error c))
                        (simple-condition-format-arguments (usocket-real-error c))))
               (otherwise
                (format stream "The condition ~A occurred with errno: ~D."
                        (usocket-real-error c)
                        (usocket-errno c))))))
  (:documentation "Error raised when there's no other - more applicable -
error available."))

(define-usocket-condition-classes
  (ns-try-again-condition)
  (ns-condition))

(define-condition ns-unknown-condition (ns-condition)
  ((real-condition :initarg :real-condition
                   :accessor ns-real-condition
                   :initform nil))
  (:documentation "Condition raised when there's no other - more applicable -
condition available."))

(define-usocket-condition-classes
  ;; the no-data error code in the Unix 98 api
  ;; isn't really an error: there's just no data to return.
  ;; with lisp, we just return NIL (indicating no data) instead of
  ;; raising an exception...
  (ns-host-not-found-error
   ns-no-recovery-error)
  (ns-error))

(define-condition ns-unknown-error (ns-error)
  ((real-error :initarg :real-error
               :accessor ns-real-error
               :initform nil))
  (:report (lambda (c stream)
             (typecase c
               (simple-condition
                (format stream
                        (simple-condition-format-control (usocket-real-error c))
                        (simple-condition-format-arguments (usocket-real-error c))))
               (otherwise
                (format stream "The condition ~A occurred." (usocket-real-error c))))))
  (:documentation "Error raised when there's no other - more applicable -
error available."))

(defmacro with-mapped-conditions ((&optional socket) &body body)
  `(handler-bind ((condition #'(lambda (c) (handle-condition c ,socket))))
    ,@body))

(defparameter +unix-errno-condition-map+
  `(((11) . ns-try-again-condition) ;; EAGAIN
    ((35) . ns-try-again-condition) ;; EDEADLCK
    ((4) . interrupted-condition))) ;; EINTR

(defparameter +unix-errno-error-map+
  ;;### the first column is for non-(linux or srv4) systems
  ;; the second for linux
  ;; the third for srv4
  ;;###FIXME: How do I determine on which Unix we're running
  ;;          (at least in clisp and sbcl; I know about cmucl...)
  ;; The table below works under the assumption we'll *only* see
  ;; socket associated errors...
  `(((48 98) . address-in-use-error)
    ((49 99) . address-not-available-error)
    ((9) . bad-file-descriptor-error)
    ((61 111) . connection-refused-error)
    ((54 104) . connection-reset-error)
    ((53 103) . connection-aborted-error)
    ((22) . invalid-argument-error)
    ((55 105) . no-buffers-error)
    ((12) . out-of-memory-error)
    ((45 95) . operation-not-supported-error)
    ((1) . operation-not-permitted-error)
    ((43 92) . protocol-not-supported-error)
    ((44 93) . socket-type-not-supported-error)
    ((51 101) . network-unreachable-error)
    ((50 100) . network-down-error)
    ((52 102) . network-reset-error)
    ((58 108) . already-shutdown-error)
    ((60 110) . timeout-error)
    ((64 112) . host-down-error)
    ((65 113) . host-unreachable-error)))

(defun map-errno-condition (errno)
  (cdr (assoc errno +unix-errno-error-map+ :test #'member)))

(defun map-errno-error (errno)
  (cdr (assoc errno +unix-errno-error-map+ :test #'member)))

(defparameter +unix-ns-error-map+
  `((1 . ns-host-not-found-error)
    (2 . ns-try-again-condition)
    (3 . ns-no-recovery-error)))

(defmacro unsupported (feature context &key minimum)
  `(cerror "Ignore it and continue" 'unsupported
	   :feature ,feature
	   :context ,context
	   :minimum ,minimum))

(defmacro unimplemented (feature context)
  `(signal 'unimplemented :feature ,feature :context ,context))


;;; People may want to ignore all unsupported warnings, here it is.
(defmacro ignore-unsupported-warnings (&body body)
  `(handler-bind ((unsupported
                   #'(lambda (c)
                       (declare (ignore c)) (continue))))
     (progn ,@body)))

(in-package :closer-mop)

;; We need a new standard-class for various things.

(defclass standard-class (cl:standard-class excl:lockable-object)
  ((valid-slot-allocations :initform '(:instance :class)
                           :accessor valid-slot-allocations
                           :reader excl::valid-slot-allocation-list)))

(define-validate-superclass-method standard-class cl:standard-class)

;; Allegro defines an extra check for :allocation kinds. AMOP expects any kind to be
;; permissible, though. This is corrected here.

(cl:defmethod direct-slot-definition-class :before ((class standard-class) &key allocation &allow-other-keys)
  (unless (eq (class-of class) (find-class 'standard-class))
    (excl:with-locked-object
     (class :non-smp :without-scheduling)
     (pushnew allocation (valid-slot-allocations class)))))

;;; In Allegro, slot-boundp-using-class and slot-makunbound-using-class are specialized
;;; on slot names instead of effective slot definitions. In order to fix this,
;;; we need to rewire the slot access protocol.

#-(version>= 8 1)
(progn
  (cl:defmethod slot-boundp-using-class
             ((class standard-class) object (slot symbol))
    (declare (optimize (speed 3) (debug 0) (safety 0)
                       (compilation-speed 0)))
    (let ((slotd (find slot (class-slots class)
                       :test #'eq
                       :key #'slot-definition-name)))
      (if slotd
        (slot-boundp-using-class class object slotd)
        (slot-missing class object slot 'slot-boundp))))

  (cl:defmethod slot-boundp-using-class
             ((class standard-class) object (slotd standard-effective-slot-definition))
    (declare (optimize (speed 3) (debug 0) (safety 0)
                       (compilation-speed 0)))
    (slot-boundp-using-class
     (load-time-value (class-prototype (find-class 'cl:standard-class)))
     object
     (slot-definition-name slotd))))

(cl:defmethod slot-makunbound-using-class
           ((class standard-class) object (slot symbol))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (let ((slotd (find slot (class-slots class)
                     :test #'eq
                     :key #'slot-definition-name)))
    (if slotd
      (slot-makunbound-using-class class object slotd)
      (slot-missing class object slot 'slot-makunbound))))

(cl:defmethod slot-makunbound-using-class
           ((class standard-class) object (slotd standard-effective-slot-definition))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (slot-makunbound-using-class
   (load-time-value (class-prototype (find-class 'cl:standard-class)))
   object
   (slot-definition-name slotd)))

;;; New generic functions.

(cl:defmethod initialize-instance :around
  ((gf standard-generic-function) &rest initargs &key (method-class nil method-class-p))
  (if (and method-class-p (symbolp method-class))
    (apply #'call-next-method gf
           :method-class (find-class method-class)
           initargs)
    (call-next-method)))

(cl:defmethod reinitialize-instance :around
  ((gf standard-generic-function) &rest initargs &key (method-class nil method-class-p))
  (if (and method-class-p (symbolp method-class))
    (apply #'call-next-method gf
           :method-class (find-class method-class)
           initargs)
    (call-next-method)))

;;; The following three methods ensure that the dependent protocol
;;; for generic function works.

;; The following method additionally ensures that
;; compute-discriminating-function is triggered.

(cl:defmethod reinitialize-instance :after
  ((gf standard-generic-function) &rest initargs)
  (set-funcallable-instance-function gf (compute-discriminating-function gf))
  (map-dependents gf (lambda (dep) (apply #'update-dependent gf dep initargs))))

(cl:defmethod add-method :after
  ((gf standard-generic-function) method)
  (map-dependents gf (lambda (dep) (update-dependent gf dep 'add-method method))))

(cl:defmethod remove-method :after
  ((gf standard-generic-function) method)
  (map-dependents gf (lambda (dep) (update-dependent gf dep 'remove-method method))))

;; The following method ensures that we get only the required arguments
;; from generic-function-argument-precedence-order

(cl:defgeneric generic-function-argument-precedence-order (gf)
  (:method ((gf generic-function))
   (required-args (mop:generic-function-argument-precedence-order gf))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))

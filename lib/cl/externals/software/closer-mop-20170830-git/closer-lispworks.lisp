(in-package :closer-mop)

;; We need a new standard-class for various things.

(defclass standard-class (cl:standard-class) ())
(define-validate-superclass-method standard-class cl:standard-class)

;; We need a new funcallable-standard-class for various things.

(defclass funcallable-standard-class (clos:funcallable-standard-class) ())
(define-validate-superclass-method funcallable-standard-class clos:funcallable-standard-class)

#-lispworks4
(cl:defmethod validate-superclass
           ((class funcallable-standard-class)
            (superclass (eql (find-class 'funcallable-standard-object))))
  t)

;; We also need a new funcallable-standard-object because the default one
;; is not an instance of clos:funcallable-standard-class.

#+lispworks4
(defclass funcallable-standard-object (clos:funcallable-standard-object) ()
  (:metaclass clos:funcallable-standard-class))

;; The following code ensures that possibly incorrect lists of direct
;; superclasses are corrected.

#+lispworks4
(defun modify-superclasses (direct-superclasses &optional (standardp t))
  (if (null direct-superclasses)
    (list (if standardp
            (find-class 'standard-object)
            (find-class 'funcallable-standard-object)))
    (let ((standard-object (if standardp
                             (find-class 'standard-object)
                             (find-class 'clos:funcallable-standard-object))))
      (if (eq (car (last direct-superclasses)) standard-object)
        (if standardp
          direct-superclasses
          (append (butlast direct-superclasses)
                  (list (find-class 'funcallable-standard-object))))
        (remove standard-object direct-superclasses)))))

;; During class re/initialization, we take care of the following things:
;; - Optimization of slot accessors is deactivated.
;; - Lists of direct superclasses are corrected.
;; - Removal of direct subclasses.

(defun optimize-slot-access-p (class)
  (flet ((applicablep (specializer)
           (if (consp specializer)
             (eql class (eql-specializer-object specializer))
             (subclassp (class-of class) specializer))))
    (and (loop for method in (generic-function-methods #'slot-value-using-class)
               never (applicablep (first (method-specializers method))))
         (loop for method in (generic-function-methods #'(setf slot-value-using-class))
               never (applicablep (second (method-specializers method)))))))

(cl:defmethod initialize-instance :around
  ((class standard-class) &rest initargs
   #+lispworks4 &key
   #+lispworks4 (direct-superclasses ()))
  (apply #'call-next-method class
         #+lispworks4 :direct-superclasses
         #+lispworks4 (modify-superclasses direct-superclasses)
         :optimize-slot-access (optimize-slot-access-p class)
         initargs))

(cl:defmethod reinitialize-instance :around
  ((class standard-class) &rest initargs
   #+lispworks4 &key
   #+lispworks4 (direct-superclasses () direct-superclasses-p))
  #+lispworks4
  (progn
    (when direct-superclasses-p
      (setq direct-superclasses (modify-superclasses direct-superclasses))
      (loop for superclass in (copy-list (class-direct-superclasses class))
            unless (member superclass direct-superclasses)
            do (remove-direct-subclass superclass class)))
    (if direct-superclasses-p
      (apply #'call-next-method class
             :direct-superclasses direct-superclasses
             :optimize-slot-access (optimize-slot-access-p class)
             initargs)
      (apply #'call-next-method class
             :optimize-slot-access (optimize-slot-access-p class)
             initargs)))
  #-lispworks4
  (apply #'call-next-method class
         :optimize-slot-access (optimize-slot-access-p class)
         initargs))

(cl:defmethod initialize-instance :around
  ((class funcallable-standard-class) &rest initargs
   #+lispworks4 &key
   #+lispworks4 (direct-superclasses ()))
  (apply #'call-next-method class
         #+lispworks4 :direct-superclasses
         #+lispworks4 (modify-superclasses direct-superclasses nil)
         :optimize-slot-access (optimize-slot-access-p class)
         initargs))

(cl:defmethod reinitialize-instance :around
  ((class funcallable-standard-class) &rest initargs
   #+lispworks4 &key
   #+lispworks4 (direct-superclasses () direct-superclasses-p))
  #+lispworks4
  (progn
    (when direct-superclasses-p
      (setq direct-superclasses (modify-superclasses direct-superclasses nil))
      (loop for superclass in (copy-list (class-direct-superclasses class))
            unless (member superclass direct-superclasses)
            do (remove-direct-subclass superclass class)))
    (if direct-superclasses-p
      (apply #'call-next-method class
             :direct-superclasses direct-superclasses
             :optimize-slot-access (optimize-slot-access-p class)
             initargs)
      (apply #'call-next-method class
             :optimize-slot-access (optimize-slot-access-p class)
             initargs)))
  #-lispworks4
  (apply #'call-next-method class
         :optimize-slot-access (optimize-slot-access-p class)
         initargs))

;; The following is necessary for forward-referenced-classes.
;; Since we replace the original funcallable-standard-object with
;; a new one, we have to prevent LispWorks from trying to use
;; the original one when forward-ferenced-classes are resolved.

#+lispworks4
(cl:defmethod change-class :around
  ((class forward-referenced-class)
   (new-class funcallable-standard-class)
   &rest initargs
   &key (direct-superclasses ()))
  (apply #'call-next-method class new-class
         :optimize-slot-access (optimize-slot-access-p new-class)
         :direct-superclasses (modify-superclasses direct-superclasses nil)
         initargs))

;;; In LispWorks, the slot accessors (slot-value-using-class, etc.) are specialized
;;; on slot names instead of effective slot definitions. In order to fix this,
;;; we need to rewire the slot access protocol.

(declaim (inline find-slot))

(defun find-slot (slot-name class)
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (loop for slot in (class-slots class)
        when (eq slot-name (slot-definition-name slot))
        return slot))

(cl:defmethod slot-value-using-class
           ((class standard-class) object (slot symbol))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (let ((slotd (find-slot slot class)))
    (if slotd
      (slot-value-using-class class object slotd)
      (slot-missing class object slot 'slot-value))))

(cl:defmethod slot-value-using-class
           ((class standard-class) object (slotd standard-effective-slot-definition))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (slot-value-using-class
   (load-time-value (class-prototype (find-class 'cl:standard-class)))
   object
   (slot-definition-name slotd)))

(cl:defmethod (setf slot-value-using-class)
           (new-value (class standard-class) object (slot symbol))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (let ((slotd (find-slot slot class)))
    (if slotd
      (setf (slot-value-using-class class object slotd)
            new-value)
      (slot-missing class object slot 'setf new-value))))

(cl:defmethod (setf slot-value-using-class)
           (new-value (class standard-class) object (slotd standard-effective-slot-definition))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (setf (slot-value-using-class
         (load-time-value (class-prototype (find-class 'cl:standard-class)))
         object
         (slot-definition-name slotd))
        new-value))

(cl:defmethod slot-boundp-using-class
           ((class standard-class) object (slot symbol))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (let ((slotd (find-slot slot class)))
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
   (slot-definition-name slotd)))

(cl:defmethod slot-makunbound-using-class
           ((class standard-class) object (slot symbol))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (let ((slotd (find-slot slot class)))
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

;; In LispWorks, eql specializers are lists. We cannot change this
;; but we can soften some of the incompatibilities.

(deftype eql-specializer ()
  '(or eql-specializer*
       (satisfies clos:eql-specializer-p)))

(cl:defgeneric eql-specializer-object (eql-specializer)
  (:method ((cons cons))
   (if (clos:eql-specializer-p cons)
     (cadr cons)
     (error "~S is not an eql-specializer." cons))))

(defun intern-eql-specializer (object)
  `(eql ,object))

(defclass eql-specializer* (metaobject)
  ((obj :reader eql-specializer-object
        :initarg eso
        :initform (error "Use intern-eql-specializer to create eql-specializers."))
   (direct-methods :reader specializer-direct-methods
                   :accessor es-direct-methods
                   :initform ())))

(defvar *eql-specializers* (make-hash-table :weak-kind :value))

#+lispworks5.0
(defvar *eql-specializers-lock* (mp:make-lock))

#-lispworks5.0
(defun intern-eql-specializer* (object)
  (or (gethash object *eql-specializers*)
      (with-hash-table-locked *eql-specializers*
        (or (gethash object *eql-specializers*)
            (setf (gethash object *eql-specializers*)
                  (make-instance 'eql-specializer* 'eso object))))))

#+lispworks5.0
(defun intern-eql-specializer* (object)
  (or (gethash object *eql-specializers*)
      (mp:with-lock (*eql-specializers-lock*)
        (or (gethash object *eql-specializers*)
            (setf (gethash object *eql-specializers*)
                  (make-instance 'eql-specializer* 'eso object))))))

(cl:defmethod add-direct-method ((specializer eql-specializer*) (method method))
  (pushnew method (es-direct-methods specializer)))

(cl:defmethod remove-direct-method ((specializer eql-specializer*) (method method))
  (removef (es-direct-methods specializer) method))

(cl:defgeneric specializer-direct-generic-functions (specializer)
  (:method ((class class))
   (remove-duplicates
    (mapcar #'method-generic-function
            (specializer-direct-methods class))))
  (:method ((eql-specializer eql-specializer*))
   (remove-duplicates
    (mapcar #'method-generic-function
            (specializer-direct-methods eql-specializer))))
  (:method ((cons cons))
   (specializer-direct-generic-functions
    (intern-eql-specializer*
     (eql-specializer-object cons)))))

;; The following method ensures that remove-method is called.

#+lispworks4
(cl:defmethod add-method :before ((gf standard-generic-function) (method method))
  (when-let (old-method (find-method gf (method-qualifiers method)
                                     (method-specializers method) nil))
    (remove-method gf old-method)))

;; The following two methods ensure that add/remove-direct-method is called,
;; and that the dependent protocol for generic function works.

(cl:defmethod add-method :after ((gf standard-generic-function) (method method))
  (dolist (specializer (method-specializers method))
    (if (consp specializer)
      (add-direct-method (intern-eql-specializer*
                          (eql-specializer-object specializer))
                         method)
      #+lispworks4
      (add-direct-method specializer method)))
  #+lispworks4.3
  (map-dependents gf (lambda (dep) (update-dependent gf dep 'add-method method))))

(cl:defmethod remove-method :after ((gf standard-generic-function) (method method))
  (dolist (specializer (method-specializers method))
    (if (consp specializer)
      (remove-direct-method (intern-eql-specializer*
                             (eql-specializer-object specializer))
                            method)
      #+lispworks4
      (remove-direct-method specializer method)))
  #+lispworks4.3
  (map-dependents gf (lambda (dep) (update-dependent gf dep 'remove-method method))))

(cl:defgeneric find-method-combination (gf combi combi-options)
  (:method ((gf generic-function) (combi symbol) combi-options)
   (when combi-options
     (error "This implementation of find-method-combination cannot handle method combination options."))
   (clos::find-a-method-combination-type combi)))

;; "Native" make-method-lambda.

(cl:defmethod make-method-lambda ((gf generic-function) (method standard-method) lambda-expression environment)
  (destructuring-bind
      (lambda (&rest args) &body body)
      lambda-expression
    (declare (ignore lambda))
    (loop with documentation = :unbound
          for (car . cdr) = body then cdr
          while (or (and cdr (stringp car))
                    (and (consp car) (eq (car car) 'declare)))
          if (stringp car)
          do (setf documentation
                   (if (eq documentation :unbound) car
                     (warn "Too many documentation strings in lambda expression ~S."
                           lambda-expression)))
          else append (loop for declaration in (cdr car) 
                            if (eq (car declaration) 'ignore)
                            collect `(ignorable ,@(cdr declaration))
                            and collect `(dynamic-extent ,@(cdr declaration))
                            else collect declaration) into declarations
          finally (multiple-value-bind
                      (method-lambda method-args)
                      (clos:make-method-lambda
                       gf method args declarations
                       `(progn ,car ,@cdr)
                       environment)
                    (if (eq documentation :unbound)
                      (return (values method-lambda method-args))
                      (return (values
                               `(lambda ,(cadr method-lambda)
                                  ,documentation
                                  ,@(cddr method-lambda))
                               method-args)))))))

;; Provide standard-instance-access and funcallable-standard-instance-access

(declaim (inline standard-instance-access (setf standard-instance-access)))

(defun standard-instance-access (instance location)
  (clos::fast-standard-instance-access instance location))

(defun (setf standard-instance-access) (new-value instance location)
  (setf (clos::fast-standard-instance-access instance location) new-value))

(declaim (inline funcallable-instance-access))

(defun funcallable-instance-access (instance location &rest args)
  (let* ((class (class-of instance))
         (slot (find location (class-slots class)
                     :key #'slot-definition-location)))
    (if slot
      (apply #'clos::funcallable-instance-access instance (slot-definition-name slot) args)
      (error "There is no slot with location ~S for instance ~S." location instance))))

(defun funcallable-standard-instance-access (instance location)
  (funcallable-instance-access instance location))

(defun (setf funcallable-standard-instance-access) (new-value instance location)
  (funcallable-instance-access instance location new-value))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))

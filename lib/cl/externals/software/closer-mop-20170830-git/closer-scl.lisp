(in-package :closer-mop)

(defgeneric add-direct-method (specializer method)
  (:method ((specializer standard-object) (method method))))

(defgeneric remove-direct-method (specializer method)
  (:method ((specializer standard-object) (method method))))

(defvar *dependents* (make-hash-table :test #'eq))

(defgeneric add-dependent (metaobject dependent)
  (:method ((metaobject standard-class) dependent)
    (pushnew dependent (gethash metaobject *dependents*)))
  (:method ((metaobject funcallable-standard-class) dependent)
    (pushnew dependent (gethash metaobject *dependents*)))
  (:method ((metaobject standard-generic-function) dependent)
    (pushnew dependent (gethash metaobject *dependents*))))

(defgeneric remove-dependent (metaobject dependent)
  (:method ((metaobject standard-class) dependent)
    (setf (gethash metaobject *dependents*)
	  (delete metaobject (gethash metaobject *dependents*))))
  (:method ((metaobject funcallable-standard-class) dependent)
    (setf (gethash metaobject *dependents*)
	  (delete metaobject (gethash metaobject *dependents*))))
  (:method ((metaobject standard-generic-function) dependent)
    (setf (gethash metaobject *dependents*)
	  (delete metaobject (gethash metaobject *dependents*)))))

(defgeneric map-dependents (metaobject function)
  (:method ((metaobject standard-class) function)
    (mapc function (gethash metaobject *dependents*)))
  (:method ((metaobject funcallable-standard-class) function)
    (mapc function (gethash metaobject *dependents*)))
  (:method ((metaobject standard-generic-function) function)
    (mapc function (gethash metaobject *dependents*))))

(defgeneric update-dependent (metaobject dependent &rest initargs))

(defmethod reinitialize-instance :after ((metaobject metaobject) &rest initargs)
  (map-dependents metaobject (lambda (dep) (apply #'update-dependent metaobject dep initargs))))

(defmethod add-method :after
  ((gf standard-generic-function) method)
  (loop for specializer in (method-specializers method)
        do (add-direct-method specializer method))
  (map-dependents gf (lambda (dep) (update-dependent gf dep 'add-method method))))

(defmethod remove-method :after
  ((gf standard-generic-function) method)
  (loop for specializer in (method-specializers method)
	do (remove-direct-method specializer method))
  (map-dependents gf (lambda (dep) (update-dependent gf dep 'remove-method method))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))

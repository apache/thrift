(in-package :closer-mop)

;; We need a new standard-class for various things.

(defclass standard-class (cl:standard-class) ())
(define-validate-superclass-method standard-class cl:standard-class)

(defmethod ccl::create-reader-method-function
           ((class standard-class)
            (reader-method-class standard-reader-method)
            (dslotd standard-direct-slot-definition))
  (let ((slot-name (slot-definition-name dslotd)))
    (compile nil `(lambda (object) (slot-value object ',slot-name)))))

(defmethod ccl::create-writer-method-function
             ((class standard-class)
              (writer-method-class standard-writer-method)
              (dslotd standard-direct-slot-definition))
    (let ((slot-name (slot-definition-name dslotd)))
      (compile nil `(lambda (new-value object)
                      (setf (slot-value object ',slot-name) new-value)))))

(defgeneric typep (object type)
  (:method (object type)
   (cl:typep object type))
  (:method (object (type class))
   (member (class-of object)
           (class-precedence-list type))))
  
(defgeneric subtypep (type1 type2)
  (:method (type1 type2)
   (cl:subtypep type1 type2))
  (:method ((type1 class) (type2 symbol))
   (let ((class2 (find-class type2 nil)))
     (if class2
       (member class2 (class-precedence-list type1))
       (cl:subtypep type1 type2))))
  (:method ((type1 symbol) (type2 class))
   (let ((class1 (find-class type1 nil)))
     (if class1
       (member type2 (class-precedence-list class1))
       (cl:subtypep type1 type2))))
  (:method ((type1 class) (type2 class))
   (member type2 (class-precedence-list type1))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))

(in-package :closer-mop)

;; In CMUCL, reader-method-class and writer-method-class are
;; not used during class initialization. The following definitions
;; correct this.

(defun modify-accessors (class)
  (loop with reader-specializers = (list class)
        with writer-specializers = (list (find-class 't) class)
        for slotd in (class-direct-slots class) do
        (loop for reader in (slot-definition-readers slotd)
              for reader-function = (fdefinition reader)
              for reader-method = (find-method reader-function () reader-specializers)
              for initargs = (list :qualifiers ()
                                   :lambda-list '(object)
                                   :specializers reader-specializers
                                   :function (method-function reader-method)
                                   :slot-definition slotd)
              for method-class = (apply #'reader-method-class class slotd initargs)
              unless (eq method-class (class-of reader-method))
              do (add-method reader-function (apply #'make-instance method-class initargs)))
        (loop for writer in (slot-definition-writers slotd)
              for writer-function = (fdefinition writer)
              for writer-method = (find-method writer-function () writer-specializers)
              for initargs = (list :qualifiers ()
                                   :lambda-list '(new-value object)
                                   :specializers writer-specializers
                                   :function (method-function writer-method)
                                   :slot-definition slotd)
              for method-class = (apply #'writer-method-class class slotd initargs)
              unless (eq method-class (class-of writer-method))
              do (add-method writer-function (apply #'make-instance method-class initargs)))))

;; The following methods additionally create a gensym for the class name
;; unless a name is explicitly provided. AMOP requires classes to be
;; potentially anonymous.

(defmethod initialize-instance :around
  ((class standard-class) &rest initargs
   &key (name (gensym)))
  (prog1 (apply #'call-next-method class :name name initargs)
    (modify-accessors class)))

(defmethod initialize-instance :around
  ((class funcallable-standard-class) &rest initargs
   &key (name (gensym)))
  (prog1 (apply #'call-next-method class :name name initargs)
    (modify-accessors class)))

(defmethod reinitialize-instance :after
  ((class standard-class) &key)
  (modify-accessors class))

(defmethod reinitialize-instance :after
  ((class funcallable-standard-class) &key)
  (modify-accessors class))

;;; The following three methods ensure that the dependent protocol
;;; for generic function works.

;; The following method additionally ensures that
;; compute-discriminating-function is triggered.

; Note that for CMUCL, these methods violate the AMOP specification
; by specializing on the original standard-generic-function metaclass. However,
; this is necassary because in CMUCL, only one subclass of
; standard-generic-function can be created, and taking away that option from user
; code doesn't make a lot of sense in our context.

(defmethod reinitialize-instance :after
  ((gf standard-generic-function) &rest initargs)
  (set-funcallable-instance-function gf (compute-discriminating-function gf)))

;; The following ensures that effective slot definitions have a documentation in CMUCL.

(defmethod compute-effective-slot-definition :around
  ((class standard-class) name direct-slot-definitions)
  (let ((effective-slot (call-next-method)))
    (loop for direct-slot in direct-slot-definitions
          for documentation = (documentation direct-slot 't)
          when documentation do
          (setf (documentation effective-slot 't) documentation)
          (loop-finish))
    effective-slot))

;; In CMUCL, TYPEP and SUBTYPEP don't work as expected
;; in conjunction with class metaobjects.

(defgeneric typep (object type)
  (:method (object type)
   (cl:typep object type))
  (:method (object (type class))
   (cl:typep object (class-name type))))

(defgeneric subtypep (type1 type2)
  (:method (type1 type2)
   (cl:subtypep type1 type2))
  (:method ((type1 class) type2)
   (cl:subtypep (class-name type1) type2))
  (:method (type1 (type2 class))
   (cl:subtypep type1 (class-name type2)))
  (:method ((type1 class) (type2 class))
   (cl:subtypep (class-name type1)
                (class-name type2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))

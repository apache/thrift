(in-package :closer-mop)

(defclass standard-class (cl:standard-class) ())
(define-validate-superclass-method standard-class cl:standard-class)

(cl:defmethod reinitialize-instance :after ((class standard-class) &key)
  (finalize-inheritance class))

;;; New generic functions.

;; Store the method function somewhere else, to circumvent
;; the native check for congruent lambda lists.

(defparameter *stub-method-functions* (make-hash-table :test #'equal))

(defun get-stub-method-function (lambda-list)
  (or (gethash lambda-list *stub-method-functions*)
      (let ((ignore-list (loop for arg in lambda-list
                               unless (member arg lambda-list-keywords)
                               collect (etypecase arg
                                         (symbol arg)
                                         (cons (etypecase (car arg)
                                                 (symbol (car arg))
                                                 (cons (assert (cdr arg))
                                                       (cadr arg))))))))
        (setf (gethash lambda-list *stub-method-functions*)
              (compile nil `(lambda ,lambda-list
                              (declare (ignore ,@ignore-list))
                              (error "This method function must not be called.")))))))

(cl:defmethod initialize-instance :around
  ((method standard-method) &rest initargs &key lambda-list function closer-patch)
  (if closer-patch
    (apply #'call-next-method method
           :real-function function
           :function (get-stub-method-function lambda-list)
           initargs)
    (apply #'call-next-method method
           :real-function function
           initargs)))

;; Adapt argument-precedence-order whenever the lambda list changes.

(cl:defmethod reinitialize-instance :around
  ((gf standard-generic-function) &rest initargs &key
   (lambda-list '() lambda-list-p)
   (argument-precedence-order '() argument-precedence-order-p))
  (declare (ignore argument-precedence-order))
  (if (and lambda-list-p (not argument-precedence-order-p))
    (apply #'call-next-method gf
           :argument-precedence-order (required-args lambda-list)
           initargs)
    (call-next-method)))

;; Ensure that the discriminating function is computed and installed
;; at the moments in time as stated in the CLOS MOP specification.

(cl:defmethod add-method :after ((gf standard-generic-function) method)
  (declare (ignore method))
  (set-funcallable-instance-function gf (compute-discriminating-function gf)))
  
(cl:defmethod remove-method :after ((gf standard-generic-function) method)
  (declare (ignore method))
  (set-funcallable-instance-function gf (compute-discriminating-function gf)))
  
(cl:defmethod initialize-instance :after ((gf standard-generic-function) &key)
  (set-funcallable-instance-function gf (compute-discriminating-function gf)))
  
(cl:defmethod reinitialize-instance :after ((gf standard-generic-function) &key)
  (set-funcallable-instance-function gf (compute-discriminating-function gf)))

;; Define compute-effective-method correctly.

(cl:defmethod compute-effective-method ((gf standard-generic-function)
                                        (combination ccl:standard-method-combination)
                                        methods)
  (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
  (loop for method in methods
        for qualifiers = (method-qualifiers method)
        if (equal qualifiers '()) collect method into primary
        else if (equal qualifiers '(:before)) collect method into before
        else if (equal qualifiers '(:after)) collect method into after
        else if (equal qualifiers '(:around)) collect method into around
        else do (invalid-method-error method "Invalid method qualifiers ~S." qualifiers)
        finally
        (unless primary (method-combination-error "No primary method."))
        (let ((form (if (or before after (rest primary))
                      `(multiple-value-prog1
                           (progn ,@(loop for method in before collect `(call-method ,method))
                             (call-method ,(first primary) ,(rest primary)))
                         ,@(loop for method in (reverse after) collect `(call-method ,method)))
                      `(call-method ,(first primary)))))
          (return
           (if around
             `(call-method ,(first around) (,@(rest around) (make-method ,form)))
             form)))))

(cl:defmethod compute-effective-method ((gf standard-generic-function)
                                        (combination ccl:short-method-combination)
                                        methods)
  (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
  (loop with primary-qualifiers = (list (ccl::method-combination-name combination))
        for method in methods
        for qualifiers = (method-qualifiers method)
        if (equal qualifiers primary-qualifiers) collect method into primary
        else if (equal qualifiers '(:around)) collect method into around
        else do (invalid-method-error method "Invalid method qualifiers ~S." qualifiers)
        finally
        (unless primary (method-combination-error "No primary method."))
        (when (eq (car (ccl::method-combination-options combination))
                  :most-specific-last)
          (setq primary (nreverse primary)))
        (let ((form (if (and (ccl::method-combination-identity-with-one-argument combination)
                             (null (rest primary)))
                      `(call-method ,(first primary))
                      `(,(ccl::method-combination-operator combination)
                        ,@(loop for method in primary collect `(call-method ,method))))))
          (return
           (if around
             `(call-method ,(first around) (,@(rest around) (make-method ,form)))
             form)))))

(cl:defmethod compute-effective-method ((gf standard-generic-function)
                                        (combination ccl:long-method-combination)
                                        methods)
  (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
  (destructuring-bind ((args-var . gf-name) . expander)
      (ccl::method-combination-expander combination)
    (declare (ignore args-var gf-name))
    (funcall expander gf methods (ccl::method-combination-options combination))))

;; "Native" make-method-lambda.

(cl:defmethod make-method-lambda ((gf generic-function) (method method) lambda-expression environment)
  (declare (ignore environment) (optimize (speed 3) (space 0) (compilation-speed 0)))
  (multiple-value-bind (documentation declarations body)
      (parse-method-body (cddr lambda-expression) lambda-expression)
    (let ((methvar (gensym)))
      (values
       `(lambda (ccl::&method ,methvar ,@(cadr lambda-expression))
          ,@(when documentation `(,documentation))
          ,@(when declarations `((declare ,@declarations)))
          (flet ((call-next-method (&rest args)
                   (if args
                     (apply #'ccl::%call-next-method-with-args ,methvar args)
                     (ccl::%call-next-method ,methvar)))
                 (next-method-p () (ccl::%next-method-p ,methvar)))
            (declare (inline call-next-method next-method-p))
            ,@body))
       (when documentation
         (list :documentation documentation))))))

;; "Native" compute-discriminating-function.

(cl:defmethod compute-discriminating-function ((gf generic-function))
  (let ((non-dt-dcode (ccl::non-dt-dcode-function gf)))
    (if non-dt-dcode
      non-dt-dcode
      (let* ((std-dfun (ccl::%gf-dcode gf))
             (dt (ccl::%gf-dispatch-table gf))
             (proto (cdr (assoc std-dfun ccl::dcode-proto-alist))))
        (if (or (eq proto #'ccl::gag-one-arg)
                (eq proto #'ccl::gag-two-arg))
          (lambda (&rest args)
            (apply std-dfun dt args))
          (lambda (&rest args)
            (funcall std-dfun dt args)))))))

;; The following ensures that slot definitions have a documentation.

(cl:defmethod documentation ((slot slot-definition) (type (eql 't)))
  (ccl:slot-definition-documentation slot))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))

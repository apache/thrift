(in-package :closer-mop)

;; TODO FIXME this below is untested
(cl:defmethod compute-effective-method-function ((gf standard-generic-function) effective-method options)
  (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
  (when options
    (cerror "Ignore these options."
            "This version of compute-effective-method-function does not support method combination options: ~S"
            options))
  (coerce `(lambda (.method-args. .next-methods. &rest passed-arguments)
             ,effective-method)
          'function)
  ;; alternatively
  #+nil
  (coerce `(lambda (dummy-method-args .next-methods. &va-rest .method-args.)
             ,effective-method)
          'function))

(declaim (inline eql-specializer-p))
(defun eql-specializer-p (thing)
  (typep thing 'eql-specializer))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))

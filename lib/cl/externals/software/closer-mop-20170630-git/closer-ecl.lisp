(in-package :closer-mop)

(cl:defmethod compute-effective-method-function ((gf standard-generic-function) effective-method options)
  (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
  (when options
    (cerror "Ignore these options."
            "This version of compute-effective-method-function does not support method combination options: ~S"
            options))
  (coerce `(lambda (&rest clos:.combined-method-args.)
             (declare (special clos:.combined-method-args. clos:*next-methods*))
             ,effective-method)
          'function))

(declaim (inline eql-specializer-p))
(defun eql-specializer-p (thing)
  (typep thing 'eql-specializer))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))

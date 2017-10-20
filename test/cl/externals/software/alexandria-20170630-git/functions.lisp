(in-package :alexandria)

;;; To propagate return type and allow the compiler to eliminate the IF when
;;; it is known if the argument is function or not.
(declaim (inline ensure-function))

(declaim (ftype (function (t) (values function &optional))
                ensure-function))
(defun ensure-function (function-designator)
  "Returns the function designated by FUNCTION-DESIGNATOR:
if FUNCTION-DESIGNATOR is a function, it is returned, otherwise
it must be a function name and its FDEFINITION is returned."
  (if (functionp function-designator)
      function-designator
      (fdefinition function-designator)))

(define-modify-macro ensure-functionf/1 () ensure-function)

(defmacro ensure-functionf (&rest places)
  "Multiple-place modify macro for ENSURE-FUNCTION: ensures that each of
PLACES contains a function."
  `(progn ,@(mapcar (lambda (x) `(ensure-functionf/1 ,x)) places)))

(defun disjoin (predicate &rest more-predicates)
  "Returns a function that applies each of PREDICATE and MORE-PREDICATE
functions in turn to its arguments, returning the primary value of the first
predicate that returns true, without calling the remaining predicates.
If none of the predicates returns true, NIL is returned."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((predicate (ensure-function predicate))
	(more-predicates (mapcar #'ensure-function more-predicates)))
    (lambda (&rest arguments)
      (or (apply predicate arguments)
	  (some (lambda (p)
		  (declare (type function p))
		  (apply p arguments))
		more-predicates)))))

(defun conjoin (predicate &rest more-predicates)
  "Returns a function that applies each of PREDICATE and MORE-PREDICATE
functions in turn to its arguments, returning NIL if any of the predicates
returns false, without calling the remaining predicates. If none of the
predicates returns false, returns the primary value of the last predicate."
  (if (null more-predicates)
      predicate
      (lambda (&rest arguments)
	(and (apply predicate arguments)
	     ;; Cannot simply use CL:EVERY because we want to return the
	     ;; non-NIL value of the last predicate if all succeed.
	     (do ((tail (cdr more-predicates) (cdr tail))
		  (head (car more-predicates) (car tail)))
		 ((not tail)
		  (apply head arguments))
	       (unless (apply head arguments)
		 (return nil)))))))


(defun compose (function &rest more-functions)
  "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies its
arguments to to each in turn, starting from the rightmost of MORE-FUNCTIONS,
and then calling the next one with the primary value of the last."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (reduce (lambda (f g)
	    (let ((f (ensure-function f))
		  (g (ensure-function g)))
	      (lambda (&rest arguments)
		(declare (dynamic-extent arguments))
		(funcall f (apply g arguments)))))
          more-functions
          :initial-value function))

(define-compiler-macro compose (function &rest more-functions)
  (labels ((compose-1 (funs)
             (if (cdr funs)
                 `(funcall ,(car funs) ,(compose-1 (cdr funs)))
                 `(apply ,(car funs) arguments))))
    (let* ((args (cons function more-functions))
           (funs (make-gensym-list (length args) "COMPOSE")))
      `(let ,(loop for f in funs for arg in args
		   collect `(,f (ensure-function ,arg)))
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest arguments)
           (declare (dynamic-extent arguments))
           ,(compose-1 funs))))))

(defun multiple-value-compose (function &rest more-functions)
    "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies
its arguments to each in turn, starting from the rightmost of
MORE-FUNCTIONS, and then calling the next one with all the return values of
the last."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (reduce (lambda (f g)
	    (let ((f (ensure-function f))
		  (g (ensure-function g)))
	      (lambda (&rest arguments)
		(declare (dynamic-extent arguments))
		(multiple-value-call f (apply g arguments)))))
          more-functions
          :initial-value function))

(define-compiler-macro multiple-value-compose (function &rest more-functions)
  (labels ((compose-1 (funs)
             (if (cdr funs)
                 `(multiple-value-call ,(car funs) ,(compose-1 (cdr funs)))
                 `(apply ,(car funs) arguments))))
    (let* ((args (cons function more-functions))
           (funs (make-gensym-list (length args) "MV-COMPOSE")))
      `(let ,(mapcar #'list funs args)
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest arguments)
           (declare (dynamic-extent arguments))
           ,(compose-1 funs))))))

(declaim (inline curry rcurry))

(defun curry (function &rest arguments)
  "Returns a function that applies ARGUMENTS and the arguments
it is called with to FUNCTION."
  (declare (optimize (speed 3) (safety 1)))
  (let ((fn (ensure-function function)))
    (lambda (&rest more)
      (declare (dynamic-extent more))
      ;; Using M-V-C we don't need to append the arguments.
      (multiple-value-call fn (values-list arguments) (values-list more)))))

(define-compiler-macro curry (function &rest arguments)
  (let ((curries (make-gensym-list (length arguments) "CURRY"))
        (fun (gensym "FUN")))
    `(let ((,fun (ensure-function ,function))
           ,@(mapcar #'list curries arguments))
       (declare (optimize (speed 3) (safety 1)))
       (lambda (&rest more)
         (declare (dynamic-extent more))
         (apply ,fun ,@curries more)))))

(defun rcurry (function &rest arguments)
  "Returns a function that applies the arguments it is called
with and ARGUMENTS to FUNCTION."
  (declare (optimize (speed 3) (safety 1)))
  (let ((fn (ensure-function function)))
    (lambda (&rest more)
      (declare (dynamic-extent more))
      (multiple-value-call fn (values-list more) (values-list arguments)))))

(define-compiler-macro rcurry (function &rest arguments)
  (let ((rcurries (make-gensym-list (length arguments) "RCURRY"))
        (fun (gensym "FUN")))
    `(let ((,fun (ensure-function ,function))
           ,@(mapcar #'list rcurries arguments))
       (declare (optimize (speed 3) (safety 1)))
       (lambda (&rest more)
         (declare (dynamic-extent more))
         (multiple-value-call ,fun (values-list more) ,@rcurries)))))

(declaim (notinline curry rcurry))

(defmacro named-lambda (name lambda-list &body body)
  "Expands into a lambda-expression within whose BODY NAME denotes the
corresponding function."
  `(labels ((,name ,lambda-list ,@body))
     #',name))

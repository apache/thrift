(in-package :closer-mop)

(defun required-args (lambda-list &optional (collector #'identity))
  (loop for arg in lambda-list
        until (member arg lambda-list-keywords)
        collect (funcall collector arg)))

(defun ensure-finalized (class &optional (errorp t))
  (if (typep class 'class)
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (when errorp (error "~S is not a class." class)))
  class)

(defun subclassp (class superclass)
  (flet ((get-class (class) (etypecase class
                              (class class)
                              (symbol (find-class class)))))
    
      (loop with class = (get-class class)
            with superclass = (get-class superclass)
            
            for superclasses = (list class)
            then (set-difference 
                  (union (class-direct-superclasses current-class) superclasses)
                  seen)

            for current-class = (first superclasses)

            while current-class
            
            if (eq current-class superclass) return t
            else collect current-class into seen
            
            finally (return nil))))

#+(or allegro clozure lispworks mcl)
(progn
  ;; validate-superclass for metaclass classes is a little bit
  ;; more tricky than for class metaobject classes because
  ;; we don't want to make all standard-classes compatible to
  ;; each other.
  
  ;; Our validate-superclass may get passed a class-prototype
  ;; as its second argument, so don't expect its readers to
  ;; yield useful information. (In ANSI parlance, "the
  ;; consequences are undefined...")

  (defmacro define-validate-superclass-method (class superclass)
    `(cl:defmethod validate-superclass ((class ,class) (superclass ,superclass))
       (or (when (eq (class-of class) (find-class ',class))
             (or (eq (class-of superclass) (find-class ',superclass))
                 (eq (class-of superclass) (find-class ',class))))
           (call-next-method)
           (when (eq (class-of superclass) (find-class ',superclass))
             (validate-superclass class (class-prototype (find-class ',class))))))))

#+(or clisp scl)
(progn
  (declaim (inline classp))

  (define-compiler-macro classp (thing)
    `(typep ,thing 'class))
  
  (defun classp (thing)
    (typep thing 'class)))

#+(or allegro clisp clozure ecl clasp lispworks sbcl)
(progn ;;; New generic functions.

  (defclass standard-generic-function (cl:standard-generic-function)
    (#+(or clozure lispworks) (argument-order :accessor argument-order)
     #-(or abcl sbcl)         (initial-methods :initform '()))
    
    (:metaclass
     #-lispworks funcallable-standard-class
     #+lispworks clos:funcallable-standard-class)
    
    #+clozure
    (:default-initargs :name (copy-symbol :name) :method-class (find-class 'standard-method)))

  #+clozure
  (progn
    (cl:defgeneric method-function (method)
      (:method ((method method))
       (ccl:method-function method)))

    (defclass standard-method (cl:standard-method)
      ((fn :initarg :real-function :reader method-function))))

  #-(or ecl clasp)
  (progn
    (declaim (inline m-function))
    
    (defun m-function (m)
      (method-function m))
    
    (define-compiler-macro m-function (m)
      (handler-case (method-function m)
        (error () `(the function (method-function (the method ,m)))))))
  
  (defun compute-argument-order (gf nof-required-args)
    (loop with specialized-count = (make-array nof-required-args :initial-element 0)
        
          for method in (generic-function-methods gf) do
          (loop for specializer in (method-specializers method)
                for index from 0
                unless (eq specializer (find-class 't))
                do (incf (svref specialized-count index)))

          finally
  
          (loop for arg in (generic-function-argument-precedence-order gf)
                for pos = (position arg (generic-function-lambda-list gf))
                when (> (svref specialized-count pos) 0)
                collect pos into argument-order
                finally (return-from compute-argument-order
                          (coerce argument-order 'simple-vector)))))

  (defun parse-method-body (body error-form)
    (loop with documentation = nil
          for (car . cdr) = body then cdr
          while (or (and cdr (stringp car))
                    (and (consp car) (eq (car car) 'declare)))
          if (stringp car)
          do (setq documentation
                   (if (null documentation) car
                     (warn "Too many documentation strings in ~S." error-form)))
          else append (cdr car) into declarations
          finally (return (values documentation declarations (cons car cdr)))))

  #-(or abcl sbcl) (cl:defgeneric make-method-lambda (generic-function method lambda-expression environment))

  #-(or ecl clasp)
  (cl:defmethod make-method-lambda ((gf standard-generic-function) (method standard-method)
                                    lambda-expression environment)
    (declare (ignore environment) (optimize (speed 3) (space 0) (compilation-speed 0)))
    #+(or abcl clozure lispworks sbcl)
    (when (only-standard-methods gf)
      (return-from make-method-lambda (call-next-method)))
    (let ((args (copy-symbol 'args)) (next-methods (copy-symbol 'next-methods))
          (more-args (copy-symbol 'more-args)) (method-function (copy-symbol 'method-function)))
      (destructuring-bind
          (lambda (&rest lambda-args) &body body)
          lambda-expression
        (declare (ignore lambda-args))
        (assert (eq lambda 'lambda))
        (values
         `(lambda (,args ,next-methods &rest ,more-args)
            (declare (ignorable ,args ,next-methods ,more-args))
            (flet ((call-next-method (&rest args)
                     (if ,next-methods
                       (apply (method-function (first ,next-methods))
                              (if args args ,args) (rest ,next-methods) ,more-args)
                       (apply #'no-next-method
                              (getf ,more-args :generic-function)
                              (getf ,more-args :method)
                              (if args args ,args))))
                   (next-method-p () (not (null ,next-methods))))
              (declare (inline call-next-method next-method-p)
                       (ignorable #'call-next-method #'next-method-p))
              (flet ((,method-function ,@(rest lambda-expression)))
                (declare (inline ,method-function))
                (apply #',method-function ,args))))
         (let ((documentation (parse-method-body body lambda-expression)))
           (nconc
            (when documentation
              (list :documentation documentation))
            #+clozure '(:closer-patch t)
            #-clozure '()))))))

  #+(or clozure lispworks)
  (cl:defgeneric compute-applicable-methods-using-classes (generic-function classes)
    (:method ((gf standard-generic-function) classes)
     (labels ((subclass* (spec1 spec2 arg-spec)
                (let ((cpl (class-precedence-list arg-spec)))
                  (declare (type list cpl))
                  (find spec2 (the list (cdr (member spec1 cpl :test #'eq))) :test #'eq)))
              (method-more-specific-p (m1 m2)
                (declare (type method m1 m2))
                (loop for n of-type fixnum across (argument-order gf)
                      for spec1 = (nth n (method-specializers m1))
                      for spec2 = (nth n (method-specializers m2))
                      unless (eq spec1 spec2)
                      return (subclass* spec1 spec2 (nth n classes)))))
       (let ((applicable-methods
              (sort
               (loop for method of-type method in (the list (generic-function-methods gf))
                     when (loop for class in classes
                                for specializer in (the list (method-specializers method))
                                if (typep specializer 'eql-specializer)
                                do (when (typep (eql-specializer-object specializer) class)
                                     (return-from compute-applicable-methods-using-classes (values '() nil)))
                                else if (not (subclassp class specializer)) return nil
                                finally (return t))
                     collect method)
               #'method-more-specific-p)))
         (values applicable-methods t)))))

  (cl:defgeneric compute-effective-method-function (gf effective-method options))

  #-(or ecl clasp)
  (cl:defmethod compute-effective-method-function ((gf generic-function) effective-method options)
    (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
    (when #-clisp options
      #+clisp (or (cdr (assoc :arguments options))
                  (cdr (assoc :duplicates options)))
      (cerror "Ignore these options."
              "This version of compute-effective-method-function does not support method combination options: ~S"
              options))
    (let ((all-t-specializers (required-args (generic-function-lambda-list gf)
                                             (constantly (find-class 't))))
          (args (copy-symbol 'args)))
      (labels ((transform-effective-method (form)
                 (if (atom form) form
                   (case (car form)
                     (call-method (transform-effective-method
                                   (let ((the-method (transform-effective-method (cadr form)))
                                         (method-var (copy-symbol 'method-var)))
                                     `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
                                        (let ((,method-var ,the-method))
                                          (declare (ignorable ,method-var))
                                          (funcall (m-function ,(if (typep the-method 'method)
                                                                  the-method method-var))
                                                   ,args
                                                   ,@(let ((subforms
                                                            (loop for subform in (the list (cddr form))
                                                                  collect `',subform)))
                                                       (if subforms subforms '(())))
                                                   :generic-function ,gf
                                                   :method ,(if (typep the-method 'method)
                                                              the-method method-var)))))))
                     (make-method (when (cddr form)
                                    (error "Incorrect make-method form: ~S." form))
                                  (let ((method-class (generic-function-method-class gf)))
                                    #+allegro (ensure-finalized method-class)
                                    (multiple-value-bind
                                        (method-lambda method-options)
                                        (make-method-lambda
                                         gf (class-prototype method-class)
                                         `(lambda (&rest ,args)
                                            (declare (ignorable ,args))
                                            ,(transform-effective-method (cadr form))) nil)
                                      (apply #'make-instance
                                             method-class
                                             :qualifiers '()
                                             :specializers all-t-specializers
                                             :lambda-list (generic-function-lambda-list gf)
                                             :function (compile nil method-lambda)
                                             method-options))))
                     (t (mapcar #'transform-effective-method (the list form)))))))
        (let ((emf-lambda `(lambda (&rest ,args)
                             (declare (ignorable ,args))
                             ,(transform-effective-method effective-method))))
          (multiple-value-bind (function warnings failure)
              (compile nil emf-lambda)
            (declare (ignore warnings))
            (assert (not failure))
            function)))))

  #+clozure
  (cl:defgeneric compute-effective-method (generic-function combination methods))
  
  #+clozure
  (cl:defgeneric compute-discriminating-function (generic-function))

  (defun get-emf (gf args nof-required-args)
    (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
    (let ((applicable-methods (compute-applicable-methods gf (subseq args 0 nof-required-args))))
      (if applicable-methods
        (multiple-value-bind
            (effective-method options)
            (compute-effective-method
             gf (generic-function-method-combination gf)
             applicable-methods)
          (compute-effective-method-function gf effective-method options))
        (lambda (&rest args)
          (apply #'no-applicable-method gf args)))))

  (defun get-emf-using-classes (gf args classes nof-required-args)
    (declare (type generic-function gf) (type list args classes)
             (optimize (speed 3) (space 0) (compilation-speed 0)))
    (multiple-value-bind
        (applicable-methods validp)
        (compute-applicable-methods-using-classes gf classes)
      (unless validp
        (setq applicable-methods
              (compute-applicable-methods gf (subseq args 0 nof-required-args))))
      (values
       (if applicable-methods
         (multiple-value-bind
             (effective-method options)
             (compute-effective-method
              gf (generic-function-method-combination gf)
              applicable-methods)
           (compute-effective-method-function gf effective-method options))
         (lambda (&rest args)
           (apply #'no-applicable-method gf args)))
       validp)))

  (defvar *standard-gfs*
    (list #'compute-applicable-methods #'compute-applicable-methods-using-classes
          #'compute-effective-method #'compute-effective-method-function
          #'generic-function-method-class
          #'make-method-lambda
          #+allegro #'compute-discriminating-function))

  (defun only-standard-methods (gf &rest other-gfs)
    (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
    (loop for other-gf in (or other-gfs *standard-gfs*)
          always (loop for method in (generic-function-methods other-gf)
                       for specializer = (first (method-specializers method))
                       if (and (typep specializer 'class)
                               (subclassp specializer (find-class 'standard-generic-function))
                               (not (eq specializer (find-class 'standard-generic-function)))
                               (typep gf specializer))
                       return nil
                       else if (and (typep specializer 'eql-specializer)
                                    (eql (eql-specializer-object specializer) gf))
                       return nil
                       finally (return t))))

  (defun methods-all-the-same-specializers (gf)
    (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
    (loop with template = (first (generic-function-methods gf))
          for method in (rest (generic-function-methods gf))
          always (loop for spec1 in (method-specializers template)
                       for spec2 in (method-specializers method)
                       always (etypecase spec1
                                (class (etypecase spec2
                                         (class (eq spec1 spec2))
                                         (eql-specializer nil)))
                                (eql-specializer
                                 (etypecase spec2
                                   (class nil)
                                   (eql-specializer
                                    (eql (eql-specializer-object spec1)
                                         (eql-specializer-object spec2)))))))))

  (defun compute-discriminator (gf compute-native-discriminator)
    (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
    (let ((nof-required-args 
           (length (required-args 
                    (handler-case (generic-function-lambda-list gf)
                      (unbound-slot ()
                        (return-from compute-discriminator
                          (funcall compute-native-discriminator)))))))
          discriminator)
      #+(or clozure lispworks)
      (setf (argument-order gf)
            (compute-argument-order gf nof-required-args))
      (flet ((discriminate (emf-setter args &optional (classes (loop for arg in args
                                                                     repeat nof-required-args
                                                                     collect (class-of arg))))
               (declare (type list args classes) (type function emf-setter))
               (multiple-value-bind (emf validp) (get-emf-using-classes gf args classes nof-required-args)
                 (funcall emf-setter (if validp emf (lambda (&rest args)
                                                      (apply (the function (get-emf gf args nof-required-args)) args))))
                 (apply (the function emf) args))))
        (when (only-standard-methods gf #'compute-applicable-methods #'compute-applicable-methods-using-classes)
          (setq discriminator
                (if (only-standard-methods
                     gf #'compute-effective-method #'compute-effective-method-function #'make-method-lambda
                     #+allegro #'compute-discriminating-function)
                  (funcall compute-native-discriminator)
                  (let ((argument-order
                         #-(or clozure lispworks) (compute-argument-order gf nof-required-args)
                         #+(or clozure lispworks) (argument-order gf)))
                    (cond ((null (generic-function-methods gf))
                           (lambda (&rest args)
                             (apply #'no-applicable-method gf args)))
                          ((methods-all-the-same-specializers gf)
                           (let ((specializers (method-specializers (first (generic-function-methods gf))))
                                 (effective-method-function nil))
                             (declare (type list specializers))
                             (lambda (&rest args)
                               (declare (optimize (speed 3) (safety 0) (debug 0)
                                                  (compilation-speed 0)))
                               (cond ((loop for arg in args
                                            for spec in specializers
                                            always (etypecase spec
                                                     (class (typep arg spec))
                                                     (eql-specializer (eql arg (eql-specializer-object spec)))))
                                      (if effective-method-function
                                        (apply (the function effective-method-function) args)
                                        (discriminate (lambda (emf) (setq effective-method-function emf)) args)))
                                     (t (apply #'no-applicable-method gf args))))))
                          ((= (length argument-order) 1)
                           (let ((dispatch-argument-index (svref argument-order 0))
                                 (emfs (make-hash-table :test #'eq)))
                             (declare (type hash-table emfs) (type fixnum dispatch-argument-index))
                             (lambda (&rest args)
                               (declare (optimize (speed 3) (safety 0) (debug 0)
                                                  (compilation-speed 0)))
                               (let* ((dispatch-class (class-of (nth dispatch-argument-index args)))
                                      (effective-method-function (gethash dispatch-class emfs)))
                                 (if effective-method-function
                                   (apply (the function effective-method-function) args)
                                   (discriminate (lambda (emf) (setf (gethash dispatch-class emfs) emf)) args)))))))))))
        (if discriminator discriminator
          (let ((emfs (make-hash-table :test #'equal)))
            (declare (type hash-table emfs))
            (lambda (&rest args)
              (declare (optimize (speed 3) (safety 0) (debug 0)
                                 (compilation-speed 0)))
              (let* ((classes (loop for arg in args
                                    repeat nof-required-args
                                    collect (class-of arg)))
                     (effective-method-function (gethash (the list classes) emfs)))
                (if effective-method-function
                  (apply (the function effective-method-function) args)
                  (discriminate (lambda (emf) (setf (gethash (the list classes) emfs) emf)) args classes)))))))))

  #-(or clisp clozure lispworks (and sbcl sb-thread))
  (cl:defmethod compute-discriminating-function ((gf standard-generic-function))
    (if (eq (class-of gf) (find-class 'standard-generic-function))
      (lambda (&rest args)
        (let ((discriminator (compute-discriminator gf #'call-next-method)))
          (set-funcallable-instance-function gf discriminator)
          (apply discriminator args)))
      (compute-discriminator gf #'call-next-method)))

  #+(or clisp clozure lispworks (and sbcl sb-thread))
  (cl:defmethod compute-discriminating-function ((gf standard-generic-function))
    (compute-discriminator gf #'call-next-method))

  #-(or abcl sbcl)
  (progn
    (defun maybe-remove-initial-methods (function-name)
      (let ((generic-function (ignore-errors (fdefinition function-name))))
        (when (and generic-function (typep generic-function 'standard-generic-function))
          (dolist (method (slot-value generic-function 'initial-methods))
            (remove-method generic-function method)))))

    #-(or allegro lispworks)
    (defmacro without-redefinition-warnings (&body body)
      `(progn ,@body))
    
    #+allegro
    (defmacro without-redefinition-warnings (&body body)
      `(excl:without-redefinition-warnings ,@body))

    #+lispworks
    (defmacro without-redefinition-warnings (&body body)
      `(let ((dspec:*redefinition-action* :quiet)) ,@body))
    
    (defmacro defgeneric (&whole form name (&rest args) &body options &environment env)
      (loop initially (unless (every #'consp options)
                        (error "Illegal options in defgeneric form ~S." form))
            with generic-function-class-name = nil
            for option in options
            if (eq (car option) :generic-function-class) do
            (when (or (cddr option) (null (cadr option)) (not (symbolp (cadr option)))
                      generic-function-class-name)
              (error "Illegal or duplicate :generic-function-class option in defgeneric form ~S." form))
            (setq generic-function-class-name (cadr option))
            end
            if (eq (car option) :method) collect option into method-options
            else collect option into non-method-options
            finally
            (let ((gf (copy-symbol 'gf))
                  (non-standard (when generic-function-class-name
                                  (let ((standard-generic-function (find-class 'standard-generic-function t env))
                                        (this-generic-function (find-class generic-function-class-name t env)))
                                    (and (subclassp this-generic-function standard-generic-function)
                                         (not (eq this-generic-function standard-generic-function)))))))
              (return-from defgeneric
                `(progn (maybe-remove-initial-methods ',name)
                   ,(if non-standard
                      `(eval-when (:compile-toplevel :load-toplevel :execute)
                         (cl:defgeneric ,name ,args ,@non-method-options))
                      `(progn
                         (eval-when (:compile-toplevel)
                           (cl:defgeneric ,name ,args ,@non-method-options))
                         (eval-when (:load-toplevel :execute)
                           (without-redefinition-warnings
                             (cl:defgeneric ,name ,args ,@options)))))
                   (let ((,gf (fdefinition ',name)))
                     ,(when non-standard
                        `(setf (slot-value ,gf 'initial-methods)
                               (list ,@(loop for method-option in method-options
                                             collect `(defmethod ,name ,@(cdr method-option))))))
                     ,gf)))))))

  #+(or abcl sbcl)
  (defmacro defgeneric (&whole form name (&rest args) &body options)
    (unless (every #'consp options)
      (error "Illegal generic function options in defgeneric form ~S." form))
    (let ((options-without-methods (remove :method options :key #'car :test #'eq)))
      `(progn
         (eval-when (:compile-toplevel)
           (cl:defgeneric ,name ,args ,@options-without-methods))
         (eval-when (:load-toplevel :execute)
           (cl:defgeneric ,name ,args ,@options)))))

  #-(or abcl sbcl)
  (progn
    (defun create-gf-lambda-list (method-lambda-list)
      (loop with stop-keywords = '#.(remove '&optional lambda-list-keywords)
            for arg in method-lambda-list
            until (member arg stop-keywords)
            collect arg into gf-lambda-list
            finally (return (let (rest)
                              (cond ((member '&key method-lambda-list)
                                     (nconc gf-lambda-list '(&key)))
                                    ((setq rest (member '&rest method-lambda-list))
                                     (nconc gf-lambda-list (subseq rest 0 2)))
                                    (t gf-lambda-list))))))
    
    (defun extract-specializers (specialized-args form)
      (loop for specializer-name in (extract-specializer-names specialized-args)
            collect (typecase specializer-name
                      (symbol `(find-class ',specializer-name))
                      (specializer specializer-name)
                      (cons (cond
                             ((> (length specializer-name) 2)
                              (error "Invalid specializer ~S in defmethod form ~S."
                                     specializer-name form))
                             ((eq (car specializer-name) 'eql)
                              `(intern-eql-specializer ,(cadr specializer-name)))
                             (t (error "Invalid specializer ~S in defmethod form ~S."
                                       specializer-name form))))
                      (t (error "Invalid specializer ~S in defmethod form ~S."
                                specializer-name form)))))
    
    (defun load-method (name gf-lambda-list type qualifiers specializers lambda-list function options)
      (let* ((gf (if (fboundp name) (fdefinition name)
                   (ensure-generic-function name :lambda-list gf-lambda-list :generic-function-class type)))
             (method (apply #'make-instance
                            (generic-function-method-class gf)
                            :qualifiers qualifiers
                            :specializers specializers
                            :lambda-list lambda-list
                            :function function
                            options)))
        (add-method gf method)
        method)))

  (define-condition defmethod-without-generic-function (style-warning)
    ((name :initarg :name :reader dwg-name))
    (:report (lambda (c s) (format s "No generic function present when encountering a defmethod for ~S. Assuming it will be an instance of standard-generic-function." (dwg-name c)))))

  (define-symbol-macro warn-on-defmethod-without-generic-function
    #-(or abcl sbcl) t
    #+(or abcl sbcl) nil)

  #-(or abcl sbcl)
  (defmacro defmethod (&whole form name &body body &environment env)
    (loop with generic-function = (when (fboundp name) (fdefinition name))
        
          initially
          (when (macroexpand 'warn-on-defmethod-without-generic-function env)
            (unless generic-function
              (warn 'defmethod-without-generic-function :name name)))
          (unless (typep generic-function 'standard-generic-function)
            (return-from defmethod `(cl:defmethod ,@(cdr form))))
          (when (only-standard-methods generic-function)
            (return-from defmethod `(cl:defmethod ,@(cdr form))))

          for tail = body then (cdr tail)
          until (listp (car tail))
          collect (car tail) into qualifiers
          finally
          (destructuring-bind
              ((&rest specialized-args) &body body) tail
            (multiple-value-bind
                (documentation declarations main-body)
                (parse-method-body body form)
              (let* ((lambda-list (extract-lambda-list specialized-args))
                     (gf-lambda-list (create-gf-lambda-list lambda-list))
                     (specializers (extract-specializers specialized-args form))
                     (method-class (generic-function-method-class generic-function)))
                #+allegro (ensure-finalized method-class)
                (multiple-value-bind
                    (method-lambda method-options)
                    (make-method-lambda generic-function (class-prototype method-class)
                                        `(lambda ,lambda-list
                                           ,@(when documentation (list documentation))
                                           (declare ,@declarations)
                                           (declare (ignorable ,@(loop for arg in specialized-args
                                                                       until (member arg lambda-list-keywords)
                                                                       when (consp arg) collect (car arg))))
                                           (block ,(if (consp name) (cadr name) name) ,@main-body))
                                        env)
                  (return-from defmethod
                    `(load-method ',name ',gf-lambda-list ',(type-of generic-function)
                                  ',qualifiers (list ,@specializers) ',lambda-list
                                  (function ,method-lambda) ',method-options))))))))

  #+(or abcl sbcl)
  (defmacro defmethod (&whole form name &body body &environment env)
    (declare (ignore body))
    (let ((generic-function (when (fboundp name) (fdefinition name))))
      (when (macroexpand 'warn-on-defmethod-without-generic-function env)
        (unless generic-function
          (warn 'defmethod-without-generic-function :name name)))
      `(cl:defmethod ,@(cdr form))))
)

#+(or allegro clisp cmu mcl scl)
(defun ensure-method (gf lambda-expression 
                         &key (qualifiers ())
                         (lambda-list (cadr lambda-expression))
                         (specializers (required-args lambda-list (constantly (find-class 't)))))
  
  (let ((form `(defmethod ,(generic-function-name gf) ,@qualifiers
                 ,(loop for specializer in specializers
                        for (arg . rest) on lambda-list
                        collect `(,arg ,specializer) into args
                        finally (return (nconc args rest)))
                 ,@(cddr lambda-expression))))
    
    #+(or allegro clisp cmu scl)
    (funcall (compile nil `(lambda () ,form)))

    #+mcl (eval form)))

#+(or abcl clozure ecl clasp lispworks sbcl)
(defun ensure-method (gf lambda-expression
                         &key (method-class (generic-function-method-class gf))
                         (qualifiers ())
                         (lambda-list (cadr lambda-expression))
                         (specializers (required-args lambda-list (constantly (find-class 't)))))
  (multiple-value-bind
      (method-lambda method-args)
      (make-method-lambda
       gf (class-prototype method-class)
       lambda-expression ())
    (let ((method (apply #'make-instance
                         method-class
                         :qualifiers qualifiers
                         :lambda-list lambda-list
                         :specializers specializers
                         :function
                         #-ecl (compile nil method-lambda)
                         #+ecl (coerce method-lambda 'function)
                         method-args)))
      (add-method gf method)
      method)))

;; The following can be used in direct-slot-definition-class to get the correct initargs
;; for a slot. Use it like this:
;;
;; (defmethod direct-slot-definition-class
;;            ((class my-standard-class) &rest initargs)
;;   (destructuring-bind
;;       (&key key-of-interest &allow-other-keys)
;;       (fix-slot-initargs initargs)
;;     ...))

(defvar *standard-slot-keys*
  '(:name :documentation
    :initargs :initform :initfunction
    :readers :writers))

#+(or cmu scl)
(define-modify-macro nconcf (&rest lists) nconc)

(defun fix-slot-initargs (initargs)
  #+(or abcl allegro clisp clozure ecl clasp lispworks mcl sbcl)
  initargs

  #+(or cmu scl)
  (let* ((counts (loop with counts
                       for (key nil) on initargs by #'cddr
                       do (incf (getf counts key 0))
                       finally (return counts)))
         (keys-to-fix (loop for (key value) on counts by #'cddr
                            if (> value 1) collect key)))
    (if keys-to-fix
      (let ((multiple-standard-keys
             (intersection keys-to-fix *standard-slot-keys*)))
        (if multiple-standard-keys
          (error "Too many occurences of ~S in slot initargs ~S."
                 multiple-standard-keys initargs)
          (loop with fixed-keys
                for (key value) on initargs by #'cddr
                if (member key keys-to-fix)
                do (nconcf (getf fixed-keys key) (list value))
                else nconc (list key value) into fixed-initargs
                finally (return (nconc fixed-initargs fixed-keys)))))
      initargs)))

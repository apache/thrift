(in-package :alexandria)

(defun extract-function-name (spec)
  "Useful for macros that want to mimic the functional interface for functions
like #'eq and 'eq."
  (if (and (consp spec)
           (member (first spec) '(quote function)))
      (second spec)
      spec))

(defun generate-switch-body (whole object clauses test key &optional default)
  (with-gensyms (value)
    (setf test (extract-function-name test))
    (setf key (extract-function-name key))
    (when (and (consp default)
               (member (first default) '(error cerror)))
      (setf default `(,@default "No keys match in SWITCH. Testing against ~S with ~S."
                      ,value ',test)))
    `(let ((,value (,key ,object)))
      (cond ,@(mapcar (lambda (clause)
                        (if (member (first clause) '(t otherwise))
                            (progn
                              (when default
                                (error "Multiple default clauses or illegal use of a default clause in ~S."
                                       whole))
                              (setf default `(progn ,@(rest clause)))
                              '(()))
                            (destructuring-bind (key-form &body forms) clause
                              `((,test ,value ,key-form)
                                ,@forms))))
                      clauses)
            (t ,default)))))

(defmacro switch (&whole whole (object &key (test 'eql) (key 'identity))
                         &body clauses)
  "Evaluates first matching clause, returning its values, or evaluates and
returns the values of DEFAULT if no keys match."
  (generate-switch-body whole object clauses test key))

(defmacro eswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  "Like SWITCH, but signals an error if no key matches."
  (generate-switch-body whole object clauses test key '(error)))

(defmacro cswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  "Like SWITCH, but signals a continuable error if no key matches."
  (generate-switch-body whole object clauses test key '(cerror "Return NIL from CSWITCH.")))

(defmacro whichever (&rest possibilities &environment env)
  "Evaluates exactly one of POSSIBILITIES, chosen at random."
  (setf possibilities (mapcar (lambda (p) (macroexpand p env)) possibilities))
  (if (every (lambda (p) (constantp p)) possibilities)
      `(svref (load-time-value (vector ,@possibilities)) (random ,(length possibilities)))
      (labels ((expand (possibilities position random-number)
                 (if (null (cdr possibilities))
                     (car possibilities)
                     (let* ((length (length possibilities))
                            (half (truncate length 2))
                            (second-half (nthcdr half possibilities))
                            (first-half (butlast possibilities (- length half))))
                       `(if (< ,random-number ,(+ position half))
                            ,(expand first-half position random-number)
                            ,(expand second-half (+ position half) random-number))))))
        (with-gensyms (random-number)
          (let ((length (length possibilities)))
            `(let ((,random-number (random ,length)))
               ,(expand possibilities 0 random-number)))))))

(defmacro xor (&rest datums)
  "Evaluates its arguments one at a time, from left to right. If more than one
argument evaluates to a true value no further DATUMS are evaluated, and NIL is
returned as both primary and secondary value. If exactly one argument
evaluates to true, its value is returned as the primary value after all the
arguments have been evaluated, and T is returned as the secondary value. If no
arguments evaluate to true NIL is retuned as primary, and T as secondary
value."
  (with-gensyms (xor tmp true)
    `(let (,tmp ,true)
       (block ,xor
         ,@(mapcar (lambda (datum)
                     `(if (setf ,tmp ,datum)
                          (if ,true
                              (return-from ,xor (values nil nil))
                              (setf ,true ,tmp))))
                   datums)
         (return-from ,xor (values ,true t))))))

(defmacro nth-value-or (nth-value &body forms)
  "Evaluates FORM arguments one at a time, until the NTH-VALUE returned by one
of the forms is true. It then returns all the values returned by evaluating
that form. If none of the forms return a true nth value, this form returns
NIL."
  (once-only (nth-value)
    (with-gensyms (values)
      `(let ((,values (multiple-value-list ,(first forms))))
         (if (nth ,nth-value ,values)
             (values-list ,values)
             ,(if (rest forms)
                  `(nth-value-or ,nth-value ,@(rest forms))
                  nil))))))

(defmacro multiple-value-prog2 (first-form second-form &body forms)
  "Evaluates FIRST-FORM, then SECOND-FORM, and then FORMS. Yields as its value
all the value returned by SECOND-FORM."
  `(progn ,first-form (multiple-value-prog1 ,second-form ,@forms)))

(in-package :alexandria)

(defmacro with-gensyms (names &body forms)
  "Binds each variable named by a symbol in NAMES to a unique symbol around
FORMS. Each of NAMES must either be either a symbol, or of the form:

 (symbol string-designator)

Bare symbols appearing in NAMES are equivalent to:

 (symbol symbol)

The string-designator is used as the argument to GENSYM when constructing the
unique symbol the named variable will be bound to."
  `(let ,(mapcar (lambda (name)
                   (multiple-value-bind (symbol string)
                       (etypecase name
                         (symbol
                          (values name (symbol-name name)))
                         ((cons symbol (cons string-designator null))
                          (values (first name) (string (second name)))))
                     `(,symbol (gensym ,string))))
                 names)
     ,@forms))

(defmacro with-unique-names (names &body forms)
  "Alias for WITH-GENSYMS."
  `(with-gensyms ,names ,@forms))

(defmacro once-only (specs &body forms)
  "Evaluates FORMS with symbols specified in SPECS rebound to temporary
variables, ensuring that each initform is evaluated only once.

Each of SPECS must either be a symbol naming the variable to be rebound, or of
the form:

  (symbol initform)

Bare symbols in SPECS are equivalent to

  (symbol symbol)

Example:

  (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
  (let ((y 0)) (cons1 (incf y))) => (1 . 1)
"
  (let ((gensyms (make-gensym-list (length specs) "ONCE-ONLY"))
        (names-and-forms (mapcar (lambda (spec)
                                   (etypecase spec
                                     (list
                                      (destructuring-bind (name form) spec
                                        (cons name form)))
                                     (symbol
                                      (cons spec spec))))
                                 specs)))
    ;; bind in user-macro
    `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
                   gensyms names-and-forms)
       ;; bind in final expansion
       `(let (,,@(mapcar (lambda (g n)
                           ``(,,g ,,(cdr n)))
                         gensyms names-and-forms))
          ;; bind in user-macro
          ,(let ,(mapcar (lambda (n g) (list (car n) g))
                         names-and-forms gensyms)
             ,@forms)))))

(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
       (setf current (car body))
       (when (and documentation (stringp current) (cdr body))
         (if doc
             (error "Too many documentation strings in ~S." (or whole body))
             (setf doc (pop body)))
         (go :declarations))
       (when (and (listp current) (eql (first current) 'declare))
         (push (pop body) decls)
         (go :declarations)))
    (values body (nreverse decls) doc)))

(defun parse-ordinary-lambda-list (lambda-list &key (normalize t)
                                   allow-specializers
                                   (normalize-optional normalize)
                                   (normalize-keyword normalize)
                                   (normalize-auxilary normalize))
  "Parses an ordinary lambda-list, returning as multiple values:

1. Required parameters.

2. Optional parameter specifications, normalized into form:

   (name init suppliedp)

3. Name of the rest parameter, or NIL.

4. Keyword parameter specifications, normalized into form:

   ((keyword-name name) init suppliedp)

5. Boolean indicating &ALLOW-OTHER-KEYS presence.

6. &AUX parameter specifications, normalized into form

   (name init).

7. Existence of &KEY in the lambda-list.

Signals a PROGRAM-ERROR is the lambda-list is malformed."
  (let ((state :required)
        (allow-other-keys nil)
        (auxp nil)
        (required nil)
        (optional nil)
        (rest nil)
        (keys nil)
        (keyp nil)
        (aux nil))
    (labels ((fail (elt)
               (simple-program-error "Misplaced ~S in ordinary lambda-list:~%  ~S"
                                     elt lambda-list))
             (check-variable (elt what &optional (allow-specializers allow-specializers))
               (unless (and (or (symbolp elt)
                                (and allow-specializers
                                     (consp elt) (= 2 (length elt)) (symbolp (first elt))))
                            (not (constantp elt)))
                 (simple-program-error "Invalid ~A ~S in ordinary lambda-list:~%  ~S"
                                       what elt lambda-list)))
             (check-spec (spec what)
               (destructuring-bind (init suppliedp) spec
                 (declare (ignore init))
                 (check-variable suppliedp what nil))))
      (dolist (elt lambda-list)
        (case elt
          (&optional
           (if (eq state :required)
               (setf state elt)
               (fail elt)))
          (&rest
           (if (member state '(:required &optional))
               (setf state elt)
               (fail elt)))
          (&key
           (if (member state '(:required &optional :after-rest))
               (setf state elt)
               (fail elt))
           (setf keyp t))
          (&allow-other-keys
           (if (eq state '&key)
               (setf allow-other-keys t
                     state elt)
               (fail elt)))
          (&aux
           (cond ((eq state '&rest)
                  (fail elt))
                 (auxp
                  (simple-program-error "Multiple ~S in ordinary lambda-list:~%  ~S"
                                        elt lambda-list))
                 (t
                  (setf auxp t
                        state elt))
                 ))
          (otherwise
           (when (member elt '#.(set-difference lambda-list-keywords
                                                '(&optional &rest &key &allow-other-keys &aux)))
             (simple-program-error
              "Bad lambda-list keyword ~S in ordinary lambda-list:~%  ~S"
              elt lambda-list))
           (case state
             (:required
              (check-variable elt "required parameter")
              (push elt required))
             (&optional
              (cond ((consp elt)
                     (destructuring-bind (name &rest tail) elt
                       (check-variable name "optional parameter")
                       (cond ((cdr tail)
                              (check-spec tail "optional-supplied-p parameter"))
                             ((and normalize-optional tail)
                              (setf elt (append elt '(nil))))
                             (normalize-optional
                              (setf elt (append elt '(nil nil)))))))
                    (t
                     (check-variable elt "optional parameter")
                     (when normalize-optional
                       (setf elt (cons elt '(nil nil))))))
              (push (ensure-list elt) optional))
             (&rest
              (check-variable elt "rest parameter")
              (setf rest elt
                    state :after-rest))
             (&key
              (cond ((consp elt)
                     (destructuring-bind (var-or-kv &rest tail) elt
                       (cond ((consp var-or-kv)
                              (destructuring-bind (keyword var) var-or-kv
                                (unless (symbolp keyword)
                                  (simple-program-error "Invalid keyword name ~S in ordinary ~
                                                         lambda-list:~%  ~S"
                                                        keyword lambda-list))
                                (check-variable var "keyword parameter")))
                             (t
                              (check-variable var-or-kv "keyword parameter")
                              (when normalize-keyword
                                (setf var-or-kv (list (make-keyword var-or-kv) var-or-kv)))))
                       (cond ((cdr tail)
                              (check-spec tail "keyword-supplied-p parameter"))
                             ((and normalize-keyword tail)
                              (setf tail (append tail '(nil))))
                             (normalize-keyword
                              (setf tail '(nil nil))))
                       (setf elt (cons var-or-kv tail))))
                    (t
                     (check-variable elt "keyword parameter")
                     (setf elt (if normalize-keyword
                                   (list (list (make-keyword elt) elt) nil nil)
                                   elt))))
              (push elt keys))
             (&aux
              (if (consp elt)
                  (destructuring-bind (var &optional init) elt
                    (declare (ignore init))
                    (check-variable var "&aux parameter"))
                  (progn
                    (check-variable elt "&aux parameter")
                    (setf elt (list* elt (when normalize-auxilary
                                           '(nil))))))
              (push elt aux))
             (t
              (simple-program-error "Invalid ordinary lambda-list:~%  ~S" lambda-list)))))))
    (values (nreverse required) (nreverse optional) rest (nreverse keys)
            allow-other-keys (nreverse aux) keyp)))

;;;; DESTRUCTURING-*CASE

(defun expand-destructuring-case (key clauses case)
  (once-only (key)
    `(if (typep ,key 'cons)
         (,case (car ,key)
           ,@(mapcar (lambda (clause)
                       (destructuring-bind ((keys . lambda-list) &body body) clause
                         `(,keys
                           (destructuring-bind ,lambda-list (cdr ,key)
                             ,@body))))
                     clauses))
         (error "Invalid key to DESTRUCTURING-~S: ~S" ',case ,key))))

(defmacro destructuring-case (keyform &body clauses)
  "DESTRUCTURING-CASE, -CCASE, and -ECASE are a combination of CASE and DESTRUCTURING-BIND.
KEYFORM must evaluate to a CONS.

Clauses are of the form:

  ((CASE-KEYS . DESTRUCTURING-LAMBDA-LIST) FORM*)

The clause whose CASE-KEYS matches CAR of KEY, as if by CASE, CCASE, or ECASE,
is selected, and FORMs are then executed with CDR of KEY is destructured and
bound by the DESTRUCTURING-LAMBDA-LIST.

Example:

 (defun dcase (x)
   (destructuring-case x
     ((:foo a b)
      (format nil \"foo: ~S, ~S\" a b))
     ((:bar &key a b)
      (format nil \"bar, ~S, ~S\" a b))
     (((:alt1 :alt2) a)
      (format nil \"alt: ~S\" a))
     ((t &rest rest)
      (format nil \"unknown: ~S\" rest))))

  (dcase (list :foo 1 2))        ; => \"foo: 1, 2\"
  (dcase (list :bar :a 1 :b 2))  ; => \"bar: 1, 2\"
  (dcase (list :alt1 1))         ; => \"alt: 1\"
  (dcase (list :alt2 2))         ; => \"alt: 2\"
  (dcase (list :quux 1 2 3))     ; => \"unknown: 1, 2, 3\"

 (defun decase (x)
   (destructuring-case x
     ((:foo a b)
      (format nil \"foo: ~S, ~S\" a b))
     ((:bar &key a b)
      (format nil \"bar, ~S, ~S\" a b))
     (((:alt1 :alt2) a)
      (format nil \"alt: ~S\" a))))

  (decase (list :foo 1 2))        ; => \"foo: 1, 2\"
  (decase (list :bar :a 1 :b 2))  ; => \"bar: 1, 2\"
  (decase (list :alt1 1))         ; => \"alt: 1\"
  (decase (list :alt2 2))         ; => \"alt: 2\"
  (decase (list :quux 1 2 3))     ; =| error
"
  (expand-destructuring-case keyform clauses 'case))

(defmacro destructuring-ccase (keyform &body clauses)
  (expand-destructuring-case keyform clauses 'ccase))

(defmacro destructuring-ecase (keyform &body clauses)
  (expand-destructuring-case keyform clauses 'ecase))

(dolist (name '(destructuring-ccase destructuring-ecase))
  (setf (documentation name 'function) (documentation 'destructuring-case 'function)))




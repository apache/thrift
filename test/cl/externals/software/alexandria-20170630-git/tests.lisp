(in-package :cl-user)

(defpackage :alexandria-tests
  (:use :cl :alexandria #+sbcl :sb-rt #-sbcl :rtest)
  (:import-from #+sbcl :sb-rt #-sbcl :rtest
                #:*compile-tests* #:*expected-failures*))

(in-package :alexandria-tests)

(defun run-tests (&key ((:compiled *compile-tests*)))
  (do-tests))

(defun hash-table-test-name (name)
  ;; Workaround for Clisp calling EQL in a hash-table FASTHASH-EQL.
  (hash-table-test (make-hash-table :test name)))

;;;; Arrays

(deftest copy-array.1
    (let* ((orig (vector 1 2 3))
           (copy (copy-array orig)))
      (values (eq orig copy) (equalp orig copy)))
  nil t)

(deftest copy-array.2
    (let ((orig (make-array 1024 :fill-pointer 0)))
      (vector-push-extend 1 orig)
      (vector-push-extend 2 orig)
      (vector-push-extend 3 orig)
      (let ((copy (copy-array orig)))
        (values (eq orig copy) (equalp orig copy)
                (array-has-fill-pointer-p copy)
                (eql (fill-pointer orig) (fill-pointer copy)))))
  nil t t t)

(deftest copy-array.3
    (let* ((orig (vector 1 2 3))
           (copy (copy-array orig)))
      (typep copy 'simple-array))
  t)

(deftest copy-array.4
   (let ((orig (make-array 21
                           :adjustable t
                           :fill-pointer 0)))
     (dotimes (n 42)
       (vector-push-extend n orig))
     (let ((copy (copy-array orig
                             :adjustable nil
                             :fill-pointer nil)))
       (typep copy 'simple-array)))
 t)

(deftest array-index.1
    (typep 0 'array-index)
  t)

;;;; Conditions

(deftest unwind-protect-case.1
    (let (result)
      (unwind-protect-case ()
          (random 10)
        (:normal (push :normal result))
        (:abort  (push :abort result))
        (:always (push :always result)))
      result)
  (:always :normal))

(deftest unwind-protect-case.2
    (let (result)
      (unwind-protect-case ()
          (random 10)
        (:always (push :always result))
        (:normal (push :normal result))
        (:abort  (push :abort result)))
      result)
  (:normal :always))

(deftest unwind-protect-case.3
    (let (result1 result2 result3)
      (ignore-errors
        (unwind-protect-case ()
            (error "FOOF!")
          (:normal (push :normal result1))
          (:abort  (push :abort result1))
          (:always (push :always result1))))
      (catch 'foof
        (unwind-protect-case ()
            (throw 'foof 42)
          (:normal (push :normal result2))
          (:abort  (push :abort result2))
          (:always (push :always result2))))
      (block foof
        (unwind-protect-case ()
            (return-from foof 42)
          (:normal (push :normal result3))
          (:abort  (push :abort result3))
          (:always (push :always result3))))
      (values result1 result2 result3))
  (:always :abort)
  (:always :abort)
  (:always :abort))

(deftest unwind-protect-case.4
    (let (result)
      (unwind-protect-case (aborted-p)
          (random 42)
        (:always (setq result aborted-p)))
      result)
  nil)

(deftest unwind-protect-case.5
    (let (result)
      (block foof
        (unwind-protect-case (aborted-p)
            (return-from foof)
          (:always (setq result aborted-p))))
      result)
  t)

;;;; Control flow

(deftest switch.1
    (switch (13 :test =)
      (12 :oops)
      (13.0 :yay))
  :yay)

(deftest switch.2
    (switch (13)
      ((+ 12 2) :oops)
      ((- 13 1) :oops2)
      (t :yay))
  :yay)

(deftest eswitch.1
    (let ((x 13))
      (eswitch (x :test =)
        (12 :oops)
        (13.0 :yay)))
  :yay)

(deftest eswitch.2
    (let ((x 13))
      (eswitch (x :key 1+)
        (11 :oops)
        (14 :yay)))
  :yay)

(deftest cswitch.1
    (cswitch (13 :test =)
      (12 :oops)
      (13.0 :yay))
  :yay)

(deftest cswitch.2
    (cswitch (13 :key 1-)
      (12 :yay)
      (13.0 :oops))
  :yay)

(deftest multiple-value-prog2.1
    (multiple-value-prog2
        (values 1 1 1)
        (values 2 20 200)
      (values 3 3 3))
  2 20 200)

(deftest nth-value-or.1
    (multiple-value-bind (a b c)
        (nth-value-or 1
                      (values 1 nil 1)
                      (values 2 2 2))
      (= a b c 2))
  t)

(deftest whichever.1
    (let ((x (whichever 1 2 3)))
      (and (member x '(1 2 3)) t))
  t)

(deftest whichever.2
    (let* ((a 1)
           (b 2)
           (c 3)
           (x (whichever a b c)))
      (and (member x '(1 2 3)) t))
  t)

(deftest xor.1
    (xor nil nil 1 nil)
  1
  t)

(deftest xor.2
    (xor nil nil 1 2)
  nil
  nil)

(deftest xor.3
    (xor nil nil nil)
  nil
  t)

;;;; Definitions

(deftest define-constant.1
    (let ((name (gensym)))
      (eval `(define-constant ,name "FOO" :test 'equal))
      (eval `(define-constant ,name "FOO" :test 'equal))
      (values (equal "FOO" (symbol-value name))
              (constantp name)))
  t
  t)

(deftest define-constant.2
    (let ((name (gensym)))
      (eval `(define-constant ,name 13))
      (eval `(define-constant ,name 13))
      (values (eql 13 (symbol-value name))
              (constantp name)))
  t
  t)

;;;; Errors

;;; TYPEP is specified to return a generalized boolean and, for
;;; example, ECL exploits this by returning the superclasses of ERROR
;;; in this case.
(defun errorp (x)
  (not (null (typep x 'error))))

(deftest required-argument.1
    (multiple-value-bind (res err)
        (ignore-errors (required-argument))
      (errorp err))
  t)

;;;; Hash tables

(deftest ensure-gethash.1
    (let ((table (make-hash-table))
          (x (list 1)))
      (multiple-value-bind (value already-there)
          (ensure-gethash x table 42)
        (and (= value 42)
             (not already-there)
             (= 42 (gethash x table))
             (multiple-value-bind (value2 already-there2)
                 (ensure-gethash x table 13)
               (and (= value2 42)
                    already-there2
                    (= 42 (gethash x table)))))))
  t)

(deftest ensure-gethash.2
    (let ((table (make-hash-table))
          (count 0))
      (multiple-value-call #'values
        (ensure-gethash (progn (incf count) :foo)
                        (progn (incf count) table)
                        (progn (incf count) :bar))
        (gethash :foo table)
        count))
  :bar nil :bar t 3)

(deftest copy-hash-table.1
    (let ((orig (make-hash-table :test 'eq :size 123))
          (foo "foo"))
      (setf (gethash orig orig) t
            (gethash foo orig) t)
      (let ((eq-copy (copy-hash-table orig))
            (eql-copy (copy-hash-table orig :test 'eql))
            (equal-copy (copy-hash-table orig :test 'equal))
            (equalp-copy (copy-hash-table orig :test 'equalp)))
        (list (eql (hash-table-size eq-copy) (hash-table-size orig))
              (eql (hash-table-rehash-size eq-copy)
                   (hash-table-rehash-size orig))
              (hash-table-count eql-copy)
              (gethash orig eq-copy)
              (gethash (copy-seq foo) eql-copy)
              (gethash foo eql-copy)
              (gethash (copy-seq foo) equal-copy)
              (gethash "FOO" equal-copy)
              (gethash "FOO" equalp-copy))))
  (t t 2 t nil t t nil t))

(deftest copy-hash-table.2
    (let ((ht (make-hash-table))
          (list (list :list (vector :A :B :C))))
      (setf (gethash 'list ht) list)
      (let* ((shallow-copy (copy-hash-table ht))
             (deep1-copy (copy-hash-table ht :key 'copy-list))
             (list         (gethash 'list ht))
             (shallow-list (gethash 'list shallow-copy))
             (deep1-list   (gethash 'list deep1-copy)))
        (list (eq ht shallow-copy)
              (eq ht deep1-copy)
              (eq list shallow-list)
              (eq list deep1-list)                   ; outer list was copied.
              (eq (second list) (second shallow-list))
              (eq (second list) (second deep1-list)) ; inner vector wasn't copied.
              )))
  (nil nil t nil t t))

(deftest maphash-keys.1
    (let ((keys nil)
          (table (make-hash-table)))
      (declare (notinline maphash-keys))
      (dotimes (i 10)
        (setf (gethash i table) t))
      (maphash-keys (lambda (k) (push k keys)) table)
      (set-equal keys '(0 1 2 3 4 5 6 7 8 9)))
  t)

(deftest maphash-values.1
    (let ((vals nil)
          (table (make-hash-table)))
      (declare (notinline maphash-values))
      (dotimes (i 10)
        (setf (gethash i table) (- i)))
      (maphash-values (lambda (v) (push v vals)) table)
      (set-equal vals '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)))
  t)

(deftest hash-table-keys.1
    (let ((table (make-hash-table)))
      (dotimes (i 10)
        (setf (gethash i table) t))
      (set-equal (hash-table-keys table) '(0 1 2 3 4 5 6 7 8 9)))
  t)

(deftest hash-table-values.1
    (let ((table (make-hash-table)))
      (dotimes (i 10)
        (setf (gethash (gensym) table) i))
      (set-equal (hash-table-values table) '(0 1 2 3 4 5 6 7 8 9)))
  t)

(deftest hash-table-alist.1
    (let ((table (make-hash-table)))
      (dotimes (i 10)
        (setf (gethash i table) (- i)))
      (let ((alist (hash-table-alist table)))
        (list (length alist)
              (assoc 0 alist)
              (assoc 3 alist)
              (assoc 9 alist)
              (assoc nil alist))))
  (10 (0 . 0) (3 . -3) (9 . -9) nil))

(deftest hash-table-plist.1
    (let ((table (make-hash-table)))
      (dotimes (i 10)
        (setf (gethash i table) (- i)))
      (let ((plist (hash-table-plist table)))
        (list (length plist)
              (getf plist 0)
              (getf plist 2)
              (getf plist 7)
              (getf plist nil))))
  (20 0 -2 -7 nil))

(deftest alist-hash-table.1
    (let* ((alist '((0 a) (1 b) (2 c)))
           (table (alist-hash-table alist)))
      (list (hash-table-count table)
            (gethash 0 table)
            (gethash 1 table)
            (gethash 2 table)
            (eq (hash-table-test-name 'eql)
                (hash-table-test table))))
  (3 (a) (b) (c) t))

(deftest alist-hash-table.duplicate-keys
    (let* ((alist '((0 a) (1 b) (0 c) (1 d) (2 e)))
           (table (alist-hash-table alist)))
      (list (hash-table-count table)
            (gethash 0 table)
            (gethash 1 table)
            (gethash 2 table)))
  (3 (a) (b) (e)))

(deftest plist-hash-table.1
    (let* ((plist '(:a 1 :b 2 :c 3))
           (table (plist-hash-table plist :test 'eq)))
      (list (hash-table-count table)
            (gethash :a table)
            (gethash :b table)
            (gethash :c table)
            (gethash 2 table)
            (gethash nil table)
            (eq (hash-table-test-name 'eq)
                (hash-table-test table))))
  (3 1 2 3 nil nil t))

(deftest plist-hash-table.duplicate-keys
    (let* ((plist '(:a 1 :b 2 :a 3 :b 4 :c 5))
           (table (plist-hash-table plist)))
      (list (hash-table-count table)
            (gethash :a table)
            (gethash :b table)
            (gethash :c table)))
  (3 1 2 5))

;;;; Functions

(deftest disjoin.1
    (let ((disjunction (disjoin (lambda (x)
                                  (and (consp x) :cons))
                                (lambda (x)
                                  (and (stringp x) :string)))))
      (list (funcall disjunction 'zot)
            (funcall disjunction '(foo bar))
            (funcall disjunction "test")))
  (nil :cons :string))

(deftest disjoin.2
    (let ((disjunction (disjoin #'zerop)))
      (list (funcall disjunction 0)
            (funcall disjunction 1)))
  (t nil))

(deftest conjoin.1
    (let ((conjunction (conjoin #'consp
                                (lambda (x)
                                  (stringp (car x)))
                                (lambda (x)
                                  (char (car x) 0)))))
      (list (funcall conjunction 'zot)
            (funcall conjunction '(foo))
            (funcall conjunction '("foo"))))
  (nil nil #\f))

(deftest conjoin.2
    (let ((conjunction (conjoin #'zerop)))
      (list (funcall conjunction 0)
            (funcall conjunction 1)))
  (t nil))

(deftest compose.1
    (let ((composite (compose '1+
                              (lambda (x)
                                (* x 2))
                              #'read-from-string)))
      (funcall composite "1"))
  3)

(deftest compose.2
    (let ((composite
           (locally (declare (notinline compose))
             (compose '1+
                      (lambda (x)
                        (* x 2))
                      #'read-from-string))))
      (funcall composite "2"))
  5)

(deftest compose.3
    (let ((compose-form (funcall (compiler-macro-function 'compose)
                                 '(compose '1+
                                   (lambda (x)
                                     (* x 2))
                                   #'read-from-string)
                                 nil)))
      (let ((fun (funcall (compile nil `(lambda () ,compose-form)))))
        (funcall fun "3")))
  7)

(deftest compose.4
    (let ((composite (compose #'zerop)))
      (list (funcall composite 0)
            (funcall composite 1)))
  (t nil))

(deftest multiple-value-compose.1
    (let ((composite (multiple-value-compose
                      #'truncate
                      (lambda (x y)
                        (values y x))
                      (lambda (x)
                        (with-input-from-string (s x)
                          (values (read s) (read s)))))))
      (multiple-value-list (funcall composite "2 7")))
  (3 1))

(deftest multiple-value-compose.2
    (let ((composite (locally (declare (notinline multiple-value-compose))
                       (multiple-value-compose
                        #'truncate
                        (lambda (x y)
                          (values y x))
                       (lambda (x)
                         (with-input-from-string (s x)
                           (values (read s) (read s))))))))
      (multiple-value-list (funcall composite "2 11")))
  (5 1))

(deftest multiple-value-compose.3
    (let ((compose-form (funcall (compiler-macro-function 'multiple-value-compose)
                                 '(multiple-value-compose
                                   #'truncate
                                   (lambda (x y)
                                     (values y x))
                                   (lambda (x)
                                     (with-input-from-string (s x)
                                       (values (read s) (read s)))))
                                 nil)))
      (let ((fun (funcall (compile nil `(lambda () ,compose-form)))))
        (multiple-value-list (funcall fun "2 9"))))
  (4 1))

(deftest multiple-value-compose.4
    (let ((composite (multiple-value-compose #'truncate)))
      (multiple-value-list (funcall composite 9 2)))
  (4 1))

(deftest curry.1
    (let ((curried (curry '+ 3)))
      (funcall curried 1 5))
  9)

(deftest curry.2
    (let ((curried (locally (declare (notinline curry))
                     (curry '* 2 3))))
      (funcall curried 7))
  42)

(deftest curry.3
    (let ((curried-form (funcall (compiler-macro-function 'curry)
                                 '(curry '/ 8)
                                 nil)))
      (let ((fun (funcall (compile nil `(lambda () ,curried-form)))))
        (funcall fun 2)))
  4)

(deftest curry.4
    (let* ((x 1)
           (curried (curry (progn
                             (incf x)
                             (lambda (y z) (* x y z)))
                           3)))
      (list (funcall curried 7)
            (funcall curried 7)
            x))
  (42 42 2))

(deftest rcurry.1
    (let ((r (rcurry '/ 2)))
      (funcall r 8))
  4)

(deftest rcurry.2
    (let* ((x 1)
           (curried (rcurry (progn
                              (incf x)
                              (lambda (y z) (* x y z)))
                            3)))
      (list (funcall curried 7)
            (funcall curried 7)
            x))
  (42 42 2))

(deftest named-lambda.1
    (let ((fac (named-lambda fac (x)
                 (if (> x 1)
                     (* x (fac (- x 1)))
                     x))))
      (funcall fac 5))
  120)

(deftest named-lambda.2
    (let ((fac (named-lambda fac (&key x)
                 (if (> x 1)
                     (* x (fac :x (- x 1)))
                     x))))
      (funcall fac :x 5))
  120)

;;;; Lists

(deftest alist-plist.1
    (alist-plist '((a . 1) (b . 2) (c . 3)))
  (a 1 b 2 c 3))

(deftest plist-alist.1
    (plist-alist '(a 1 b 2 c 3))
  ((a . 1) (b . 2) (c . 3)))

(deftest unionf.1
    (let* ((list (list 1 2 3))
           (orig list))
      (unionf list (list 1 2 4))
      (values (equal orig (list 1 2 3))
              (eql (length list) 4)
              (set-difference list (list 1 2 3 4))
              (set-difference (list 1 2 3 4) list)))
  t
  t
  nil
  nil)

(deftest nunionf.1
    (let ((list (list 1 2 3)))
      (nunionf list (list 1 2 4))
      (values (eql (length list) 4)
              (set-difference (list 1 2 3 4) list)
              (set-difference list (list 1 2 3 4))))
  t
  nil
  nil)

(deftest appendf.1
    (let* ((list (list 1 2 3))
           (orig list))
      (appendf list '(4 5 6) '(7 8))
      (list list (eq list orig)))
  ((1 2 3 4 5 6 7 8) nil))

(deftest nconcf.1
    (let ((list1 (list 1 2 3))
          (list2 (list 4 5 6)))
      (nconcf list1 list2 (list 7 8 9))
      list1)
  (1 2 3 4 5 6 7 8 9))

(deftest circular-list.1
    (let ((circle (circular-list 1 2 3)))
      (list (first circle)
            (second circle)
            (third circle)
            (fourth circle)
            (eq circle (nthcdr 3 circle))))
  (1 2 3 1 t))

(deftest circular-list-p.1
    (let* ((circle (circular-list 1 2 3 4))
           (tree (list circle circle))
           (dotted (cons circle t))
           (proper (list 1 2 3 circle))
           (tailcirc (list* 1 2 3 circle)))
      (list (circular-list-p circle)
            (circular-list-p tree)
            (circular-list-p dotted)
            (circular-list-p proper)
            (circular-list-p tailcirc)))
  (t nil nil nil t))

(deftest circular-list-p.2
    (circular-list-p 'foo)
  nil)

(deftest circular-tree-p.1
    (let* ((circle (circular-list 1 2 3 4))
           (tree1 (list circle circle))
           (tree2 (let* ((level2 (list 1 nil 2))
                         (level1 (list level2)))
                    (setf (second level2) level1)
                    level1))
           (dotted (cons circle t))
           (proper (list 1 2 3 circle))
           (tailcirc (list* 1 2 3 circle))
           (quite-proper (list 1 2 3))
           (quite-dotted (list 1 (cons 2 3))))
      (list (circular-tree-p circle)
            (circular-tree-p tree1)
            (circular-tree-p tree2)
            (circular-tree-p dotted)
            (circular-tree-p proper)
            (circular-tree-p tailcirc)
            (circular-tree-p quite-proper)
            (circular-tree-p quite-dotted)))
  (t t t t t t nil nil))

(deftest circular-tree-p.2
    (alexandria:circular-tree-p '#1=(#1#))
  t)

(deftest proper-list-p.1
    (let ((l1 (list 1))
          (l2 (list 1 2))
          (l3 (cons 1 2))
          (l4 (list (cons 1 2) 3))
          (l5 (circular-list 1 2)))
      (list (proper-list-p l1)
            (proper-list-p l2)
            (proper-list-p l3)
            (proper-list-p l4)
            (proper-list-p l5)))
  (t t nil t nil))

(deftest proper-list-p.2
    (proper-list-p '(1 2 . 3))
  nil)

(deftest proper-list.type.1
    (let ((l1 (list 1))
          (l2 (list 1 2))
          (l3 (cons 1 2))
          (l4 (list (cons 1 2) 3))
          (l5 (circular-list 1 2)))
      (list (typep l1 'proper-list)
            (typep l2 'proper-list)
            (typep l3 'proper-list)
            (typep l4 'proper-list)
            (typep l5 'proper-list)))
  (t t nil t nil))

(deftest proper-list-length.1
    (values
     (proper-list-length nil)
     (proper-list-length (list 1))
     (proper-list-length (list 2 2))
     (proper-list-length (list 3 3 3))
     (proper-list-length (list 4 4 4 4))
     (proper-list-length (list 5 5 5 5 5))
     (proper-list-length (list 6 6 6 6 6 6))
     (proper-list-length (list 7 7 7 7 7 7 7))
     (proper-list-length (list 8 8 8 8 8 8 8 8))
     (proper-list-length (list 9 9 9 9 9 9 9 9 9)))
  0 1 2 3 4 5 6 7 8 9)

(deftest proper-list-length.2
    (flet ((plength (x)
             (handler-case
                 (proper-list-length x)
               (type-error ()
                 :ok))))
      (values
       (plength (list* 1))
       (plength (list* 2 2))
       (plength (list* 3 3 3))
       (plength (list* 4 4 4 4))
       (plength (list* 5 5 5 5 5))
       (plength (list* 6 6 6 6 6 6))
       (plength (list* 7 7 7 7 7 7 7))
       (plength (list* 8 8 8 8 8 8 8 8))
       (plength (list* 9 9 9 9 9 9 9 9 9))))
  :ok :ok :ok
  :ok :ok :ok
  :ok :ok :ok)

(deftest lastcar.1
    (let ((l1 (list 1))
          (l2 (list 1 2)))
      (list (lastcar l1)
            (lastcar l2)))
  (1 2))

(deftest lastcar.error.2
    (handler-case
        (progn
          (lastcar (circular-list 1 2 3))
          nil)
      (error ()
        t))
  t)

(deftest setf-lastcar.1
    (let ((l (list 1 2 3 4)))
      (values (lastcar l)
              (progn
                (setf (lastcar l) 42)
                (lastcar l))))
  4
  42)

(deftest setf-lastcar.2
    (let ((l (circular-list 1 2 3)))
      (multiple-value-bind (res err)
          (ignore-errors (setf (lastcar l) 4))
        (typep err 'type-error)))
  t)

(deftest make-circular-list.1
    (let ((l (make-circular-list 3 :initial-element :x)))
      (setf (car l) :y)
      (list (eq l (nthcdr 3 l))
            (first l)
            (second l)
            (third l)
            (fourth l)))
  (t :y :x :x :y))

(deftest circular-list.type.1
    (let* ((l1 (list 1 2 3))
           (l2 (circular-list 1 2 3))
           (l3 (list* 1 2 3 l2)))
      (list (typep l1 'circular-list)
            (typep l2 'circular-list)
            (typep l3 'circular-list)))
  (nil t t))

(deftest ensure-list.1
    (let ((x (list 1))
          (y 2))
      (list (ensure-list x)
            (ensure-list y)))
  ((1) (2)))

(deftest ensure-cons.1
    (let ((x (cons 1 2))
          (y nil)
          (z "foo"))
      (values (ensure-cons x)
              (ensure-cons y)
              (ensure-cons z)))
  (1 . 2)
  (nil)
  ("foo"))

(deftest setp.1
    (setp '(1))
  t)

(deftest setp.2
    (setp nil)
  t)

(deftest setp.3
    (setp "foo")
  nil)

(deftest setp.4
    (setp '(1 2 3 1))
  nil)

(deftest setp.5
    (setp '(1 2 3))
  t)

(deftest setp.6
    (setp '(a :a))
  t)

(deftest setp.7
    (setp '(a :a) :key 'character)
  nil)

(deftest setp.8
    (setp '(a :a) :key 'character :test (constantly nil))
  t)

(deftest set-equal.1
    (set-equal '(1 2 3) '(3 1 2))
  t)

(deftest set-equal.2
    (set-equal '("Xa") '("Xb")
               :test (lambda (a b) (eql (char a 0) (char b 0))))
  t)

(deftest set-equal.3
    (set-equal '(1 2) '(4 2))
  nil)

(deftest set-equal.4
    (set-equal '(a b c) '(:a :b :c) :key 'string :test 'equal)
  t)

(deftest set-equal.5
    (set-equal '(a d c) '(:a :b :c) :key 'string :test 'equal)
  nil)

(deftest set-equal.6
    (set-equal '(a b c) '(a b c d))
  nil)

(deftest map-product.1
    (map-product 'cons '(2 3) '(1 4))
  ((2 . 1) (2 . 4) (3 . 1) (3 . 4)))

(deftest map-product.2
    (map-product #'cons '(2 3) '(1 4))
  ((2 . 1) (2 . 4) (3 . 1) (3 . 4)))

(deftest flatten.1
    (flatten '((1) 2 (((3 4))) ((((5)) 6)) 7))
  (1 2 3 4 5 6 7))

(deftest remove-from-plist.1
    (let ((orig '(a 1 b 2 c 3 d 4)))
      (list (remove-from-plist orig 'a 'c)
            (remove-from-plist orig 'b 'd)
            (remove-from-plist orig 'b)
            (remove-from-plist orig 'a)
            (remove-from-plist orig 'd 42 "zot")
            (remove-from-plist orig 'a 'b 'c 'd)
            (remove-from-plist orig 'a 'b 'c 'd 'x)
            (equal orig '(a 1 b 2 c 3 d 4))))
  ((b 2 d 4)
   (a 1 c 3)
   (a 1 c 3 d 4)
   (b 2 c 3 d 4)
   (a 1 b 2 c 3)
   nil
   nil
   t))

(deftest delete-from-plist.1
    (let ((orig '(a 1 b 2 c 3 d 4 d 5)))
      (list (delete-from-plist (copy-list orig) 'a 'c)
            (delete-from-plist (copy-list orig) 'b 'd)
            (delete-from-plist (copy-list orig) 'b)
            (delete-from-plist (copy-list orig) 'a)
            (delete-from-plist (copy-list orig) 'd 42 "zot")
            (delete-from-plist (copy-list orig) 'a 'b 'c 'd)
            (delete-from-plist (copy-list orig) 'a 'b 'c 'd 'x)
            (equal orig (delete-from-plist orig))
            (eq orig (delete-from-plist orig))))
  ((b 2 d 4 d 5)
   (a 1 c 3)
   (a 1 c 3 d 4 d 5)
   (b 2 c 3 d 4 d 5)
   (a 1 b 2 c 3)
   nil
   nil
   t
   t))

(deftest mappend.1
    (mappend (compose 'list '*) '(1 2 3) '(1 2 3))
  (1 4 9))

(deftest assoc-value.1
    (let ((key1 '(complex key))
          (key2 'simple-key)
          (alist '())
          (result '()))
      (push 1 (assoc-value alist key1 :test #'equal))
      (push 2 (assoc-value alist key1 :test 'equal))
      (push 42 (assoc-value alist key2))
      (push 43 (assoc-value alist key2 :test 'eq))
      (push (assoc-value alist key1 :test #'equal) result)
      (push (assoc-value alist key2) result)

      (push 'very (rassoc-value alist (list 2 1) :test #'equal))
      (push (cdr (assoc '(very complex key) alist :test #'equal)) result)
      result)
  ((2 1) (43 42) (2 1)))

;;;; Numbers

(deftest clamp.1
    (list (clamp 1.5 1 2)
          (clamp 2.0 1 2)
          (clamp 1.0 1 2)
          (clamp 3 1 2)
          (clamp 0 1 2))
  (1.5 2.0 1.0 2 1))

(deftest gaussian-random.1
    (let ((min -0.2)
          (max +0.2))
      (multiple-value-bind (g1 g2)
          (gaussian-random min max)
        (values (<= min g1 max)
                (<= min g2 max)
                (/= g1 g2) ;uh
                )))
  t
  t
  t)

#+sbcl
(deftest gaussian-random.2
    (handler-case
        (sb-ext:with-timeout 2
          (progn
            (loop
              :repeat 10000
              :do (gaussian-random 0 nil))
            'done))
      (sb-ext:timeout ()
        'timed-out))
  done)

(deftest iota.1
    (iota 3)
  (0 1 2))

(deftest iota.2
    (iota 3 :start 0.0d0)
  (0.0d0 1.0d0 2.0d0))

(deftest iota.3
    (iota 3 :start 2 :step 3.0)
  (2.0 5.0 8.0))

(deftest map-iota.1
    (let (all)
      (declare (notinline map-iota))
      (values (map-iota (lambda (x) (push x all))
                        3
                        :start 2
                        :step 1.1d0)
              all))
  3
  (4.2d0 3.1d0 2.0d0))

(deftest lerp.1
    (lerp 0.5 1 2)
  1.5)

(deftest lerp.2
    (lerp 0.1 1 2)
  1.1)

(deftest lerp.3
    (lerp 0.1 4 25)
  6.1)

(deftest mean.1
    (mean '(1 2 3))
  2)

(deftest mean.2
    (mean '(1 2 3 4))
  5/2)

(deftest mean.3
    (mean '(1 2 10))
  13/3)

(deftest median.1
    (median '(100 0 99 1 98 2 97))
  97)

(deftest median.2
    (median '(100 0 99 1 98 2 97 96))
  193/2)

(deftest variance.1
    (variance (list 1 2 3))
  2/3)

(deftest standard-deviation.1
    (< 0 (standard-deviation (list 1 2 3)) 1)
  t)

(deftest maxf.1
    (let ((x 1))
      (maxf x 2)
      x)
  2)

(deftest maxf.2
    (let ((x 1))
      (maxf x 0)
      x)
  1)

(deftest maxf.3
    (let ((x 1)
          (c 0))
      (maxf x (incf c))
      (list x c))
  (1 1))

(deftest maxf.4
    (let ((xv (vector 0 0 0))
          (p 0))
      (maxf (svref xv (incf p)) (incf p))
      (list p xv))
  (2 #(0 2 0)))

(deftest minf.1
    (let ((y 1))
      (minf y 0)
      y)
  0)

(deftest minf.2
    (let ((xv (vector 10 10 10))
          (p 0))
      (minf (svref xv (incf p)) (incf p))
      (list p xv))
  (2 #(10 2 10)))

(deftest subfactorial.1
    (mapcar #'subfactorial (iota 22))
  (1
   0
   1
   2
   9
   44
   265
   1854
   14833
   133496
   1334961
   14684570
   176214841
   2290792932
   32071101049
   481066515734
   7697064251745
   130850092279664
   2355301661033953
   44750731559645106
   895014631192902121
   18795307255050944540))

;;;; Arrays

#+nil
(deftest array-index.type)

#+nil
(deftest copy-array)

;;;; Sequences

(deftest rotate.1
    (list (rotate (list 1 2 3) 0)
          (rotate (list 1 2 3) 1)
          (rotate (list 1 2 3) 2)
          (rotate (list 1 2 3) 3)
          (rotate (list 1 2 3) 4))
  ((1 2 3)
   (3 1 2)
   (2 3 1)
   (1 2 3)
   (3 1 2)))

(deftest rotate.2
    (list (rotate (vector 1 2 3 4) 0)
          (rotate (vector 1 2 3 4))
          (rotate (vector 1 2 3 4) 2)
          (rotate (vector 1 2 3 4) 3)
          (rotate (vector 1 2 3 4) 4)
          (rotate (vector 1 2 3 4) 5))
  (#(1 2 3 4)
    #(4 1 2 3)
    #(3 4 1 2)
    #(2 3 4 1)
    #(1 2 3 4)
    #(4 1 2 3)))

(deftest rotate.3
    (list (rotate (list 1 2 3) 0)
          (rotate (list 1 2 3) -1)
          (rotate (list 1 2 3) -2)
          (rotate (list 1 2 3) -3)
          (rotate (list 1 2 3) -4))
  ((1 2 3)
   (2 3 1)
   (3 1 2)
   (1 2 3)
   (2 3 1)))

(deftest rotate.4
    (list (rotate (vector 1 2 3 4) 0)
          (rotate (vector 1 2 3 4) -1)
          (rotate (vector 1 2 3 4) -2)
          (rotate (vector 1 2 3 4) -3)
          (rotate (vector 1 2 3 4) -4)
          (rotate (vector 1 2 3 4) -5))
  (#(1 2 3 4)
   #(2 3 4 1)
   #(3 4 1 2)
   #(4 1 2 3)
   #(1 2 3 4)
   #(2 3 4 1)))

(deftest rotate.5
    (values (rotate (list 1) 17)
            (rotate (list 1) -5))
  (1)
  (1))

(deftest shuffle.1
    (let ((s (shuffle (iota 100))))
      (list (equal s (iota 100))
            (every (lambda (x)
                     (member x s))
                   (iota 100))
            (every (lambda (x)
                     (typep x '(integer 0 99)))
                   s)))
  (nil t t))

(deftest shuffle.2
    (let ((s (shuffle (coerce (iota 100) 'vector))))
      (list (equal s (coerce (iota 100) 'vector))
            (every (lambda (x)
                     (find x s))
                   (iota 100))
            (every (lambda (x)
                     (typep x '(integer 0 99)))
                   s)))
  (nil t t))

(deftest shuffle.3
    (let* ((orig (coerce (iota 21) 'vector))
           (copy (copy-seq orig)))
      (shuffle copy :start 10 :end 15)
      (list (every #'eql (subseq copy 0 10) (subseq orig 0 10))
            (every #'eql (subseq copy 15) (subseq orig 15))))
  (t t))

(deftest random-elt.1
    (let ((s1 #(1 2 3 4))
          (s2 '(1 2 3 4)))
      (list (dotimes (i 1000 nil)
              (unless (member (random-elt s1) s2)
                (return nil))
              (when (/= (random-elt s1) (random-elt s1))
                (return t)))
            (dotimes (i 1000 nil)
              (unless (member (random-elt s2) s2)
                (return nil))
              (when (/= (random-elt s2) (random-elt s2))
                (return t)))))
  (t t))

(deftest removef.1
    (let* ((x '(1 2 3))
           (x* x)
           (y #(1 2 3))
           (y* y))
      (removef x 1)
      (removef y 3)
      (list x x* y y*))
  ((2 3)
   (1 2 3)
   #(1 2)
   #(1 2 3)))

(deftest deletef.1
    (let* ((x (list 1 2 3))
           (x* x)
           (y (vector 1 2 3)))
      (deletef x 2)
      (deletef y 1)
      (list x x* y))
  ((1 3)
   (1 3)
   #(2 3)))

(deftest map-permutations.1
    (let ((seq (list 1 2 3))
          (seen nil)
          (ok t))
      (map-permutations (lambda (s)
                          (unless (set-equal s seq)
                            (setf ok nil))
                          (when (member s seen :test 'equal)
                            (setf ok nil))
                          (push s seen))
                        seq
                        :copy t)
      (values ok (length seen)))
  t
  6)

(deftest proper-sequence.type.1
    (mapcar (lambda (x)
              (typep x 'proper-sequence))
            (list (list 1 2 3)
                  (vector 1 2 3)
                  #2a((1 2) (3 4))
                  (circular-list 1 2 3 4)))
  (t t nil nil))

(deftest emptyp.1
    (mapcar #'emptyp
            (list (list 1)
                  (circular-list 1)
                  nil
                  (vector)
                  (vector 1)))
  (nil nil t t nil))

(deftest sequence-of-length-p.1
    (mapcar #'sequence-of-length-p
            (list nil
                  #()
                  (list 1)
                  (vector 1)
                  (list 1 2)
                  (vector 1 2)
                  (list 1 2)
                  (vector 1 2)
                  (list 1 2)
                  (vector 1 2))
            (list 0
                  0
                  1
                  1
                  2
                  2
                  1
                  1
                  4
                  4))
  (t t t t t t nil nil nil nil))

(deftest length=.1
    (mapcar #'length=
            (list nil
                  #()
                  (list 1)
                  (vector 1)
                  (list 1 2)
                  (vector 1 2)
                  (list 1 2)
                  (vector 1 2)
                  (list 1 2)
                  (vector 1 2))
            (list 0
                  0
                  1
                  1
                  2
                  2
                  1
                  1
                  4
                  4))
  (t t t t t t nil nil nil nil))

(deftest length=.2
    ;; test the compiler macro
    (macrolet ((x (&rest args)
                 (funcall
                  (compile nil
                           `(lambda ()
                              (length= ,@args))))))
      (list (x 2 '(1 2))
            (x '(1 2) '(3 4))
            (x '(1 2) 2)
            (x '(1 2) 2 '(3 4))
            (x 1 2 3)))
  (t t t t nil))

(deftest copy-sequence.1
    (let ((l (list 1 2 3))
          (v (vector #\a #\b #\c)))
      (declare (notinline copy-sequence))
      (let ((l.list (copy-sequence 'list l))
            (l.vector (copy-sequence 'vector l))
            (l.spec-v (copy-sequence '(vector fixnum) l))
            (v.vector (copy-sequence 'vector v))
            (v.list (copy-sequence 'list v))
            (v.string (copy-sequence 'string v)))
        (list (member l (list l.list l.vector l.spec-v))
              (member v (list v.vector v.list v.string))
              (equal l.list l)
              (equalp l.vector #(1 2 3))
              (type= (upgraded-array-element-type 'fixnum)
                     (array-element-type l.spec-v))
              (equalp v.vector v)
              (equal v.list '(#\a #\b #\c))
              (equal "abc" v.string))))
  (nil nil t t t t t t))

(deftest first-elt.1
    (mapcar #'first-elt
            (list (list 1 2 3)
                  "abc"
                  (vector :a :b :c)))
  (1 #\a :a))

(deftest first-elt.error.1
    (mapcar (lambda (x)
              (handler-case
                  (first-elt x)
                (type-error ()
                  :type-error)))
            (list nil
                  #()
                  12
                  :zot))
  (:type-error
   :type-error
   :type-error
   :type-error))

(deftest setf-first-elt.1
    (let ((l (list 1 2 3))
          (s (copy-seq "foobar"))
          (v (vector :a :b :c)))
      (setf (first-elt l) -1
            (first-elt s) #\x
            (first-elt v) 'zot)
      (values l s v))
  (-1 2 3)
  "xoobar"
  #(zot :b :c))

(deftest setf-first-elt.error.1
    (let ((l 'foo))
      (multiple-value-bind (res err)
          (ignore-errors (setf (first-elt l) 4))
        (typep err 'type-error)))
  t)

(deftest last-elt.1
    (mapcar #'last-elt
            (list (list 1 2 3)
                  (vector :a :b :c)
                  "FOOBAR"
                  #*001
                  #*010))
  (3 :c #\R 1 0))

(deftest last-elt.error.1
    (mapcar (lambda (x)
              (handler-case
                  (last-elt x)
                (type-error ()
                  :type-error)))
            (list nil
                  #()
                  12
                  :zot
                  (circular-list 1 2 3)
                  (list* 1 2 3 (circular-list 4 5))))
  (:type-error
   :type-error
   :type-error
   :type-error
   :type-error
   :type-error))

(deftest setf-last-elt.1
    (let ((l (list 1 2 3))
          (s (copy-seq "foobar"))
          (b (copy-seq #*010101001)))
      (setf (last-elt l) '???
            (last-elt s) #\?
            (last-elt b) 0)
      (values l s b))
  (1 2 ???)
  "fooba?"
  #*010101000)

(deftest setf-last-elt.error.1
    (handler-case
        (setf (last-elt 'foo) 13)
      (type-error ()
        :type-error))
  :type-error)

(deftest starts-with.1
    (list (starts-with 1 '(1 2 3))
          (starts-with 1 #(1 2 3))
          (starts-with #\x "xyz")
          (starts-with 2 '(1 2 3))
          (starts-with 3 #(1 2 3))
          (starts-with 1 1)
          (starts-with nil nil))
  (t t t nil nil nil nil))

(deftest starts-with.2
    (values (starts-with 1 '(-1 2 3) :key '-)
            (starts-with "foo" '("foo" "bar") :test 'equal)
            (starts-with "f" '(#\f) :key 'string :test 'equal)
            (starts-with -1 '(0 1 2) :key #'1+)
            (starts-with "zot" '("ZOT") :test 'equal))
  t
  t
  t
  nil
  nil)

(deftest ends-with.1
    (list (ends-with 3 '(1 2 3))
          (ends-with 3 #(1 2 3))
          (ends-with #\z "xyz")
          (ends-with 2 '(1 2 3))
          (ends-with 1 #(1 2 3))
          (ends-with 1 1)
          (ends-with nil nil))
  (t t t nil nil nil nil))

(deftest ends-with.2
    (values (ends-with 2 '(0 13 1) :key '1+)
            (ends-with "foo" (vector "bar" "foo") :test 'equal)
            (ends-with "X" (vector 1 2 #\X) :key 'string :test 'equal)
            (ends-with "foo" "foo" :test 'equal))
  t
  t
  t
  nil)

(deftest ends-with.error.1
    (handler-case
        (ends-with 3 (circular-list 3 3 3 1 3 3))
      (type-error ()
        :type-error))
  :type-error)

(deftest sequences.passing-improper-lists
    (macrolet ((signals-error-p (form)
                 `(handler-case
                      (progn ,form nil)
                    (type-error (e)
                      t)))
               (cut (fn &rest args)
                 (with-gensyms (arg)
                   (print`(lambda (,arg)
                       (apply ,fn (list ,@(substitute arg '_ args))))))))
      (let ((circular-list (make-circular-list 5 :initial-element :foo))
            (dotted-list (list* 'a 'b 'c 'd)))
        (loop for nth from 0
              for fn in (list
                         (cut #'lastcar _)
                         (cut #'rotate _ 3)
                         (cut #'rotate _ -3)
                         (cut #'shuffle _)
                         (cut #'random-elt _)
                         (cut #'last-elt _)
                         (cut #'ends-with :foo _))
              nconcing
                 (let ((on-circular-p (signals-error-p (funcall fn circular-list)))
                       (on-dotted-p (signals-error-p (funcall fn dotted-list))))
                   (when (or (not on-circular-p) (not on-dotted-p))
                     (append
                      (unless on-circular-p
                        (let ((*print-circle* t))
                          (list
                           (format nil
                                   "No appropriate error signalled when passing ~S to ~Ath entry."
                                   circular-list nth))))
                      (unless on-dotted-p
                        (list
                         (format nil
                                 "No appropriate error signalled when passing ~S to ~Ath entry."
                                 dotted-list nth)))))))))
  nil)

;;;; IO

(deftest read-stream-content-into-string.1
    (values (with-input-from-string (stream "foo bar")
              (read-stream-content-into-string stream))
            (with-input-from-string (stream "foo bar")
              (read-stream-content-into-string stream :buffer-size 1))
            (with-input-from-string (stream "foo bar")
              (read-stream-content-into-string stream :buffer-size 6))
            (with-input-from-string (stream "foo bar")
              (read-stream-content-into-string stream :buffer-size 7)))
  "foo bar"
  "foo bar"
  "foo bar"
  "foo bar")

(deftest read-stream-content-into-string.2
    (handler-case
        (let ((stream (make-broadcast-stream)))
          (read-stream-content-into-string stream :buffer-size 0))
      (type-error ()
        :type-error))
  :type-error)

#+(or)
(defvar *octets*
  (map '(simple-array (unsigned-byte 8) (7)) #'char-code "foo bar"))

#+(or)
(deftest read-stream-content-into-byte-vector.1
    (values (with-input-from-byte-vector (stream *octets*)
              (read-stream-content-into-byte-vector stream))
            (with-input-from-byte-vector (stream *octets*)
              (read-stream-content-into-byte-vector stream :initial-size 1))
            (with-input-from-byte-vector (stream *octets*)
              (read-stream-content-into-byte-vector stream 'alexandria::%length 6))
            (with-input-from-byte-vector (stream *octets*)
              (read-stream-content-into-byte-vector stream 'alexandria::%length 3)))
  *octets*
  *octets*
  *octets*
  (subseq *octets* 0 3))

(deftest read-stream-content-into-byte-vector.2
    (handler-case
        (let ((stream (make-broadcast-stream)))
          (read-stream-content-into-byte-vector stream :initial-size 0))
      (type-error ()
        :type-error))
  :type-error)

;;;; Macros

(deftest with-unique-names.1
    (let ((*gensym-counter* 0))
      (let ((syms (with-unique-names (foo bar quux)
                    (list foo bar quux))))
        (list (find-if #'symbol-package syms)
              (equal '("FOO0" "BAR1" "QUUX2")
                     (mapcar #'symbol-name syms)))))
  (nil t))

(deftest with-unique-names.2
    (let ((*gensym-counter* 0))
      (let ((syms (with-unique-names ((foo "_foo_") (bar -bar-) (quux #\q))
                    (list foo bar quux))))
        (list (find-if #'symbol-package syms)
              (equal '("_foo_0" "-BAR-1" "q2")
                     (mapcar #'symbol-name syms)))))
  (nil t))

(deftest with-unique-names.3
    (let ((*gensym-counter* 0))
      (multiple-value-bind (res err)
          (ignore-errors
            (eval
             '(let ((syms
                     (with-unique-names ((foo "_foo_") (bar -bar-) (quux 42))
                       (list foo bar quux))))
               (list (find-if #'symbol-package syms)
                (equal '("_foo_0" "-BAR-1" "q2")
                 (mapcar #'symbol-name syms))))))
        (errorp err)))
  t)

(deftest once-only.1
    (macrolet ((cons1.good (x)
                 (once-only (x)
                   `(cons ,x ,x)))
               (cons1.bad (x)
                 `(cons ,x ,x)))
      (let ((y 0))
        (list (cons1.good (incf y))
              y
              (cons1.bad (incf y))
              y)))
  ((1 . 1) 1 (2 . 3) 3))

(deftest once-only.2
    (macrolet ((cons1 (x)
                 (once-only ((y x))
                   `(cons ,y ,y))))
      (let ((z 0))
        (list (cons1 (incf z))
              z
              (cons1 (incf z)))))
  ((1 . 1) 1 (2 . 2)))

(deftest parse-body.1
    (parse-body '("doc" "body") :documentation t)
  ("body")
  nil
  "doc")

(deftest parse-body.2
    (parse-body '("body") :documentation t)
  ("body")
  nil
  nil)

(deftest parse-body.3
    (parse-body '("doc" "body"))
  ("doc" "body")
  nil
  nil)

(deftest parse-body.4
    (parse-body '((declare (foo)) "doc" (declare (bar)) body) :documentation t)
  (body)
  ((declare (foo)) (declare (bar)))
  "doc")

(deftest parse-body.5
    (parse-body '((declare (foo)) "doc" (declare (bar)) body))
  ("doc" (declare (bar)) body)
  ((declare (foo)))
  nil)

(deftest parse-body.6
    (multiple-value-bind (res err)
        (ignore-errors
          (parse-body '("foo" "bar" "quux")
                      :documentation t))
      (errorp err))
  t)

;;;; Symbols

(deftest ensure-symbol.1
    (ensure-symbol :cons :cl)
  cons
  :external)

(deftest ensure-symbol.2
    (ensure-symbol "CONS" :alexandria)
  cons
  :inherited)

(deftest ensure-symbol.3
    (ensure-symbol 'foo :keyword)
  :foo
  :external)

(deftest ensure-symbol.4
    (ensure-symbol #\* :alexandria)
  *
  :inherited)

(deftest format-symbol.1
    (let ((s (format-symbol nil '#:x-~d 13)))
      (list (symbol-package s)
            (string= (string '#:x-13) (symbol-name s))))
  (nil t))

(deftest format-symbol.2
    (format-symbol :keyword '#:sym-~a (string :bolic))
  :sym-bolic)

(deftest format-symbol.3
    (let ((*package* (find-package :cl)))
      (format-symbol t '#:find-~a (string 'package)))
  find-package)

(deftest make-keyword.1
    (list (make-keyword 'zot)
          (make-keyword "FOO")
          (make-keyword #\Q))
  (:zot :foo :q))

(deftest make-gensym-list.1
    (let ((*gensym-counter* 0))
      (let ((syms (make-gensym-list 3 "FOO")))
        (list (find-if 'symbol-package syms)
              (equal '("FOO0" "FOO1" "FOO2")
                     (mapcar 'symbol-name syms)))))
  (nil t))

(deftest make-gensym-list.2
    (let ((*gensym-counter* 0))
      (let ((syms (make-gensym-list 3)))
        (list (find-if 'symbol-package syms)
              (equal '("G0" "G1" "G2")
                     (mapcar 'symbol-name syms)))))
  (nil t))

;;;; Type-system

(deftest of-type.1
    (locally
        (declare (notinline of-type))
    (let ((f (of-type 'string)))
      (list (funcall f "foo")
            (funcall f 'bar))))
  (t nil))

(deftest type=.1
    (type= 'string 'string)
  t
  t)

(deftest type=.2
    (type= 'list '(or null cons))
  t
  t)

(deftest type=.3
    (type= 'null '(and symbol list))
  t
  t)

(deftest type=.4
    (type= 'string '(satisfies emptyp))
  nil
  nil)

(deftest type=.5
    (type= 'string 'list)
  nil
  t)

(macrolet
    ((test (type numbers)
       `(deftest ,(format-symbol t '#:cdr5.~a (string type))
            (let ((numbers ,numbers))
              (values (mapcar (of-type ',(format-symbol t '#:negative-~a (string type))) numbers)
                      (mapcar (of-type ',(format-symbol t '#:non-positive-~a (string type))) numbers)
                      (mapcar (of-type ',(format-symbol t '#:non-negative-~a (string type))) numbers)
                      (mapcar (of-type ',(format-symbol t '#:positive-~a (string type))) numbers)))
          (t t t nil nil nil nil)
          (t t t t nil nil nil)
          (nil nil nil t t t t)
          (nil nil nil nil t t t))))
  (test fixnum       (list most-negative-fixnum       -42      -1     0     1     42      most-positive-fixnum))
  (test integer      (list (1- most-negative-fixnum)  -42      -1     0     1     42      (1+ most-positive-fixnum)))
  (test rational     (list (1- most-negative-fixnum)  -42/13   -1     0     1     42/13   (1+ most-positive-fixnum)))
  (test real         (list most-negative-long-float   -42/13   -1     0     1     42/13   most-positive-long-float))
  (test float        (list most-negative-short-float  -42.02   -1.0   0.0   1.0   42.02   most-positive-short-float))
  (test short-float  (list most-negative-short-float  -42.02s0 -1.0s0 0.0s0 1.0s0 42.02s0 most-positive-short-float))
  (test single-float (list most-negative-single-float -42.02f0 -1.0f0 0.0f0 1.0f0 42.02f0 most-positive-single-float))
  (test double-float (list most-negative-double-float -42.02d0 -1.0d0 0.0d0 1.0d0 42.02d0 most-positive-double-float))
  (test long-float   (list most-negative-long-float   -42.02l0 -1.0l0 0.0l0 1.0l0 42.02l0 most-positive-long-float)))

;;;; Bindings

(declaim (notinline opaque))
(defun opaque (x)
  x)

(deftest if-let.1
    (if-let (x (opaque :ok))
            x
            :bad)
  :ok)

(deftest if-let.2
    (if-let (x (opaque nil))
            :bad
            (and (not x) :ok))
  :ok)

(deftest if-let.3
    (let ((x 1))
      (if-let ((x 2)
               (y x))
              (+ x y)
              :oops))
  3)

(deftest if-let.4
    (if-let ((x 1)
             (y nil))
            :oops
            (and (not y) x))
  1)

(deftest if-let.5
    (if-let (x)
            :oops
            (not x))
  t)

(deftest if-let.error.1
    (handler-case
        (eval '(if-let x
                :oops
                :oops))
      (type-error ()
        :type-error))
  :type-error)

(deftest when-let.1
    (when-let (x (opaque :ok))
      (setf x (cons x x))
      x)
  (:ok . :ok))

(deftest when-let.2
    (when-let ((x 1)
               (y nil)
               (z 3))
      :oops)
  nil)

(deftest when-let.3
    (let ((x 1))
      (when-let ((x 2)
                 (y x))
        (+ x y)))
  3)

(deftest when-let.error.1
    (handler-case
        (eval '(when-let x :oops))
      (type-error ()
        :type-error))
  :type-error)

(deftest when-let*.1
    (let ((x 1))
      (when-let* ((x 2)
                  (y x))
        (+ x y)))
  4)

(deftest when-let*.2
    (let ((y 1))
      (when-let* (x y)
        (1+ x)))
  2)

(deftest when-let*.3
    (when-let* ((x t)
                (y (consp x))
                (z (error "OOPS")))
      t)
  nil)

(deftest when-let*.error.1
    (handler-case
        (eval '(when-let* x :oops))
      (type-error ()
        :type-error))
  :type-error)

(deftest doplist.1
    (let (keys values)
      (doplist (k v '(a 1 b 2 c 3) (values t (reverse keys) (reverse values) k v))
        (push k keys)
        (push v values)))
  t
  (a b c)
  (1 2 3)
  nil
  nil)

(deftest count-permutations.1
    (values (count-permutations 31 7)
            (count-permutations 1 1)
            (count-permutations 2 1)
            (count-permutations 2 2)
            (count-permutations 3 2)
            (count-permutations 3 1))
  13253058000
  1
  2
  2
  6
  3)

(deftest binomial-coefficient.1
    (alexandria:binomial-coefficient 1239 139)
  28794902202288970200771694600561826718847179309929858835480006683522184441358211423695124921058123706380656375919763349913245306834194782172712255592710204598527867804110129489943080460154)

;; Exercise bignum case (at least on x86).
(deftest binomial-coefficient.2
    (alexandria:binomial-coefficient 2000000000000 20)
  430998041177272843950422879590338454856322722740402365741730748431530623813012487773080486408378680853987520854296499536311275320016878730999689934464711239072435565454954447356845336730100919970769793030177499999999900000000000)

(deftest copy-stream.1
    (let ((data "sdkfjhsakfh weior763495ewofhsdfk sdfadlkfjhsadf woif sdlkjfhslkdfh sdklfjh"))
      (values (equal data
                     (with-input-from-string (in data)
                       (with-output-to-string (out)
                         (alexandria:copy-stream in out))))
              (equal (subseq data 10 20)
                     (with-input-from-string (in data)
                       (with-output-to-string (out)
                         (alexandria:copy-stream in out :start 10 :end 20))))
              (equal (subseq data 10)
                     (with-input-from-string (in data)
                       (with-output-to-string (out)
                         (alexandria:copy-stream in out :start 10))))
              (equal (subseq data 0 20)
                     (with-input-from-string (in data)
                       (with-output-to-string (out)
                         (alexandria:copy-stream in out :end 20))))))
  t
  t
  t
  t)

(deftest extremum.1
    (let ((n 0))
      (dotimes (i 10)
       (let ((data (shuffle (coerce (iota 10000 :start i) 'vector)))
             (ok t))
         (unless (eql i (extremum data #'<))
           (setf ok nil))
         (unless (eql i (extremum (coerce data 'list) #'<))
           (setf ok nil))
         (unless (eql (+ 9999 i) (extremum data #'>))
           (setf ok nil))
         (unless (eql (+ 9999 i) (extremum (coerce  data 'list) #'>))
           (setf ok nil))
         (when ok
           (incf n))))
      (when (eql 10 (extremum #(100 1 10 1000) #'> :start 1 :end 3))
        (incf n))
      (when (eql -1000 (extremum #(100 1 10 -1000) #'> :key 'abs))
        (incf n))
      (when (eq nil (extremum "" (lambda (a b) (error "wtf? ~S, ~S" a b))))
        (incf n))
      n)
  13)

(deftest starts-with-subseq.string
    (starts-with-subseq "f" "foo" :return-suffix t)
  t
  "oo")

(deftest starts-with-subseq.vector
    (starts-with-subseq #(1) #(1 2 3) :return-suffix t)
  t
  #(2 3))

(deftest starts-with-subseq.list
    (starts-with-subseq '(1) '(1 2 3) :return-suffix t)
  t
  (2 3))

(deftest starts-with-subseq.start1
    (starts-with-subseq "foo" "oop" :start1 1)
  t
  nil)

(deftest starts-with-subseq.start2
    (starts-with-subseq "foo" "xfoop" :start2 1)
  t
  nil)

(deftest format-symbol.print-case-bound
    (let ((upper (intern "FOO-BAR"))
          (lower (intern "foo-bar"))
          (*print-escape* nil))
      (values
       (let ((*print-case* :downcase))
         (and (eq upper (format-symbol t "~A" upper))
               (eq lower (format-symbol t "~A" lower))))
       (let ((*print-case* :upcase))
         (and (eq upper (format-symbol t "~A" upper))
               (eq lower (format-symbol t "~A" lower))))
       (let ((*print-case* :capitalize))
         (and (eq upper (format-symbol t "~A" upper))
              (eq lower (format-symbol t "~A" lower))))))
  t
  t
  t)

(deftest iota.fp-start-and-complex-integer-step
    (equal '(#C(0.0 0.0) #C(0.0 2.0) #C(0.0 4.0))
           (iota 3 :start 0.0 :step #C(0 2)))
  t)

(deftest parse-ordinary-lambda-list.1
    (multiple-value-bind (req opt rest keys allowp aux keyp)
        (parse-ordinary-lambda-list '(a b c
                                      &optional o1 (o2 42) (o3 42 o3-supplied?)
                                      &key (k1) ((:key k2)) (k3 42 k3-supplied?))
                                    :normalize t)
      (and (equal '(a b c) req)
           (equal '((o1 nil nil)
                    (o2 42 nil)
                    (o3 42 o3-supplied?))
                  opt)
           (equal '(((:k1 k1) nil nil)
                    ((:key k2) nil nil)
                    ((:k3 k3) 42 k3-supplied?))
                  keys)
           (not allowp)
           (not aux)
           (eq t keyp)))
  t)

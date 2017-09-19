;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: org.apache.thrift.implementation; -*-

(in-package :org.apache.thrift.implementation)

;;; This file defines symbols construction operators for the `org.apache.thrift` library.
;;;
;;; copyright 2010 [james anderson](james.anderson@setf.de)
;;;
;;; Licensed to the Apache Software Foundation (ASF) under one
;;; or more contributor license agreements. See the NOTICE file
;;; distributed with this work for additional information
;;; regarding copyright ownership. The ASF licenses this file
;;; to you under the Apache License, Version 2.0 (the
;;; "License"); you may not use this file except in compliance
;;; with the License. You may obtain a copy of the License at
;;; 
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing,
;;; software distributed under the License is distributed on an
;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;; KIND, either express or implied. See the License for the
;;; specific language governing permissions and limitations
;;; under the License.

;;; The IDL translator emits definition forms which retain the original identifer
;;; strings. These operators perform symbol name canonicalization, and symbol construction.
;;; They are used at compile-time by the IDL macros to construct symbols for classes, fields,
;;; and methods. Cross-references between namespaces are implemented as prefixed identifiers.
;;; The resective operators cache the original identifiers in metaobjects for use at run-time
;;; to decode/encode messages.


(eval-when (:compile-toplevel :load-toplevel :execute)          ; for batch compilation

  (defun implementation-package ()
    (let ((package (concatenate 'string (package-name *package*) (string :-implementation))))
      (or (find-package package)
          (make-package package :use nil))))
  
  (defun response-package ()
    (let ((package (concatenate 'string (package-name *package*) (string :-response))))
      (or (find-package package)
          (make-package package :use nil))))

  (defun canonicalize-name (string)
    "Replace a camel-case pattern with lower case and '-' separation."
    (let ((result (make-array (length string) :element-type 'character :fill-pointer 0 :adjustable t))
          (case :upper))
      (loop for c across string
            do (ecase case
                 (:lower (cond ((upper-case-p c)
                                (setf case :upper)
                                (vector-push-extend #\- result)
                                (vector-push-extend (char-downcase c) result))
                               ((eql c #\_)
                                (vector-push-extend #\- result))
                               (t
                                (vector-push-extend c result))))
                 (:upper (cond ((upper-case-p c)
                                (vector-push-extend (char-downcase c) result))
                               ((eql c #\_)
                                (vector-push-extend #\- result))
                               (t
                                (setf case :lower)
                                (vector-push-extend c result))))))
      (subseq result 0)))
  
  (defun cons-symbol (package &rest args)
    "Construct a symbol given string designators. If package is null, the symbol is
 a new, uninterned symbol."
    (declare (dynamic-extent args))
    
    (flet ((element-length (element)
             (if element (length (string element)) 0)))
      (declare (dynamic-extent #'element-length))
      (setf args (mapcar #'(lambda (elt)
                             (etypecase elt
                               (null nil)       ; ignored
                               (symbol (symbol-name elt))       ; use literal name
                               (string (canonicalize-name elt))))       ; canonicalize strings
                         args))
      (let* ((length (reduce #'+ args :key #'element-length :initial-value 0))
             (name (make-string length))
             (position 0))
        (declare (dynamic-extent name))
        (dolist (el args)
          (when el
            (replace name  el :start1 position)
            (incf position (length el))))
        (ecase (readtable-case *readtable*)
          (:upcase (map-into name #'char-upcase name))
          (:downcase (map-into name #'char-downcase name))
          (:preserve )
          (:invert (flet ((char-invert (c)
                            (cond ((upper-case-p c) (char-downcase c))
                                  ((lower-case-p c) (char-upcase c))
                                  (t c))))
                     (declare (dynamic-extent #'char-invert))
                     (map-into name #'char-invert name))))
        (if package
          (or (find-symbol name package)
              (intern (copy-seq name) package))
          (make-symbol (copy-seq name))))))
  
  (defun str-sym (&rest strs)
    "Given a sequence of symbol name consititents, construct a symbol observing current
 reader case settings. By default intern the symbol in the current *package*.
 Iff the first constituent includes a ':' use that as the symbol prefix."
    (declare (dynamic-extent strs))
    (when strs                            ; if none are given, return nil
      (if (and (symbolp (first strs)) (null (rest strs)))
        (first strs)
        (let* ((first (pop strs))
               (colon (position #\: first)))
          (if colon
            ;; extract the package prefix from the first constituent
            ;; pass it as a constructed symbol to observe current read case rules
            (apply #'cons-symbol (cons-symbol :keyword (subseq first 0 colon))
                   (subseq first (1+ colon)) strs)
            (apply #'cons-symbol *package* first strs))))))
  
  ;;; (assert (equal (list (str-sym "keyword:a") (str-sym "keyword:" "a") (str-sym "a" "sdf")) '(:a :a thrift-generated::asdf)))
  
  (defun implementation-str-sym (&rest identifiers)
    (let* ((*package* (implementation-package))
           (sym (apply #'str-sym identifiers)))
      (export sym *package*)
      sym))

  (defun response-str-sym (&rest identifiers)
    (let* ((*package* (response-package))
           (sym (apply #'str-sym identifiers)))
      (export sym *package*)
      sym))
  
  (defun strs-syms (strs &key (key #'identity))
    (mapcar #'str-sym (mapcar key strs)))

  (defmacro with-gensyms (syms &body b)
    `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(string s)))) syms)
       ,@b))

  (defmacro with-optional-gensyms (symbol-list env form)
    "for any symbol in the list, iff it is bound to a for or to a symbol-macro, bind a gensym for at and
     effect the substitution in the body"
    `(let ((rebindings ()))
       ,@(loop for sym in symbol-list
               collect `(unless (and (symbolp ,sym) (eq (macroexpand-1 ,sym ,env) ,sym))
                          (push (list (if (symbolp ,sym) (gensym (string ,sym)) (gensym)) ,sym) rebindings)))
       (let ((form ,form))
         (if rebindings
           (let ((rewritten-form (loop for (gensym original) in rebindings
                                  do (setf form (subst gensym original form))
                                  finally (return form))))
             (case (first rewritten-form)
               (progn (list* 'let* rebindings (cdr rewritten-form)))
               ;; presumes all the let cases are ok as let* as well
               ((let let*) (list* 'let* (append rebindings (second rewritten-form)) (cddr rewritten-form)))
               (t (list 'let rebindings rewritten-form))))
           form))))

  (defun str (&rest args)
    (declare (dynamic-extent args))
    (apply #'concatenate 'string args))
  )

;;; -*- mode: Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :fiasco)

(defun extract-assert-expression-and-message (input-form)
  (let* ((negatedp nil)
         (predicate)
         (arguments '()))
    (labels ((process (form)
               (if (consp form)
                   (case (first form)
                     ((not)
                      (assert (= (length form) 2))
                      (setf negatedp (not negatedp))
                      (process (second form)))
                     (t (setf predicate (first form))
                        (setf arguments (rest form))))
                   (setf predicate form))))
      (process input-form)
      (cond ((ignore-errors
               (macro-function predicate))
             (values '() input-form "Macro expression ~S evaluated to false."
                     (list `(quote ,input-form))))
            ((and (ignore-errors
                    (fdefinition predicate))
                  ;; let's just skip CL:IF and don't change its evaluation
                  ;; semantics while trying to be more informative...
                  (not (eq predicate 'if)))
             (cond ((= (length arguments) 0)
                    (values '()
                            input-form
                            "Expression ~A evaluated to false."
                            (list `(quote ,input-form))))
                   ((= (length arguments) 2)
                    (with-unique-names (x y)
                      (values `((,x ,(first arguments))
                                (,y ,(second arguments)))
                              (if negatedp
                                  `(not (,predicate ,x ,y))
                                  `(,predicate ,x ,y))
                              "Binary predicate ~A failed.~%~
                               x: ~S => ~S~%~
                               y: ~S => ~S"
                              (list (if negatedp
                                        `(quote (not (,predicate x y)))
                                        `(quote (,predicate x y)))
                                    `(quote ,(first arguments)) x
                                    `(quote ,(second arguments)) y))))
                   (t (let* ((arg-values (mapcar (lambda (el)
                                                   (unless (keywordp el)
                                                     (gensym)))
                                                 arguments))
                             (bindings (loop
                                         :for arg :in arguments
                                         :for arg-value :in arg-values
                                         :when arg-value
                                           :collect `(,arg-value ,arg)))
                             (expression-values
                               (mapcar (lambda (arg-value argument)
                                         (or arg-value argument))
                                       arg-values
                                       arguments))
                             (expression
                               (if negatedp
                                   `(not (,predicate ,@expression-values))
                                   `(,predicate ,@expression-values))))
                        (loop
                          :with message = "Expression ~A evaluated to ~A"
                          :for arg :in arguments
                          :for idx :upfrom 0
                          :for arg-value :in arg-values
                          :when arg-value
                            :do (setf message (concatenate
                                               'string message
                                               "~%~D: ~A => ~S"))
                            :and :append `(,idx (quote ,arg) ,arg-value)
                                   :into message-args
                          :finally
                             (return
                               (values bindings
                                       expression
                                       message
                                       (nconc
                                        (list `(quote (,predicate ,@arguments))
                                              (if negatedp "true" "false"))
                                        message-args))))))))
            (t
             (values '() input-form "Expression ~A evaluated to false."
                     (list `(quote ,input-form))))))))


(defvar *progress-char-count* 0)

(defun write-progress-char (char)
  (when *print-test-run-progress*
      (when (and (not (zerop *progress-char-count*))
                 (zerop (mod *progress-char-count*
                             *test-progress-print-right-margin*)))
        (terpri *debug-io*))
      (incf *progress-char-count*)
      (write-char char *debug-io*)))

(defun register-assertion-was-successful ()
  (write-progress-char #\.))

(defun record-failure (condition-type &rest args)
  (assert (subtypep condition-type 'failure))
  (let ((failure (apply #'make-condition condition-type args)))
    ;; Remember that FIASCO:IS might be called in any context
    ;; and so *CONTEXT* might be nil.
    ;;
    (when *context*
      (push failure (slot-value *context* 'self-failures)))
    (write-progress-char (progress-char-of failure))
    (unless (eq condition-type 'unexpected-error)
      (restart-case
       (error failure)
       (continue ()
                 :report (lambda (stream)
                           (if *context*
                               (format stream "~@<Roger, go on testing...~@:>")                               
                               (format stream "~@<Ignore the failure and continue~@:>"))))))))

(defmacro is (&whole whole form
              &optional (message nil message-p) &rest message-args)
  (multiple-value-bind (bindings expression
                        expression-message
                        expression-message-args)
      (extract-assert-expression-and-message form)
    (with-unique-names (result format-control format-arguments)
      `(progn
         (warn 'is-assertion :form ',form :message ,message :message-args ',message-args)
         (let* (,@bindings
                (,result (multiple-value-list ,expression)))
           (multiple-value-bind (,format-control ,format-arguments)
               (if (and ,message-p *always-show-failed-sexp*)
                   (values (format nil "~A~%~%~A" ,message ,expression-message)
                           (list ,@message-args ,@expression-message-args))
                   ,(if message-p
                        `(values ,message (list ,@message-args))
                        `(values ,expression-message
                                 (list ,@expression-message-args))))

             (if (first ,result)
                 (register-assertion-was-successful)
                 (record-failure 'failed-assertion
                                 :form ',whole
                                 :format-control ,format-control
                                 :format-arguments ,format-arguments)))
           (values-list ,result))))))

(defmacro signals (&whole whole what &body body)
  (declare (ignore whole))
  (let* ((condition-type what))
    (unless (symbolp condition-type)
      (error "SIGNALS expects a symbol as condition-type! (Is ~
there a superfulous quote at ~S?)" condition-type))
    `(progn
      (warn 'signals-assertion :expected-condition-type ',what)
      (block test-block
        (handler-bind ((,condition-type
                        (lambda (c)
                          (register-assertion-was-successful)
                          (return-from test-block c))))
          ,@body)
        (record-failure 'missing-condition
                        :expected-condition-type 'what)
        (values)))))

(defmacro not-signals (&whole whole what &body body)
  (declare (ignore whole))
  (let* ((condition-type what))
    (unless (symbolp condition-type)
      (error "SIGNALS expects a symbol as condition-type! (Is ~
there a superfulous quote at ~S?)" condition-type))
    `(progn
       (warn 'not-signals-assertion :expected-condition-type ',what)
       (block test-block
         (multiple-value-prog1
             (handler-bind ((,condition-type
                             (lambda (c)
                               (record-failure 'unwanted-condition
                                               :expected-condition-type ',what
                                               :observed-condition c)
                               (return-from test-block c))))
               ,@body)
           (register-assertion-was-successful))))))

(defmacro finishes (&whole whole_ &body body)
  ;; could be `(not-signals t ,@body), but that would register a
  ;; confusing failed-assertion
  (with-unique-names (success? whole ;; context
                               )
    `(let* ((,success? nil)
            (,whole ',whole_)
            ;; (,context *context*)
            )
       (warn 'finishes-assertion)
       (unwind-protect
            (multiple-value-prog1
                (progn
                  ,@body)
              (setf ,success? t)
              (register-assertion-was-successful))
         (unless ,success?
           ;; TODO painfully broken: when we don't finish due to a restart, then
           ;; we don't want this here to be triggered...
           ;;
           (record-failure 'failed-assertion
                           :form ,whole
                           :format-control "FINISHES block did not finish: ~S"
                           :format-arguments ,whole))))))



;; Local Variables:
;; coding: utf-8-unix
;; End:

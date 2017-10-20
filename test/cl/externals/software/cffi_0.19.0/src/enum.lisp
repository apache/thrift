;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enum.lisp --- Defining foreign constants as Lisp keywords.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package #:cffi)

;; TODO the accessors names are rather inconsistent:
;; FOREIGN-ENUM-VALUE           FOREIGN-BITFIELD-VALUE
;; FOREIGN-ENUM-KEYWORD         FOREIGN-BITFIELD-SYMBOLS
;; FOREIGN-ENUM-KEYWORD-LIST    FOREIGN-BITFIELD-SYMBOL-LIST
;; I'd rename them to: FOREIGN-*-KEY(S) and FOREIGN-*-ALL-KEYS -- attila

;; TODO bitfield is a confusing name, because the C standard calls
;; the "int foo : 3" type as a bitfield. Maybe rename to defbitmask?
;; -- attila

;;;# Foreign Constants as Lisp Keywords
;;;
;;; This module defines the DEFCENUM macro, which provides an
;;; interface for defining a type and associating a set of integer
;;; constants with keyword symbols for that type.
;;;
;;; The keywords are automatically translated to the appropriate
;;; constant for the type by a type translator when passed as
;;; arguments or a return value to a foreign function.

(defclass foreign-enum (named-foreign-type enhanced-foreign-type)
  ((keyword-values
    :initform (error "Must specify KEYWORD-VALUES.")
    :initarg :keyword-values
    :reader keyword-values)
   (value-keywords
    :initform (error "Must specify VALUE-KEYWORDS.")
    :initarg :value-keywords
    :reader value-keywords))
  (:documentation "Describes a foreign enumerated type."))

(deftype enum-key ()
  '(and symbol (not null)))

(defparameter +valid-enum-base-types+ *built-in-integer-types*)

(defun parse-foreign-enum-like (type-name base-type values
                                &optional field-mode-p)
  (let ((keyword-values (make-hash-table :test 'eq))
        (value-keywords (make-hash-table))
        (field-keywords (list))
        (bit-index->keyword (make-array 0 :adjustable t
                                        :element-type t))
        (default-value (if field-mode-p 1 0))
        (most-extreme-value 0)
        (has-negative-value? nil))
    (dolist (pair values)
      (destructuring-bind (keyword &optional (value default-value valuep))
          (ensure-list pair)
        (check-type keyword enum-key)
        ;;(check-type value integer)
        (when (> (abs value) (abs most-extreme-value))
          (setf most-extreme-value value))
        (when (minusp value)
          (setf has-negative-value? t))
        (if field-mode-p
            (if valuep
                (when (and (>= value default-value)
                           (single-bit-p value))
                  (setf default-value (ash value 1)))
                (setf default-value (ash default-value 1)))
            (setf default-value (1+ value)))
        (if (gethash keyword keyword-values)
            (error "A foreign enum cannot contain duplicate keywords: ~S."
                   keyword)
            (setf (gethash keyword keyword-values) value))
        ;; This is completely arbitrary behaviour: we keep the last
        ;; value->keyword mapping. I suppose the opposite would be
        ;; just as good (keeping the first). Returning a list with all
        ;; the keywords might be a solution too? Suggestions
        ;; welcome. --luis
        (setf (gethash value value-keywords) keyword)
        (when (and field-mode-p
                   (single-bit-p value))
          (let ((bit-index (1- (integer-length value))))
            (push keyword field-keywords)
            (when (<= (array-dimension bit-index->keyword 0)
                      bit-index)
              (setf bit-index->keyword
                    (adjust-array bit-index->keyword (1+ bit-index)
                                  :initial-element nil)))
            (setf (aref bit-index->keyword bit-index)
                  keyword)))))
    (if base-type
        (progn
          (setf base-type (canonicalize-foreign-type base-type))
          ;; I guess we don't lose much by not strictly adhering to
          ;; the C standard here, and some libs out in the wild are
          ;; already using e.g. :double.
          #+nil
          (assert (member base-type +valid-enum-base-types+ :test 'eq) ()
                  "Invalid base type ~S for enum type ~S. Must be one of ~S."
                  base-type type-name +valid-enum-base-types+))
        ;; details: https://stackoverflow.com/questions/1122096/what-is-the-underlying-type-of-a-c-enum
        (let ((bits (integer-length most-extreme-value)))
          (setf base-type
                (let ((most-uint-bits      (load-time-value (* (foreign-type-size :unsigned-int) 8)))
                      (most-ulong-bits     (load-time-value (* (foreign-type-size :unsigned-long) 8)))
                      (most-ulonglong-bits (load-time-value (* (foreign-type-size :unsigned-long-long) 8))))
                  (or (if has-negative-value?
                          (cond
                            ((<= (1+ bits) most-uint-bits)
                             :int)
                            ((<= (1+ bits) most-ulong-bits)
                             :long)
                            ((<= (1+ bits) most-ulonglong-bits)
                             :long-long))
                          (cond
                            ((<= bits most-uint-bits)
                             :unsigned-int)
                            ((<= bits most-ulong-bits)
                             :unsigned-long)
                            ((<= bits most-ulonglong-bits)
                             :unsigned-long-long)))
                      (error "Enum value ~S of enum ~S is too large to store."
                             most-extreme-value type-name))))))
    (values base-type keyword-values value-keywords
            field-keywords (when field-mode-p
                             (alexandria:copy-array
                              bit-index->keyword :adjustable nil
                              :fill-pointer nil)))))

(defun make-foreign-enum (type-name base-type values)
  "Makes a new instance of the foreign-enum class."
  (multiple-value-bind
        (base-type keyword-values value-keywords)
      (parse-foreign-enum-like type-name base-type values)
    (make-instance 'foreign-enum
                   :name type-name
                   :actual-type (parse-type base-type)
                   :keyword-values keyword-values
                   :value-keywords value-keywords)))

(defun %defcenum-like (name-and-options enum-list type-factory)
  (discard-docstring enum-list)
  (destructuring-bind (name &optional base-type)
      (ensure-list name-and-options)
    (let ((type (funcall type-factory name base-type enum-list)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (notice-foreign-type ',name
                              ;; ,type is not enough here, someone needs to
                              ;; define it when we're being loaded from a fasl.
                              (,type-factory ',name ',base-type ',enum-list))
         ,@(remove nil
                   (mapcar (lambda (key)
                             (unless (keywordp key)
                               `(defconstant ,key ,(foreign-enum-value type key))))
                           (foreign-enum-keyword-list type)))))))

(defmacro defcenum (name-and-options &body enum-list)
  "Define an foreign enumerated type."
  (%defcenum-like name-and-options enum-list 'make-foreign-enum))

(defun hash-keys-to-list (ht)
  (loop for k being the hash-keys in ht collect k))

(defun foreign-enum-keyword-list (enum-type)
  "Return a list of KEYWORDS defined in ENUM-TYPE."
  (hash-keys-to-list (keyword-values (ensure-parsed-base-type enum-type))))

;;; These [four] functions could be good canditates for compiler macros
;;; when the value or keyword is constant.  I am not going to bother
;;; until someone has a serious performance need to do so though. --jamesjb
(defun %foreign-enum-value (type keyword &key errorp)
  (check-type keyword enum-key)
  (or (gethash keyword (keyword-values type))
      (when errorp
        (error "~S is not defined as a keyword for enum type ~S."
               keyword type))))

(defun foreign-enum-value (type keyword &key (errorp t))
  "Convert a KEYWORD into an integer according to the enum TYPE."
  (let ((type-obj (ensure-parsed-base-type type)))
    (if (not (typep type-obj 'foreign-enum))
      (error "~S is not a foreign enum type." type)
      (%foreign-enum-value type-obj keyword :errorp errorp))))

(defun %foreign-enum-keyword (type value &key errorp)
  (check-type value integer)
  (or (gethash value (value-keywords type))
      (when errorp
        (error "~S is not defined as a value for enum type ~S."
               value type))))

(defun foreign-enum-keyword (type value &key (errorp t))
  "Convert an integer VALUE into a keyword according to the enum TYPE."
  (let ((type-obj (ensure-parsed-base-type type)))
    (if (not (typep type-obj 'foreign-enum))
        (error "~S is not a foreign enum type." type)
        (%foreign-enum-keyword type-obj value :errorp errorp))))

(defmethod translate-to-foreign (value (type foreign-enum))
  (if (keywordp value)
      (%foreign-enum-value type value :errorp t)
      value))

(defmethod translate-into-foreign-memory
    (value (type foreign-enum) pointer)
  (setf (mem-aref pointer (unparse-type (actual-type type)))
        (translate-to-foreign value type)))

(defmethod translate-from-foreign (value (type foreign-enum))
  (%foreign-enum-keyword type value :errorp t))

(defmethod expand-to-foreign (value (type foreign-enum))
  (once-only (value)
    `(if (keywordp ,value)
         (%foreign-enum-value ,type ,value :errorp t)
         ,value)))

;;; There are two expansions necessary for an enum: first, the enum
;;; keyword needs to be translated to an int, and then the int needs
;;; to be made indirect.
(defmethod expand-to-foreign-dyn-indirect (value var body (type foreign-enum))
  (expand-to-foreign-dyn-indirect       ; Make the integer indirect
   (with-unique-names (feint)
     (call-next-method value feint (list feint) type)) ; TRANSLATABLE-FOREIGN-TYPE method
   var
   body
   (actual-type type)))

;;;# Foreign Bitfields as Lisp keywords
;;;
;;; DEFBITFIELD is an abstraction similar to the one provided by DEFCENUM.
;;; With some changes to DEFCENUM, this could certainly be implemented on
;;; top of it.

(defclass foreign-bitfield (foreign-enum)
  ((field-keywords
    :initform (error "Must specify FIELD-KEYWORDS.")
    :initarg :field-keywords
    :reader field-keywords)
   (bit-index->keyword
    :initform (error "Must specify BIT-INDEX->KEYWORD")
    :initarg :bit-index->keyword
    :reader bit-index->keyword))
  (:documentation "Describes a foreign bitfield type."))

(defun make-foreign-bitfield (type-name base-type values)
  "Makes a new instance of the foreign-bitfield class."
  (multiple-value-bind
        (base-type keyword-values value-keywords
                   field-keywords bit-index->keyword)
      (parse-foreign-enum-like type-name base-type values t)
    (make-instance 'foreign-bitfield
                   :name type-name
                   :actual-type (parse-type base-type)
                   :keyword-values keyword-values
                   :value-keywords value-keywords
                   :field-keywords field-keywords
                   :bit-index->keyword bit-index->keyword)))

(defmacro defbitfield (name-and-options &body masks)
  "Define an foreign enumerated type."
  (%defcenum-like name-and-options masks 'make-foreign-bitfield))

(defun foreign-bitfield-symbol-list (bitfield-type)
  "Return a list of SYMBOLS defined in BITFIELD-TYPE."
  (field-keywords (ensure-parsed-base-type bitfield-type)))

(defun %foreign-bitfield-value (type symbols)
  (declare (optimize speed))
  (labels ((process-one (symbol)
             (check-type symbol symbol)
             (or (gethash symbol (keyword-values type))
                 (error "~S is not a valid symbol for bitfield type ~S."
                        symbol type))))
    (declare (dynamic-extent #'process-one))
    (cond
      ((consp symbols)
       (reduce #'logior symbols :key #'process-one))
      ((null symbols)
       0)
      (t
       (process-one symbols)))))

(defun foreign-bitfield-value (type symbols)
  "Convert a list of symbols into an integer according to the TYPE bitfield."
  (let ((type-obj (ensure-parsed-base-type type)))
    (assert (typep type-obj 'foreign-bitfield) ()
            "~S is not a foreign bitfield type." type)
    (%foreign-bitfield-value type-obj symbols)))

(define-compiler-macro foreign-bitfield-value (&whole form type symbols)
  "Optimize for when TYPE and SYMBOLS are constant."
  (declare (notinline foreign-bitfield-value))
  (if (and (constantp type) (constantp symbols))
      (foreign-bitfield-value (eval type) (eval symbols))
      form))

(defun %foreign-bitfield-symbols (type value)
  (check-type value integer)
  (check-type type foreign-bitfield)
  (loop
    :with bit-index->keyword = (bit-index->keyword type)
    :for bit-index :from 0 :below (array-dimension bit-index->keyword 0)
    :for mask = 1 :then (ash mask 1)
    :for key = (aref bit-index->keyword bit-index)
    :when (and key
               (= (logand value mask) mask))
    :collect key))

(defun foreign-bitfield-symbols (type value)
  "Convert an integer VALUE into a list of matching symbols according to
the bitfield TYPE."
  (let ((type-obj (ensure-parsed-base-type type)))
    (if (not (typep type-obj 'foreign-bitfield))
        (error "~S is not a foreign bitfield type." type)
        (%foreign-bitfield-symbols type-obj value))))

(define-compiler-macro foreign-bitfield-symbols (&whole form type value)
  "Optimize for when TYPE and SYMBOLS are constant."
  (declare (notinline foreign-bitfield-symbols))
  (if (and (constantp type) (constantp value))
      `(quote ,(foreign-bitfield-symbols (eval type) (eval value)))
      form))

(defmethod translate-to-foreign (value (type foreign-bitfield))
  (if (integerp value)
      value
      (%foreign-bitfield-value type (ensure-list value))))

(defmethod translate-from-foreign (value (type foreign-bitfield))
  (%foreign-bitfield-symbols type value))

(defmethod expand-to-foreign (value (type foreign-bitfield))
  (flet ((expander (value type)
           `(if (integerp ,value)
                ,value
                (%foreign-bitfield-value ,type (ensure-list ,value)))))
    (if (constantp value)
        (eval (expander value type))
        (expander value type))))

(defmethod expand-from-foreign (value (type foreign-bitfield))
  (flet ((expander (value type)
           `(%foreign-bitfield-symbols ,type ,value)))
    (if (constantp value)
        (eval (expander value type))
        (expander value type))))

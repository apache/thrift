;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; types.lisp --- User-defined CFFI types.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2005-2007, Luis Oliveira  <loliveira@common-lisp.net>
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

;;;# Built-In Types

;; NOTE: In the C standard there's a "signed-char":
;; https://stackoverflow.com/questions/436513/char-signed-char-char-unsigned-char
;; and "char" may be either signed or unsigned, i.e. treating it as a small int
;; is not wise. At the level of CFFI we can safely ignore this and assume that
;; :char is mapped to "signed-char" by the CL implementation under us.
(define-built-in-foreign-type :char)
(define-built-in-foreign-type :unsigned-char)
(define-built-in-foreign-type :short)
(define-built-in-foreign-type :unsigned-short)
(define-built-in-foreign-type :int)
(define-built-in-foreign-type :unsigned-int)
(define-built-in-foreign-type :long)
(define-built-in-foreign-type :unsigned-long)
(define-built-in-foreign-type :float)
(define-built-in-foreign-type :double)
(define-built-in-foreign-type :void)

#-cffi-sys::no-long-long
(progn
  (define-built-in-foreign-type :long-long)
  (define-built-in-foreign-type :unsigned-long-long))

;;; Define emulated LONG-LONG types.  Needs checking whether we're
;;; using the right sizes on various platforms.
;;;
;;; A possibly better, certainly faster though more intrusive,
;;; alternative is available here:
;;;   <http://article.gmane.org/gmane.lisp.cffi.devel/1091>
#+cffi-sys::no-long-long
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass emulated-llong-type (foreign-type) ())
  (defmethod foreign-type-size ((tp emulated-llong-type)) 8)
  (defmethod foreign-type-alignment ((tp emulated-llong-type))
    ;; better than assuming that the alignment is 8
    (foreign-type-alignment :long))
  (defmethod aggregatep ((tp emulated-llong-type)) nil)

  (define-foreign-type emulated-llong (emulated-llong-type)
    ()
    (:simple-parser :long-long))

  (define-foreign-type emulated-ullong (emulated-llong-type)
    ()
    (:simple-parser :unsigned-long-long))

  (defmethod canonicalize ((tp emulated-llong)) :long-long)
  (defmethod unparse-type ((tp emulated-llong)) :long-long)
  (defmethod canonicalize ((tp emulated-ullong)) :unsigned-long-long)
  (defmethod unparse-type ((tp emulated-ullong)) :unsigned-long-long)

  (defun %emulated-mem-ref-64 (ptr type offset)
    (let ((value #+big-endian
                 (+ (ash (mem-ref ptr :unsigned-long offset) 32)
                    (mem-ref ptr :unsigned-long (+ offset 4)))
                 #+little-endian
                 (+ (mem-ref ptr :unsigned-long offset)
                    (ash (mem-ref ptr :unsigned-long (+ offset 4)) 32))))
      (if (and (eq type :long-long) (logbitp 63 value))
          (lognot (logxor value #xFFFFFFFFFFFFFFFF))
          value)))

  (defun %emulated-mem-set-64 (value ptr type offset)
    (when (and (eq type :long-long) (minusp value))
      (setq value (lognot (logxor value #xFFFFFFFFFFFFFFFF))))
    (%mem-set (ldb (byte 32 0) value) ptr :unsigned-long
              #+big-endian (+ offset 4) #+little-endian offset)
    (%mem-set (ldb (byte 32 32) value) ptr :unsigned-long
              #+big-endian offset #+little-endian (+ offset 4))
    value))

;;; When some lisp other than SCL supports :long-double we should
;;; use #-cffi-sys::no-long-double here instead.
#+(and scl long-float) (define-built-in-foreign-type :long-double)

(defparameter *possible-float-types* '(:float :double :long-double))

(defparameter *other-builtin-types* '(:pointer :void)
  "List of types other than integer or float built in to CFFI.")

(defparameter *built-in-integer-types*
  (set-difference
   cffi:*built-in-foreign-types*
   (append *possible-float-types* *other-builtin-types*))
  "List of integer types supported by CFFI.")

(defparameter *built-in-float-types*
  (set-difference
   cffi:*built-in-foreign-types*
   (append *built-in-integer-types* *other-builtin-types*))
  "List of real float types supported by CFFI.")

;;;# Foreign Pointers

(define-modify-macro incf-pointer (&optional (offset 1)) inc-pointer)

(defun mem-ref (ptr type &optional (offset 0))
  "Return the value of TYPE at OFFSET bytes from PTR. If TYPE is aggregate,
we don't return its 'value' but a pointer to it, which is PTR itself."
  (let* ((parsed-type (parse-type type))
         (ctype (canonicalize parsed-type)))
          #+cffi-sys::no-long-long
          (when (member ctype '(:long-long :unsigned-long-long))
            (return-from mem-ref
              (translate-from-foreign (%emulated-mem-ref-64 ptr ctype offset)
                                      parsed-type)))
          ;; normal branch
    (if (aggregatep parsed-type)
        (if (bare-struct-type-p parsed-type)
            (inc-pointer ptr offset)
            (translate-from-foreign (inc-pointer ptr offset) parsed-type))
        (translate-from-foreign (%mem-ref ptr ctype offset) parsed-type))))

(define-compiler-macro mem-ref (&whole form ptr type &optional (offset 0))
  "Compiler macro to open-code MEM-REF when TYPE is constant."
  (if (constantp type)
      (let* ((parsed-type (parse-type (eval type)))
             (ctype (canonicalize parsed-type)))
        ;; Bail out when using emulated long long types.
        #+cffi-sys::no-long-long
        (when (member ctype '(:long-long :unsigned-long-long))
          (return-from mem-ref form))
        (if (aggregatep parsed-type)
            (if (bare-struct-type-p parsed-type)
                `(inc-pointer ,ptr ,offset)
                (expand-from-foreign `(inc-pointer ,ptr ,offset) parsed-type))
            (expand-from-foreign `(%mem-ref ,ptr ,ctype ,offset) parsed-type)))
      form))

(defun mem-set (value ptr type &optional (offset 0))
  "Set the value of TYPE at OFFSET bytes from PTR to VALUE."
  (let* ((ptype (parse-type type))
         (ctype (canonicalize ptype)))
    #+cffi-sys::no-long-long
    (when (or (eq ctype :long-long) (eq ctype :unsigned-long-long))
      (return-from mem-set
        (%emulated-mem-set-64 (translate-to-foreign value ptype)
                              ptr ctype offset)))
    (if (aggregatep ptype) ; XXX: backwards incompatible?
        (translate-into-foreign-memory value ptype (inc-pointer ptr offset))
        (%mem-set (translate-to-foreign value ptype) ptr ctype offset))))

(define-setf-expander mem-ref (ptr type &optional (offset 0) &environment env)
  "SETF expander for MEM-REF that doesn't rebind TYPE.
This is necessary for the compiler macro on MEM-SET to be able
to open-code (SETF MEM-REF) forms."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion ptr env)
    (declare (ignore setter newval))
    ;; if either TYPE or OFFSET are constant, we avoid rebinding them
    ;; so that the compiler macros on MEM-SET and %MEM-SET work.
    (with-unique-names (store type-tmp offset-tmp)
      (values
       (append (unless (constantp type)   (list type-tmp))
               (unless (constantp offset) (list offset-tmp))
               dummies)
       (append (unless (constantp type)   (list type))
               (unless (constantp offset) (list offset))
               vals)
       (list store)
       `(progn
          (mem-set ,store ,getter
                   ,@(if (constantp type)   (list type)   (list type-tmp))
                   ,@(if (constantp offset) (list offset) (list offset-tmp)))
          ,store)
       `(mem-ref ,getter
                 ,@(if (constantp type)   (list type)   (list type-tmp))
                 ,@(if (constantp offset) (list offset) (list offset-tmp)))))))

(define-compiler-macro mem-set
    (&whole form value ptr type &optional (offset 0))
  "Compiler macro to open-code (SETF MEM-REF) when type is constant."
  (if (constantp type)
      (let* ((parsed-type (parse-type (eval type)))
             (ctype (canonicalize parsed-type)))
        ;; Bail out when using emulated long long types.
        #+cffi-sys::no-long-long
        (when (member ctype '(:long-long :unsigned-long-long))
          (return-from mem-set form))
        (if (aggregatep parsed-type)
            (expand-into-foreign-memory
             value parsed-type `(inc-pointer ,ptr ,offset))
            `(%mem-set ,(expand-to-foreign value parsed-type)
                       ,ptr ,ctype ,offset)))
      form))

;;;# Dereferencing Foreign Arrays

;;; Maybe this should be named MEM-SVREF? [2007-02-28 LO]
(defun mem-aref (ptr type &optional (index 0))
  "Like MEM-REF except for accessing 1d arrays."
  (mem-ref ptr type (* index (foreign-type-size type))))

(define-compiler-macro mem-aref (&whole form ptr type &optional (index 0))
  "Compiler macro to open-code MEM-AREF when TYPE (and eventually INDEX)."
  (if (constantp type)
      (if (constantp index)
          `(mem-ref ,ptr ,type
                    ,(* (eval index) (foreign-type-size (eval type))))
          `(mem-ref ,ptr ,type (* ,index ,(foreign-type-size (eval type)))))
      form))

(define-setf-expander mem-aref (ptr type &optional (index 0) &environment env)
  "SETF expander for MEM-AREF."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion ptr env)
    (declare (ignore setter newval))
    ;; we avoid rebinding type and index, if possible (and if type is not
    ;; constant, we don't bother about the index), so that the compiler macros
    ;; on MEM-SET or %MEM-SET can work.
    (with-unique-names (store type-tmp index-tmp)
      (values
       (append (unless (constantp type)
                 (list type-tmp))
               (unless (and (constantp type) (constantp index))
                 (list index-tmp))
               dummies)
       (append (unless (constantp type)
                 (list type))
               (unless (and (constantp type) (constantp index))
                 (list index))
               vals)
       (list store)
       ;; Here we'll try to calculate the offset from the type and index,
       ;; or if not possible at least get the type size early.
       `(progn
          ,(if (constantp type)
               (if (constantp index)
                   `(mem-set ,store ,getter ,type
                             ,(* (eval index) (foreign-type-size (eval type))))
                   `(mem-set ,store ,getter ,type
                             (* ,index-tmp ,(foreign-type-size (eval type)))))
               `(mem-set ,store ,getter ,type-tmp
                         (* ,index-tmp (foreign-type-size ,type-tmp))))
          ,store)
       `(mem-aref ,getter
                  ,@(if (constantp type)
                        (list type)
                        (list type-tmp))
                  ,@(if (and (constantp type) (constantp index))
                        (list index)
                        (list index-tmp)))))))

(defmethod translate-into-foreign-memory
    (value (type foreign-pointer-type) pointer)
  (setf (mem-aref pointer :pointer) value))

(defmethod translate-into-foreign-memory
    (value (type foreign-built-in-type) pointer)
  (setf (mem-aref pointer (unparse-type type)) value))

(defun mem-aptr (ptr type &optional (index 0))
  "The pointer to the element."
  (inc-pointer ptr (* index (foreign-type-size type))))

(define-compiler-macro mem-aptr (&whole form ptr type &optional (index 0))
  "The pointer to the element."
  (cond ((not (constantp type))
         form)
        ((not (constantp index))
         `(inc-pointer ,ptr (* ,index ,(foreign-type-size (eval type)))))
        ((zerop (eval index))
         ptr)
        (t
         `(inc-pointer ,ptr ,(* (eval index)
                                (foreign-type-size (eval type)))))))

(define-foreign-type foreign-array-type ()
  ((dimensions :reader dimensions :initarg :dimensions)
   (element-type :reader element-type :initarg :element-type))
  (:actual-type :pointer))

(defmethod aggregatep ((type foreign-array-type))
  t)

(defmethod print-object ((type foreign-array-type) stream)
  "Print a FOREIGN-ARRAY-TYPE instance to STREAM unreadably."
  (print-unreadable-object (type stream :type t :identity nil)
    (format stream "~S ~S" (element-type type) (dimensions type))))

(defun array-element-size (array-type)
  (foreign-type-size (element-type array-type)))

(defmethod foreign-type-size ((type foreign-array-type))
  (* (array-element-size type) (reduce #'* (dimensions type))))

(defmethod foreign-type-alignment ((type foreign-array-type))
  (foreign-type-alignment (element-type type)))

(define-parse-method :array (element-type &rest dimensions)
  (assert (plusp (length dimensions)))
  (make-instance 'foreign-array-type
                 :element-type element-type
                 :dimensions dimensions))

(defun indexes-to-row-major-index (dimensions &rest subscripts)
  (apply #'+ (maplist (lambda (x y)
                        (* (car x) (apply #'* (cdr y))))
                      subscripts
                      dimensions)))

(defun row-major-index-to-indexes (index dimensions)
  (loop with idx = index
        with rank = (length dimensions)
        with indexes = (make-list rank)
        for dim-index from (- rank 1) downto 0 do
        (setf (values idx (nth dim-index indexes))
              (floor idx (nth dim-index dimensions)))
        finally (return indexes)))

(defun foreign-alloc (type &key (initial-element nil initial-element-p)
                      (initial-contents nil initial-contents-p)
                      (count 1 count-p) null-terminated-p)
  "Allocate enough memory to hold COUNT objects of type TYPE. If
INITIAL-ELEMENT is supplied, each element of the newly allocated
memory is initialized with its value. If INITIAL-CONTENTS is supplied,
each of its elements will be used to initialize the contents of the
newly allocated memory."
  (let (contents-length)
    ;; Some error checking, etc...
    (when (and null-terminated-p
               (not (eq (canonicalize-foreign-type type) :pointer)))
      (error "Cannot use :NULL-TERMINATED-P with non-pointer types."))
    (when (and initial-element-p initial-contents-p)
      (error "Cannot specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
    (when initial-contents-p
      (setq contents-length (length initial-contents))
      (if count-p
          (assert (>= count contents-length))
          (setq count contents-length)))
    ;; Everything looks good.
    (let ((ptr (%foreign-alloc (* (foreign-type-size type)
                                  (if null-terminated-p (1+ count) count)))))
      (when initial-element-p
        (dotimes (i count)
          (setf (mem-aref ptr type i) initial-element)))
      (when initial-contents-p
        (dotimes (i contents-length)
          (setf (mem-aref ptr type i) (elt initial-contents i))))
      (when null-terminated-p
        (setf (mem-aref ptr :pointer count) (null-pointer)))
      ptr)))

;;; Simple compiler macro that kicks in when TYPE is constant and only
;;; the COUNT argument is passed.  (Note: hard-coding the type's size
;;; into the fasl will likely break CLISP fasl cross-platform
;;; compatibilty.)
(define-compiler-macro foreign-alloc (&whole form type &rest args
                                      &key (count 1 count-p) &allow-other-keys)
  (if (or (and count-p (<= (length args) 2)) (null args))
      (cond
        ((and (constantp type) (constantp count))
         `(%foreign-alloc ,(* (eval count) (foreign-type-size (eval type)))))
        ((constantp type)
         `(%foreign-alloc (* ,count ,(foreign-type-size (eval type)))))
        (t form))
      form))

(defun lisp-array-to-foreign (array pointer array-type)
  "Copy elements from a Lisp array to POINTER."
  (let* ((type (ensure-parsed-base-type array-type))
         (el-type (element-type type))
         (dimensions (dimensions type)))
    (loop with foreign-type-size = (array-element-size type)
          with size = (reduce #'* dimensions)
          for i from 0 below size
          for offset = (* i foreign-type-size)
          for element = (apply #'aref array
                               (row-major-index-to-indexes i dimensions))
          do (setf (mem-ref pointer el-type offset) element))))

(defun foreign-array-to-lisp (pointer array-type)
  "Copy elements from ptr into a Lisp array. If POINTER is a null
pointer, returns NIL."
  (unless (null-pointer-p pointer)
    (let* ((type (ensure-parsed-base-type array-type))
           (el-type (element-type type))
           (dimensions (dimensions type))
           (array (make-array dimensions)))
      (loop with foreign-type-size = (array-element-size type)
            with size = (reduce #'* dimensions)
            for i from 0 below size
            for offset = (* i foreign-type-size)
            for element = (mem-ref pointer el-type offset)
            do (setf (apply #'aref array
                            (row-major-index-to-indexes i dimensions))
                     element))
      array)))

(defun foreign-array-alloc (array array-type)
  "Allocate a foreign array containing the elements of lisp array.
The foreign array must be freed with foreign-array-free."
  (check-type array array)
  (let* ((type (ensure-parsed-base-type array-type))
         (ptr (foreign-alloc (element-type type)
                             :count (reduce #'* (dimensions type)))))
    (lisp-array-to-foreign array ptr array-type)
    ptr))

(defun foreign-array-free (ptr)
  "Free a foreign array allocated by foreign-array-alloc."
  (foreign-free ptr))

(defmacro with-foreign-array ((var lisp-array array-type) &body body)
  "Bind var to a foreign array containing lisp-array elements in body."
  (with-unique-names (type)
    `(let ((,type (ensure-parsed-base-type ,array-type)))
       (with-foreign-pointer (,var (* (reduce #'* (dimensions ,type))
                                      (array-element-size ,type)))
         (lisp-array-to-foreign ,lisp-array ,var ,array-type)
         ,@body))))

(defun foreign-aref (ptr array-type &rest indexes)
  (let* ((type (ensure-parsed-base-type array-type))
         (offset (* (array-element-size type)
                    (apply #'indexes-to-row-major-index
                           (dimensions type) indexes))))
    (mem-ref ptr (element-type type) offset)))

(defun (setf foreign-aref) (value ptr array-type &rest indexes)
  (let* ((type (ensure-parsed-base-type array-type))
         (offset (* (array-element-size type)
                    (apply #'indexes-to-row-major-index
                           (dimensions type) indexes))))
    (setf (mem-ref ptr (element-type type) offset) value)))

;;; Automatic translations for the :ARRAY type. Notice that these
;;; translators will also invoke the appropriate translators for for
;;; each of the array's elements since that's the normal behaviour of
;;; the FOREIGN-ARRAY-* operators, but there's a FIXME: **it doesn't
;;; free them yet**

;;; This used to be in a separate type but let's experiment with just
;;; one type for a while. [2008-12-30 LO]

;;; FIXME: those ugly invocations of UNPARSE-TYPE suggest that these
;;; foreign array operators should take the type and dimention
;;; arguments "unboxed". [2008-12-31 LO]

(defmethod translate-to-foreign (array (type foreign-array-type))
  (foreign-array-alloc array (unparse-type type)))

(defmethod translate-aggregate-to-foreign (ptr value (type foreign-array-type))
  (lisp-array-to-foreign value ptr (unparse-type type)))

(defmethod translate-from-foreign (pointer (type foreign-array-type))
  (foreign-array-to-lisp pointer (unparse-type type)))

(defmethod free-translated-object (pointer (type foreign-array-type) param)
  (declare (ignore param))
  (foreign-array-free pointer))

;;;# Foreign Structures

;;;## Foreign Structure Slots

(defgeneric foreign-struct-slot-pointer (ptr slot)
  (:documentation
   "Get the address of SLOT relative to PTR."))

(defgeneric foreign-struct-slot-pointer-form (ptr slot)
  (:documentation
   "Return a form to get the address of SLOT in PTR."))

(defgeneric foreign-struct-slot-value (ptr slot)
  (:documentation
   "Return the value of SLOT in structure PTR."))

(defgeneric (setf foreign-struct-slot-value) (value ptr slot)
  (:documentation
   "Set the value of a SLOT in structure PTR."))

(defgeneric foreign-struct-slot-value-form (ptr slot)
  (:documentation
   "Return a form to get the value of SLOT in struct PTR."))

(defgeneric foreign-struct-slot-set-form (value ptr slot)
  (:documentation
   "Return a form to set the value of SLOT in struct PTR."))

(defclass foreign-struct-slot ()
  ((name   :initarg :name   :reader   slot-name)
   (offset :initarg :offset :accessor slot-offset)
   ;; FIXME: the type should probably be parsed?
   (type   :initarg :type   :accessor slot-type))
  (:documentation "Base class for simple and aggregate slots."))

(defmethod foreign-struct-slot-pointer (ptr (slot foreign-struct-slot))
  "Return the address of SLOT relative to PTR."
  (inc-pointer ptr (slot-offset slot)))

(defmethod foreign-struct-slot-pointer-form (ptr (slot foreign-struct-slot))
  "Return a form to get the address of SLOT relative to PTR."
  (let ((offset (slot-offset slot)))
    (if (zerop offset)
        ptr
        `(inc-pointer ,ptr ,offset))))

(defun foreign-slot-names (type)
  "Returns a list of TYPE's slot names in no particular order."
  (loop for value being the hash-values
        in (slots (ensure-parsed-base-type type))
        collect (slot-name value)))

;;;### Simple Slots

(defclass simple-struct-slot (foreign-struct-slot)
  ()
  (:documentation "Non-aggregate structure slots."))

(defmethod foreign-struct-slot-value (ptr (slot simple-struct-slot))
  "Return the value of a simple SLOT from a struct at PTR."
  (mem-ref ptr (slot-type slot) (slot-offset slot)))

(defmethod foreign-struct-slot-value-form (ptr (slot simple-struct-slot))
  "Return a form to get the value of a slot from PTR."
  `(mem-ref ,ptr ',(slot-type slot) ,(slot-offset slot)))

(defmethod (setf foreign-struct-slot-value) (value ptr (slot simple-struct-slot))
  "Set the value of a simple SLOT to VALUE in PTR."
  (setf (mem-ref ptr (slot-type slot) (slot-offset slot)) value))

(defmethod foreign-struct-slot-set-form (value ptr (slot simple-struct-slot))
  "Return a form to set the value of a simple structure slot."
  `(setf (mem-ref ,ptr ',(slot-type slot) ,(slot-offset slot)) ,value))

;;;### Aggregate Slots

(defclass aggregate-struct-slot (foreign-struct-slot)
  ((count :initarg :count :accessor slot-count))
  (:documentation "Aggregate structure slots."))

;;; Since MEM-REF returns a pointer for struct types we are able to
;;; chain together slot names when accessing slot values in nested
;;; structures.
(defmethod foreign-struct-slot-value (ptr (slot aggregate-struct-slot))
  "Return a pointer to SLOT relative to PTR."
  (convert-from-foreign (inc-pointer ptr (slot-offset slot))
                        (slot-type slot)))

(defmethod foreign-struct-slot-value-form (ptr (slot aggregate-struct-slot))
  "Return a form to get the value of SLOT relative to PTR."
  `(convert-from-foreign (inc-pointer ,ptr ,(slot-offset slot))
                         ',(slot-type slot)))

(defmethod translate-aggregate-to-foreign (ptr value (type foreign-struct-type))
  ;;; FIXME: use the block memory interface instead.
  (loop for i below (foreign-type-size type)
        do (%mem-set (%mem-ref value :char i) ptr :char i)))

(defmethod (setf foreign-struct-slot-value)
    (value ptr (slot aggregate-struct-slot))
  "Set the value of an aggregate SLOT to VALUE in PTR."
  (translate-aggregate-to-foreign (inc-pointer ptr (slot-offset slot))
                                  value
                                  (parse-type (slot-type slot))))

(defmethod foreign-struct-slot-set-form (value ptr (slot aggregate-struct-slot))
  "Return a form to get the value of an aggregate SLOT relative to PTR."
  `(translate-aggregate-to-foreign (inc-pointer ,ptr ,(slot-offset slot))
                                   ,value
                                   ,(parse-type (slot-type slot))))

;;;## Defining Foreign Structures

(defun make-struct-slot (name offset type count)
  "Make the appropriate type of structure slot."
  ;; If TYPE is an aggregate type or COUNT is >1, create an
  ;; AGGREGATE-STRUCT-SLOT, otherwise a SIMPLE-STRUCT-SLOT.
  (if (or (> count 1) (aggregatep (parse-type type)))
      (make-instance 'aggregate-struct-slot :offset offset :type type
                     :name name :count count)
      (make-instance 'simple-struct-slot :offset offset :type type
                     :name name)))

(defun parse-deprecated-struct-type (name struct-or-union)
  (check-type struct-or-union (member :struct :union))
  (let* ((struct-type-name `(,struct-or-union ,name))
         (struct-type (parse-type struct-type-name)))
    (simple-style-warning
     "bare references to struct types are deprecated. ~
      Please use ~S or ~S instead."
     `(:pointer ,struct-type-name) struct-type-name)
    (make-instance (class-of struct-type)
                   :alignment (alignment struct-type)
                   :size (size struct-type)
                   :slots (slots struct-type)
                   :name (name struct-type)
                   :bare t)))

;;; Regarding structure alignment, the following ABIs were checked:
;;;   - System-V ABI: x86, x86-64, ppc, arm, mips and itanium. (more?)
;;;   - Mac OS X ABI Function Call Guide: ppc32, ppc64 and x86.
;;;
;;; Rules used here:
;;;
;;;   1. "An entire structure or union object is aligned on the same
;;;       boundary as its most strictly aligned member."
;;;
;;;   2. "Each member is assigned to the lowest available offset with
;;;       the appropriate alignment. This may require internal
;;;       padding, depending on the previous member."
;;;
;;;   3. "A structure's size is increased, if necessary, to make it a
;;;       multiple of the alignment. This may require tail padding,
;;;       depending on the last member."
;;;
;;; Special cases from darwin/ppc32's ABI:
;;; http://developer.apple.com/documentation/DeveloperTools/Conceptual/LowLevelABI/index.html
;;;
;;;   4. "The embedding alignment of the first element in a data
;;;       structure is equal to the element's natural alignment."
;;;
;;;   5. "For subsequent elements that have a natural alignment
;;;       greater than 4 bytes, the embedding alignment is 4, unless
;;;       the element is a vector."  (note: this applies for
;;;       structures too)

;; FIXME: get a better name for this. --luis
(defun get-alignment (type alignment-type firstp)
  "Return alignment for TYPE according to ALIGNMENT-TYPE."
  (declare (ignorable firstp))
  (ecase alignment-type
    (:normal #-(and darwin ppc)
             (foreign-type-alignment type)
             #+(and darwin ppc)
             (if firstp
                 (foreign-type-alignment type)
                 (min 4 (foreign-type-alignment type))))))

(defun adjust-for-alignment (type offset alignment-type firstp)
  "Return OFFSET aligned properly for TYPE according to ALIGNMENT-TYPE."
  (let* ((align (get-alignment type alignment-type firstp))
         (rem (mod offset align)))
    (if (zerop rem)
        offset
        (+ offset (- align rem)))))

(defmacro with-tentative-type-definition ((name value namespace) &body body)
  (once-only (name namespace)
    `(unwind-protect-case ()
          (progn
            (notice-foreign-type ,name ,value ,namespace)
            ,@body)
       (:abort (undefine-foreign-type ,name ,namespace)))))

(defun notice-foreign-struct-definition (name options slots)
  "Parse and install a foreign structure definition."
  (destructuring-bind (&key size (class 'foreign-struct-type))
      options
    (let ((struct (make-instance class :name name))
          (current-offset 0)
          (max-align 1)
          (firstp t))
      (with-tentative-type-definition (name struct :struct)
        ;; determine offsets
        (dolist (slotdef slots)
          (destructuring-bind (slotname type &key (count 1) offset) slotdef
            (when (eq (canonicalize-foreign-type type) :void)
              (simple-foreign-type-error type :struct
                                         "In struct ~S: void type not allowed in field ~S"
                                         name slotdef))
            (setq current-offset
                  (or offset
                      (adjust-for-alignment type current-offset :normal firstp)))
            (let* ((slot (make-struct-slot slotname current-offset type count))
                   (align (get-alignment (slot-type slot) :normal firstp)))
              (setf (gethash slotname (slots struct)) slot)
              (when (> align max-align)
                (setq max-align align)))
            (incf current-offset (* count (foreign-type-size type))))
          (setq firstp nil))
        ;; calculate padding and alignment
        (setf (alignment struct) max-align) ; See point 1 above.
        (let ((tail-padding (- max-align (rem current-offset max-align))))
          (unless (= tail-padding max-align) ; See point 3 above.
            (incf current-offset tail-padding)))
        (setf (size struct) (or size current-offset))))))

(defun generate-struct-accessors (name conc-name slot-names)
  (loop with pointer-arg = (symbolicate '#:pointer-to- name)
        for slot in slot-names
        for accessor = (symbolicate conc-name slot)
        collect `(defun ,accessor (,pointer-arg)
                   (foreign-slot-value ,pointer-arg '(:struct ,name) ',slot))
        collect `(defun (setf ,accessor) (value ,pointer-arg)
                   (foreign-slot-set value ,pointer-arg '(:struct ,name) ',slot))))

(define-parse-method :struct (name)
  (funcall (find-type-parser name :struct)))

(defvar *defcstruct-hook* nil)

(defmacro defcstruct (name-and-options &body fields)
  "Define the layout of a foreign structure."
  (discard-docstring fields)
  (destructuring-bind (name . options)
      (ensure-list name-and-options)
    (let ((conc-name (getf options :conc-name)))
      (remf options :conc-name)
      (unless (getf options :class) (setf (getf options :class) (symbolicate name '-tclass)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ;; m-f-s-t could do with this with mop:ensure-class.
         ,(when-let (class (getf options :class))
            `(defclass ,class (foreign-struct-type
                               translatable-foreign-type)
               ()))
         (notice-foreign-struct-definition ',name ',options ',fields)
         ,@(when conc-name
             (generate-struct-accessors name conc-name
                                        (mapcar #'car fields)))
         ,@(when *defcstruct-hook*
             ;; If non-nil, *defcstruct-hook* should be a function
             ;; of the arguments that returns NIL or a list of
             ;; forms to include in the expansion.
             (apply *defcstruct-hook* name-and-options fields))
         (define-parse-method ,name ()
           (parse-deprecated-struct-type ',name :struct))
         '(:struct ,name)))))

;;;## Accessing Foreign Structure Slots

(defun get-slot-info (type slot-name)
  "Return the slot info for SLOT-NAME or raise an error."
  (let* ((struct (ensure-parsed-base-type type))
         (info (gethash slot-name (slots struct))))
    (unless info
      (simple-foreign-type-error type :struct
                                 "Undefined slot ~A in foreign type ~A."
                                 slot-name type))
    info))

(defun foreign-slot-pointer (ptr type slot-name)
  "Return the address of SLOT-NAME in the structure at PTR."
  (foreign-struct-slot-pointer ptr (get-slot-info type slot-name)))

(define-compiler-macro foreign-slot-pointer (&whole whole ptr type slot-name)
  (if (and (constantp type) (constantp slot-name))
      (foreign-struct-slot-pointer-form
       ptr (get-slot-info (eval type) (eval slot-name)))
      whole))

(defun foreign-slot-type (type slot-name)
  "Return the type of SLOT in a struct TYPE."
  (slot-type (get-slot-info type slot-name)))

(defun foreign-slot-offset (type slot-name)
  "Return the offset of SLOT in a struct TYPE."
  (slot-offset (get-slot-info type slot-name)))

(defun foreign-slot-count (type slot-name)
  "Return the number of items in SLOT in a struct TYPE."
  (slot-count (get-slot-info type slot-name)))

(defun foreign-slot-value (ptr type slot-name)
  "Return the value of SLOT-NAME in the foreign structure at PTR."
  (foreign-struct-slot-value ptr (get-slot-info type slot-name)))

(define-compiler-macro foreign-slot-value (&whole form ptr type slot-name)
  "Optimizer for FOREIGN-SLOT-VALUE when TYPE is constant."
  (if (and (constantp type) (constantp slot-name))
      (foreign-struct-slot-value-form
       ptr (get-slot-info (eval type) (eval slot-name)))
      form))

(define-setf-expander foreign-slot-value (ptr type slot-name &environment env)
  "SETF expander for FOREIGN-SLOT-VALUE."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion ptr env)
    (declare (ignore setter newval))
    (if (and (constantp type) (constantp slot-name))
        ;; if TYPE and SLOT-NAME are constant we avoid rebinding them
        ;; so that the compiler macro on FOREIGN-SLOT-SET works.
        (with-unique-names (store)
          (values
           dummies
           vals
           (list store)
           `(progn
              (foreign-slot-set ,store ,getter ,type ,slot-name)
              ,store)
           `(foreign-slot-value ,getter ,type ,slot-name)))
        ;; if not...
        (with-unique-names (store slot-name-tmp type-tmp)
          (values
           (list* type-tmp slot-name-tmp dummies)
           (list* type slot-name vals)
           (list store)
           `(progn
              (foreign-slot-set ,store ,getter ,type-tmp ,slot-name-tmp)
              ,store)
           `(foreign-slot-value ,getter ,type-tmp ,slot-name-tmp))))))

(defun foreign-slot-set (value ptr type slot-name)
  "Set the value of SLOT-NAME in a foreign structure."
  (setf (foreign-struct-slot-value ptr (get-slot-info type slot-name)) value))

(define-compiler-macro foreign-slot-set
    (&whole form value ptr type slot-name)
  "Optimizer when TYPE and SLOT-NAME are constant."
  (if (and (constantp type) (constantp slot-name))
      (foreign-struct-slot-set-form
       value ptr (get-slot-info (eval type) (eval slot-name)))
      form))

(defmacro with-foreign-slots ((vars ptr type) &body body)
  "Create local symbol macros for each var in VARS to reference
foreign slots in PTR of TYPE. Similar to WITH-SLOTS.
Each var can be of the form: slot-name - in which case slot-name will
be bound to the value of the slot or: (:pointer slot-name) - in which
case slot-name will be bound to the pointer to that slot."
  (let ((ptr-var (gensym "PTR")))
    `(let ((,ptr-var ,ptr))
       (symbol-macrolet
           ,(loop :for var :in vars
              :collect
              (if (listp var)
                  (if (eq (first var) :pointer)
                      `(,(second var) (foreign-slot-pointer
                                       ,ptr-var ',type ',(second var)))
                      (error
                       "Malformed slot specification ~a; must be:`name' or `(:pointer name)'"
                       var))
                  `(,var (foreign-slot-value ,ptr-var ',type ',var))))
         ,@body))))

;;; We could add an option to define a struct instead of a class, in
;;; the unlikely event someone needs something like that.
(defmacro define-c-struct-wrapper (class-and-type supers &optional slots)
  "Define a new class with CLOS slots matching those of a foreign
struct type.  An INITIALIZE-INSTANCE method is defined which
takes a :POINTER initarg that is used to store the slots of a
foreign object.  This pointer is only used for initialization and
it is not retained.

CLASS-AND-TYPE is either a list of the form (class-name
struct-type) or a single symbol naming both.  The class will
inherit SUPERS.  If a list of SLOTS is specified, only those
slots will be defined and stored."
  (destructuring-bind (class-name &optional (struct-type (list :struct class-name)))
      (ensure-list class-and-type)
    (let ((slots (or slots (foreign-slot-names struct-type))))
      `(progn
         (defclass ,class-name ,supers
           ,(loop for slot in slots collect
                  `(,slot :reader ,(format-symbol t "~A-~A" class-name slot))))
         ;; This could be done in a parent class by using
         ;; FOREIGN-SLOT-NAMES when instantiating but then the compiler
         ;; macros wouldn't kick in.
         (defmethod initialize-instance :after ((inst ,class-name) &key pointer)
           (with-foreign-slots (,slots pointer ,struct-type)
             ,@(loop for slot in slots collect
                     `(setf (slot-value inst ',slot) ,slot))))
         ',class-name))))

;;;# Foreign Unions
;;;
;;; A union is a subclass of FOREIGN-STRUCT-TYPE in which all slots
;;; have an offset of zero.

;;; See also the notes regarding ABI requirements in
;;; NOTICE-FOREIGN-STRUCT-DEFINITION
(defun notice-foreign-union-definition (name-and-options slots)
  "Parse and install a foreign union definition."
  (destructuring-bind (name &key size)
      (ensure-list name-and-options)
    (let ((union (make-instance 'foreign-union-type :name name))
          (max-size 0)
          (max-align 0))
      (with-tentative-type-definition (name union :union)
        (dolist (slotdef slots)
          (destructuring-bind (slotname type &key (count 1)) slotdef
            (when (eq (canonicalize-foreign-type type) :void)
              (simple-foreign-type-error name :struct
                                         "In union ~S: void type not allowed in field ~S"
                                         name slotdef))
            (let* ((slot (make-struct-slot slotname 0 type count))
                   (size (* count (foreign-type-size type)))
                   (align (foreign-type-alignment (slot-type slot))))
              (setf (gethash slotname (slots union)) slot)
              (when (> size max-size)
                (setf max-size size))
              (when (> align max-align)
                (setf max-align align)))))
        (setf (size union) (or size max-size))
        (setf (alignment union) max-align)))))

(define-parse-method :union (name)
  (funcall (find-type-parser name :union)))

(defmacro defcunion (name-and-options &body fields)
  "Define the layout of a foreign union."
  (discard-docstring fields)
  (destructuring-bind (name &key size)
      (ensure-list name-and-options)
    (declare (ignore size))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (notice-foreign-union-definition ',name-and-options ',fields)
       (define-parse-method ,name ()
         (parse-deprecated-struct-type ',name :union))
       '(:union ,name))))

;;;# Operations on Types

(defmethod foreign-type-alignment (type)
  "Return the alignment in bytes of a foreign type."
  (foreign-type-alignment (parse-type type)))

(defmacro with-foreign-object ((var type &optional (count 1)) &body body)
  "Bind VAR to a pointer to COUNT objects of TYPE during BODY.
The buffer has dynamic extent and may be stack allocated."
  `(with-foreign-pointer
       (,var ,(if (constantp type)
                  ;; with-foreign-pointer may benefit from constant folding:
                  (if (constantp count)
                      (* (eval count) (foreign-type-size (eval type)))
                      `(* ,count ,(foreign-type-size (eval type))))
                  `(* ,count (foreign-type-size ,type))))
     ,@body))

(defmacro with-foreign-objects (bindings &body body)
  (if bindings
      `(with-foreign-object ,(car bindings)
         (with-foreign-objects ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

;;;## Anonymous Type Translators
;;;
;;; (:wrapper :to-c some-function :from-c another-function)
;;;
;;; TODO: We will need to add a FREE function to this as well I think.
;;; --james

(define-foreign-type foreign-type-wrapper ()
  ((to-c   :initarg :to-c   :reader wrapper-to-c)
   (from-c :initarg :from-c :reader wrapper-from-c))
  (:documentation "Wrapper type."))

(define-parse-method :wrapper (base-type &key to-c from-c)
  (make-instance 'foreign-type-wrapper
                 :actual-type (parse-type base-type)
                 :to-c (or to-c 'identity)
                 :from-c (or from-c 'identity)))

(defmethod translate-to-foreign (value (type foreign-type-wrapper))
  (translate-to-foreign
   (funcall (slot-value type 'to-c) value) (actual-type type)))

(defmethod translate-from-foreign (value (type foreign-type-wrapper))
  (funcall (slot-value type 'from-c)
           (translate-from-foreign value (actual-type type))))

;;;# Other types

;;; Boolean type. Maps to an :int by default. Only accepts integer types.
(define-foreign-type foreign-boolean-type ()
  ())

(define-parse-method :boolean (&optional (base-type :int))
  (make-instance
   'foreign-boolean-type :actual-type
   (ecase (canonicalize-foreign-type base-type)
     ((:char :unsigned-char :int :unsigned-int :long :unsigned-long
       #-cffi-sys::no-long-long :long-long
       #-cffi-sys::no-long-long :unsigned-long-long) base-type))))

(defmethod translate-to-foreign (value (type foreign-boolean-type))
  (if value 1 0))

(defmethod translate-from-foreign (value (type foreign-boolean-type))
  (not (zerop value)))

(defmethod expand-to-foreign (value (type foreign-boolean-type))
  "Optimization for the :boolean type."
  (if (constantp value)
      (if (eval value) 1 0)
      `(if ,value 1 0)))

(defmethod expand-from-foreign (value (type foreign-boolean-type))
  "Optimization for the :boolean type."
  (if (constantp value) ; very unlikely, heh
      (not (zerop (eval value)))
      `(not (zerop ,value))))

;;; Boolean type that represents C99 _Bool
(defctype :bool (:boolean :char))

;;;# Typedefs for built-in types.

(defctype :uchar  :unsigned-char)
(defctype :ushort :unsigned-short)
(defctype :uint   :unsigned-int)
(defctype :ulong  :unsigned-long)
(defctype :llong  :long-long)
(defctype :ullong :unsigned-long-long)

;;; We try to define the :[u]int{8,16,32,64} types by looking at
;;; the sizes of the built-in integer types and defining typedefs.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet
      ((match-types (sized-types mtypes)
         `(progn
            ,@(loop for (type . size-or-type) in sized-types
                    for m = (car (member (if (keywordp size-or-type)
                                             (foreign-type-size size-or-type)
                                             size-or-type)
                                         mtypes :key #'foreign-type-size))
                    when m collect `(defctype ,type ,m)))))
    ;; signed
    (match-types ((:int8 . 1) (:int16 . 2) (:int32 . 4) (:int64 . 8)
                  (:intptr . :pointer))
                 (:char :short :int :long :long-long))
    ;; unsigned
    (match-types ((:uint8 . 1) (:uint16 . 2) (:uint32 . 4) (:uint64 . 8)
                  (:uintptr . :pointer))
                 (:unsigned-char :unsigned-short :unsigned-int :unsigned-long
                  :unsigned-long-long))))

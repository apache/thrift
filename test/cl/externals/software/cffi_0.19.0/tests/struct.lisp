;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; struct.lisp --- Foreign structure type tests.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2005-2011, Luis Oliveira  <loliveira@common-lisp.net>
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

(in-package #:cffi-tests)

(defcstruct timeval
  (tv-secs :long)
  (tv-usecs :long))

(defparameter *timeval-size* (* 2 (max (foreign-type-size :long)
                                       (foreign-type-alignment :long))))

;;;# Basic Structure Tests

(deftest struct.1
    (- (foreign-type-size 'timeval) *timeval-size*)
  0)

(deftest struct.2
    (with-foreign-object (tv 'timeval)
      (setf (foreign-slot-value tv 'timeval 'tv-secs) 0)
      (setf (foreign-slot-value tv 'timeval 'tv-usecs) 1)
      (values (foreign-slot-value tv 'timeval 'tv-secs)
              (foreign-slot-value tv 'timeval 'tv-usecs)))
  0 1)

(deftest struct.3
    (with-foreign-object (tv 'timeval)
      (with-foreign-slots ((tv-secs tv-usecs) tv timeval)
        (setf tv-secs 100 tv-usecs 200)
        (values tv-secs tv-usecs)))
  100 200)

;; regression test: accessing a struct through a typedef

(defctype xpto (:struct timeval))

(deftest struct.4
    (with-foreign-object (tv 'xpto)
      (setf (foreign-slot-value tv 'xpto 'tv-usecs) 1)
      (values (foreign-slot-value tv 'xpto 'tv-usecs)
              (foreign-slot-value tv 'timeval 'tv-usecs)))
  1 1)

(deftest struct.names
    (sort (foreign-slot-names 'xpto) #'<
          :key (lambda (x) (foreign-slot-offset 'xpto x)))
  (tv-secs tv-usecs))

;; regression test: compiler macro not quoting the type in the
;; resulting mem-ref form. The compiler macro on foreign-slot-value
;; is not guaranteed to be expanded though.

(defctype my-int :int)
(defcstruct s5 (a my-int))

(deftest struct.5
    (with-foreign-object (s 's5)
      (setf (foreign-slot-value s 's5 'a) 42)
      (foreign-slot-value s 's5 'a))
  42)

;;;# Structs with type translators

(defcstruct struct-string
  (s :string))

(deftest struct.string.1
    (with-foreign-object (ptr 'struct-string)
      (with-foreign-slots ((s) ptr struct-string)
        (setf s "So long and thanks for all the fish!")
        s))
  "So long and thanks for all the fish!")

(deftest struct.string.2
    (with-foreign-object (ptr 'struct-string)
      (setf (foreign-slot-value ptr 'struct-string 's) "Cha")
      (foreign-slot-value ptr 'struct-string 's))
  "Cha")

;;;# Structure Alignment Tests
;;;
;;; See libtest.c and types.lisp for some comments about alignments.

(defcstruct s-ch
  (a-char :char))

(defctype s-ch (:struct s-ch))

(defcstruct s-s-ch
  (another-char :char)
  (a-s-ch s-ch))

(defctype s-s-ch (:struct s-s-ch))

(defcvar "the_s_s_ch" s-s-ch)

(deftest struct.alignment.1
    (list 'a-char (foreign-slot-value
                   (foreign-slot-pointer *the-s-s-ch* 's-s-ch 'a-s-ch)
                   's-ch 'a-char)
          'another-char (foreign-slot-value *the-s-s-ch* 's-s-ch 'another-char))
  (a-char 1 another-char 2))


(defcstruct s-short
  (a-char :char)
  (another-char :char)
  (a-short :short))

(defctype s-short (:struct s-short))

(defcstruct s-s-short
  (yet-another-char :char)
  (a-s-short s-short))

(defctype s-s-short (:struct s-s-short))

(defcvar "the_s_s_short" s-s-short)

(deftest struct.alignment.2
    (with-foreign-slots ((yet-another-char a-s-short) *the-s-s-short* s-s-short)
      (with-foreign-slots ((a-char another-char a-short) a-s-short s-short)
        (list 'a-char           a-char
              'another-char     another-char
              'a-short          a-short
              'yet-another-char yet-another-char)))
  (a-char 1 another-char 2 a-short 3 yet-another-char 4))


(defcstruct s-double
  (a-char :char)
  (a-double :double)
  (another-char :char))

(defctype s-double (:struct s-double))

(defcstruct s-s-double
  (yet-another-char :char)
  (a-s-double s-double)
  (a-short :short))

(defctype s-s-double (:struct s-s-double))

(defcvar "the_s_s_double" s-s-double)

(deftest struct.alignment.3
    (with-foreign-slots
        ((yet-another-char a-s-double a-short) *the-s-s-double* s-s-double)
      (with-foreign-slots ((a-char a-double another-char) a-s-double s-double)
        (list 'a-char            a-char
              'a-double          a-double
              'another-char      another-char
              'yet-another-char  yet-another-char
              'a-short           a-short)))
  (a-char 1 a-double 2.0d0 another-char 3 yet-another-char 4 a-short 5))


(defcstruct s-s-s-double
  (another-short :short)
  (a-s-s-double s-s-double)
  (last-char :char))

(defctype s-s-s-double (:struct s-s-s-double))

(defcvar "the_s_s_s_double" s-s-s-double)

(deftest struct.alignment.4
    (with-foreign-slots
        ((another-short a-s-s-double last-char) *the-s-s-s-double* s-s-s-double)
      (with-foreign-slots
          ((yet-another-char a-s-double a-short) a-s-s-double s-s-double)
        (with-foreign-slots ((a-char a-double another-char) a-s-double s-double)
          (list 'a-char            a-char
                'a-double          a-double
                'another-char      another-char
                'yet-another-char  yet-another-char
                'a-short           a-short
                'another-short     another-short
                'last-char         last-char))))
  (a-char 1 a-double 2.0d0 another-char 3 yet-another-char 4 a-short 5
   another-short 6 last-char 7))


(defcstruct s-double2
  (a-double :double)
  (a-short  :short))

(defctype s-double2 (:struct s-double2))

(defcstruct s-s-double2
  (a-char        :char)
  (a-s-double2   s-double2)
  (another-short :short))

(defctype s-s-double2 (:struct s-s-double2))

(defcvar "the_s_s_double2" s-s-double2)

(deftest struct.alignment.5
    (with-foreign-slots
        ((a-char a-s-double2 another-short) *the-s-s-double2* s-s-double2)
      (with-foreign-slots ((a-double a-short) a-s-double2 s-double2)
        (list 'a-double       a-double
              'a-short        a-short
              'a-char         a-char
              'another-short  another-short)))
  (a-double 1.0d0 a-short 2 a-char 3 another-short 4))

(defcstruct s-long-long
  (a-long-long :long-long)
  (a-short     :short))

(defctype s-long-long (:struct s-long-long))

(defcstruct s-s-long-long
  (a-char        :char)
  (a-s-long-long s-long-long)
  (another-short :short))

(defctype s-s-long-long (:struct s-s-long-long))

(defcvar "the_s_s_long_long" s-s-long-long)

(deftest struct.alignment.6
    (with-foreign-slots
        ((a-char a-s-long-long another-short) *the-s-s-long-long* s-s-long-long)
      (with-foreign-slots ((a-long-long a-short) a-s-long-long s-long-long)
        (list 'a-long-long    a-long-long
              'a-short        a-short
              'a-char         a-char
              'another-short  another-short)))
  (a-long-long 1 a-short 2 a-char 3 another-short 4))

(defcstruct s-s-double3
  (a-s-double2   s-double2)
  (another-short :short))

(defctype s-s-double3 (:struct s-s-double3))

(defcstruct s-s-s-double3
  (a-s-s-double3  s-s-double3)
  (a-char         :char))

(defctype s-s-s-double3 (:struct s-s-s-double3))

(defcvar "the_s_s_s_double3" s-s-s-double3)

(deftest struct.alignment.7
    (with-foreign-slots ((a-s-s-double3 a-char) *the-s-s-s-double3* s-s-s-double3)
      (with-foreign-slots ((a-s-double2 another-short) a-s-s-double3 s-s-double3)
        (with-foreign-slots ((a-double a-short) a-s-double2 s-double2)
          (list 'a-double      a-double
                'a-short       a-short
                'another-short another-short
                'a-char        a-char))))
  (a-double 1.0d0 a-short 2 another-short 3 a-char 4))


(defcstruct empty-struct)

(defctype empty-struct (:struct empty-struct))

(defcstruct with-empty-struct
  (foo empty-struct)
  (an-int :int))

;; commented out this test because an empty struct is not valid/standard C
;; left the struct declarations anyway because they should be handled
;; gracefuly anyway.

; (defcvar "the_with_empty_struct" with-empty-struct)
;
; (deftest struct.alignment.5
;     (with-foreign-slots ((foo an-int) *the-with-empty-struct* with-empty-struct)
;       an-int)
;   42)


;; regression test, setf-ing nested foreign-slot-value forms
;; the setf expander used to return a bogus getter

(defcstruct s1
  (an-int :int))

(defctype s1 (:struct s1))

(defcstruct s2
  (an-s1 s1))

(defctype s2 (:struct s2))

(deftest struct.nested-setf
    (with-foreign-object (an-s2 's2)
      (setf (foreign-slot-value (foreign-slot-value an-s2 's2 'an-s1)
                                's1 'an-int)
            1984)
      (foreign-slot-value (foreign-slot-value an-s2 's2 'an-s1)
                          's1 'an-int))
  1984)

;; regression test, some Lisps were returning 4 instead of 8 for
;; (foreign-type-alignment :unsigned-long-long) on darwin/ppc32

(defcstruct s-unsigned-long-long
  (an-unsigned-long-long :unsigned-long-long)
  (a-short               :short))

(defctype s-unsigned-long-long (:struct s-unsigned-long-long))

(defcstruct s-s-unsigned-long-long
  (a-char                 :char)
  (a-s-unsigned-long-long s-unsigned-long-long)
  (another-short          :short))

(defctype s-s-unsigned-long-long (:struct s-s-unsigned-long-long))

(defcvar "the_s_s_unsigned_long_long" s-s-unsigned-long-long)

(deftest struct.alignment.8
    (with-foreign-slots
        ((a-char a-s-unsigned-long-long another-short)
         *the-s-s-unsigned-long-long* s-s-unsigned-long-long)
      (with-foreign-slots ((an-unsigned-long-long a-short)
                           a-s-unsigned-long-long s-unsigned-long-long)
        (list 'an-unsigned-long-long  an-unsigned-long-long
              'a-short                a-short
              'a-char                 a-char
              'another-short          another-short)))
  (an-unsigned-long-long 1 a-short 2 a-char 3 another-short 4))

;;;# C Struct Wrappers

(define-c-struct-wrapper timeval ())

(define-c-struct-wrapper (timeval2 (:struct timeval)) ()
  (tv-secs))

(defmacro with-example-timeval (var &body body)
  `(with-foreign-object (,var 'timeval)
     (with-foreign-slots ((tv-secs tv-usecs) ,var timeval)
       (setf tv-secs 42 tv-usecs 1984)
       ,@body)))

(deftest struct-wrapper.1
    (with-example-timeval ptr
      (let ((obj (make-instance 'timeval :pointer ptr)))
        (values (timeval-tv-secs obj)
                (timeval-tv-usecs obj))))
  42 1984)

(deftest struct-wrapper.2
    (with-example-timeval ptr
      (let ((obj (make-instance 'timeval2 :pointer ptr)))
        (timeval2-tv-secs obj)))
  42)

;;;# Structures as Values

(defcstruct (struct-pair :class pair)
  (a :int)
  (b :int))

(defctype struct-pair-typedef1 (:struct struct-pair))
(defctype struct-pair-typedef2 (:pointer (:struct struct-pair)))

(deftest struct.unparse.1
    (mapcar (alexandria:compose #'cffi::unparse-type #'cffi::parse-type)
            '(struct-pair
              (:struct struct-pair)
              struct-pair-typedef1
              struct-pair-typedef2))
  (struct-pair
   (:struct struct-pair)
   struct-pair-typedef1
   struct-pair-typedef2))

(deftest struct.canonicalize.1
    (mapcar #'cffi::canonicalize-foreign-type
            '(struct-pair
              (:struct struct-pair)
              struct-pair-typedef1
              struct-pair-typedef2))
  (:pointer
   (:struct struct-pair)
   (:struct struct-pair)
   :pointer))

(deftest struct.canonicalize.2
    (mapcar #'cffi::canonicalize-foreign-type
            '(struct-pair
              (:struct struct-pair)
              struct-pair-typedef1
              struct-pair-typedef2))
  (:pointer
   (:struct struct-pair)
   (:struct struct-pair)
   :pointer))

(defmethod translate-from-foreign (pointer (type pair))
  (with-foreign-slots ((a b) pointer (:struct struct-pair))
    (cons a b)))

(defmethod translate-into-foreign-memory (object (type pair) pointer)
  (with-foreign-slots ((a b) pointer (:struct struct-pair))
    (setf a (car object)
          b (cdr object))))

(defmethod translate-to-foreign (object (type pair))
  (let ((p (foreign-alloc '(:struct struct-pair))))
    (translate-into-foreign-memory object type p)
    (values p t)))

(defmethod free-translated-object (pointer (type pair) freep)
  (when freep
    (foreign-free pointer)))

(deftest struct-values.translation.1
    (multiple-value-bind (p freep)
        (convert-to-foreign '(1 . 2) 'struct-pair)
      (assert freep)
      (unwind-protect
           (convert-from-foreign p 'struct-pair)
        (free-converted-object p 'struct-pair freep)))
  (1 . 2))

(defcfun "pair_pointer_sum" :int
  (p (:pointer (:struct struct-pair))))

#+#:pointer-translation-not-yet-implemented
(deftest struct-values.translation.2
    (pair-pointer-sum '(1 . 2))
  3)

;;; should the return type be something along the lines of
;;; (:pointer (:struct pair) :free t)?
;;; LMH: error on ":free t" option?
(defcfun "alloc_pair" (:pointer (:struct struct-pair))
  (a :int)
  (b :int))

;; bogus: doesn't free() pointer.
#+#:pointer-translation-not-yet-implemented
(deftest struct-values.translation.3
    (alloc-pair 1 2)
  (1 . 2))

(deftest struct-values.translation.mem-ref.1
    (with-foreign-object (p '(:struct struct-pair))
      (setf (mem-ref p '(:struct struct-pair)) '(1 . 2))
      (with-foreign-slots ((a b) p (:struct struct-pair))
        (values (mem-ref p '(:struct struct-pair))
                a
                b)))
  (1 . 2)
  1
  2)

(deftest struct-values.translation.mem-aref.1
    (with-foreign-object (p '(:struct struct-pair) 2)
      (setf (mem-aref p '(:struct struct-pair) 0) '(1 . 2)
            (mem-aref p '(:struct struct-pair) 1) '(3 . 4))
      (values (mem-aref p '(:struct struct-pair) 0)
              (mem-aref p '(:struct struct-pair) 1)))
  (1 . 2)
  (3 . 4))

(defcstruct (struct-pair-default-translate :class pair-default)
  (a :int)
  (b :int))

(deftest struct-values-default.translation.mem-ref.1
    (with-foreign-object (p '(:struct struct-pair-default-translate))
      (setf (mem-ref p '(:struct struct-pair-default-translate)) '(a 1 b 2))
      (with-foreign-slots ((a b) p (:struct struct-pair-default-translate))
        (let ((plist (mem-ref p '(:struct struct-pair-default-translate))))
          (values (getf plist 'a)
                  (getf plist 'b)
                  a
                  b))))
  1
  2
  1
  2)

(defcstruct (struct-pair+double :class pair+double)
  (pr (:struct struct-pair-default-translate))
  (dbl :double))

(deftest struct-values-default.translation.mem-ref.2
    (with-foreign-object (p '(:struct struct-pair+double))
      (setf (mem-ref p '(:struct struct-pair+double)) '(pr (a 4 b 5) dbl 2.5d0))
      (with-foreign-slots ((pr dbl) p (:struct struct-pair+double))
        (let ((plist (mem-ref p '(:struct struct-pair+double))))
          (values (getf (getf plist 'pr) 'a)
                  (getf (getf plist 'pr) 'b)
                  (getf plist 'dbl)))))
  4
  5
  2.5d0)

(defcstruct (struct-pair+1 :class pair+1)
  (p (:pointer (:struct struct-pair)))
  (c :int))

(defctype struct-pair+1 (:struct struct-pair+1))

(defmethod translate-from-foreign (pointer (type pair+1))
  (with-foreign-slots ((p c) pointer struct-pair+1)
    (cons p c)))

(defmethod translate-into-foreign-memory (object (type pair+1) pointer)
  (with-foreign-slots ((c) pointer struct-pair+1)
    (convert-into-foreign-memory (car object)
                                 'struct-pair
                                 (foreign-slot-pointer pointer
                                                       'struct-pair+1
                                                       'p))
    (setf c (cdr object))))

(defmethod translate-to-foreign (object (type pair+1))
  (let ((p (foreign-alloc 'struct-pair+1)))
    (translate-into-foreign-memory object type p)
    (values p t)))

(defmethod free-translated-object (pointer (type pair+1) freep)
  (when freep
    (foreign-free pointer)))

#+#:pointer-translation-not-yet-implemented
(deftest struct-values.translation.ppo.1
    (multiple-value-bind (p freep)
        (convert-to-foreign '((1 . 2) . 3) 'struct-pair+1)
      (assert freep)
      (unwind-protect
           (convert-from-foreign p 'struct-pair+1)
        (free-converted-object p 'struct-pair+1 freep)))
  ((1 . 2) . 3))

#+#:unimplemented
(defcfun "pair_plus_one_sum" :int
  (p (:struct pair+1)))

(defcfun "pair_plus_one_pointer_sum" :int
  (p (:pointer (:struct struct-pair+1))))

#+#:pointer-translation-not-yet-implemented
(deftest struct-values.translation.ppo.2
    (pair-plus-one-pointer-sum '((1 . 2) . 3))
  6)

#+#:unimplemented
(defcfun "make_pair_plus_one" (:struct pair+1)
  (a :int)
  (b :int)
  (c :int))

(defcfun "alloc_pair_plus_one" struct-pair+1
  (a :int)
  (b :int)
  (c :int))

;; bogus: doesn't free() pointer.
#+#:pointer-translation-not-yet-implemented
(deftest struct-values.translation.ppo.3
    (alloc-pair-plus-one 1 2 3)
  ((1 . 2) . 3))

#+#:unimplemented
(defcfun "pair_sum" :int
  (p (:struct pair)))

#+#:unimplemented
(defcfun "make_pair" (:struct pair)
  (a :int)
  (b :int))

#|| ; TODO: load cffi-libffi for these tests to work.
(deftest struct-values.fn.1
    (with-foreign-object (p '(:struct pair))
      (with-foreign-slots ((a b) p (:struct pair))
        (setf a -1 b 2)
        (pair-sum p)))
  1)

(deftest struct-values.fn.2
    (pair-sum '(3 . 5))
  8)

(deftest struct-values.fn.3
    (with-foreign-object (p '(:struct pair))
      (make-pair 7 11 :result-pointer p)
      (with-foreign-slots ((a b) p (:struct pair))
        (cons a b)))
  (7 . 11))

(deftest struct-values.fn.4
    (make-pair 13 17)
  (13 . 17))
||#

(defcstruct single-byte-struct
  (a :uint8))

(deftest bare-struct-types.1
    (eql (foreign-type-size 'single-byte-struct)
         (foreign-type-size '(:struct single-byte-struct)))
  t)

(defctype single-byte-struct-alias (:struct single-byte-struct))

(deftest bare-struct-types.2
    (eql (foreign-type-size 'single-byte-struct-alias)
         (foreign-type-size '(:struct single-byte-struct)))
  t)

;;; Old-style access to inner structure fields.

(defcstruct inner-struct (x :int))
(defcstruct old-style-outer (inner inner-struct))
(defcstruct new-style-outer (inner (:struct inner-struct)))

(deftest old-style-struct-access
    (with-foreign-object (s '(:struct old-style-outer))
      (let ((inner-ptr (foreign-slot-pointer s 'old-style-outer 'inner)))
        (setf (foreign-slot-value inner-ptr 'inner-struct 'x) 42))
      (assert (pointerp (foreign-slot-value s 'old-style-outer 'inner)))
      (foreign-slot-value (foreign-slot-value s 'old-style-outer 'inner)
                          'inner-struct 'x))
  42)

(deftest new-style-struct-access
    (with-foreign-object (s '(:struct new-style-outer))
      (let ((inner-ptr (foreign-slot-pointer s 'new-style-outer 'inner)))
        (setf (foreign-slot-value inner-ptr 'inner-struct 'x) 42))
      (foreign-slot-value s 'new-style-outer 'inner))
  (x 42))

;;; regression test: setting the value of aggregate slots.

(defcstruct aggregate-struct
  (x :int)
  (pair (:struct struct-pair))
  (y :int))

(deftest set-aggregate-struct-slot
    (with-foreign-objects ((pair-struct '(:struct struct-pair))
                           (aggregate-struct '(:struct aggregate-struct)))
      (with-foreign-slots ((a b) pair-struct (:struct struct-pair))
        (setf a 1 b 2)
        (with-foreign-slots ((x pair y) aggregate-struct (:struct aggregate-struct))
          (setf x 42 y 42)
          (setf pair pair-struct)
          (values x pair y))))
  42
  (1 . 2)
  42)

;; TODO this needs to go through compile-file to exhibit the error
;; ("don't know how to dump #<CFFI::AGGREGATE-STRUCT-SLOT>"), but
;; there's no support for that, so let's leave it at toplevel here.
(defcstruct (aggregate-struct.acc :conc-name acc-)
  (x :int)
  (pair (:struct struct-pair))
  (y :int))

(deftest set-aggregate-struct-slot.acc
    (with-foreign-objects ((pair-struct '(:struct struct-pair))
                           (aggregate-struct '(:struct aggregate-struct)))
      (with-foreign-slots ((a b) pair-struct (:struct struct-pair))
        (setf a 1 b 2)
        (setf (acc-x aggregate-struct) 42)
        (setf (acc-y aggregate-struct) 42)
        (setf (acc-pair aggregate-struct) pair-struct)
        (values (acc-x aggregate-struct)
                (acc-pair aggregate-struct)
                (acc-y aggregate-struct))))
  42
  (1 . 2)
  42)

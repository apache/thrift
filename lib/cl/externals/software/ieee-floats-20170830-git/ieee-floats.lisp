;;; Functions for converting floating point numbers represented in
;;; IEEE 754 style to lisp numbers.
;;;
;;; See http://common-lisp.net/project/ieee-floats/

(defpackage :ieee-floats
  (:use :common-lisp)
  (:export :make-float-converters
	   :encode-float32
	   :decode-float32
	   :encode-float64
	   :decode-float64))

(in-package :ieee-floats)

;; The following macro may look a bit overcomplicated to the casual
;; reader. The main culprit is the fact that NaN and infinity can be
;; optionally included, which adds a bunch of conditional parts.
;;
;; Assuming you already know more or less how floating point numbers
;; are typically represented, I'll try to elaborate a bit on the more
;; confusing parts, as marked by letters:
;;
;; (A) Exponents in IEEE floats are offset by half their range, for
;;     example with 8 exponent bits a number with exponent 2 has 129
;;     stored in its exponent field.
;;
;; (B) The maximum possible exponent is reserved for special cases
;;     (NaN, infinity).
;;
;; (C) If the exponent fits in the exponent-bits, we have to adjust
;;     the significand for the hidden bit. Because decode-float will
;;     return a significand between 0 and 1, and we want one between 1
;;     and 2 to be able to hide the hidden bit, we double it and then
;;     subtract one (the hidden bit) before converting it to integer
;;     representation (to adjust for this, 1 is subtracted from the
;;     exponent earlier). When the exponent is too small, we set it to
;;     zero (meaning no hidden bit, exponent of 1), and adjust the
;;     significand downward to compensate for this.
;;
;; (D) Here the hidden bit is added. When the exponent is 0, there is
;;     no hidden bit, and the exponent is interpreted as 1.
;;
;; (E) Here the exponent offset is subtracted, but also an extra
;;     factor to account for the fact that the bits stored in the
;;     significand are supposed to come after the 'decimal dot'.

(defmacro make-float-converters (encoder-name
				 decoder-name
				 exponent-bits
				 significand-bits
				 support-nan-and-infinity-p)
  "Writes an encoder and decoder function for floating point
numbers with the given amount of exponent and significand
bits (plus an extra sign bit). If support-nan-and-infinity-p is
true, the decoders will also understand these special cases. NaN
is represented as :not-a-number, and the infinities as 
:positive-infinity and :negative-infinity. Note that this means
that the in- or output of these functions is not just floating
point numbers anymore, but also keywords."
  (let* ((total-bits (+ 1 exponent-bits significand-bits))
	 (exponent-offset (1- (expt 2 (1- exponent-bits)))) ; (A)
	 (sign-part `(ldb (byte 1 ,(1- total-bits)) bits))
	 (exponent-part `(ldb (byte ,exponent-bits ,significand-bits) bits))
	 (significand-part `(ldb (byte ,significand-bits 0) bits))
	 (nan support-nan-and-infinity-p)
	 (max-exponent (1- (expt 2 exponent-bits)))) ; (B)
    `(progn
       (defun ,encoder-name (float)
	 ,@(unless nan `((declare (type float float))))
         (multiple-value-bind (sign significand exponent)
             (cond ,@(when nan `(((eq float :not-a-number)
                                  (values 0 1 ,max-exponent))
                                 ((eq float :positive-infinity)
                                  (values 0 0 ,max-exponent))
                                 ((eq float :negative-infinity)
                                  (values 1 0 ,max-exponent))))
                   (t
                    (multiple-value-bind (significand exponent sign) (decode-float float)
                      (let ((exponent (if (= 0 significand)
                                          exponent
                                          (+ (1- exponent) ,exponent-offset)))
                            (sign (if (= sign 1.0) 0 1)))
                        (unless (< exponent ,(expt 2 exponent-bits))
                          (error "Floating point overflow when encoding ~A." float))
                        (if (<= exponent 0) ; (C)
                            (values sign (ash (round (* ,(expt 2 significand-bits) significand)) exponent) 0)
                            (values sign (round (* ,(expt 2 significand-bits) (1- (* significand 2)))) exponent))))))
	   (let ((bits 0))
	     (declare (type (unsigned-byte ,total-bits) bits))
	     (setf ,sign-part sign
		   ,exponent-part exponent
		   ,significand-part significand)
	     bits)))

       (defun ,decoder-name (bits)
	 (declare (type (unsigned-byte ,total-bits) bits))
	 (let* ((sign ,sign-part)
		(exponent ,exponent-part)
		(significand ,significand-part))
	   ,@(when nan `((when (= exponent ,max-exponent)
			   (return-from ,decoder-name 
			     (cond ((not (zerop significand)) :not-a-number)
				   ((zerop sign) :positive-infinity)
				   (t :negative-infinity))))))
           (if (zerop exponent)         ; (D)
               (setf exponent 1)
               (setf (ldb (byte 1 ,significand-bits) significand) 1))
           (let ((float-significand (float significand ,(if (> total-bits 32) 1.0d0 1.0f0))))
             (scale-float (if (zerop sign) float-significand (- float-significand))
                          (- exponent ,(+ exponent-offset significand-bits))))))))) ; (E)

;; And instances of the above for the common forms of floats.
(declaim (inline encode-float32 decode-float32 encode-float64 decode-float64))
(make-float-converters encode-float32 decode-float32 8 23 nil)
(make-float-converters encode-float64 decode-float64 11 52 nil)

;;; Copyright (c) 2006 Marijn Haverbeke
;;;
;;; This software is provided 'as-is', without any express or implied
;;; warranty. In no event will the authors be held liable for any
;;; damages arising from the use of this software.
;;;
;;; Permission is granted to anyone to use this software for any
;;; purpose, including commercial applications, and to alter it and
;;; redistribute it freely, subject to the following restrictions:
;;;
;;; 1. The origin of this software must not be misrepresented; you must
;;;    not claim that you wrote the original software. If you use this
;;;    software in a product, an acknowledgment in the product
;;;    documentation would be appreciated but is not required.
;;;
;;; 2. Altered source versions must be plainly marked as such, and must
;;;    not be misrepresented as being the original software.
;;;
;;; 3. This notice may not be removed or altered from any source
;;;    distribution.

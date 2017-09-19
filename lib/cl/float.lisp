;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: org.apache.thrift.implementation; -*-

(in-package :org.apache.thrift.implementation)

;;; This file defines codec operators for ieee short and long float data for the `org.apache.thrift` library.
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
;;;
;;; 2010-06-29  excerpted from de.setf.amqp;data-wire-coding.lisp.
;;;   validated for mcl-5.2, clozure-1.4, sbcl-1.0.35
;;;
;;;
;;; these functions implement portable translation from integer values and long/short floating point values
;;; float construction as described in the wikipedia pages. Translation in the other direction relies on
;;; runtime-specific access to the unmodified float components, as the standard deconstruction operators
;;; do not return the raw values and those changes would need to be inverted.
;;;
;;; there are various sources for information:
;;; * http://en.wikipedia.org/wiki/IEEE_754-2008
;;; * http://www.validlab.com/754R/nonabelian.com/754/comments/Q754.129.pdf
;;; * http://en.wikipedia.org/wiki/Single_precision_floating-point_format
;;; * http://en.wikipedia.org/wiki/Double_precision_floating-point_format
;;; * http://babbage.cs.qc.cuny.edu/IEEE-754/References.xhtml
;;;
;;; the latter is most directly useful as it depicts the corner-values for the respective domains
;;; and includes several pages for interactive conversions.
;;;
;;; internal operators: deconstruct a float value
;;;   raw-decode-short-float
;;;   raw-decode-long-float
;;;
;;; interface operators: convert between short/long float and integer
;;;   ieee-754-32-integer-to-float
;;;   ieee-754-64-integer-to-float
;;;   ieee-754-32-float-to-integer
;;;   ieee-754-64-float-to-integer
;;;
;;; test/examples forms are included inline conditional on :test.thrift
;;; 


(defun raw-decode-short-float (float)
  (etypecase float
    (short-float )
    (long-float (setf float (float float 1.0s0))))
  #+ccl (multiple-value-bind (fraction exponent sign)
                             (ccl::fixnum-decode-short-float float)
          (values fraction exponent (plusp sign)))
  ;; from sbcl:src;code;float.lisp
  #+sbcl (let* ((bits (sb-kernel::single-float-bits (abs float)))
                (exp (ldb sb-vm:single-float-exponent-byte bits))
                (sig (ldb sb-vm:single-float-significand-byte bits))
                (sign (minusp (float-sign float))))
           (values sig exp sign))
  #-(or ccl sbcl) (error "NYI: fixnum-decode-short-float"))

(defun raw-decode-long-float (float)
  (etypecase float
    (short-float (setf float (float float 1.0d0)))
    (long-float ))
  #+ccl (multiple-value-bind (hi lo exp sign) (ccl::%integer-decode-double-float float)
          (values (logior (ash hi 28) lo) exp (minusp sign)))
  #+sbcl (let* ((abs (abs float))
                (hi (sb-kernel::double-float-high-bits abs))
                (lo (sb-kernel::double-float-low-bits abs))
                (exp (ldb sb-vm:double-float-exponent-byte hi))
                ;(sig (ldb sb-vm:double-float-significand-byte hi))
                (sign (minusp (float-sign float))))
           (values
            (logior (ash (logior (ldb sb-vm:double-float-significand-byte hi)
                                 sb-vm:double-float-hidden-bit)
                         32)
                    lo)
            exp sign))
  #-(or ccl sbcl) (error "NYI: fixnum-decode-long-float"))



(defun ieee-754-32-integer-to-float (integer)
  (let* ((negative-p (logbitp 31 integer))
         (sign (if negative-p -1 +1))
         (raw-exponent (ash (logand #x7f800000 integer) -23))
         (exponent (- raw-exponent 127))
         (fraction (logand #x007fffff integer)))
    (case raw-exponent
      (#xff
       (if (zerop fraction)
         (if negative-p single-float-negative-infinity single-float-positive-infinity)
         #-sbcl single-float-nan
         #+sbcl (eval 'single-float-nan)))
      (#x00
       ;; (print (cl:list :to-float sign raw-exponent exponent fraction))
       (if (zerop fraction)
         (if negative-p -0.0s0 0.0s0)
         (float (* sign (* fraction (expt 2 (- exponent 22)))) single-float-epsilon)))
      (t
       ;; (print (cl:list :to-float sign raw-exponent exponent fraction))
       (float (* sign (1+ (* fraction #.(expt 2 -23))) (expt 2 exponent))
              single-float-epsilon)))))

#+thrift.test
(mapcar #'(lambda (x) (cl:list (format nil "#x~8,'0x" x) (ieee-754-32-integer-to-float x)))
        '(;; would need to bind more nan constants to distinguish the variations
          ;; (format nil "#x~8,'0x" (ieee-754-32-float-to-integer (ieee-754-32-integer-to-float #xffffffff))) quiet
          ;; (format nil "#x~8,'0x" (ieee-754-32-float-to-integer (ieee-754-32-integer-to-float #xFFC00000))) indeterminate
          ;; (format nil "#x~8,'0x" (ieee-754-32-float-to-integer (ieee-754-32-integer-to-float #xFF800001))) signaling
          #xFFFFFFFF #xFFC00001 #xFFC00000 #xFFBFFFFF #xFF800001 #xFF800000
          #xFF7FFFFF #x80800000 #x807FFFFF #x80000001
          #x80000000 #x0000000
          #x00000001 #x007FFFFF #x00800000 #x7F7FFFFF
          #x7F800000 #x7f800001 #x7FBFFFFF #x7FC00000 #x7fffffff
          ;; various numbers
          #x41c80000 #xc1c80000 #x3f800000 #xbf800000
          #x40000000 #xc0000000 #x3eaaaaab #xbeaaaaab))


(defun ieee-754-64-integer-to-float (integer)
  (let* ((negative-p (logbitp 63 integer))
         (sign (if negative-p -1 +1))
         (raw-exponent (ash (logand #x7ff0000000000000 integer) -52))
         (exponent (- raw-exponent 1023))
         (fraction (logand #x000fffffffffffff integer)))
    (case raw-exponent
      (#x7ff
       (if (zerop fraction)
         (if negative-p double-float-negative-infinity double-float-positive-infinity)
         #-sbcl double-float-nan
         #+sbcl (eval 'double-float-nan)))
      (#x000
       ;; (print (cl:list :to-float sign raw-exponent exponent fraction))
       (if (zerop fraction)
         (if negative-p -0.0d0 0.0d0)
         (float (* sign (* fraction (expt 2 (- exponent 51)))) double-float-epsilon)))
      (t
       ;; (print (cl:list :to-float sign raw-exponent exponent fraction))
       (float (* sign (1+ (* fraction #.(expt 2 -52))) (expt 2 exponent))
              double-float-epsilon)))))

#+thrift.test
(mapcar #'(lambda (x) (cl:list (format nil "#x~16,'0x" x) (ieee-754-64-integer-to-float x)))
        '(#xFFFFFFFFFFFFFFFF #xFFF8000000000001 #xFFF8000000000000 #xFFF7FFFFFFFFFFFF #xFFF0000000000001 #xFFF0000000000000
          #xFFEFFFFFFFFFFFFF #x8010000000000000 #x800FFFFFFFFFFFFF #x8000000000000001
          #x8000000000000000 #x0000000000000000
          #x0000000000000001 #x000FFFFFFFFFFFFF #x0010000000000000 #x7FEFFFFFFFFFFFFF
          #x7FF0000000000000 #x7FF0000000000001 #x7FF7FFFFFFFFFFFF #x7FF8000000000000 #x7FFFFFFFFFFFFFFF
          #x4039000000000000 #xC039000000000000 #x3FF0000000000000 #xBFF0000000000000
          #x4000000000000000 #xC000000000000000 #x3FD5555555555555 #xBFD5555555555555))


(defun ieee-754-32-float-to-integer (float)
  (cond ((= float single-float-negative-infinity)
         #xff800000)
        ((= float single-float-positive-infinity)
         #x7f800000)
        ;; allow for sbcl inability to compile code with nan constants 
        (#-sbcl (eql float single-float-nan)
         #+sbcl (sb-ext:float-nan-p float)
         ;; http://en.wikipedia.org/wiki/NaN#Encodings
         ;; http://java.sun.com/javase/6/docs/api/java/lang/Double.html#doubleToLongBits(double)
         #x7fc00000)
        ((= float 0.0s0)
         (if (minusp (float-sign float)) #x80000000 #x00000000))
        (t
         (multiple-value-bind (fraction exponent sign)
                              (raw-decode-short-float float)
           (if (zerop exponent)
             (logior (if sign #x80000000 0)
                     (logand fraction #x007fffff))
             (logior (if sign #x80000000 0)
                     (ash exponent 23)
                     (logand fraction #x007fffff)))))))

#+thrift.test
(remove t '(;; all NAN are encoded as positive silent
            #xFF800000
            #xFF7FFFFF #x80800000 #x807FFFFF #x80000001
            #x80000000 #x0000000
            #x00000001 #x007FFFFF #x00800000 #x7F7FFFFF
            #x7F800000  
            ;; various numbers
            #x41c80000 #xc1c80000 #x3f800000 #xbf800000
            #x40000000 #xc0000000 #x3eaaaaab #xbeaaaaab)
        :key #'(lambda (x)
                 (cond ((eql (ieee-754-32-float-to-integer (ieee-754-32-integer-to-float x)) x))
                       (t
                        (warn "ieee-754-32 failed: #x~8,'0x -> ~d -> #x~8,'0x, ~d"
                              x (ieee-754-32-integer-to-float x)
                              (ieee-754-32-float-to-integer (ieee-754-32-integer-to-float x))
                              (ieee-754-32-integer-to-float (ieee-754-32-float-to-integer (ieee-754-32-integer-to-float x))))
                        x))))


(defun ieee-754-64-float-to-integer (float)
  (cond ((= float double-float-negative-infinity)
         #xfff0000000000000)
        ((= float double-float-positive-infinity)
         #x7ff0000000000000)
        ;; allow for sbcl inability to compile code with nan constants                                                                                
        (#-sbcl (eql float double-float-nan)
         #+sbcl (sb-ext:float-nan-p float)
         ;; http://en.wikipedia.org/wiki/NaN#Encodings
         ;; http://java.sun.com/javase/6/docs/api/java/lang/Double.html#doubleToLongBits(double)
         #x7ff8000000000000)        
        ((= float 0.0d0)
         (if (minusp (float-sign float)) #x8000000000000000 #x0000000000000000))
        (t
         (multiple-value-bind (fraction exponent sign)
                              (raw-decode-long-float float)
           (if (zerop exponent)
             (logior (if sign #x8000000000000000 0)
                     (logand fraction #x000fffffffffffff))
             (logior (if sign #x8000000000000000 0)
                     (ash exponent 52)
                     (logand fraction #x000fffffffffffff)))))))

#+thrift.test
(remove t '(;; all NAN are encoded as positive silent
            #xFFF0000000000000
            #xFFEFFFFFFFFFFFFF #x8010000000000000 #x800FFFFFFFFFFFFF #x8000000000000001
            #x8000000000000000 #x0000000000000000
            #x0000000000000001 #x000FFFFFFFFFFFFF #x0010000000000000 #x7FEFFFFFFFFFFFFF
            #x7FF8000000000000
            #x4039000000000000 #xC039000000000000 #x3FF0000000000000 #xBFF0000000000000
            #x4000000000000000 #xC000000000000000 #x3FD5555555555555 #xBFD5555555555555)
        :key #'(lambda (x)
                 (cond ((eql (ieee-754-64-float-to-integer (ieee-754-64-integer-to-float x)) x))
                       (t
                        (warn "ieee-754-64 failed: #x~16,'0x -> ~d -> #x~16,'0x, ~d"
                              x (ieee-754-64-integer-to-float x)
                              (ieee-754-64-float-to-integer (ieee-754-64-integer-to-float x))
                              (ieee-754-64-integer-to-float (ieee-754-64-float-to-integer (ieee-754-64-integer-to-float x))))
                        x))))

;;; (ieee-754-64-integer-to-float #xFFF0000000000001)


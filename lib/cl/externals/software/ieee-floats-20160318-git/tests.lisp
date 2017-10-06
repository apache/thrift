(defpackage :ieee-floats-tests
  (:use :common-lisp :ieee-floats :Eos))

(in-package :ieee-floats-tests)

;; After loading, run the tests with (Eos:run! :ieee-floats)

;; The tiny-XX tests will error on systems that do not support 64-bit
;; floats, CLISP is one of those.

(def-suite :ieee-floats)
(in-suite :ieee-floats)

(defmacro pairs-correspond (decode encode &body pairs)
  `(progn ,@(loop :for (float bits) :in pairs
                  :collect `(is (eql ,float (,decode ,bits)))
                  :collect `(is (eql ,bits (,encode ,float))))))

(make-float-converters encode-float64* decode-float64* 11 52 t)
(make-float-converters encode-float32* decode-float32* 8 23 t)
(make-float-converters encode-float16 decode-float16 5 10 nil)

(test sanity-32
  (pairs-correspond decode-float32 encode-float32
    (0.0          #b00000000000000000000000000000000)
    (5.0          #b01000000101000000000000000000000)
    (-5.0         #b11000000101000000000000000000000)
    (3.3333333e20 #b01100001100100001000111101101111)
    (-.44e-30     #b10001101000011101100100111000101)))

(test tiny-32
  (pairs-correspond decode-float32 encode-float32
    (9.949219e-44 #b00000000000000000000000001000111)))

(test tiny-16
  (dolist (n '(#b0000001111111111 #b0000001100000000 #b0000001000000000))
    (is (eql n (encode-float16 (decode-float16 n))))))
      
(test overflow-32
  (signals error
    (encode-float32 1.0d60)))

(test specials-32
  (pairs-correspond decode-float32* encode-float32*
    (-0.0               #b10000000000000000000000000000000)
    (5.0e2              #b01000011111110100000000000000000)
    (-5.0e-2            #b10111101010011001100110011001101)
    (:not-a-number      #b01111111100000000000000000000001)
    (:positive-infinity #b01111111100000000000000000000000)
    (:negative-infinity #b11111111100000000000000000000000)))

(test sanity-64
  (pairs-correspond decode-float64 encode-float64
    (0.0d0          #b0000000000000000000000000000000000000000000000000000000000000000)
    (42d42          #b0100100011111110001000100010111010000010011001101101001001111111)
    (-42d42         #b1100100011111110001000100010111010000010011001101101001001111111)
    (.555555555d-30 #b0011100110100110100010010011011111111110011011000100011010001000)))

(test tiny-64
  (pairs-correspond decode-float64 encode-float64
    (4.1995579896505956d-322 #b0000000000000000000000000000000000000000000000000000000001010101)))

(test specials-64
  (pairs-correspond decode-float64* encode-float64*
    (-0d0               #b1000000000000000000000000000000000000000000000000000000000000000)
    (42d42              #b0100100011111110001000100010111010000010011001101101001001111111)
    (-42d42             #b1100100011111110001000100010111010000010011001101101001001111111)
    (:not-a-number      #b0111111111110000000000000000000000000000000000000000000000000001)
    (:positive-infinity #b0111111111110000000000000000000000000000000000000000000000000000)
    (:negative-infinity #b1111111111110000000000000000000000000000000000000000000000000000)))

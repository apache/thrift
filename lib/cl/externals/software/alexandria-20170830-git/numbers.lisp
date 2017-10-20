(in-package :alexandria)

(declaim (inline clamp))
(defun clamp (number min max)
  "Clamps the NUMBER into [min, max] range. Returns MIN if NUMBER is lesser then
MIN and MAX if NUMBER is greater then MAX, otherwise returns NUMBER."
  (if (< number min)
      min
      (if (> number max)
          max
          number)))

(defun gaussian-random (&optional min max)
  "Returns two gaussian random double floats as the primary and secondary value,
optionally constrained by MIN and MAX. Gaussian random numbers form a standard
normal distribution around 0.0d0.

Sufficiently positive MIN or negative MAX will cause the algorithm used to
take a very long time. If MIN is positive it should be close to zero, and
similarly if MAX is negative it should be close to zero."
  (macrolet
      ((valid (x)
         `(<= (or min ,x) ,x (or max ,x)) ))
    (labels
        ((gauss ()
           (loop
                 for x1 = (- (random 2.0d0) 1.0d0)
                 for x2 = (- (random 2.0d0) 1.0d0)
                 for w = (+ (expt x1 2) (expt x2 2))
                 when (< w 1.0d0)
                 do (let ((v (sqrt (/ (* -2.0d0 (log w)) w))))
                      (return (values (* x1 v) (* x2 v))))))
         (guard (x)
           (unless (valid x)
             (tagbody
              :retry
                (multiple-value-bind (x1 x2) (gauss)
                  (when (valid x1)
                    (setf x x1)
                    (go :done))
                  (when (valid x2)
                    (setf x x2)
                    (go :done))
                  (go :retry))
              :done))
           x))
      (multiple-value-bind
            (g1 g2) (gauss)
        (values (guard g1) (guard g2))))))

(declaim (inline iota))
(defun iota (n &key (start 0) (step 1))
  "Return a list of n numbers, starting from START (with numeric contagion
from STEP applied), each consequtive number being the sum of the previous one
and STEP. START defaults to 0 and STEP to 1.

Examples:

  (iota 4)                      => (0 1 2 3)
  (iota 3 :start 1 :step 1.0)   => (1.0 2.0 3.0)
  (iota 3 :start -1 :step -1/2) => (-1 -3/2 -2)
"
  (declare (type (integer 0) n) (number start step))
  (loop repeat n
        ;; KLUDGE: get numeric contagion right for the first element too
        for i = (+ (- (+ start step) step)) then (+ i step)
        collect i))

(declaim (inline map-iota))
(defun map-iota (function n &key (start 0) (step 1))
  "Calls FUNCTION with N numbers, starting from START (with numeric contagion
from STEP applied), each consequtive number being the sum of the previous one
and STEP. START defaults to 0 and STEP to 1. Returns N.

Examples:

  (map-iota #'print 3 :start 1 :step 1.0) => 3
    ;;; 1.0
    ;;; 2.0
    ;;; 3.0
"
  (declare (type (integer 0) n) (number start step))
  (loop repeat n
        ;; KLUDGE: get numeric contagion right for the first element too
        for i = (+ start (- step step)) then (+ i step)
        do (funcall function i))
  n)

(declaim (inline lerp))
(defun lerp (v a b)
  "Returns the result of linear interpolation between A and B, using the
interpolation coefficient V."
  ;; The correct version is numerically stable, at the expense of an
  ;; extra multiply. See (lerp 0.1 4 25) with (+ a (* v (- b a))). The
  ;; unstable version can often be converted to a fast instruction on
  ;; a lot of machines, though this is machine/implementation
  ;; specific. As alexandria is more about correct code, than
  ;; efficiency, and we're only talking about a single extra multiply,
  ;; many would prefer the stable version
  (+ (* (- 1.0 v) a) (* v b)))

(declaim (inline mean))
(defun mean (sample)
  "Returns the mean of SAMPLE. SAMPLE must be a sequence of numbers."
  (/ (reduce #'+ sample) (length sample)))

(declaim (inline median))
(defun median (sample)
  "Returns median of SAMPLE. SAMPLE must be a sequence of real numbers."
  (let* ((vector (sort (copy-sequence 'vector sample) #'<))
         (length (length vector))
         (middle (truncate length 2)))
    (if (oddp length)
        (aref vector middle)
        (/ (+ (aref vector middle) (aref vector (1- middle))) 2))))

(declaim (inline variance))
(defun variance (sample &key (biased t))
  "Variance of SAMPLE. Returns the biased variance if BIASED is true (the default),
and the unbiased estimator of variance if BIASED is false. SAMPLE must be a
sequence of numbers."
  (let ((mean (mean sample)))
    (/ (reduce (lambda (a b)
                 (+ a (expt (- b mean) 2)))
               sample
               :initial-value 0)
       (- (length sample) (if biased 0 1)))))

(declaim (inline standard-deviation))
(defun standard-deviation (sample &key (biased t))
  "Standard deviation of SAMPLE. Returns the biased standard deviation if
BIASED is true (the default), and the square root of the unbiased estimator
for variance if BIASED is false (which is not the same as the unbiased
estimator for standard deviation). SAMPLE must be a sequence of numbers."
  (sqrt (variance sample :biased biased)))

(define-modify-macro maxf (&rest numbers) max
  "Modify-macro for MAX. Sets place designated by the first argument to the
maximum of its original value and NUMBERS.")

(define-modify-macro minf (&rest numbers) min
  "Modify-macro for MIN. Sets place designated by the first argument to the
minimum of its original value and NUMBERS.")

;;;; Factorial

;;; KLUDGE: This is really dependant on the numbers in question: for
;;; small numbers this is larger, and vice versa. Ideally instead of a
;;; constant we would have RANGE-FAST-TO-MULTIPLY-DIRECTLY-P.
(defconstant +factorial-bisection-range-limit+ 8)

;;; KLUDGE: This is really platform dependant: ideally we would use
;;; (load-time-value (find-good-direct-multiplication-limit)) instead.
(defconstant +factorial-direct-multiplication-limit+ 13)

(defun %multiply-range (i j)
  ;; We use a a bit of cleverness here:
  ;;
  ;; 1. For large factorials we bisect in order to avoid expensive bignum
  ;;    multiplications: 1 x 2 x 3 x ... runs into bignums pretty soon,
  ;;    and once it does that all further multiplications will be with bignums.
  ;;
  ;;    By instead doing the multiplication in a tree like
  ;;       ((1 x 2) x (3 x 4)) x ((5 x 6) x (7 x 8))
  ;;    we manage to get less bignums.
  ;;
  ;; 2. Division isn't exactly free either, however, so we don't bisect
  ;;    all the way down, but multiply ranges of integers close to each
  ;;    other directly.
  ;;
  ;; For even better results it should be possible to use prime
  ;; factorization magic, but Nikodemus ran out of steam.
  ;;
  ;; KLUDGE: We support factorials of bignums, but it seems quite
  ;; unlikely anyone would ever be able to use them on a modern lisp,
  ;; since the resulting numbers are unlikely to fit in memory... but
  ;; it would be extremely unelegant to define FACTORIAL only on
  ;; fixnums, _and_ on lisps with 16 bit fixnums this can actually be
  ;; needed.
  (labels ((bisect (j k)
             (declare (type (integer 1 #.most-positive-fixnum) j k))
             (if (< (- k j) +factorial-bisection-range-limit+)
                 (multiply-range j k)
                 (let ((middle (+ j (truncate (- k j) 2))))
                   (* (bisect j middle)
                      (bisect (+ middle 1) k)))))
           (bisect-big (j k)
             (declare (type (integer 1) j k))
             (if (= j k)
                 j
                 (let ((middle (+ j (truncate (- k j) 2))))
                   (* (if (<= middle most-positive-fixnum)
                          (bisect j middle)
                          (bisect-big j middle))
                      (bisect-big (+ middle 1) k)))))
           (multiply-range (j k)
             (declare (type (integer 1 #.most-positive-fixnum) j k))
             (do ((f k (* f m))
                  (m (1- k) (1- m)))
                 ((< m j) f)
               (declare (type (integer 0 (#.most-positive-fixnum)) m)
                        (type unsigned-byte f)))))
    (if (and (typep i 'fixnum) (typep j 'fixnum))
        (bisect i j)
        (bisect-big i j))))

(declaim (inline factorial))
(defun %factorial (n)
  (if (< n 2)
      1
      (%multiply-range 1 n)))

(defun factorial (n)
  "Factorial of non-negative integer N."
  (check-type n (integer 0))
  (%factorial n))

;;;; Combinatorics

(defun binomial-coefficient (n k)
  "Binomial coefficient of N and K, also expressed as N choose K. This is the
number of K element combinations given N choises. N must be equal to or
greater then K."
  (check-type n (integer 0))
  (check-type k (integer 0))
  (assert (>= n k))
  (if (or (zerop k) (= n k))
      1
      (let ((n-k (- n k)))
        ;; Swaps K and N-K if K < N-K because the algorithm
        ;; below is faster for bigger K and smaller N-K
        (when (< k n-k)
          (rotatef k n-k))
        (if (= 1 n-k)
            n
            ;; General case, avoid computing the 1x...xK twice:
            ;;
            ;;    N!           1x...xN          (K+1)x...xN
            ;; --------  =  ---------------- =  ------------, N>1
            ;; K!(N-K)!     1x...xK x (N-K)!       (N-K)!
            (/ (%multiply-range (+ k 1) n)
               (%factorial n-k))))))

(defun subfactorial (n)
  "Subfactorial of the non-negative integer N."
  (check-type n (integer 0))
  (if (zerop n)
      1
      (do ((x 1 (1+ x))
           (a 0 (* x (+ a b)))
           (b 1 a))
          ((= n x) a))))

(defun count-permutations (n &optional (k n))
  "Number of K element permutations for a sequence of N objects.
K defaults to N"
  (check-type n (integer 0))
  (check-type k (integer 0))
  (assert (>= n k))
  (%multiply-range (1+ (- n k)) n))

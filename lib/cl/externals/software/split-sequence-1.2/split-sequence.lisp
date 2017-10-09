;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; SPLIT-SEQUENCE
;;;
;;; This code was based on Arthur Lemmens' in
;;; <URL:http://groups.google.com/groups?as_umsgid=39F36F1A.B8F19D20%40simplex.nl>;
;;;
;;; changes include:
;;;
;;; * altering the behaviour of the :from-end keyword argument to
;;; return the subsequences in original order, for consistency with
;;; CL:REMOVE, CL:SUBSTITUTE et al. (:from-end being non-NIL only
;;; affects the answer if :count is less than the number of
;;; subsequences, by analogy with the above-referenced functions).
;;;
;;; * changing the :maximum keyword argument to :count, by analogy
;;; with CL:REMOVE, CL:SUBSTITUTE, and so on.
;;;
;;; * naming the function SPLIT-SEQUENCE rather than PARTITION rather
;;; than SPLIT.
;;;
;;; * adding SPLIT-SEQUENCE-IF and SPLIT-SEQUENCE-IF-NOT.
;;;
;;; * The second return value is now an index rather than a copy of a
;;; portion of the sequence; this index is the `right' one to feed to
;;; CL:SUBSEQ for continued processing.

;;; There's a certain amount of code duplication here, which is kept
;;; to illustrate the relationship between the SPLIT-SEQUENCE
;;; functions and the CL:POSITION functions.

(defpackage :split-sequence
  (:use :common-lisp)
  (:export #:split-sequence
           #:split-sequence-if
           #:split-sequence-if-not))

(in-package :split-sequence)

(macrolet ((check-bounds (sequence start end)
             (let ((length (gensym (string '#:length))))
               `(let ((,length (length ,sequence)))
                  (check-type ,start unsigned-byte "a non-negative integer")
                  (when ,end (check-type ,end unsigned-byte "a non-negative integer or NIL"))
                  (unless ,end
                    (setf ,end ,length))
                  (unless (<= ,start ,end ,length)
                    (error "Wrong sequence bounds. start: ~S end: ~S" ,start ,end))))))

  (defun split-sequence (delimiter sequence &key (start 0) (end nil) (from-end nil)
                         (count nil) (remove-empty-subseqs nil)
                         (test #'eql) (test-not nil) (key #'identity))
    "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
    (check-bounds sequence start end)
    (cond
      ((and (not from-end) (null test-not))
       (split-from-start (lambda (sequence start)
                           (position delimiter sequence :start start :key key :test test))
                         sequence start end count remove-empty-subseqs))
      ((and (not from-end) test-not)
       (split-from-start (lambda (sequence start)
                           (position delimiter sequence :start start :key key :test-not test-not))
                         sequence start end count remove-empty-subseqs))
      ((and from-end (null test-not))
       (split-from-end (lambda (sequence end)
                         (position delimiter sequence :end end :from-end t :key key :test test))
                       sequence start end count remove-empty-subseqs))
      ((and from-end test-not)
       (split-from-end (lambda (sequence end)
                         (position delimiter sequence :end end :from-end t :key key :test-not test-not))
                       sequence start end count remove-empty-subseqs))))

  (defun split-sequence-if (predicate sequence &key (start 0) (end nil) (from-end nil)
                            (count nil) (remove-empty-subseqs nil) (key #'identity))
    "Return a list of subsequences in seq delimited by items satisfying
predicate.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
    (check-bounds sequence start end)
    (if from-end
        (split-from-end (lambda (sequence end)
                          (position-if predicate sequence :end end :from-end t :key key))
                        sequence start end count remove-empty-subseqs)
        (split-from-start (lambda (sequence start)
                            (position-if predicate sequence :start start :key key))
                          sequence start end count remove-empty-subseqs)))

  (defun split-sequence-if-not (predicate sequence &key (count nil) (remove-empty-subseqs nil)
                                (from-end nil) (start 0) (end nil) (key #'identity))
    "Return a list of subsequences in seq delimited by items satisfying
\(CL:COMPLEMENT predicate).

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF-NOT.  In particular,
the behaviour of :from-end is possibly different from other versions
of this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
    (check-bounds sequence start end)
    (if from-end
        (split-from-end (lambda (sequence end)
                          (position-if-not predicate sequence :end end :from-end t :key key))
                        sequence start end count remove-empty-subseqs)
        (split-from-start (lambda (sequence start)
                            (position-if-not predicate sequence :start start :key key))
                          sequence start end count remove-empty-subseqs))))

(defun split-from-end (position-fn sequence start end count remove-empty-subseqs)
  (loop
     :for right := end :then left
     :for left := (max (or (funcall position-fn sequence right) -1)
                       (1- start))
     :unless (and (= right (1+ left))
                  remove-empty-subseqs) ; empty subseq we don't want
     :if (and count (>= nr-elts count))
     ;; We can't take any more. Return now.
       :return (values (nreverse subseqs) right)
     :else
       :collect (subseq sequence (1+ left) right) into subseqs
       :and :sum 1 :into nr-elts
     :until (< left start)
   :finally (return (values (nreverse subseqs) (1+ left)))))

(defun split-from-start (position-fn sequence start end count remove-empty-subseqs)
  (let ((length (length sequence)))
    (loop
       :for left := start :then (+ right 1)
       :for right := (min (or (funcall position-fn sequence left) length)
                          end)
       :unless (and (= right left)
                    remove-empty-subseqs) ; empty subseq we don't want
       :if (and count (>= nr-elts count))
       ;; We can't take any more. Return now.
         :return (values subseqs left)
       :else
         :collect (subseq sequence left right) :into subseqs
         :and :sum 1 :into nr-elts
       :until (>= right end)
     :finally (return (values subseqs right)))))

(pushnew :split-sequence *features*)

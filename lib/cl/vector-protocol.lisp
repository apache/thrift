(in-package #:org.apache.thrift.implementation)

;;; define a binary stream to wrap a vector for use in tests.
;;; adapted from the cl-xml version to restrict i/o to unsigned byte operations.
;;; this version uses a signed byte stream, as that's the basis of the thrift binary transport
;;; 


;;;
;;; abstract

(defclass vector-stream ()
  ((position
    :initform 0
    :reader get-stream-position :writer setf-stream-position)
   (vector
    :reader get-vector-stream-vector :writer setf-vector-stream-vector
    :type vector)
   (force-output-hook
    :initform nil :initarg :force-output-hook
    :accessor stream-force-output-hook
    :documentation "A function of one argument, the stream, called as the
     base implementation of stream-force-output.")
   #+(or CMU sbcl lispworks) (direction :initarg :direction)
   )
  (:default-initargs
    #+CormanLisp :element-type #+CormanLisp 'character))

(defclass vector-input-stream (vector-stream
                               #+ALLEGRO excl::fundamental-binary-input-stream
                               #+LispWorks stream:fundamental-stream
                               #+(and MCL digitool) ccl::input-binary-stream
                               #+(and MCL openmcl) fundamental-binary-input-stream
                               #+CMU extensions:fundamental-binary-input-stream
                               #+sbcl sb-gray:fundamental-binary-input-stream
                               #+CormanLisp stream
                               )
  ()
  (:default-initargs :direction :input))

(defclass vector-output-stream (vector-stream
                                #+ALLEGRO excl::fundamental-binary-output-stream
                                #+LispWorks stream:fundamental-stream
                                #+(and MCL digitool) ccl::output-binary-stream
                                #+(and MCL openmcl) fundamental-binary-output-stream
                                #+CMU extensions:fundamental-binary-output-stream
                                #+sbcl sb-gray:fundamental-binary-output-stream
                                #+CormanLisp stream
                                )
  ()
  (:default-initargs :direction :output))

(defclass vector-stream-transport (vector-input-stream vector-output-stream binary-transport)
  ((stream :initform nil)))


(defun make-vector-stream-buffer (length &optional (type *binary-transport-element-type*))
  (make-array length :element-type type :initial-element 0))

(defmethod shared-initialize
           ((instance vector-stream) (slots t) &key (vector nil vector-s) (length 128))
  (with-slots (position) instance
    (setf position 0)
    (when vector-s
      (setf-vector-stream-vector
       (etypecase vector
         (string (map-into (make-vector-stream-buffer (length vector)) #'char-code vector))
         (cl:cons (map-into (make-vector-stream-buffer (length vector))
                            #'(lambda (datum)
                                (etypecase datum
                                  (fixnum datum)
                                  (character (char-code datum))))
                            vector))
         (vector vector)
         (null (make-vector-stream-buffer length)))
       instance))
    (call-next-method)
    (unless (slot-boundp instance 'vector)
      (setf-vector-stream-vector (make-vector-stream-buffer length) instance))))

#+cmu
(let ((old-definition (fdefinition 'stream-element-type)))
  (unless (typep old-definition 'generic-function)
    (fmakunbound 'stream-element-type)
    (defgeneric stream-element-type (stream))
    (setf (documentation 'stream-element-type 'function)
          (documentation old-definition 'function))
    (defmethod stream-element-type (stream)
      (funcall old-definition stream))))

(defmethod stream-element-type ((stream vector-stream))
  *binary-transport-element-type*)

(defmethod stream-position ((stream vector-stream) &optional new)
  (with-slots (vector) stream
    (if new
      (setf-stream-position (min (length vector) new) stream)
      (get-stream-position stream))))

(defmethod stream-eofp ((stream vector-stream))
  (with-slots (position vector) stream
    (>= position (length vector))))

(defmethod stream-finish-output ((stream vector-stream))
  nil)

(defmethod print-object
           ((vs vector-stream) (stream t)
            &aux (*print-array* t) (*print-length* 32) (*print-base* 16))
  (print-unreadable-object (vs stream :type t)
    (princ (get-vector-stream-vector vs) stream)))

(defmethod stream-force-output ((stream vector-stream))
  (let ((hook (stream-force-output-hook stream)))
    (when hook (funcall hook stream))))

#-mcl
(defmethod open-stream-p ((stream vector-stream))
  t)

(defgeneric vector-stream-vector (vector-stream)
  (:documentation "Return the written subsequence and reset the position")
  (:method ((stream vector-stream))
    (with-slots (vector position) stream
      (prog1 (subseq vector 0 position)
        (setf position 0)))))

(defgeneric (setf vector-stream-vector) (vector vector-stream)
  (:method ((new-vector vector) (stream vector-stream))
    (assert (equal (array-element-type new-vector) *binary-transport-element-type*) ()
            "Invalid vector stream element type: ~s." (array-element-type new-vector))
    (with-slots (vector position) stream
      (setf position 0
            vector new-vector))))

;;;
;;; input

(defmethod stream-read-byte ((stream vector-input-stream))
  (with-slots (position vector) stream
    (when (< position (length vector))
      (let ((byte (aref vector position)))
        (incf position)
        (if (> byte 127)
          (- (logxor 255 (1- byte)))
          byte)))))

(defmethod stream-read-unsigned-byte ((stream vector-input-stream))
  (with-slots (position vector) stream
    (when (< position (length vector))
      (let ((byte (aref vector position)))
        (incf position)
        byte))))

#+mcl
(defmethod ccl:stream-tyi ((stream vector-input-stream))
  (stream-read-byte stream))

(defmethod stream-reader ((stream vector-input-stream))
  (values #'(lambda (stream)
              (with-slots (position vector) stream
                (when (< position (length vector))
                  (let ((byte (aref vector position)))
                    (incf position)
                    byte))))
              stream))


(defmethod stream-read-sequence ((stream vector-input-stream) (sequence vector)
                                  #+mcl &key #-mcl &optional (start 0) (end nil))
  (unless end (setf end (length sequence)))
  (assert (typep start '(integer 0)))
  (assert (>= end start))
  (with-slots (vector position) stream
    (let* ((new-position (min (+ position (- end start)) (length vector))))
      (when (> new-position position)
        (replace sequence vector
                 :start1 start :end1 end
                 :start2 position :end2 new-position)
        (setf position new-position))
      new-position)))


;;;
;;; output


(defmethod stream-write-byte ((stream vector-output-stream) (datum integer) &aux next)
  (with-slots (position vector) stream
    (unless (< (setf next (1+ position)) (length vector))
      (setf vector
            (adjust-array vector (+ next (floor (/ next 4)))
                          :element-type *binary-transport-element-type*)))
    (setf (aref vector position)
          (logand #xff datum))
    (setf position next)))


#+mcl
(defmethod ccl:stream-tyo ((stream vector-output-stream) byte)
  (stream-write-byte stream byte))

(defmethod stream-writer ((stream vector-output-stream))
  (values #'(lambda (stream byte &aux next)
              (with-slots (position vector) stream
                (unless (< (setf next (1+ position)) (length vector))
                  (setf vector
                        (adjust-array vector (+ next (floor (/ next 4)))
                                      :element-type *binary-transport-element-type*)))
                (setf (aref vector position)
                      (logand #xff byte))
                (setf position next)))
              stream))

(defmethod stream-write-sequence ((stream vector-output-stream) (sequence vector)
                                  #+mcl &key #-mcl &optional (start 0) (end nil))
  (unless end (setf end (length sequence)))
  (assert (typep start '(integer 0)))
  (assert (>= end start))
  (with-slots (vector position) stream
    (let* ((new-position (+ position (- end start))))
      (when (> new-position position)
        (unless (< new-position (length vector))
          (setf vector
                (adjust-array vector (floor (+ new-position (floor (/ new-position 4))))
                              :element-type *binary-transport-element-type*)))
        (replace vector sequence
                 :start1 position :end1 new-position
                 :start2 start :end2 end)
        (setf position new-position))
      new-position)))

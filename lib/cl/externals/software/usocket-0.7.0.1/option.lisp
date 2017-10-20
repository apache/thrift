;;;; SOCKET-OPTION, a high-level socket option get/set framework

;;;; See LICENSE for licensing information.

(in-package :usocket)

;;; Interface definition

(defgeneric socket-option (socket option &key)
  (:documentation
   "Get a socket's internal options"))

(defgeneric (setf socket-option) (new-value socket option &key)
  (:documentation
   "Set a socket's internal options"))

;;; Handling of wrong type of arguments

(defmethod socket-option ((socket usocket) (option t) &key)
  (error 'type-error :datum option :expected-type 'keyword))

(defmethod (setf socket-option) (new-value (socket usocket) (option t) &key)
  (declare (ignore new-value))
  (socket-option socket option))

(defmethod socket-option ((socket usocket) (option symbol) &key)
  (if (keywordp option)
    (error 'unimplemented :feature option :context 'socket-option)
    (error 'type-error :datum option :expected-type 'keyword)))

(defmethod (setf socket-option) (new-value (socket usocket) (option symbol) &key)
  (declare (ignore new-value))
  (socket-option socket option))

;;; Socket option: RECEIVE-TIMEOUT (SO_RCVTIMEO)

(defmethod socket-option ((usocket stream-usocket)
                          (option (eql :receive-timeout)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    #+abcl
    () ; TODO
    #+allegro
    () ; TODO
    #+clisp
    (socket:socket-options socket :so-rcvtimeo)
    #+clozure
    (ccl:stream-input-timeout socket)
    #+cmu
    (lisp::fd-stream-timeout (socket-stream usocket))
    #+ecl
    (sb-bsd-sockets:sockopt-receive-timeout socket)
    #+lispworks
    (get-socket-receive-timeout socket)
    #+mcl
    () ; TODO
    #+mocl
    () ; unknown
    #+sbcl
    (sb-impl::fd-stream-timeout (socket-stream usocket))
    #+scl
    ())) ; TODO

(defmethod (setf socket-option) (new-value (usocket stream-usocket)
                                           (option (eql :receive-timeout)) &key)
  (declare (type number new-value) (ignorable new-value option))
  (let ((socket (socket usocket))
        (timeout new-value))
    (declare (ignorable socket timeout))
    #+abcl
    () ; TODO
    #+allegro
    () ; TODO
    #+clisp
    (socket:socket-options socket :so-rcvtimeo timeout)
    #+clozure
    (setf (ccl:stream-input-timeout socket) timeout)
    #+cmu
    (setf (lisp::fd-stream-timeout (socket-stream usocket))
          (coerce timeout 'integer))
    #+ecl
    (setf (sb-bsd-sockets:sockopt-receive-timeout socket) timeout)
    #+lispworks
    (set-socket-receive-timeout socket timeout)
    #+mcl
    () ; TODO
    #+mocl
    () ; unknown
    #+sbcl
    (setf (sb-impl::fd-stream-timeout (socket-stream usocket))
          (coerce timeout 'single-float))
    #+scl
    () ; TODO
    new-value))

;;; Socket option: SEND-TIMEOUT (SO_SNDTIMEO)

(defmethod socket-option ((usocket stream-usocket)
                          (option (eql :send-timeout)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    #+abcl
    () ; TODO
    #+allegro
    () ; TODO
    #+clisp
    (socket:socket-options socket :so-sndtimeo)
    #+clozure
    (ccl:stream-output-timeout socket)
    #+cmu
    (lisp::fd-stream-timeout (socket-stream usocket))
    #+ecl
    (sb-bsd-sockets:sockopt-send-timeout socket)
    #+lispworks
    (get-socket-send-timeout socket)
    #+mcl
    () ; TODO
    #+mocl
    () ; unknown
    #+sbcl
    (sb-impl::fd-stream-timeout (socket-stream usocket))
    #+scl
    ())) ; TODO

(defmethod (setf socket-option) (new-value (usocket stream-usocket)
                                           (option (eql :send-timeout)) &key)
  (declare (type number new-value) (ignorable new-value option))
  (let ((socket (socket usocket))
        (timeout new-value))
    (declare (ignorable socket timeout))
    #+abcl
    () ; TODO
    #+allegro
    () ; TODO
    #+clisp
    (socket:socket-options socket :so-sndtimeo timeout)
    #+clozure
    (setf (ccl:stream-output-timeout socket) timeout)
    #+cmu
    (setf (lisp::fd-stream-timeout (socket-stream usocket))
          (coerce timeout 'integer))
    #+ecl
    (setf (sb-bsd-sockets:sockopt-send-timeout socket) timeout)
    #+lispworks
    (set-socket-send-timeout socket timeout)
    #+mcl
    () ; TODO
    #+mocl
    () ; unknown
    #+sbcl
    (setf (sb-impl::fd-stream-timeout (socket-stream usocket))
          (coerce timeout 'single-float))
    #+scl
    () ; TODO
    new-value))

;;; Socket option: REUSE-ADDRESS (SO_REUSEADDR), for TCP server

(defmethod socket-option ((usocket stream-server-usocket)
                          (option (eql :reuse-address)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    #+abcl
    () ; TODO
    #+allegro
    () ; TODO
    #+clisp
    (int->bool (socket:socket-options socket :so-reuseaddr))
    #+clozure
    (int->bool (get-socket-option-reuseaddr socket))
    #+cmu
    () ; TODO
    #+lispworks
    (get-socket-reuse-address socket)
    #+mcl
    () ; TODO
    #+mocl
    () ; unknown
    #+(or ecl sbcl)
    (sb-bsd-sockets:sockopt-reuse-address socket)
    #+scl
    ())) ; TODO

(defmethod (setf socket-option) (new-value (usocket stream-server-usocket)
                                           (option (eql :reuse-address)) &key)
  (declare (type boolean new-value) (ignorable new-value option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    #+abcl
    () ; TODO
    #+allegro
    (socket:set-socket-options socket option new-value)
    #+clisp
    (socket:socket-options socket :so-reuseaddr (bool->int new-value))
    #+clozure
    (set-socket-option-reuseaddr socket (bool->int new-value))
    #+cmu
    () ; TODO
    #+lispworks
    (set-socket-reuse-address socket new-value)
    #+mcl
    () ; TODO
    #+mocl
    () ; unknown
    #+(or ecl sbcl)
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) new-value)
    #+scl
    () ; TODO
    new-value))

;;; Socket option: BROADCAST (SO_BROADCAST), for UDP client

(defmethod socket-option ((usocket datagram-usocket)
                          (option (eql :broadcast)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    #+abcl
    () ; TODO
    #+allegro
    () ; TODO
    #+clisp
    (int->bool (socket:socket-options socket :so-broadcast))
    #+clozure
    (int->bool (get-socket-option-broadcast socket))
    #+cmu
    () ; TODO
    #+ecl
    () ; TODO
    #+lispworks
    () ; TODO
    #+mcl
    () ; TODO
    #+mocl
    () ; unknown
    #+sbcl
    (sb-bsd-sockets:sockopt-broadcast socket)
    #+scl
    ())) ; TODO

(defmethod (setf socket-option) (new-value (usocket datagram-usocket)
                                           (option (eql :broadcast)) &key)
  (declare (type boolean new-value) (ignorable new-value option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    #+abcl
    () ; TODO
    #+allegro
    (socket:set-socket-options socket option new-value)
    #+clisp
    (socket:socket-options socket :so-broadcast (bool->int new-value))
    #+clozure
    (set-socket-option-broadcast socket (bool->int new-value))
    #+cmu
    () ; TODO
    #+ecl
    () ; TODO
    #+lispworks
    () ; TODO
    #+mcl
    () ; TODO
    #+mocl
    () ; unknown
    #+sbcl
    (setf (sb-bsd-sockets:sockopt-broadcast socket) new-value)
    #+scl
    () ; TODO
    new-value))

;;; Socket option: TCP-NODELAY (TCP_NODELAY), for TCP client

(defmethod socket-option ((usocket stream-usocket)
                          (option (eql :tcp-no-delay)) &key)
  (declare (ignore option))
  (socket-option usocket :tcp-nodelay))

(defmethod socket-option ((usocket stream-usocket)
                          (option (eql :tcp-nodelay)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    #+abcl
    () ; TODO
    #+allegro
    () ; TODO
    #+clisp
    (int->bool (socket:socket-options socket :tcp-nodelay))
    #+clozure
    (int->bool (get-socket-option-tcp-nodelay socket))
    #+cmu
    ()
    #+ecl
    (sb-bsd-sockets::sockopt-tcp-nodelay socket)
    #+lispworks
    (int->bool (get-socket-tcp-nodelay socket))
    #+mcl
    () ; TODO
    #+mocl
    () ; unknown
    #+sbcl
    (sb-bsd-sockets::sockopt-tcp-nodelay socket)
    #+scl
    ())) ; TODO

(defmethod (setf socket-option) (new-value (usocket stream-usocket)
                                           (option (eql :tcp-no-delay)) &key)
  (declare (ignore option))
  (setf (socket-option usocket :tcp-nodelay) new-value))

(defmethod (setf socket-option) (new-value (usocket stream-usocket)
                                           (option (eql :tcp-nodelay)) &key)
  (declare (type boolean new-value) (ignorable new-value option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    #+abcl
    () ; TODO
    #+allegro
    (socket:set-socket-options socket :no-delay new-value)
    #+clisp
    (socket:socket-options socket :tcp-nodelay (bool->int new-value))
    #+clozure
    (set-socket-option-tcp-nodelay socket (bool->int new-value))
    #+cmu
    ()
    #+ecl
    (setf (sb-bsd-sockets::sockopt-tcp-nodelay socket) new-value)
    #+lispworks
    (progn
      #-lispworks4
      (comm::set-socket-tcp-nodelay socket new-value)
      #+lispworks4
      (set-socket-tcp-nodelay socket (bool->int new-value)))
    #+mcl
    () ; TODO
    #+mocl
    () ; unknown
    #+sbcl
    (setf (sb-bsd-sockets::sockopt-tcp-nodelay socket) new-value)
    #+scl
    () ; TODO
    new-value))

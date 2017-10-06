;;;; $Id$
;;;; $URL$

;; MCL backend for USOCKET 0.4.1
;; Terje Norderhaug <terje@in-progress.com>, January 1, 2009

(in-package :usocket)

(defun handle-condition (condition &optional socket)
  ; incomplete, needs to handle additional conditions
  (flet ((raise-error (&optional socket-condition)
           (if socket-condition
           (error socket-condition :socket socket)
           (error  'unknown-error :socket socket :real-error condition))))
    (typecase condition
      (ccl:host-stopped-responding
       (raise-error 'host-down-error))
      (ccl:host-not-responding
       (raise-error 'host-unreachable-error))
      (ccl:connection-reset 
       (raise-error 'connection-reset-error))
      (ccl:connection-timed-out
       (raise-error 'timeout-error))
      (ccl:opentransport-protocol-error
       (raise-error 'protocol-not-supported-error))       
      (otherwise
       (raise-error)))))

(defun socket-connect (host port &key (element-type 'character) timeout deadline nodelay 
                            local-host local-port (protocol :stream))
  (when (eq nodelay :if-supported)
    (setf nodelay t))
  (ecase protocol
    (:stream
     (with-mapped-conditions ()
       (let* ((socket
               (make-instance 'active-socket
		 :remote-host (when host (host-to-hostname host)) 
                 :remote-port port
                 :local-host (when local-host (host-to-hostname local-host)) 
                 :local-port local-port
                 :deadline deadline
                 :nodelay nodelay
                 :connect-timeout (and timeout (round (* timeout 60)))
                 :element-type element-type))
              (stream (socket-open-stream socket)))
         (make-stream-socket :socket socket :stream stream))))
    (:datagram
     (with-mapped-conditions ()
       (make-datagram-socket
	 (ccl::open-udp-socket :local-address (and local-host (host-to-hbo local-host))
			       :local-port local-port))))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  (let* ((reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
	 (socket (with-mapped-conditions ()
		   (make-instance 'passive-socket 
				  :local-port port
				  :local-host (host-to-hbo host)
				  :reuse-address reuseaddress
				  :backlog backlog))))
    (make-stream-server-socket socket :element-type element-type)))

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (let* ((socket (socket usocket))
         (stream (with-mapped-conditions (usocket)
                   (socket-accept socket :element-type element-type))))
    (make-stream-socket :socket socket :stream stream)))

(defmethod socket-close ((usocket usocket))
  (with-mapped-conditions (usocket)
    (socket-close (socket usocket))))

(defmethod socket-shutdown ((usocket usocket) direction)
  (declare (ignore usocket direction))
  ;; As far as I can tell there isn't a way to shutdown a socket in mcl.
  (unsupported "shutdown" 'socket-shutdown))

(defmethod ccl::stream-close ((usocket usocket))
  (socket-close usocket))

(defun get-hosts-by-name (name)
  (with-mapped-conditions ()
    (list (hbo-to-vector-quad (ccl::get-host-address
                               (host-to-hostname name))))))

(defun get-host-by-address (address)
  (with-mapped-conditions ()
    (ccl::inet-host-name (host-to-hbo address))))

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))

(defmethod get-local-address ((usocket usocket))
  (hbo-to-vector-quad (ccl::get-host-address (or (local-host (socket usocket)) ""))))

(defmethod get-local-port ((usocket usocket))
  (local-port (socket usocket)))

(defmethod get-peer-address ((usocket stream-usocket))
  (hbo-to-vector-quad (ccl::get-host-address (remote-host (socket usocket)))))

(defmethod get-peer-port ((usocket stream-usocket))
  (remote-port (socket usocket)))


(defun %setup-wait-list (wait-list)
  (declare (ignore wait-list)))

(defun %add-waiter (wait-list waiter)
  (declare (ignore wait-list waiter)))

(defun %remove-waiter (wait-list waiter)
  (declare (ignore wait-list waiter)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC MCL SOCKET IMPLEMENTATION

(defclass socket ()
  ((local-port :reader local-port :initarg :local-port)
   (local-host :reader local-host :initarg :local-host)
   (element-type :reader element-type :initform 'ccl::base-character :initarg :element-type)))

(defclass active-socket (socket)
  ((remote-host :reader remote-host :initarg :remote-host)
   (remote-port :reader remote-port :initarg :remote-port)
   (deadline :initarg :deadline)
   (nodelay :initarg :nodelay)
   (connect-timeout :reader connect-timeout :initform NIL :initarg :connect-timeout
                    :type (or null fixnum) :documentation "ticks (60th of a second)")))

(defmethod socket-open-stream ((socket active-socket))
  (ccl::open-tcp-stream (or (remote-host socket)(ccl::local-interface-ip-address)) (remote-port socket)
   :element-type (if (subtypep (element-type socket) 'character) 'ccl::base-character 'unsigned-byte)
   :connect-timeout (connect-timeout socket)))

(defmethod socket-close ((socket active-socket))
  NIL)

(defclass passive-socket (socket)
  ((streams :accessor socket-streams :type list :initform NIL
	    :documentation "Circular list of streams with first element the next to open")
   (reuse-address :reader reuse-address :initarg :reuse-address)
   (lock :reader socket-lock :initform (ccl:make-lock "Socket"))))

(defmethod initialize-instance :after ((socket passive-socket) &key backlog)
  (loop repeat backlog
        collect (socket-open-listener socket) into streams
        finally (setf (socket-streams socket)
                      (cdr (rplacd (last streams) streams))))
  (when (zerop (local-port socket))
    (setf (slot-value socket 'local-port)
          (or (ccl::process-wait-with-timeout "binding port" (* 10 60) 
               #'ccl::stream-local-port (car (socket-streams socket)))
              (error "timeout")))))

(defmethod socket-accept ((socket passive-socket) &key element-type &aux (lock (socket-lock socket)))
  (flet ((connection-established-p (stream)
	   (ccl::with-io-buffer-locked ((ccl::stream-io-buffer stream nil))
	     (let ((state (ccl::opentransport-stream-connection-state stream)))
	       (not (eq :unbnd state))))))
    (with-mapped-conditions ()
      (ccl:with-lock-grabbed (lock nil "Socket Lock")
	(let ((connection (shiftf (car (socket-streams socket))
				  (socket-open-listener socket element-type))))
	  (pop (socket-streams socket))
	  (ccl:process-wait "Accepting" #'connection-established-p connection)
	  connection)))))

(defmethod socket-close ((socket passive-socket))
  (loop
    with streams = (socket-streams socket)
    for (stream tail) on streams
    do (close stream :abort T)
    until (eq tail streams)
    finally (setf (socket-streams socket) NIL)))

(defmethod socket-open-listener (socket &optional element-type)
  ; see http://code.google.com/p/mcl/issues/detail?id=28
  (let* ((ccl::*passive-interface-address* (local-host socket))
         (new (ccl::open-tcp-stream NIL (or (local-port socket) #$kOTAnyInetAddress) 
                                    :reuse-local-port-p (reuse-address socket) 
                                    :element-type (if (subtypep (or element-type (element-type socket))
                                                                'character) 
                                                    'ccl::base-character 
                                                    'unsigned-byte))))
    (declare (special ccl::*passive-interface-address*))
    new))

(defmethod input-available-p ((stream ccl::opentransport-stream))
  (macrolet ((when-io-buffer-lock-grabbed ((lock &optional multiple-value-p) &body body)
	       "Evaluates the body if and only if the lock is successfully grabbed"
	       ;; like with-io-buffer-lock-grabbed but returns immediately instead of polling the lock
	       (let ((needs-unlocking-p (gensym))
		     (lock-var (gensym)))
		 `(let* ((,lock-var ,lock)
			 (ccl::*grabbed-io-buffer-locks* (cons ,lock-var ccl::*grabbed-io-buffer-locks*))
			 (,needs-unlocking-p (needs-unlocking-p ,lock-var)))
		    (declare (dynamic-extent ccl::*grabbed-io-buffer-locks*))
		    (when ,needs-unlocking-p
		      (,(if multiple-value-p 'multiple-value-prog1 'prog1)
                        (progn ,@body)
                        (ccl::%release-io-buffer-lock ,lock-var)))))))
    (labels ((needs-unlocking-p (lock)
	       (declare (type ccl::lock lock))
	       ;; crucial - clears bogus lock.value as in grab-io-buffer-lock-out-of-line:
	       (ccl::%io-buffer-lock-really-grabbed-p lock)
	       (ccl:store-conditional lock nil ccl:*current-process*)))
      "similar to stream-listen on buffered-input-stream-mixin but without waiting for lock"
      (let ((io-buffer (ccl::stream-io-buffer stream)))
	(or (not (eql 0 (ccl::io-buffer-incount io-buffer)))
	    (ccl::io-buffer-untyi-char io-buffer)
	    (locally (declare (optimize (speed 3) (safety 0)))
	      (when-io-buffer-lock-grabbed ((ccl::io-buffer-lock io-buffer))
       	        (funcall (ccl::io-buffer-listen-function io-buffer) stream io-buffer))))))))

(defmethod connection-established-p ((stream ccl::opentransport-stream))
  (ccl::with-io-buffer-locked ((ccl::stream-io-buffer stream nil))
    (let ((state (ccl::opentransport-stream-connection-state stream)))
      (not (eq :unbnd state)))))

(defun wait-for-input-internal (wait-list &key timeout &aux result)
  (labels ((ready-sockets (sockets)
	     (dolist (sock sockets result)
	       (when (cond ((stream-usocket-p sock)
			    (input-available-p (socket-stream sock)))
			   ((stream-server-usocket-p sock)
			    (let ((ot-stream (first (socket-streams (socket sock)))))
			      (or (input-available-p ot-stream)
				  (connection-established-p ot-stream)))))
		 (push sock result)))))
    (with-mapped-conditions ()
      (ccl:process-wait-with-timeout
       "socket input"
       (when timeout (truncate (* timeout 60)))
       #'ready-sockets
       (wait-list-waiters wait-list)))
    (nreverse result)))

;;; datagram socket methods

(defmethod initialize-instance :after ((usocket datagram-usocket) &key)
  (with-slots (socket send-buffer recv-buffer) usocket
    (setq send-buffer
	  (ccl::make-TUnitData (ccl::ot-conn-endpoint socket)))
    (setq recv-buffer
	  (ccl::make-TUnitData (ccl::ot-conn-endpoint socket)))))

(defmethod socket-send ((usocket datagram-usocket) buffer size &key host port (offset 0))
  (with-mapped-conditions (usocket)
    (with-slots (socket send-buffer) usocket
      (unless (and host port)
	(unsupported 'host 'socket-send))
      (ccl::send-message socket send-buffer buffer size host port offset))))

(defmethod socket-receive ((usocket datagram-usocket) buffer length &key)
  (with-mapped-conditions (usocket)
    (with-slots (socket recv-buffer) usocket
      (ccl::receive-message socket recv-buffer buffer length))))

(defmethod socket-close ((socket datagram-usocket))
  nil) ; TODO

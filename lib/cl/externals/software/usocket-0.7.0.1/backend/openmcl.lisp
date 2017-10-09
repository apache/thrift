;;;; See LICENSE for licensing information.

(in-package :usocket)

(defun get-host-name ()
  (ccl::%stack-block ((resultbuf 256))
    (when (zerop (#_gethostname resultbuf 256))
      (ccl::%get-cstring resultbuf))))

(defparameter +openmcl-error-map+
  '((:address-in-use . address-in-use-error)
    (:connection-aborted . connection-aborted-error)
    (:no-buffer-space . no-buffers-error)
    (:connection-timed-out . timeout-error)
    (:connection-refused . connection-refused-error)
    (:host-unreachable . host-unreachable-error)
    (:host-down . host-down-error)
    (:network-down . network-down-error)
    (:address-not-available . address-not-available-error)
    (:network-reset . network-reset-error)
    (:connection-reset . connection-reset-error)
    (:shutdown . shutdown-error)
    (:access-denied . operation-not-permitted-error)))

(defparameter +openmcl-nameserver-error-map+
  '((:no-recovery . ns-no-recovery-error)
    (:try-again . ns-try-again-condition)
    (:host-not-found . ns-host-not-found-error)))

;; we need something which the openmcl implementors 'forgot' to do:
;; wait for more than one socket-or-fd

(defun input-available-p (sockets &optional ticks-to-wait)
  (ccl::rletZ ((tv :timeval))
    (ccl::ticks-to-timeval ticks-to-wait tv)
    ;;### The trickery below can be moved to the wait-list now...
    (ccl::%stack-block ((infds ccl::*fd-set-size*))
      (ccl::fd-zero infds)
      (let ((max-fd -1))
        (dolist (sock sockets)
          (let ((fd (openmcl-socket:socket-os-fd (socket sock))))
            (when fd ;; may be NIL if closed
              (setf max-fd (max max-fd fd))
              (ccl::fd-set fd infds))))
        (let ((res (#_select (1+ max-fd)
                             infds (ccl::%null-ptr) (ccl::%null-ptr)
                             (if ticks-to-wait tv (ccl::%null-ptr)))))
          (when (> res 0)
            (dolist (sock sockets)
              (let ((fd (openmcl-socket:socket-os-fd (socket sock))))
                (when (and fd (ccl::fd-is-set fd infds))
                  (setf (state sock) :READ)))))
          sockets)))))

(defun raise-error-from-id (condition-id socket real-condition)
  (let ((usock-err (cdr (assoc condition-id +openmcl-error-map+))))
    (if usock-err
        (error usock-err :socket socket)
      (error 'unknown-error :socket socket :real-error real-condition))))

(defun handle-condition (condition &optional socket)
  (typecase condition
    (openmcl-socket:socket-error
       (raise-error-from-id (openmcl-socket:socket-error-identifier condition)
                            socket condition))
    (ccl:input-timeout
       (error 'timeout-error :socket socket))
    (ccl:communication-deadline-expired
       (error 'deadline-timeout-error :socket socket))
    (ccl::socket-creation-error #| ugh! |#
       (let* ((condition-id (ccl::socket-creation-error-identifier condition))
	      (nameserver-error (cdr (assoc condition-id
					    +openmcl-nameserver-error-map+))))
	 (if nameserver-error
             (if (typep nameserver-error 'serious-condition)
                 (error nameserver-error :host-or-ip nil)
                 (signal nameserver-error :host-or-ip nil))
	   (raise-error-from-id condition-id socket condition))))))

(defun to-format (element-type protocol)
  (cond ((null element-type)
	 (ecase protocol ; default value of different protocol
	   (:stream :text)
	   (:datagram :binary)))
	((subtypep element-type 'character)
	 :text)
	(t :binary)))

#-ipv6
(defun socket-connect (host port &key (protocol :stream) element-type
		       timeout deadline nodelay
		       local-host local-port)
  (when (eq nodelay :if-supported)
    (setf nodelay t))
  (with-mapped-conditions ()
    (ecase protocol
      (:stream
       (let ((mcl-sock
	      (openmcl-socket:make-socket :remote-host (host-to-hostname host)
					  :remote-port port
					  :local-host local-host
					  :local-port local-port
					  :format (to-format element-type protocol)
					  :external-format ccl:*default-external-format*
					  :deadline deadline
					  :nodelay nodelay
					  :connect-timeout timeout)))
	 (make-stream-socket :stream mcl-sock :socket mcl-sock)))
      (:datagram
       (let* ((mcl-sock
	       (openmcl-socket:make-socket :address-family :internet
					   :type :datagram
					   :local-host local-host
					   :local-port local-port
					   :input-timeout timeout
					   :format (to-format element-type protocol)
					   :external-format ccl:*default-external-format*))
	      (usocket (make-datagram-socket mcl-sock)))
	 (when (and host port)
	   (ccl::inet-connect (ccl::socket-device mcl-sock)
			      (ccl::host-as-inet-host host)
			      (ccl::port-as-inet-port port "udp")))
	 (setf (connected-p usocket) t)
	 usocket)))))

#-ipv6
(defun socket-listen (host port
                      &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  (let* ((reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
	 (real-host (host-to-hostname host))
         (sock (with-mapped-conditions ()
                  (apply #'openmcl-socket:make-socket
                         (append (list :connect :passive
                                       :reuse-address reuseaddress
                                       :local-port port
                                       :backlog backlog
                                       :format (to-format element-type :stream))
                                 (unless (eq host *wildcard-host*)
                                   (list :local-host real-host)))))))
    (make-stream-server-socket sock :element-type element-type)))

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (declare (ignore element-type)) ;; openmcl streams are bi/multivalent
  (let ((sock (with-mapped-conditions (usocket)
                 (openmcl-socket:accept-connection (socket usocket)))))
    (make-stream-socket :socket sock :stream sock)))

;; One close method is sufficient because sockets
;; and their associated objects are represented
;; by the same object.
(defmethod socket-close ((usocket usocket))
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (with-mapped-conditions (usocket)
    (close (socket usocket))))

(defmethod socket-shutdown ((usocket usocket) direction)
  (with-mapped-conditions (usocket)
    (openmcl-socket:shutdown (socket usocket) :direction direction)))

#-ipv6
(defmethod socket-send ((usocket datagram-usocket) buffer size &key host port (offset 0))
  (with-mapped-conditions (usocket)
    (if (and host port)
	(openmcl-socket:send-to (socket usocket) buffer size
				:remote-host (host-to-hbo host)
				:remote-port port
				:offset offset)
	;; Clozure CL's socket function SEND-TO doesn't support operations on connected UDP sockets,
	;; so we have to define our own.
	(let* ((socket (socket usocket))
	       (fd (ccl::socket-device socket)))
	  (multiple-value-setq (buffer offset)
	    (ccl::verify-socket-buffer buffer offset size))
	  (ccl::%stack-block ((bufptr size))
	    (ccl::%copy-ivector-to-ptr buffer offset bufptr 0 size)
	    (ccl::socket-call socket "send"
	      (ccl::with-eagain fd :output
		(ccl::ignoring-eintr
		  (ccl::check-socket-error (#_send fd bufptr size 0))))))))))

(defmethod socket-receive ((usocket datagram-usocket) buffer length &key)
  (with-mapped-conditions (usocket)
    (openmcl-socket:receive-from (socket usocket) length :buffer buffer)))

(defun usocket-host-address (address)
  (cond
    ((integerp address)
     (hbo-to-vector-quad address))
    ((and (arrayp address)
          (= (length address) 16)
          (every #'= address #(0 0 0 0 0 0 0 0 0 0 #xff #xff)))
     (make-array 4 :displaced-to address :displaced-index-offset 12))
    (t
     address)))

(defmethod get-local-address ((usocket usocket))
  (usocket-host-address (openmcl-socket:local-host (socket usocket))))

(defmethod get-peer-address ((usocket stream-usocket))
  (usocket-host-address (openmcl-socket:remote-host (socket usocket))))

(defmethod get-local-port ((usocket usocket))
  (openmcl-socket:local-port (socket usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (openmcl-socket:remote-port (socket usocket)))

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))

(defun get-host-by-address (address)
  (with-mapped-conditions ()
     (openmcl-socket:ipaddr-to-hostname (host-to-hbo address))))

(defun get-hosts-by-name (name)
  (with-mapped-conditions ()
     (list (hbo-to-vector-quad (openmcl-socket:lookup-hostname
                                (host-to-hostname name))))))

(defun %setup-wait-list (wait-list)
  (declare (ignore wait-list)))

(defun %add-waiter (wait-list waiter)
  (declare (ignore wait-list waiter)))

(defun %remove-waiter (wait-list waiter)
  (declare (ignore wait-list waiter)))

(defun wait-for-input-internal (wait-list &key timeout)
  (with-mapped-conditions ()
    (let* ((ticks-timeout (truncate (* (or timeout 1)
                                       ccl::*ticks-per-second*))))
      (input-available-p (wait-list-waiters wait-list)
			 (when timeout ticks-timeout))
      wait-list)))

;;; Helper functions for option.lisp

(defun get-socket-option-reuseaddr (socket)
  (ccl::int-getsockopt (ccl::socket-device socket)
                                  #$SOL_SOCKET #$SO_REUSEADDR))

(defun set-socket-option-reuseaddr (socket value)
  (ccl::int-setsockopt (ccl::socket-device socket)
                                  #$SOL_SOCKET #$SO_REUSEADDR value))

(defun get-socket-option-broadcast (socket)
  (ccl::int-getsockopt (ccl::socket-device socket)
                                  #$SOL_SOCKET #$SO_BROADCAST))

(defun set-socket-option-broadcast (socket value)
  (ccl::int-setsockopt (ccl::socket-device socket)
                                  #$SOL_SOCKET #$SO_BROADCAST value))

(defun get-socket-option-tcp-nodelay (socket)
  (ccl::int-getsockopt (ccl::socket-device socket)
                                  #$IPPROTO_TCP #$TCP_NODELAY))

(defun set-socket-option-tcp-nodelay (socket value)
  (ccl::int-setsockopt (ccl::socket-device socket)
                                  #$IPPROTO_TCP #$TCP_NODELAY value))

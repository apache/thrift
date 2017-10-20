;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

(defparameter +scl-error-map+
  (append +unix-errno-condition-map+
          +unix-errno-error-map+))

(defun scl-map-socket-error (err &key condition socket)
  (let ((usock-err (cdr (assoc err +scl-error-map+ :test #'member))))
    (cond (usock-err
       (if (subtypep usock-err 'error)
           (error usock-err :socket socket)
           (signal usock-err :socket socket)))
      (t
       (error 'unknown-error
          :socket socket
          :real-error condition)))))

(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (ext::socket-error
     (scl-map-socket-error (ext::socket-errno condition)
			   :socket socket
			   :condition condition))))

(defun socket-connect (host port &key (protocol :stream) (element-type 'character)
                       timeout deadline (nodelay t nodelay-specified)
		       (local-host nil local-host-p)
		       (local-port nil local-port-p)
		       &aux
		       (patch-udp-p (fboundp 'ext::inet-socket-send-to)))
  (when (and nodelay-specified 
             (not (eq nodelay :if-supported)))
    (unsupported 'nodelay 'socket-connect))
  (when deadline (unsupported 'deadline 'socket-connect))
  (when timeout (unsupported 'timeout 'socket-connect))
  (when (and local-host-p (not patch-udp-p))
     (unsupported 'local-host 'socket-connect :minimum "1.3.9"))
  (when (and local-port-p (not patch-udp-p))
     (unsupported 'local-port 'socket-connect :minimum "1.3.9"))

  (let ((socket))
    (ecase protocol
      (:stream
       (setf socket (let ((args (list (host-to-hbo host) port :kind protocol)))
		      (when (and patch-udp-p (or local-host-p local-port-p))
			(nconc args (list :local-host (when local-host
							(host-to-hbo local-host))
					  :local-port local-port)))
		      (with-mapped-conditions (socket)
			(apply #'ext:connect-to-inet-socket args))))
       (let ((stream (sys:make-fd-stream socket :input t :output t
					 :element-type element-type
					 :buffering :full)))
	 (make-stream-socket :socket socket :stream stream)))
      (:datagram
       (when (not patch-udp-p)
	 (error 'unsupported
		:feature '(protocol :datagram)
		:context 'socket-connect
		:minumum "1.3.9"))
       (setf socket
	     (if (and host port)
		 (let ((args (list (host-to-hbo host) port :kind protocol)))
		   (when (and patch-udp-p (or local-host-p local-port-p))
		     (nconc args (list :local-host (when local-host
						     (host-to-hbo local-host))
				       :local-port local-port)))
		   (with-mapped-conditions (socket)
		     (apply #'ext:connect-to-inet-socket args)))
		 (if (or local-host-p local-port-p)
		     (with-mapped-conditions ()
		       (ext:create-inet-listener (or local-port 0)
						 protocol
						 :host (when local-host
							 (if (ip= local-host *wildcard-host*)
							     0
							     (host-to-hbo local-host)))))
		     (with-mapped-conditions ()
		       (ext:create-inet-socket protocol)))))
       (let ((usocket (make-datagram-socket socket :connected-p (and host port t))))
	 (ext:finalize usocket #'(lambda ()
				   (when (%open-p usocket)
				     (ext:close-socket socket))))
	 usocket)))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  (let* ((reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
         (host (if (ip= host *wildcard-host*)
                   0
                 (host-to-hbo host)))
         (server-sock
          (with-mapped-conditions ()
            (ext:create-inet-listener port :stream
                                      :host host
                                      :reuse-address reuseaddress
                                      :backlog backlog))))
   (make-stream-server-socket server-sock :element-type element-type)))

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (with-mapped-conditions (usocket)
    (let* ((sock (ext:accept-tcp-connection (socket usocket)))
           (stream (sys:make-fd-stream sock :input t :output t
                                      :element-type (or element-type
                                                        (element-type usocket))
                                      :buffering :full)))
      (make-stream-socket :socket sock :stream stream))))

;; Sockets and their associated streams are modelled as
;; different objects. Be sure to close the socket stream
;; when closing stream-sockets; it makes sure buffers
;; are flushed and the socket is closed correctly afterwards.
(defmethod socket-close ((usocket usocket))
  "Close socket."
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (with-mapped-conditions (usocket)
    (ext:close-socket (socket usocket))))

(defmethod socket-close ((usocket stream-usocket))
  "Close socket."
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (with-mapped-conditions (usocket)
    (close (socket-stream usocket))))

(defmethod socket-close :after ((socket datagram-usocket))
  (setf (%open-p socket) nil))

(defmethod socket-shutdown ((usocket usocket) direction)
  (declare (ignore usocket direction))
  (unsupported "shutdown" 'socket-shutdown))

(defmethod socket-send ((usocket datagram-usocket) buffer size &key host port)
  (let ((s (socket usocket))
	(host (if host (host-to-hbo host)))
	(real-buffer (if (zerop offset)
			 buffer
			 (subseq buffer offset (+ offset size)))))
    (multiple-value-bind (result errno)
	(ext:inet-socket-send-to s real-buffer size
				 :remote-host host :remote-port port)
      (or result
	  (scl-map-socket-error errno :socket usocket)))))

(defmethod socket-receive ((socket datagram-usocket) buffer length &key)
  (declare (values (simple-array (unsigned-byte 8) (*)) ; buffer
		   (integer 0)                          ; size
		   (unsigned-byte 32)                   ; host
		   (unsigned-byte 16)))                 ; port
  (let ((s (socket socket)))
    (let ((real-buffer (or buffer
			   (make-array length :element-type '(unsigned-byte 8))))
	  (real-length (or length
			   (length buffer))))
      (multiple-value-bind (result errno remote-host remote-port)
	  (ext:inet-socket-receive-from s real-buffer real-length)
	(if result
	    (values real-buffer result remote-host remote-port)
	    (scl-map-socket-error errno :socket socket))))))

(defmethod get-local-name ((usocket usocket))
  (multiple-value-bind (address port)
      (with-mapped-conditions (usocket)
        (ext:get-socket-host-and-port (socket usocket)))
    (values (hbo-to-vector-quad address) port)))

(defmethod get-peer-name ((usocket stream-usocket))
  (multiple-value-bind (address port)
      (with-mapped-conditions (usocket)
        (ext:get-peer-host-and-port (socket usocket)))
    (values (hbo-to-vector-quad address) port)))

(defmethod get-local-address ((usocket usocket))
  (nth-value 0 (get-local-name usocket)))

(defmethod get-peer-address ((usocket stream-usocket))
  (nth-value 0 (get-peer-name usocket)))

(defmethod get-local-port ((usocket usocket))
  (nth-value 1 (get-local-name usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (nth-value 1 (get-peer-name usocket)))


(defun get-host-by-address (address)
  (multiple-value-bind (host errno)
      (ext:lookup-host-entry (host-byte-order address))
    (cond (host
           (ext:host-entry-name host))
          (t
           (let ((condition (cdr (assoc errno +unix-ns-error-map+))))
             (cond (condition
                    (error condition :host-or-ip address))
                   (t
                    (error 'ns-unknown-error :host-or-ip address
                           :real-error errno))))))))

(defun get-hosts-by-name (name)
  (multiple-value-bind (host errno)
      (ext:lookup-host-entry name)
    (cond (host
           (mapcar #'hbo-to-vector-quad
                   (ext:host-entry-addr-list host)))
          (t
           (let ((condition (cdr (assoc errno +unix-ns-error-map+))))
             (cond (condition
                    (error condition :host-or-ip name))
                   (t
                    (error 'ns-unknown-error :host-or-ip name
                           :real-error errno))))))))

(defun get-host-name ()
  (unix:unix-gethostname))


;;
;;
;;  WAIT-LIST part
;;


(defun %add-waiter (wl waiter)
  (declare (ignore wl waiter)))

(defun %remove-waiter (wl waiter)
  (declare (ignore wl waiter)))

(defun %setup-wait-list (wl)
  (declare (ignore wl)))

(defun wait-for-input-internal (wait-list &key timeout)
  (let* ((sockets (wait-list-waiters wait-list))
         (pollfd-size (alien:alien-size (alien:struct unix::pollfd) :bytes))
         (nfds (length sockets))
         (bytes (* nfds pollfd-size)))
    (alien:with-bytes (fds-sap bytes)
      (do ((sockets sockets (rest sockets))
          (base 0 (+ base 8)))
         ((endp sockets))
       (let ((fd (socket (first sockets))))
         (setf (sys:sap-ref-32 fds-sap base) fd)
         (setf (sys:sap-ref-16 fds-sap (+ base 4)) unix::pollin)))
      (multiple-value-bind (result errno)
         (let ((thread:*thread-whostate* "Poll wait")
               (timeout (if timeout
                            (truncate (* timeout 1000))
                            -1)))
           (declare (inline unix:unix-poll))
           (unix:unix-poll (alien:sap-alien fds-sap
                                            (* (alien:struct unix::pollfd)))
                           nfds timeout))
       (cond ((not result)
              (error "~@<Polling error: ~A~:@>"
                     (unix:get-unix-error-msg errno)))
             (t
              (do ((sockets sockets (rest sockets))
                   (base 0 (+ base 8)))
                  ((endp sockets))
                (let ((flags (sys:sap-ref-16 fds-sap (+ base 6))))
                  (unless (zerop (logand flags unix::pollin))
                    (setf (state (first sockets)) :READ))))))))))


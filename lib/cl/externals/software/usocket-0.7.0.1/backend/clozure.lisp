;;;; See LICENSE for licensing information.

;;;; Functions for CCL 1.11 (IPv6) only, see openmcl.lisp for rest of functions.

(in-package :usocket)

#+ipv6
(defun socket-connect (host port &key (protocol :stream) element-type
                                   timeout deadline nodelay
                                   local-host local-port)
  (when (eq nodelay :if-supported)
    (setf nodelay t))
  (with-mapped-conditions ()
    (let* ((remote (when (and host port)
		     (openmcl-socket:resolve-address :host (host-to-hostname host)
						     :port port
						     :socket-type protocol)))
	   (local  (when (and local-host local-port)
		     (openmcl-socket:resolve-address :host (host-to-hostname local-host)
						     :port local-port
						     :socket-type protocol)))
	   (mcl-sock (apply #'openmcl-socket:make-socket
			    `(:type ,protocol
			      ,@(when (or remote local)
				  `(:address-family ,(openmcl-socket:socket-address-family (or remote local))))
			      ,@(when remote
				  `(:remote-address ,remote))
			      ,@(when local
				  `(:local-address ,local))
			      :format ,(to-format element-type protocol)
			      :external-format ,ccl:*default-external-format*
			      :deadline ,deadline
			      :nodelay ,nodelay
			      :connect-timeout ,timeout
			      :input-timeout ,timeout))))
      (ecase protocol
        (:stream
         (make-stream-socket :stream mcl-sock :socket mcl-sock))
        (:datagram
         (make-datagram-socket mcl-sock :connected-p (and remote t)))))))

#+ipv6
(defun socket-listen (host port
                      &key
                        (reuse-address nil reuse-address-supplied-p)
                        (reuseaddress (when reuse-address-supplied-p reuse-address))
                        (backlog 5)
                        (element-type 'character))
  (let ((local-address (openmcl-socket:resolve-address :host (host-to-hostname host)
						       :port port :connect :passive)))
    (with-mapped-conditions ()
      (make-stream-server-socket
        (openmcl-socket:make-socket :connect :passive
				    :address-family (openmcl-socket:socket-address-family local-address)
				    :local-address local-address
				    :reuse-address reuseaddress
				    :backlog backlog
				    :format (to-format element-type :stream))
	:element-type element-type))))

#+ipv6
(defmethod socket-send ((usocket datagram-usocket) buffer size &key host port (offset 0))
  (let* ((ccl-socket (socket usocket))
	 (socket-keys (ccl::socket-keys ccl-socket)))
    (with-mapped-conditions (usocket)
      (if (and host port)
	  (openmcl-socket:send-to ccl-socket buffer size
				  :remote-host (host-to-hostname host)
				  :remote-port port
				  :offset offset)
	  (openmcl-socket:send-to ccl-socket buffer size
				  :remote-address (getf socket-keys :remote-address)
				  :offset offset)))))

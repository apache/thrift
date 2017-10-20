;;;-*-Mode: LISP; Package: CCL -*-
;;
;;; OpenTransportUDP.lisp
;;; Copyright 2012 Chun Tian (binghe) <binghe.lisp@gmail.com>

;;; UDP extension to OpenTransport.lisp (with some TCP patches)

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :opentransport))

;; MCL Issue 28: Passive TCP streams should be able to listen to the loopback interface
;; see http://code.google.com/p/mcl/issues/detail?id=28 for details

(defparameter *passive-interface-address* NIL
  "Address to use for passive connections - optionally bind to loopback address while opening a tcp stream")

(advise local-interface-ip-address
  (or *passive-interface-address* (:do-it))
  :when :around :name 'override-local-interface-ip-address)

;; MCL Issue 29: Passive TCP connections on OS assigned ports
;; see http://code.google.com/p/mcl/issues/detail?id=29 for details
(advise ot-conn-tcp-passive-connect
  (destructuring-bind (conn port &optional (allow-reuse t)) arglist
    (declare (ignore allow-reuse))
    (if (eql port #$kOTAnyInetAddress)
	;; Avoids registering a proxy for port 0 but instead registers one for the true port:
	(multiple-value-bind (proxy result)
	    (let* ((*opentransport-class-proxies* NIL) ; makes ot-find-proxy return NIL
		   (result (:do-it)) ;; pushes onto *opentransport-class-proxies*
		   (proxy (prog1
			      (pop *opentransport-class-proxies*)
			    (assert (not *opentransport-class-proxies*))))
		   (context (cdr proxy))
		   (tmpconn (make-ot-conn :context context 
					  :endpoint (pref context :ot-context.ref)))
		   (localaddress (ot-conn-tcp-get-addresses tmpconn)))
	      (declare (dynamic-extent tmpconn))
	      ;; replace original set in body of function
	      (setf (ot-conn-local-address conn) localaddress)
	      (values
	       (cons localaddress context)
	       result))
	  ;; need to be outside local binding of *opentransport-class-proxies* 
	  (without-interrupts
	      (push proxy *opentransport-class-proxies*))
	  result)
	(:do-it)))
  :when :around :name 'ot-conn-tcp-passive-connect-any-address)

(defun open-udp-socket (&key local-address local-port)
  (init-opentransport)
  (let (endpoint ; TODO: opentransport-alloc-endpoint-from-freelist
	(err #$kOTNoError)
	(configptr (ot-cloned-configuration traps::$kUDPName)))
    (rlet ((errP :osstatus))
      (setq endpoint #+carbon-compat (#_OTOpenEndpointInContext configptr 0 (%null-ptr) errP *null-ptr*)
	             #-carbon-compat (#_OTOpenEndpoint configptr 0 (%null-ptr) errP)
	    err (pref errP :osstatus))
      (if (eql err #$kOTNoError)
	  (let* ((context (ot-make-endpoint-context endpoint nil nil)) ; no notifier, not minimal
		 (conn (make-ot-conn :context context :endpoint endpoint)))
	    (macrolet ((check-ot-error-return (error-context)
			 `(unless (eql (setq err (pref errP :osstatus)) #$kOTNoError)
			    (values (ot-error err ,error-context)))))
	      (setf (ot-conn-bindreq conn) 
		    #-carbon-compat (#_OTAlloc endpoint #$T_BIND #$T_ADDR errP)
		    #+carbon-compat (#_OTAllocInContext endpoint #$T_BIND #$T_ADDR errP *null-ptr*)
		    )
	      (check-ot-error-return :alloc)
	      (setf (ot-conn-bindret conn) 
		    #-carbon-compat (#_OTAlloc endpoint #$T_BIND #$T_ADDR errP)
		    #+carbon-compat (#_OTAllocInContext endpoint #$T_BIND #$T_ADDR errP *null-ptr*)
		    )
	      (check-ot-error-return :alloc)
	      (setf (ot-conn-options conn) 
		    #-carbon-compat (#_OTAlloc endpoint #$T_OPTMGMT #$T_OPT errP)
		    #+carbon-compat (#_OTAllocInContext endpoint #$T_OPTMGMT #$T_OPT errP *null-ptr*)
		    )
	      (check-ot-error-return :alloc))
	    ;; BIND to local address (for UDP server)
	    (when local-port ; local-address
	      (let* ((host (or local-address (local-interface-ip-address)))
		     (port (tcp-service-port-number local-port))
		     (localaddress `(:tcp ,host ,port))
		     (bindreq (ot-conn-bindreq conn))
		     (bindret (ot-conn-bindret conn)))
		(let* ((netbuf (pref bindreq :tbind.addr)))
		  (declare (dynamic-extent netbuf))
		  (setf (pref netbuf :tnetbuf.len) (record-length :inetaddress)
			(pref bindreq :tbind.qlen) 5)       ; arbitrary qlen
		  (#_OTInitInetAddress (pref netbuf :tnetbuf.buf) port host)
		  (setf (pref context :ot-context.completed) nil)
		  (unless (= (setq err (#_OTBind endpoint bindreq bindret)) #$kOTNoError)
		    (ot-error err :bind)))
		(setf (ot-conn-local-address conn) localaddress)))
	    conn)
	(ot-error err :create)))))

(defun make-TUnitData (endpoint)
  "create the send/recv buffer for UDP sockets"
  (let ((err #$kOTNoError))
    (rlet ((errP :osstatus))
      (macrolet ((check-ot-error-return (error-context)
		   `(unless (eql (setq err (pref errP :osstatus)) #$kOTNoError)
		      (values (ot-error err ,error-context)))))
	(let ((udata #-carbon-compat (#_OTAlloc endpoint #$T_UNITDATA #$T_ALL errP)
		     #+carbon-compat (#_OTAllocInContext endpoint #$T_UNITDATA #$T_ALL errP *null-ptr*)))
	  (check-ot-error-return :alloc)
	  udata)))))

(defun send-message (conn data buffer size host port &optional (offset 0))
  ;; prepare dest address
  (let ((addr (pref data :tunitdata.addr)))
    (declare (dynamic-extent addr))
    (setf (pref addr :tnetbuf.len) (record-length :inetaddress))
    (#_OTInitInetAddress (pref addr :tnetbuf.buf) port host))
  ;; prepare data buffer
  (let* ((udata (pref data :tunitdata.udata))
	 (outptr (pref udata :tnetbuf.buf)))
    (declare (dynamic-extent udata))
    (%copy-ivector-to-ptr buffer offset outptr 0 size)
    (setf (pref udata :tnetbuf.len) size))
  ;; send the packet
  (let* ((endpoint (ot-conn-endpoint conn))
	 (result (#_OTSndUData endpoint data)))
    (the fixnum result)))

(defun receive-message (conn data buffer length)
  (let* ((endpoint (ot-conn-endpoint conn))
	 (err (#_OTRcvUData endpoint data *null-ptr*)))
    (if (eql err #$kOTNoError)
	(let* (;(addr (pref data :tunitdata.addr))
	       (udata (pref data :tunitdata.udata))
	       (inptr (pref udata :tnetbuf.buf))
	       (read-bytes (pref udata :tnetbuf.len))
	       (buffer (or buffer (make-array read-bytes :element-type '(unsigned-byte 8))))
	       (length (or length (length buffer)))
	       (actual-size (min read-bytes length)))
	  (%copy-ptr-to-ivector inptr 0 buffer 0 actual-size)
	  (values buffer
		  actual-size
		  0 0)) ; TODO: retrieve address and port
      (ot-error err :receive)))) ; TODO: use OTRcvUDErr instead

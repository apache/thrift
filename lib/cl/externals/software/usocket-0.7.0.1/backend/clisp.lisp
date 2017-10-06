;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-ffi
  (warn "This image doesn't contain FFI package, GET-HOST-NAME won't work.")
  #-(or ffi rawsock)
  (warn "This image doesn't contain either FFI or RAWSOCK package, no UDP support."))

;; utility routine for looking up the current host name
#+ffi
(ffi:def-call-out get-host-name-internal
         (:name "gethostname")
         (:arguments (name (FFI:C-PTR (FFI:C-ARRAY-MAX ffi:character 256))
                           :OUT :ALLOCA)
                     (len ffi:int))
         #+win32 (:library "WS2_32")
	 #-win32 (:library :default)
         (:language #-win32 :stdc
                    #+win32 :stdc-stdcall)
         (:return-type ffi:int))

(defun get-host-name ()
  #+ffi
  (multiple-value-bind (retcode name)
      (get-host-name-internal 256)
    (when (= retcode 0)
      name))
  #-ffi
  "localhost")

(defun get-host-by-address (address)
  (with-mapped-conditions ()
    (let ((hostent (posix:resolve-host-ipaddr (host-to-hostname address))))
      (posix:hostent-name hostent))))

(defun get-hosts-by-name (name)
  (with-mapped-conditions ()
    (let ((hostent (posix:resolve-host-ipaddr name)))
      (mapcar #'host-to-vector-quad
              (posix:hostent-addr-list hostent)))))

;; Format: ((UNIX Windows) . CONDITION)
(defparameter +clisp-error-map+
  #-win32
  `((:EADDRINUSE . address-in-use-error)
    (:EADDRNOTAVAIL . address-not-available-error)
    (:EBADF . bad-file-descriptor-error)
    (:ECONNREFUSED  . connection-refused-error)
    (:ECONNRESET . connection-reset-error)
    (:ECONNABORTED . connection-aborted-error)
    (:EINVAL . invalid-argument-error)
    (:ENOBUFS . no-buffers-error)
    (:ENOMEM . out-of-memory-error)
    (:ENOTSUP . operation-not-supported-error)
    (:EPERM . operation-not-permitted-error)
    (:EPROTONOSUPPORT . protocol-not-supported-error)
    (:ESOCKTNOSUPPORT . socket-type-not-supported-error)
    (:ENETUNREACH . network-unreachable-error)
    (:ENETDOWN . network-down-error)
    (:ENETRESET . network-reset-error)
    (:ESHUTDOWN . already-shutdown-error)
    (:ETIMEDOUT . timeout-error)
    (:EHOSTDOWN . host-down-error)
    (:EHOSTUNREACH . host-unreachable-error))
  #+win32
  `((:WSAEADDRINUSE . address-in-use-error)
    (:WSAEADDRNOTAVAIL . address-not-available-error)
    (:WSAEBADF . bad-file-descriptor-error)
    (:WSAECONNREFUSED  . connection-refused-error)
    (:WSAECONNRESET . connection-reset-error)
    (:WSAECONNABORTED . connection-aborted-error)
    (:WSAEINVAL . invalid-argument-error)
    (:WSAENOBUFS . no-buffers-error)
    (:WSAENOMEM . out-of-memory-error)
    (:WSAENOTSUP . operation-not-supported-error)
    (:WSAEPERM . operation-not-permitted-error)
    (:WSAEPROTONOSUPPORT . protocol-not-supported-error)
    (:WSAESOCKTNOSUPPORT . socket-type-not-supported-error)
    (:WSAENETUNREACH . network-unreachable-error)
    (:WSAENETDOWN . network-down-error)
    (:WSAENETRESET . network-reset-error)
    (:WSAESHUTDOWN . already-shutdown-error)
    (:WSAETIMEDOUT . timeout-error)
    (:WSAEHOSTDOWN . host-down-error)
    (:WSAEHOSTUNREACH . host-unreachable-error)))

(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (let (error-keyword error-string)
    (typecase condition
      (ext:os-error
       (let ((errno (car (simple-condition-format-arguments condition))))
	 #+ffi
	 (setq error-keyword (os:errno errno)
	       error-string (os:strerror errno))))
      (simple-error
       (let ((keyword
	      (car (simple-condition-format-arguments condition))))
	 (setq error-keyword keyword)
	 #+ffi
	 (setq error-string (os:strerror keyword))))
      (error (error 'unknown-error :real-error condition))
      (condition (signal 'unknown-condition :real-condition condition)))
    (when error-keyword
      (let ((usocket-error
	     (cdr (assoc error-keyword +clisp-error-map+ :test #'eq))))
	(if usocket-error
	    (if (subtypep usocket-error 'error)
		(error usocket-error :socket socket)
		(signal usocket-error :socket socket))
	    (error "Unknown OS error: ~A (~A)" error-string error-keyword))))))

(defun socket-connect (host port &key (protocol :stream) (element-type 'character)
                       timeout deadline (nodelay t nodelay-specified)
                       local-host local-port)
  (declare (ignorable timeout local-host local-port))
  (when deadline (unsupported 'deadline 'socket-connect))
  (when (and nodelay-specified 
             (not (eq nodelay :if-supported)))
    (unsupported 'nodelay 'socket-connect))
  (case protocol
    (:stream
     (let ((socket)
	   (hostname (host-to-hostname host)))
       (with-mapped-conditions (socket)
	 (setf socket
	       (if timeout
		   (socket:socket-connect port hostname
					  :element-type element-type
					  :buffered t
					  :timeout timeout)
		   (socket:socket-connect port hostname
					  :element-type element-type
					  :buffered t))))
       (make-stream-socket :socket socket
			   :stream socket))) ;; the socket is a stream too
    (:datagram
     #+(or rawsock ffi)
     (socket-create-datagram (or local-port *auto-port*)
			     :local-host (or local-host *wildcard-host*)
			     :remote-host (and host (host-to-vector-quad host))
			     :remote-port port)
     #-(or rawsock ffi)
     (unsupported '(protocol :datagram) 'socket-connect))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  ;; clisp 2.39 sets SO_REUSEADDRESS to 1 by default; no need to
  ;; to explicitly turn it on; unfortunately, there's no way to turn it off...
  (declare (ignore reuseaddress reuse-address reuse-address-supplied-p))
  (let ((sock (apply #'socket:socket-server
                     (append (list port
                                   :backlog backlog)
                             (when (ip/= host *wildcard-host*)
                               (list :interface host))))))
    (with-mapped-conditions ()
        (make-stream-server-socket sock :element-type element-type))))

(defmethod socket-accept ((socket stream-server-usocket) &key element-type)
  (let ((stream
         (with-mapped-conditions (socket)
           (socket:socket-accept (socket socket)
                                 :element-type (or element-type
                                                   (element-type socket))))))
    (make-stream-socket :socket stream
                        :stream stream)))

;; Only one close method required:
;; sockets and their associated streams
;; are the same object
(defmethod socket-close ((usocket usocket))
  "Close socket."
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (with-mapped-conditions (usocket)
    (close (socket usocket))))

(defmethod socket-close ((usocket stream-server-usocket))
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (socket:socket-server-close (socket usocket)))

(defmethod socket-shutdown ((usocket stream-usocket) direction)
  (with-mapped-conditions (usocket)
    (socket:socket-stream-shutdown (socket usocket) direction)))

(defmethod get-local-name ((usocket stream-usocket))
  (multiple-value-bind
      (address port)
      (socket:socket-stream-local (socket usocket) t)
    (values (dotted-quad-to-vector-quad address) port)))

(defmethod get-local-name ((usocket stream-server-usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (multiple-value-bind
      (address port)
      (socket:socket-stream-peer (socket usocket) t)
    (values (dotted-quad-to-vector-quad address) port)))

(defmethod get-local-address ((usocket usocket))
  (nth-value 0 (get-local-name usocket)))

(defmethod get-local-address ((usocket stream-server-usocket))
  (dotted-quad-to-vector-quad
   (socket:socket-server-host (socket usocket))))

(defmethod get-peer-address ((usocket usocket))
  (nth-value 0 (get-peer-name usocket)))

(defmethod get-local-port ((usocket usocket))
  (nth-value 1 (get-local-name usocket)))

(defmethod get-local-port ((usocket stream-server-usocket))
  (socket:socket-server-port (socket usocket)))

(defmethod get-peer-port ((usocket usocket))
  (nth-value 1 (get-peer-name usocket)))

(defun %setup-wait-list (wait-list)
  (declare (ignore wait-list)))

(defun %add-waiter (wait-list waiter)
  (push (cons (socket waiter) NIL) (wait-list-%wait wait-list)))

(defun %remove-waiter (wait-list waiter)
  (setf (wait-list-%wait wait-list)
        (remove (socket waiter) (wait-list-%wait wait-list) :key #'car)))

(defmethod wait-for-input-internal (wait-list &key timeout)
  (with-mapped-conditions ()
    (multiple-value-bind
        (secs musecs)
        (split-timeout (or timeout 1))
      (dolist (x (wait-list-%wait wait-list))
        (setf (cdr x) :INPUT))
      (let* ((request-list (wait-list-%wait wait-list))
             (status-list (if timeout
                              (socket:socket-status request-list secs musecs)
                            (socket:socket-status request-list)))
             (sockets (wait-list-waiters wait-list)))
        (do* ((x (pop sockets) (pop sockets))
              (y (pop status-list) (pop status-list)))
             ((null x))
          (when (member y '(T :INPUT))
            (setf (state x) :READ)))
        wait-list))))

;;;
;;; UDP/Datagram sockets (RAWSOCK version)
;;;

#+rawsock
(progn
  (defun make-sockaddr_in ()
    (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))

  (declaim (inline fill-sockaddr_in))
  (defun fill-sockaddr_in (sockaddr_in ip port)
    (port-to-octet-buffer port sockaddr_in)
    (ip-to-octet-buffer ip sockaddr_in :start 2)
    sockaddr_in)

  (defun socket-create-datagram (local-port
                                 &key (local-host *wildcard-host*)
                                      remote-host
                                      remote-port)
    (let ((sock (rawsock:socket :inet :dgram 0))
          (lsock_addr (fill-sockaddr_in (make-sockaddr_in)
                                        local-host local-port))
          (rsock_addr (when remote-host
                        (fill-sockaddr_in (make-sockaddr_in)
                                          remote-host (or remote-port
                                                          local-port)))))
      (rawsock:bind sock (rawsock:make-sockaddr :inet lsock_addr))
      (when rsock_addr
        (rawsock:connect sock (rawsock:make-sockaddr :inet rsock_addr)))
      (make-datagram-socket sock :connected-p (if rsock_addr t nil))))

  (defmethod socket-receive ((socket datagram-usocket) buffer length &key)
    "Returns the buffer, the number of octets copied into the buffer (received)
and the address of the sender as values."
    (let* ((sock (socket socket))
           (sockaddr (rawsock:make-sockaddr :inet))
           (real-length (or length +max-datagram-packet-size+))
           (real-buffer (or buffer
                            (make-array real-length
                                        :element-type '(unsigned-byte 8)))))
      (let ((rv (rawsock:recvfrom sock real-buffer sockaddr
                                 :start 0 :end real-length))
            (host 0) (port 0))
        (unless (connected-p socket)
          (let ((data (rawsock:sockaddr-data sockaddr)))
            (setq host (ip-from-octet-buffer data :start 4)
                  port (port-from-octet-buffer data :start 2))))
        (values (if buffer real-buffer (subseq real-buffer 0 rv))
                rv
                host
                port))))

  (defmethod socket-send ((socket datagram-usocket) buffer size &key host port (offset 0))
    "Returns the number of octets sent."
    (let* ((sock (socket socket))
           (sockaddr (when (and host port)
                       (rawsock:make-sockaddr :inet
                                              (fill-sockaddr_in
                                               (make-sockaddr_in)
                                               (host-byte-order host)
                                               port))))
           (real-size (min size +max-datagram-packet-size+))
           (real-buffer (if (typep buffer '(simple-array (unsigned-byte 8) (*)))
                            buffer
                          (make-array real-size
                                      :element-type '(unsigned-byte 8)
                                      :initial-contents (subseq buffer 0 real-size))))
           (rv (if (and host port)
                   (rawsock:sendto sock real-buffer sockaddr
                                   :start offset
                                   :end (+ offset real-size))
                   (rawsock:send sock real-buffer
                                 :start offset
                                 :end (+ offset real-size)))))
      rv))

  (defmethod socket-close ((usocket datagram-usocket))
    (when (wait-list usocket)
       (remove-waiter (wait-list usocket) usocket))
    (rawsock:sock-close (socket usocket)))

  (declaim (inline get-socket-name))
  (defun get-socket-name (socket function)
    (let ((sockaddr (rawsock:make-sockaddr :inet (make-sockaddr_in))))
      (funcall function socket sockaddr)
      (let ((data (rawsock:sockaddr-data sockaddr)))
        (values (hbo-to-vector-quad (ip-from-octet-buffer data :start 2))
                (port-from-octet-buffer data :start 0)))))

  (defmethod get-local-name ((usocket datagram-usocket))
    (get-socket-name (socket usocket) 'rawsock:getsockname))

  (defmethod get-peer-name ((usocket datagram-usocket))
    (get-socket-name (socket usocket) 'rawsock:getpeername))

) ; progn

;;;
;;; UDP/Datagram sockets (FFI version)
;;;

#+(and ffi (not rawsock))
(progn
  ;; C primitive types
  (ffi:def-c-type socklen_t ffi:uint32)

  ;; C structures
  (ffi:def-c-struct sockaddr
    #+macos (sa_len ffi:uint8)
    (sa_family  #-macos ffi:ushort
		#+macos ffi:uint8)
    (sa_data    (ffi:c-array ffi:char 14)))

  (ffi:def-c-struct sockaddr_in
    #+macos (sin_len ffi:uint8)
    (sin_family #-macos ffi:short
		#+macos ffi:uint8)
    (sin_port   #-macos ffi:ushort
		#+macos ffi:uint16)
    (sin_addr   ffi:uint32)
    (sin_zero   (ffi:c-array ffi:char 8)))

  (ffi:def-c-struct timeval
    (tv_sec     ffi:long)
    (tv_usec    ffi:long))

  ;; foreign functions
  (ffi:def-call-out %sendto (:name "sendto")
    (:arguments (socket ffi:int)
		(buffer ffi:c-pointer)
		(length ffi:int)
		(flags ffi:int)
		(address (ffi:c-ptr sockaddr))
		(address-len ffi:int))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %send (:name "send")
    (:arguments (socket ffi:int)
		(buffer ffi:c-pointer)
		(length ffi:int)
		(flags ffi:int))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %recvfrom (:name "recvfrom")
    (:arguments (socket ffi:int)
		(buffer ffi:c-pointer)
		(length ffi:int)
		(flags ffi:int)
		(address (ffi:c-ptr sockaddr) :in-out)
		(address-len (ffi:c-ptr ffi:int) :in-out))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %socket (:name "socket")
    (:arguments (family ffi:int)
		(type ffi:int)
		(protocol ffi:int))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %connect (:name "connect")
    (:arguments (socket ffi:int)
		(address (ffi:c-ptr sockaddr) :in)
		(address_len socklen_t))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %bind (:name "bind")
    (:arguments (socket ffi:int)
		(address (ffi:c-ptr sockaddr) :in)
		(address_len socklen_t))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %close (:name #-win32 "close" #+win32 "closesocket")
    (:arguments (socket ffi:int))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %getsockopt (:name "getsockopt")
    (:arguments (sockfd ffi:int)
		(level ffi:int)
		(optname ffi:int)
		(optval ffi:c-pointer)
		(optlen (ffi:c-ptr socklen_t) :out))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %setsockopt (:name "setsockopt")
    (:arguments (sockfd ffi:int)
		(level ffi:int)
		(optname ffi:int)
		(optval ffi:c-pointer)
		(optlen socklen_t))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %htonl (:name "htonl")
    (:arguments (hostlong ffi:uint32))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:uint32))

  (ffi:def-call-out %htons (:name "htons")
    (:arguments (hostshort ffi:uint16))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:uint16))

  (ffi:def-call-out %ntohl (:name "ntohl")
    (:arguments (netlong ffi:uint32))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:uint32))

  (ffi:def-call-out %ntohs (:name "ntohs")
    (:arguments (netshort ffi:uint16))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:uint16))

  (ffi:def-call-out %getsockname (:name "getsockname")
    (:arguments (sockfd ffi:int)
		(localaddr (ffi:c-ptr sockaddr) :in-out)
		(addrlen (ffi:c-ptr socklen_t) :in-out))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %getpeername (:name "getpeername")
    (:arguments (sockfd ffi:int)
		(peeraddr (ffi:c-ptr sockaddr) :in-out)
		(addrlen (ffi:c-ptr socklen_t) :in-out))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  ;; socket constants
  (defconstant +socket-af-inet+ 2)
  (defconstant +socket-sock-dgram+ 2)
  (defconstant +socket-ip-proto-udp+ 17)

  (defconstant +sockopt-so-rcvtimeo+ #-linux #x1006 #+linux 20 "Socket receive timeout")

  (defparameter *length-of-sockaddr_in* (ffi:sizeof 'sockaddr_in))

  (declaim (inline fill-sockaddr_in))
  (defun fill-sockaddr_in (sockaddr host port)
    (let ((hbo (host-to-hbo host)))
      (ffi:with-c-place (place sockaddr)
	#+macos
	(setf (ffi:slot place 'sin_len) *length-of-sockaddr_in*)
	(setf (ffi:slot place 'sin_family) +socket-af-inet+
	      (ffi:slot place 'sin_port) (%htons port)
	      (ffi:slot place 'sin_addr) (%htonl hbo)))
      sockaddr))

  (defun socket-create-datagram (local-port
				 &key (local-host *wildcard-host*)
				      remote-host
				      remote-port)
    (let ((sock (%socket +socket-af-inet+ +socket-sock-dgram+ +socket-ip-proto-udp+))
	  (lsock_addr (fill-sockaddr_in (ffi:allocate-shallow 'sockaddr_in)
					local-host local-port))
	  (rsock_addr (when remote-host
			(fill-sockaddr_in (ffi:allocate-shallow 'sockaddr_in)
					  remote-host (or remote-port local-port)))))
      (unless (plusp sock)
	(error "SOCKET-CREATE-DATAGRAM ERROR (socket): ~A" (os:errno)))
      (unwind-protect
	   (let ((rv (%bind sock (ffi:cast (ffi:foreign-value lsock_addr) 'sockaddr)
			    *length-of-sockaddr_in*)))
	     (unless (zerop rv)
	       (error "SOCKET-CREATE-DATAGRAM ERROR (bind): ~A" (os:errno)))
	     (when rsock_addr
	       (let ((rv (%connect sock
				   (ffi:cast (ffi:foreign-value rsock_addr) 'sockaddr)
				   *length-of-sockaddr_in*)))
		 (unless (zerop rv)
		   (error "SOCKET-CREATE-DATAGRAM ERROR (connect): ~A" (os:errno))))))
	(ffi:foreign-free lsock_addr)
	(when remote-host
	  (ffi:foreign-free rsock_addr)))
      (make-datagram-socket sock :connected-p (if rsock_addr t nil))))

  (defun finalize-datagram-usocket (object)
    (when (datagram-usocket-p object)
      (socket-close object)))

  (defmethod initialize-instance :after ((usocket datagram-usocket) &key)
    (setf (slot-value usocket 'recv-buffer)
	  (ffi:allocate-shallow 'ffi:uint8 :count +max-datagram-packet-size+))
    ;; finalize the object
    (ext:finalize usocket 'finalize-datagram-usocket))

  (defmethod socket-close ((usocket datagram-usocket))
    (when (wait-list usocket)
      (remove-waiter (wait-list usocket) usocket))
    (with-slots (recv-buffer socket) usocket
      (ffi:foreign-free recv-buffer)
      (zerop (%close socket))))

  (defmethod socket-receive ((usocket datagram-usocket) buffer length &key)
    (let ((remote-address (ffi:allocate-shallow 'sockaddr_in))
	  (remote-address-length (ffi:allocate-shallow 'ffi:int))
	  nbytes (host 0) (port 0))
      (setf (ffi:foreign-value remote-address-length)
	    *length-of-sockaddr_in*)
      (unwind-protect
	   (multiple-value-bind (n address address-length)
	       (%recvfrom (socket usocket)
			  (ffi:foreign-address (slot-value usocket 'recv-buffer))
			  +max-datagram-packet-size+
			  0 ; flags
			  (ffi:cast (ffi:foreign-value remote-address) 'sockaddr)
			  (ffi:foreign-value remote-address-length))
	     (when (minusp n)
	       (error "SOCKET-RECEIVE ERROR: ~A" (os:errno)))
	     (setq nbytes n)
	     (when (= address-length *length-of-sockaddr_in*)
	       (let ((data (sockaddr-sa_data address)))
		 (setq host (ip-from-octet-buffer data :start 2)
		       port (port-from-octet-buffer data))))
	     (cond ((plusp n)
		    (let ((return-buffer (ffi:foreign-value (slot-value usocket 'recv-buffer))))
		      (if buffer ; replace exist buffer of create new return buffer
			  (let ((end-1 (min (or length (length buffer)) +max-datagram-packet-size+))
				(end-2 (min n +max-datagram-packet-size+)))
			    (replace buffer return-buffer :end1 end-1 :end2 end-2))
			  (setq buffer (subseq return-buffer 0 (min n +max-datagram-packet-size+))))))
		   ((zerop n))))
	(ffi:foreign-free remote-address)
	(ffi:foreign-free remote-address-length))
      (values buffer nbytes host port)))

  ;; implementation note: different from socket-receive, we know how many bytes we want to send everytime,
  ;; so, a send buffer will not needed, and if there is a buffer, it's hard to fill its content like those
  ;; in LispWorks. So, we allocate new foreign buffer for holding data (unknown sequence subtype) every time.
  ;; 
  ;; I don't know if anyone is watching my coding work, but I think this design is reasonable for CLISP.
  (defmethod socket-send ((usocket datagram-usocket) buffer size &key host port (offset 0))
    (declare (type sequence buffer)
	     (type (integer 0 *) size offset))
    (let ((remote-address
	   (when (and host port)
	     (fill-sockaddr_in (ffi:allocate-shallow 'sockaddr_in) host port)))
	  (send-buffer
	   (ffi:allocate-deep 'ffi:uint8
			      (if (zerop offset)
				  buffer
				  (subseq buffer offset (+ offset size)))
			      :count size :read-only t))
	  (real-size (min size +max-datagram-packet-size+))
	  (nbytes 0))
      (unwind-protect
	   (let ((n (if remote-address
			(%sendto (socket usocket)
				 (ffi:foreign-address send-buffer)
				 real-size
				 0 ; flags
				 (ffi:cast (ffi:foreign-value remote-address) 'sockaddr)
				 *length-of-sockaddr_in*)
			(%send (socket usocket)
			       (ffi:foreign-address send-buffer)
			       real-size
			       0))))
	     (cond ((plusp n)
		    (setq nbytes n))
		   ((zerop n)
		    (setq nbytes n))
		   (t (error "SOCKET-SEND ERROR: ~A" (os:errno)))))
	(ffi:foreign-free send-buffer)
	(when remote-address
	  (ffi:foreign-free remote-address))
	nbytes)))

  (declaim (inline get-socket-name))
  (defun get-socket-name (socket function)
    (let ((address (ffi:allocate-shallow 'sockaddr_in))
	  (address-length (ffi:allocate-shallow 'ffi:int))
	  (host 0) (port 0))
      (setf (ffi:foreign-value address-length) *length-of-sockaddr_in*)
      (unwind-protect
	   (multiple-value-bind (rv return-address return-address-length)
	       (funcall function socket
			(ffi:cast (ffi:foreign-value address) 'sockaddr)
			(ffi:foreign-value address-length))
	     (declare (ignore return-address-length))
	     (if (zerop rv)
		 (let ((data (sockaddr-sa_data return-address)))
		   (setq host (ip-from-octet-buffer data :start 2)
			 port (port-from-octet-buffer data)))
		 (error "GET-SOCKET-NAME ERROR: ~A" (os:errno))))
	(ffi:foreign-free address)
	(ffi:foreign-free address-length))
      (values (hbo-to-vector-quad host) port)))

  (defmethod get-local-name ((usocket datagram-usocket))
    (get-socket-name (socket usocket) '%getsockname))

  (defmethod get-peer-name ((usocket datagram-usocket))
    (get-socket-name (socket usocket) '%getpeername))

) ; progn

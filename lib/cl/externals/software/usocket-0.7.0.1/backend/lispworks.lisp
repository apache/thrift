;;;; See LICENSE for licensing information.

(in-package :usocket)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm")

  #+lispworks3
  (error "LispWorks 3 is not supported by USOCKET any more."))

;;; ---------------------------------------------------------------------------
;;;  Warn if multiprocessing is not running on Lispworks

(defun check-for-multiprocessing-started (&optional errorp)
  (unless mp:*current-process*
    (funcall (if errorp 'error 'warn)
             "You must start multiprocessing on Lispworks by calling~
              ~%~3t(~s)~
              ~%for ~s function properly."
             'mp:initialize-multiprocessing
             'wait-for-input)))

(eval-when (:load-toplevel :execute)
  (check-for-multiprocessing-started))

#+win32
(eval-when (:load-toplevel :execute)
  (fli:register-module "ws2_32"))

(fli:define-foreign-function (get-host-name-internal "gethostname" :source)
      ((return-string (:reference-return (:ef-mb-string :limit 257)))
       (namelen :int))
      :lambda-list (&aux (namelen 256) return-string)
      :result-type :int
      #+win32 :module
      #+win32 "ws2_32")

(defun get-host-name ()
  (multiple-value-bind (return-code name)
      (get-host-name-internal)
    (when (zerop return-code)
      name)))

#+win32
(defun remap-maybe-for-win32 (z)
  (mapcar #'(lambda (x)
              (cons (mapcar #'(lambda (y) (+ 10000 y)) (car x))
                    (cdr x)))
          z))

(defparameter +lispworks-error-map+
  #+win32
  (append (remap-maybe-for-win32 +unix-errno-condition-map+)
          (remap-maybe-for-win32 +unix-errno-error-map+))
  #-win32
  (append +unix-errno-condition-map+
          +unix-errno-error-map+))

(defun raise-usock-err (errno socket &optional condition)
  (let ((usock-err
         (cdr (assoc errno +lispworks-error-map+ :test #'member))))
    (if usock-err
        (if (subtypep usock-err 'error)
            (error usock-err :socket socket)
          (signal usock-err))
      (error 'unknown-error
             :socket socket
             :real-error condition
             :errno errno))))

(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (condition (let ((errno #-win32 (lw:errno-value)
                            #+win32 (wsa-get-last-error)))
                 (unless (zerop errno)
                   (raise-usock-err errno socket condition))))))

(defconstant *socket_sock_dgram* 2
  "Connectionless, unreliable datagrams of fixed maximum length.")

(defconstant *socket_ip_proto_udp* 17)

(defconstant *sockopt_so_rcvtimeo*
  #-linux #x1006
  #+linux 20
  "Socket receive timeout")

(defconstant *sockopt_so_sndtimeo*
  #-linux #x1007
  #+linux 21
  "Socket send timeout")

(fli:define-c-struct timeval
  (tv-sec :long)
  (tv-usec :long))

;;; ssize_t
;;; recvfrom(int socket, void *restrict buffer, size_t length, int flags,
;;;          struct sockaddr *restrict address, socklen_t *restrict address_len);
(fli:define-foreign-function (%recvfrom "recvfrom" :source)
    ((socket :int)
     (buffer (:pointer (:unsigned :byte)))
     (length :int)
     (flags :int)
     (address (:pointer (:struct comm::sockaddr)))
     (address-len (:pointer :int)))
  :result-type :int
  #+win32 :module
  #+win32 "ws2_32")

;;; ssize_t
;;; sendto(int socket, const void *buffer, size_t length, int flags,
;;;        const struct sockaddr *dest_addr, socklen_t dest_len);
(fli:define-foreign-function (%sendto "sendto" :source)
    ((socket :int)
     (buffer (:pointer (:unsigned :byte)))
     (length :int)
     (flags :int)
     (address (:pointer (:struct comm::sockaddr)))
     (address-len :int))
  :result-type :int
  #+win32 :module
  #+win32 "ws2_32")

#-win32
(defun set-socket-receive-timeout (socket-fd seconds)
  "Set socket option: RCVTIMEO, argument seconds can be a float number"
  (declare (type integer socket-fd)
           (type number seconds))
  (multiple-value-bind (sec usec) (truncate seconds)
    (fli:with-dynamic-foreign-objects ((timeout (:struct timeval)))
      (fli:with-foreign-slots (tv-sec tv-usec) timeout
        (setf tv-sec sec
              tv-usec (truncate (* 1000000 usec)))
        (if (zerop (comm::setsockopt socket-fd
                               comm::*sockopt_sol_socket*
                               *sockopt_so_rcvtimeo*
                               (fli:copy-pointer timeout
                                                 :type '(:pointer :void))
                               (fli:size-of '(:struct timeval))))
            seconds)))))

#-win32
(defun set-socket-send-timeout (socket-fd seconds)
  "Set socket option: SNDTIMEO, argument seconds can be a float number"
  (declare (type integer socket-fd)
           (type number seconds))
  (multiple-value-bind (sec usec) (truncate seconds)
    (fli:with-dynamic-foreign-objects ((timeout (:struct timeval)))
      (fli:with-foreign-slots (tv-sec tv-usec) timeout
        (setf tv-sec sec
              tv-usec (truncate (* 1000000 usec)))
        (if (zerop (comm::setsockopt socket-fd
                               comm::*sockopt_sol_socket*
                               *sockopt_so_sndtimeo*
                               (fli:copy-pointer timeout
                                                 :type '(:pointer :void))
                               (fli:size-of '(:struct timeval))))
            seconds)))))

#+win32
(defun set-socket-receive-timeout (socket-fd seconds)
  "Set socket option: RCVTIMEO, argument seconds can be a float number.
   On win32, you must bind the socket before use this function."
  (declare (type integer socket-fd)
           (type number seconds))
  (fli:with-dynamic-foreign-objects ((timeout :int))
    (setf (fli:dereference timeout)
          (truncate (* 1000 seconds)))
    (if (zerop (comm::setsockopt socket-fd
                           comm::*sockopt_sol_socket*
                           *sockopt_so_rcvtimeo*
                           (fli:copy-pointer timeout
                                             :type '(:pointer :char))
                           (fli:size-of :int)))
        seconds)))

#+win32
(defun set-socket-send-timeout (socket-fd seconds)
  "Set socket option: SNDTIMEO, argument seconds can be a float number.
   On win32, you must bind the socket before use this function."
  (declare (type integer socket-fd)
           (type number seconds))
  (fli:with-dynamic-foreign-objects ((timeout :int))
    (setf (fli:dereference timeout)
          (truncate (* 1000 seconds)))
    (if (zerop (comm::setsockopt socket-fd
                           comm::*sockopt_sol_socket*
                           *sockopt_so_sndtimeo*
                           (fli:copy-pointer timeout
                                             :type '(:pointer :char))
                           (fli:size-of :int)))
        seconds)))

#-win32
(defun get-socket-receive-timeout (socket-fd)
  "Get socket option: RCVTIMEO, return value is a float number"
  (declare (type integer socket-fd))
  (fli:with-dynamic-foreign-objects ((timeout (:struct timeval))
                                     (len :int))
    (comm::getsockopt socket-fd
                comm::*sockopt_sol_socket*
                *sockopt_so_rcvtimeo*
                (fli:copy-pointer timeout
                                  :type '(:pointer :void))
                len)
    (fli:with-foreign-slots (tv-sec tv-usec) timeout
      (float (+ tv-sec (/ tv-usec 1000000))))))

#-win32
(defun get-socket-send-timeout (socket-fd)
  "Get socket option: SNDTIMEO, return value is a float number"
  (declare (type integer socket-fd))
  (fli:with-dynamic-foreign-objects ((timeout (:struct timeval))
                                     (len :int))
    (comm::getsockopt socket-fd
                comm::*sockopt_sol_socket*
                *sockopt_so_sndtimeo*
                (fli:copy-pointer timeout
                                  :type '(:pointer :void))
                len)
    (fli:with-foreign-slots (tv-sec tv-usec) timeout
      (float (+ tv-sec (/ tv-usec 1000000))))))

#+win32
(defun get-socket-receive-timeout (socket-fd)
  "Get socket option: RCVTIMEO, return value is a float number"
  (declare (type integer socket-fd))
  (fli:with-dynamic-foreign-objects ((timeout :int)
                                     (len :int))
    (comm::getsockopt socket-fd
                comm::*sockopt_sol_socket*
                *sockopt_so_rcvtimeo*
                (fli:copy-pointer timeout
                                  :type '(:pointer :void))
                len)
    (float (/ (fli:dereference timeout) 1000))))

#+win32
(defun get-socket-send-timeout (socket-fd)
  "Get socket option: SNDTIMEO, return value is a float number"
  (declare (type integer socket-fd))
  (fli:with-dynamic-foreign-objects ((timeout :int)
                                     (len :int))
    (comm::getsockopt socket-fd
                comm::*sockopt_sol_socket*
                *sockopt_so_sndtimeo*
                (fli:copy-pointer timeout
                                  :type '(:pointer :void))
                len)
    (float (/ (fli:dereference timeout) 1000))))

#+lispworks4
(defun set-socket-tcp-nodelay (socket-fd new-value)
  "Set socket option: TCP_NODELAY, argument is a fixnum (0 or 1)"
  (declare (type integer socket-fd)
           (type (integer 0 1) new-value))
  (fli:with-dynamic-foreign-objects ((zero-or-one :int))
    (setf (fli:dereference zero-or-one) new-value)
    (when (zerop (comm::setsockopt socket-fd
                                   comm::*sockopt_sol_socket*
                                   comm::*sockopt_tcp_nodelay*
                                   (fli:copy-pointer zero-or-one
                                                     :type '(:pointer #+win32 :char #-win32 :void))
                                   (fli:size-of :int)))
        new-value)))

(defun get-socket-tcp-nodelay (socket-fd)
  "Get socket option: TCP_NODELAY, return value is a fixnum (0 or 1)"
  (declare (type integer socket-fd))
  (fli:with-dynamic-foreign-objects ((zero-or-one :int)
                                     (len :int))
    (if (zerop (comm::getsockopt socket-fd
                                 comm::*sockopt_sol_socket*
                                 comm::*sockopt_tcp_nodelay*
                                 (fli:copy-pointer zero-or-one
                                                   :type '(:pointer #+win32 :char #-win32 :void))
                                 len))
        zero-or-one 0))) ; on error, return 0

(defun initialize-dynamic-sockaddr (hostname service protocol &aux (original-hostname hostname))
  (declare (ignorable original-hostname))
  #+(or lispworks4 lispworks5 lispworks6.0)
  (let ((server-addr (fli:allocate-dynamic-foreign-object
                      :type '(:struct comm::sockaddr_in))))
    (values (comm::initialize-sockaddr_in 
             server-addr 
             comm::*socket_af_inet*
             hostname
             service protocol)
            comm::*socket_af_inet*
            server-addr
            (fli:pointer-element-size server-addr)))
  #-(or lispworks4 lispworks5 lispworks6.0) ; version>=6.1
  (progn
    (when (stringp hostname)
      (setq hostname (comm:string-ip-address hostname))
      (unless hostname
        (let ((resolved-hostname (comm:get-host-entry original-hostname :fields '(:address))))
          (unless resolved-hostname
            (return-from initialize-dynamic-sockaddr :unknown-host))
          (setq hostname resolved-hostname))))
    (if (or (null hostname)
            (integerp hostname)
            (comm:ipv6-address-p hostname))
        (let ((server-addr (fli:allocate-dynamic-foreign-object
                            :type '(:struct comm::lw-sockaddr))))
          (multiple-value-bind (error family)
              (comm::initialize-sockaddr_in 
               server-addr 
               hostname
               service protocol)
            (values error family
                    server-addr
                    (if (eql family comm::*socket_af_inet*)
                        (fli:size-of '(:struct comm::sockaddr_in))
			(fli:size-of '(:struct comm::sockaddr_in6))))))
	:bad-host)))

(defun open-udp-socket (&key local-address local-port read-timeout
                             (address-family comm::*socket_af_inet*))
  "Open a unconnected UDP socket.
   For binding on address ANY(*), just not set LOCAL-ADDRESS (NIL),
   for binding on random free unused port, set LOCAL-PORT to 0."

  ;; Note: move (ensure-sockets) here to make sure delivered applications
  ;; correctly have networking support initialized.
  ;;
  ;; Following words was from Martin Simmons, forwarded by Camille Troillard:

  ;; Calling comm::ensure-sockets at load time looks like a bug in Lispworks-udp
  ;; (it is too early and also unnecessary).

  ;; The LispWorks comm package calls comm::ensure-sockets when it is needed, so I
  ;; think open-udp-socket should probably do it too.  Calling it more than once is
  ;; safe and it will be very fast after the first time.
  #+win32 (comm::ensure-sockets)

  (let ((socket-fd (comm::socket address-family *socket_sock_dgram* *socket_ip_proto_udp*)))
    (if socket-fd
        (progn
          (when read-timeout (set-socket-receive-timeout socket-fd read-timeout))
          (if local-port
              (fli:with-dynamic-foreign-objects ()
                (multiple-value-bind (error local-address-family
                                            client-addr client-addr-length)
                    (initialize-dynamic-sockaddr local-address local-port "udp")
                  (if (or error (not (eql address-family local-address-family)))
                      (progn
                        (comm::close-socket socket-fd)
                        (error "cannot resolve hostname ~S, service ~S: ~A"
                               local-address local-port (or error "address family mismatch")))
                    (if (comm::bind socket-fd client-addr client-addr-length)
                        ;; success, return socket fd
                        socket-fd
                      (progn
                        (comm::close-socket socket-fd)
                        (error "cannot bind"))))))
	    socket-fd))
      (error "cannot create socket"))))

(defun connect-to-udp-server (hostname service
                                       &key local-address local-port read-timeout)
  "Something like CONNECT-TO-TCP-SERVER"
  (fli:with-dynamic-foreign-objects ()
    (multiple-value-bind (error address-family server-addr server-addr-length)
        (initialize-dynamic-sockaddr hostname service "udp")
      (when error
        (error "cannot resolve hostname ~S, service ~S: ~A"
               hostname service error))
      (let ((socket-fd (open-udp-socket :local-address local-address
                                        :local-port local-port
                                        :read-timeout read-timeout
                                        :address-family address-family)))
        (if socket-fd
            (if (comm::connect socket-fd server-addr server-addr-length)
                ;; success, return socket fd
                socket-fd
              ;; fail, close socket and return nil
              (progn
                (comm::close-socket socket-fd)
                (error "cannot connect")))
          (error "cannot create socket"))))))

(defun socket-connect (host port &key (protocol :stream) (element-type 'base-char)
                       timeout deadline (nodelay t)
                       local-host local-port)
  ;; What's the meaning of this keyword?
  (when deadline
    (unimplemented 'deadline 'socket-connect))

  #+(and lispworks4 (not lispworks4.4)) ; < 4.4.5
  (when timeout
    (unsupported 'timeout 'socket-connect :minimum "LispWorks 4.4.5"))

  #+lispworks4
  (when local-host
     (unsupported 'local-host 'socket-connect :minimum "LispWorks 5.0"))
  #+lispworks4
  (when local-port
     (unsupported 'local-port 'socket-connect :minimum "LispWorks 5.0"))

  (ecase protocol
    (:stream
     (let ((hostname (host-to-hostname host))
           (stream))
       (setq stream
             (with-mapped-conditions ()
               (comm:open-tcp-stream hostname port
                                     :element-type element-type
                                     #-(and lispworks4 (not lispworks4.4)) ; >= 4.4.5
                                     #-(and lispworks4 (not lispworks4.4))
                                     :timeout timeout
                                     #-lispworks4 #-lispworks4
                                     #-lispworks4 #-lispworks4
                                     :local-address (when local-host (host-to-hostname local-host))
                                     :local-port local-port
                                     #-(or lispworks4 lispworks5.0) ; >= 5.1
                                     #-(or lispworks4 lispworks5.0)
                                     :nodelay nodelay)))

       ;; Then handle `nodelay' separately for older versions <= 5.0
       #+(or lispworks4 lispworks5.0)
       (when (and stream nodelay)
         (#+lispworks4 set-socket-tcp-nodelay
          #+lispworks5.0 comm::set-socket-tcp-nodelay
           (comm:socket-stream-socket stream)
           (bool->int nodelay))) ; ":if-supported" maps to 1 too.

       (if stream
           (make-stream-socket :socket (comm:socket-stream-socket stream)
                               :stream stream)
         ;; if no other error catched by above with-mapped-conditions and still fails, then it's a timeout
         (error 'timeout-error))))
    (:datagram
     (let ((usocket (make-datagram-socket
                     (if (and host port)
                         (with-mapped-conditions ()
                           (connect-to-udp-server (host-to-hostname host) port
                                                  :local-address (and local-host (host-to-hostname local-host))
                                                  :local-port local-port
                                                  :read-timeout timeout))
                         (with-mapped-conditions ()
                           (open-udp-socket       :local-address (and local-host (host-to-hostname local-host))
                                                  :local-port local-port
                                                  :read-timeout timeout)))
                     :connected-p (and host port t))))
       usocket))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'base-char))
  #+lispworks4.1
  (unsupported 'host 'socket-listen :minimum "LispWorks 4.0 or newer than 4.1")
  #+lispworks4.1
  (unsupported 'backlog 'socket-listen :minimum "LispWorks 4.0 or newer than 4.1")

  (let* ((reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
         (comm::*use_so_reuseaddr* reuseaddress)
         (hostname (host-to-hostname host))
         (socket-res-list (with-mapped-conditions ()
                            (multiple-value-list
                             #-lispworks4.1 (comm::create-tcp-socket-for-service
                                             port :address hostname :backlog backlog)
                             #+lispworks4.1 (comm::create-tcp-socket-for-service port))))
         (sock (if (not (or (second socket-res-list) (third socket-res-list)))
                   (first socket-res-list)
                 (when (eq (second socket-res-list) :bind)
                   (error 'address-in-use-error)))))
    (make-stream-server-socket sock :element-type element-type)))

;; Note: COMM::GET-FD-FROM-SOCKET contains addition socket wait operations, which
;; should NOT be applied on socket FDs who have already been called on W-F-I,
;; so we have to check the %READY-P slot to decide if this waiting is necessary,
;; or SOCKET-ACCEPT will just hang. -- Chun Tian (binghe), May 1, 2011

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (let* ((socket (with-mapped-conditions (usocket)
                   #+win32
                   (if (%ready-p usocket)
                       (comm::accept-connection-to-socket (socket usocket))
                     (comm::get-fd-from-socket (socket usocket)))
                   #-win32
                   (comm::get-fd-from-socket (socket usocket))))
         (stream (make-instance 'comm:socket-stream
                                :socket socket
                                :direction :io
                                :element-type (or element-type
                                                  (element-type usocket)))))
    #+win32
    (when socket
      (setf (%ready-p usocket) nil))
    (make-stream-socket :socket socket :stream stream)))

;; Sockets and their streams are different objects
;; close the stream in order to make sure buffers
;; are correctly flushed and the socket closed.
(defmethod socket-close ((usocket stream-usocket))
  "Close socket."
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (close (socket-stream usocket)))

(defmethod socket-close ((usocket usocket))
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (with-mapped-conditions (usocket)
     (comm::close-socket (socket usocket))))

(defmethod socket-close :after ((socket datagram-usocket))
  "Additional socket-close method for datagram-usocket"
  (setf (%open-p socket) nil))

(defconstant +shutdown-read+ 0)
(defconstant +shutdown-write+ 1)
(defconstant +shutdown-read-write+ 2)

;;; int
;;; shutdown(int socket, int what);
(fli:define-foreign-function (%shutdown "shutdown" :source)
    ((socket :int)
     (what :int))
  :result-type :int
  #+win32 :module
  #+win32 "ws2_32")

(defmethod socket-shutdown ((usocket datagram-usocket) direction)
  (unless (member direction '(:input :output :io))
    (error 'invalid-argument-error))
  (let ((what (case direction
		(:input +shutdown-read+)
		(:output +shutdown-write+)
		(:io +shutdown-read-write+))))
    (with-mapped-conditions (usocket)
      #-(or lispworks4 lispworks5 lispworks6) ; lispworks 7.0+
      (comm::shutdown (socket usocket) what)
      #+(or lispworks4 lispworks5 lispworks6)
      (= 0 (%shutdown (socket usocket) what)))))

(defmethod socket-shutdown ((usocket stream-usocket) direction)
  (unless (member direction '(:input :output :io))
    (error 'invalid-argument-error))
  (with-mapped-conditions (usocket)
    #-(or lispworks4 lispworks5 lispworks6)
    (comm:socket-stream-shutdown (socket usocket) direction)
    #+(or lispworks4 lispworks5 lispworks6)
    (let ((what (case direction
		  (:input +shutdown-read+)
		  (:output +shutdown-write+)
		  (:io +shutdown-read-write+))))
      (= 0 (%shutdown (comm:socket-stream-socket (socket usocket)) what)))))

(defmethod initialize-instance :after ((socket datagram-usocket) &key)
  (setf (slot-value socket 'send-buffer)
        (make-array +max-datagram-packet-size+
                    :element-type '(unsigned-byte 8)
                    :allocation :static))
  (setf (slot-value socket 'recv-buffer)
        (make-array +max-datagram-packet-size+
                    :element-type '(unsigned-byte 8)
                    :allocation :static)))

(defvar *length-of-sockaddr_in*
  (fli:size-of '(:struct comm::sockaddr_in)))

(defmethod socket-send ((usocket datagram-usocket) buffer size &key host port (offset 0)
                        &aux (socket-fd (socket usocket))
                             (message (slot-value usocket 'send-buffer))) ; TODO: multiple threads send together?
  "Send message to a socket, using sendto()/send()"
  (declare (type integer socket-fd)
           (type sequence buffer))
  (when host (setq host (host-to-hostname host)))
  (fli:with-dynamic-lisp-array-pointer (ptr message :type '(:unsigned :byte))
    (replace message buffer :start2 offset :end2 (+ offset size))
    (let ((n (if (and host port)
                 (fli:with-dynamic-foreign-objects ()
                   (multiple-value-bind (error family client-addr client-addr-length)
                       (initialize-dynamic-sockaddr host port "udp")
                     (declare (ignore family))
                     (when error
                       (error "cannot resolve hostname ~S, port ~S: ~A"
                              host port error))
                     (%sendto socket-fd ptr (min size +max-datagram-packet-size+) 0
                              (fli:copy-pointer client-addr :type '(:struct comm::sockaddr))
                              client-addr-length)))
               (comm::%send socket-fd ptr (min size +max-datagram-packet-size+) 0))))
      (declare (type fixnum n))
      (if (plusp n)
          n
        (let ((errno #-win32 (lw:errno-value)
                     #+win32 (wsa-get-last-error)))
          (if (zerop errno)
              n
            (raise-usock-err errno socket-fd)))))))

(defmethod socket-receive ((socket datagram-usocket) buffer length &key timeout (max-buffer-size +max-datagram-packet-size+))
  "Receive message from socket, read-timeout is a float number in seconds.

   This function will return 4 values:
   1. receive buffer
   2. number of receive bytes
   3. remote address
   4. remote port"
  (declare (values (simple-array (unsigned-byte 8) (*)) ; buffer
                   (integer 0)                          ; size
                   (unsigned-byte 32)                   ; host
                   (unsigned-byte 16))                  ; port
	   (type sequence buffer))
  (let ((socket-fd (socket socket))
        (message (slot-value socket 'recv-buffer)) ; TODO: how multiple threads do this in parallel?
        (read-timeout timeout)
        old-timeout)
    (declare (type integer socket-fd))
    (fli:with-dynamic-foreign-objects ((client-addr (:struct comm::sockaddr_in))
                                       (len :int
                                            #-(or lispworks4 lispworks5.0) ; <= 5.0
                                            :initial-element *length-of-sockaddr_in*))
      #+(or lispworks4 lispworks5.0) ; <= 5.0
      (setf (fli:dereference len) *length-of-sockaddr_in*)
      (fli:with-dynamic-lisp-array-pointer (ptr message :type '(:unsigned :byte))
        ;; setup new read timeout
        (when read-timeout
          (setf old-timeout (get-socket-receive-timeout socket-fd))
          (set-socket-receive-timeout socket-fd read-timeout))
        (let ((n (%recvfrom socket-fd ptr max-buffer-size 0
                            (fli:copy-pointer client-addr :type '(:struct comm::sockaddr))
                            len)))
          (declare (type fixnum n))
          ;; restore old read timeout
          (when (and read-timeout (/= old-timeout read-timeout))
            (set-socket-receive-timeout socket-fd old-timeout))
          ;; Frank James' patch: reset the %read-p for WAIT-FOR-INPUT
          #+win32 (setf (%ready-p socket) nil)
          (if (plusp n)
              (values (if buffer
                          (replace buffer message
                                   :end1 (min length max-buffer-size)
                                   :end2 (min n max-buffer-size))
                          (subseq message 0 (min n max-buffer-size)))
                      (min n max-buffer-size)
                      (comm::ntohl (fli:foreign-slot-value
                                    (fli:foreign-slot-value client-addr
                                                            'comm::sin_addr
                                                            :object-type '(:struct comm::sockaddr_in)
                                                            :type '(:struct comm::in_addr)
                                                            :copy-foreign-object nil)
                                    'comm::s_addr
                                    :object-type '(:struct comm::in_addr)))
                      (comm::ntohs (fli:foreign-slot-value client-addr
                                                           'comm::sin_port
                                                           :object-type '(:struct comm::sockaddr_in)
                                                           :type '(:unsigned :short)
                                                           :copy-foreign-object nil)))
            (let ((errno #-win32 (lw:errno-value)
                         #+win32 (wsa-get-last-error)))
              (if (zerop errno)
                  (values nil n 0 0)
                (raise-usock-err errno socket-fd)))))))))

(defmethod get-local-name ((usocket usocket))
  (multiple-value-bind
      (address port)
      (comm:get-socket-address (socket usocket))
    (values (hbo-to-vector-quad address) port)))

(defmethod get-peer-name ((usocket stream-usocket))
  (multiple-value-bind
      (address port)
      (comm:get-socket-peer-address (socket usocket))
    (values (hbo-to-vector-quad address) port)))

(defmethod get-local-address ((usocket usocket))
  (nth-value 0 (get-local-name usocket)))

(defmethod get-peer-address ((usocket stream-usocket))
  (nth-value 0 (get-peer-name usocket)))

(defmethod get-local-port ((usocket usocket))
  (nth-value 1 (get-local-name usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (nth-value 1 (get-peer-name usocket)))

(defun lw-hbo-to-vector-quad (hbo)
  #+(or lispworks4 lispworks5 lispworks6.0)
  (hbo-to-vector-quad hbo)
  #-(or lispworks4 lispworks5 lispworks6.0) ; version>= 6.1
  (if (comm:ipv6-address-p hbo)
      (ipv6-host-to-vector (comm:ipv6-address-string hbo))
    (hbo-to-vector-quad hbo)))

(defun get-hosts-by-name (name)
  (with-mapped-conditions ()
     (mapcar #'lw-hbo-to-vector-quad
             (comm:get-host-entry name :fields '(:addresses)))))

(defun os-socket-handle (usocket)
  (socket usocket))

(defun usocket-listen (usocket)
  (if (stream-usocket-p usocket)
      (when (listen (socket-stream usocket))
        usocket)
    (when (comm::socket-listen (socket usocket))
      usocket)))

;;;
;;; Non Windows implementation
;;;   The Windows implementation needs to resort to the Windows API in order
;;;   to achieve what we want (what we want is waiting without busy-looping)
;;;

#-win32
(progn

  (defun %setup-wait-list (wait-list)
    (declare (ignore wait-list)))

  (defun %add-waiter (wait-list waiter)
    (declare (ignore wait-list waiter)))

  (defun %remove-waiter (wait-list waiter)
    (declare (ignore wait-list waiter)))

  (defun wait-for-input-internal (wait-list &key timeout)
    (with-mapped-conditions ()
      ;; unfortunately, it's impossible to share code between
      ;; non-win32 and win32 platforms...
      ;; Can we have a sane -pref. complete [UDP!?]- API next time, please?
      (dolist (x (wait-list-waiters wait-list))
        (mp:notice-fd (os-socket-handle x)))
      (labels ((wait-function (socks)
		 (let (rv)
		   (dolist (x socks rv)
		     (when (usocket-listen x)
		       (setf (state x) :READ
			     rv t))))))
	(if timeout
	    (mp:process-wait-with-timeout "Waiting for a socket to become active"
					(truncate timeout)
					#'wait-function
					(wait-list-waiters wait-list))
	    (mp:process-wait "Waiting for a socket to become active"
			     #'wait-function
			     (wait-list-waiters wait-list))))
      (dolist (x (wait-list-waiters wait-list))
        (mp:unnotice-fd (os-socket-handle x)))
      wait-list))

) ; end of block


;;;
;;;  The Windows side of the story
;;;    We want to wait without busy looping
;;;    This code only works in threads which don't have (hidden)
;;;    windows which need to receive messages. There are workarounds in the Windows API
;;;    but are those available to 'us'.
;;;


#+win32
(progn

  ;; LispWorks doesn't provide an interface to wait for a socket
  ;; to become ready (under Win32, that is) meaning that we need
  ;; to resort to system calls to achieve the same thing.
  ;; Luckily, it provides us access to the raw socket handles (as we 
  ;; wrote the code above.

  (defconstant fd-read 1)
  (defconstant fd-read-bit 0)
  (defconstant fd-write 2)
  (defconstant fd-write-bit 1)
  (defconstant fd-oob 4)
  (defconstant fd-oob-bit 2)
  (defconstant fd-accept 8)
  (defconstant fd-accept-bit 3)
  (defconstant fd-connect 16)
  (defconstant fd-connect-bit 4)
  (defconstant fd-close 32)
  (defconstant fd-close-bit 5)
  (defconstant fd-qos 64)
  (defconstant fd-qos-bit 6)
  (defconstant fd-group-qos 128)
  (defconstant fd-group-qos-bit 7)
  (defconstant fd-routing-interface 256)
  (defconstant fd-routing-interface-bit 8)
  (defconstant fd-address-list-change 512)
  (defconstant fd-address-list-change-bit 9)
  
  (defconstant fd-max-events 10)

  (defconstant fionread 1074030207)


  ;; Note:
  ;;
  ;;  If special finalization has to occur for a given
  ;;  system resource (handle), an associated object should
  ;;  be created.  A special cleanup action should be added
  ;;  to the system and a special cleanup action should
  ;;  be flagged on all objects created for resources like it
  ;;
  ;;  We have 2 functions to do so:
  ;;   * hcl:add-special-free-action (function-symbol)
  ;;   * hcl:flag-special-free-action (object)
  ;;
  ;;  Note that the special free action will be called on all
  ;;  objects which have been flagged for special free, so be
  ;;  sure to check for the right argument type!
  
  (fli:define-foreign-type ws-socket () '(:unsigned :int))
  (fli:define-foreign-type win32-handle () '(:unsigned :int))
  (fli:define-c-struct wsa-network-events
    (network-events :long)
    (error-code (:c-array :int 10)))

  (fli:define-foreign-function (wsa-event-create "WSACreateEvent" :source)
      ()
      :lambda-list nil
    :result-type :int
    :module "ws2_32")

  (fli:define-foreign-function (wsa-event-close "WSACloseEvent" :source)
      ((event-object win32-handle))
    :result-type :int
    :module "ws2_32")

  (fli:define-foreign-function (wsa-enum-network-events "WSAEnumNetworkEvents" :source)
      ((socket ws-socket)
       (event-object win32-handle)
       (network-events (:reference-return wsa-network-events)))
    :result-type :int
    :module "ws2_32")
  
  (fli:define-foreign-function (wsa-event-select "WSAEventSelect" :source)
      ((socket ws-socket)
       (event-object win32-handle)
       (network-events :long))
    :result-type :int
    :module "ws2_32")

  (fli:define-foreign-function (wsa-get-last-error "WSAGetLastError" :source)
      ()
    :result-type :int
    :module "ws2_32")

  (fli:define-foreign-function (wsa-ioctlsocket "ioctlsocket" :source)
      ((socket :long) (cmd :long) (argp (:ptr :long)))
    :result-type :int
    :module "ws2_32")


  ;; The Windows system 


  ;; Now that we have access to the system calls, this is the plan:

  ;; 1. Receive a wait-list with associated sockets to wait for
  ;; 2. Add all those sockets to an event handle
  ;; 3. Listen for an event on that handle (we have a LispWorks system:: internal for that)
  ;; 4. After listening, detect if there are errors
  ;;    (this step is different from Unix, where we can have only one error)
  ;; 5. If so, raise one of them
  ;; 6. If not so, return the sockets which have input waiting for them


  (defun maybe-wsa-error (rv &optional socket)
    (unless (zerop rv)
      (raise-usock-err (wsa-get-last-error) socket)))

  (defun bytes-available-for-read (socket)
    (fli:with-dynamic-foreign-objects ((int-ptr :long))
      (let ((rv (wsa-ioctlsocket (os-socket-handle socket) fionread int-ptr)))
        (if (= 0 rv)
            (fli:dereference int-ptr)
          0))))

  (defun socket-ready-p (socket)
    (if (typep socket 'stream-usocket)
        (< 0 (bytes-available-for-read socket))
      (%ready-p socket)))

  (defun waiting-required (sockets)
    (notany #'socket-ready-p sockets))

  (defun wait-for-input-internal (wait-list &key timeout)
    (when (waiting-required (wait-list-waiters wait-list))
      (system:wait-for-single-object (wait-list-%wait wait-list)
                                     "Waiting for socket activity" timeout))
    (update-ready-and-state-slots (wait-list-waiters wait-list)))
  
  (defun map-network-events (func network-events)
    (let ((event-map (fli:foreign-slot-value network-events 'network-events))
          (error-array (fli:foreign-slot-pointer network-events 'error-code)))
      (unless (zerop event-map)
        (dotimes (i fd-max-events)
          (unless (zerop (ldb (byte 1 i) event-map)) ;;### could be faster with ash and logand?
            (funcall func (fli:foreign-aref error-array i)))))))

  (defun update-ready-and-state-slots (sockets)
    (dolist (socket sockets)
      (if (or (and (stream-usocket-p socket)
                   (listen (socket-stream socket)))
              (%ready-p socket))
          (setf (state socket) :READ)
        (multiple-value-bind
            (rv network-events)
            (wsa-enum-network-events (os-socket-handle socket) 0 t)
          (if (zerop rv)
              (map-network-events #'(lambda (err-code)
                                      (if (zerop err-code)
                                          (setf (%ready-p socket) t
                                                (state socket) :READ)
                                        (raise-usock-err err-code socket)))
                                  network-events)
            (maybe-wsa-error rv socket))))))

  ;; The wait-list part

  (defun free-wait-list (wl)
    (when (wait-list-p wl)
      (unless (null (wait-list-%wait wl))
        (wsa-event-close (wait-list-%wait wl))
        (setf (wait-list-%wait wl) nil))))
  
  (eval-when (:load-toplevel :execute)
    (hcl:add-special-free-action 'free-wait-list))
  
  (defun %setup-wait-list (wait-list)
    (hcl:flag-special-free-action wait-list)
    (setf (wait-list-%wait wait-list) (wsa-event-create)))

  (defun %add-waiter (wait-list waiter)
    (let ((events (etypecase waiter
                    (stream-server-usocket (logior fd-connect fd-accept fd-close))
                    (stream-usocket (logior fd-connect fd-read fd-oob fd-close))
                    (datagram-usocket (logior fd-read)))))
      (maybe-wsa-error
       (wsa-event-select (os-socket-handle waiter) (wait-list-%wait wait-list) events)
       waiter)))

  (defun %remove-waiter (wait-list waiter)
    (maybe-wsa-error
     (wsa-event-select (os-socket-handle waiter) (wait-list-%wait wait-list) 0)
     waiter))
  
) ; end of WIN32-block

(defun set-socket-reuse-address (socket-fd reuse-address-p)
  (declare (type integer socket-fd)
           (type boolean reuse-address-p))
  (fli:with-dynamic-foreign-objects ((value :int))
    (setf (fli:dereference value) (if reuse-address-p 1 0))
    (if (zerop (comm::setsockopt socket-fd
                                 comm::*sockopt_sol_socket*
                                 comm::*sockopt_so_reuseaddr*
                                 (fli:copy-pointer value
                                                   :type '(:pointer :void))
                                 (fli:size-of :int)))
        reuse-address-p)))

(defun get-socket-reuse-address (socket-fd)
  (declare (type integer socket-fd))
  (fli:with-dynamic-foreign-objects ((value :int) (len :int))
    (if (zerop (comm::getsockopt socket-fd
                                 comm::*sockopt_sol_socket*
                                 comm::*sockopt_so_reuseaddr*
                                 (fli:copy-pointer value
                                                   :type '(:pointer :void))
                                 len))
        (= 1 (fli:dereference value)))))

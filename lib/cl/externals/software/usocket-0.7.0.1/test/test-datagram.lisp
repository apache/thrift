(in-package :usocket-test)

(defvar *echo-server*)
(defvar *echo-server-port*)

(defun start-server ()
  (multiple-value-bind (thread socket)
      (usocket:socket-server "127.0.0.1" 0 #'identity nil
			     :in-new-thread t
			     :protocol :datagram)
    (setq *echo-server* thread
	  *echo-server-port* (usocket:get-local-port socket))))

(defparameter *max-buffer-size* 32)

(defvar *send-buffer*
  (make-array *max-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0))

(defvar *receive-buffer*
  (make-array *max-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0))

(defun clean-buffers ()
  (fill *send-buffer* 0)
  (fill *receive-buffer* 0))

;;; UDP Send Test #1: connected socket
(deftest udp-send.1
  (progn
    (unless (and *echo-server* *echo-server-port*)
      (start-server))
    (let ((s (usocket:socket-connect "127.0.0.1" *echo-server-port* :protocol :datagram)))
      (clean-buffers)
      (replace *send-buffer* #(1 2 3 4 5))
      (usocket:socket-send s *send-buffer* 5)
      (usocket:wait-for-input s :timeout 3)
      (multiple-value-bind (buffer size host port)
	  (usocket:socket-receive s *receive-buffer* *max-buffer-size*)
	(declare (ignore buffer size host port))
	(reduce #'+ *receive-buffer* :start 0 :end 5))))
  15)

;;; UDP Send Test #2: unconnected socket
(deftest udp-send.2
  (progn
    (unless (and *echo-server* *echo-server-port*)
      (start-server))
    (let ((s (usocket:socket-connect nil nil :protocol :datagram)))
      (clean-buffers)
      (replace *send-buffer* #(1 2 3 4 5))
      (usocket:socket-send s *send-buffer* 5 :host "127.0.0.1" :port *echo-server-port*)
      (usocket:wait-for-input s :timeout 3)
      (multiple-value-bind (buffer size host port)
	  (usocket:socket-receive s *receive-buffer* *max-buffer-size*)
	(declare (ignore buffer size host port))
	(reduce #'+ *receive-buffer* :start 0 :end 5))))
  15)

(deftest mark-h-david ; Mark H. David's remarkable UDP test code
  (let* ((host "localhost")
	 (port 1111)
	 (server-sock
	  (usocket:socket-connect nil nil :protocol ':datagram :local-host host :local-port port))
	 (client-sock
	  (usocket:socket-connect host port :protocol ':datagram))
	 (octet-vector
	  (make-array 2 :element-type '(unsigned-byte 8) :initial-contents `(,(char-code #\O) ,(char-code #\K))))
	 (recv-octet-vector
	  (make-array 2 :element-type '(unsigned-byte 8))))
    (usocket:socket-send client-sock octet-vector 2)
    (usocket:socket-receive server-sock recv-octet-vector 2)
    (prog1 (and (equalp octet-vector recv-octet-vector)
		recv-octet-vector)
      (usocket:socket-close server-sock)
      (usocket:socket-close client-sock)))
  #(79 75))

(deftest frank-james ; Frank James' test code for LispWorks/UDP
  (with-caught-conditions (#+win32 USOCKET:CONNECTION-RESET-ERROR
			   #-win32 USOCKET:CONNECTION-REFUSED-ERROR
			   nil)
    (let ((sock (usocket:socket-connect "localhost" 1234
                                        :protocol ':datagram :element-type '(unsigned-byte 8))))
      (unwind-protect
          (progn
            (usocket:socket-send sock (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0) 16)
            (let ((buffer (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
              (usocket:socket-receive sock buffer 16)))
        (usocket:socket-close sock))))
  nil)

(defun frank-wfi-test ()
  (let ((s (usocket:socket-connect nil nil
                                   :protocol :datagram
                                   :element-type '(unsigned-byte 8)
                                   :local-port 8001)))
    (unwind-protect
        (do ((i 0 (1+ i))
             (buffer (make-array 1024 :element-type '(unsigned-byte 8)
                                 :initial-element 0))
             (now (get-universal-time))
             (done nil))
            ((or done (= i 4))
             nil)
          (format t "~Ds ~D Waiting state ~S~%" (- (get-universal-time) now) i (usocket::state s))
          (when (usocket:wait-for-input s :ready-only t :timeout 5)
            (format t "~D state ~S~%" i (usocket::state s))
            (handler-bind 
                ((error (lambda (c) 
                          (format t "socket-receive error: ~A~%" c)
                          (break)
                          nil)))
              (multiple-value-bind (buffer count remote-host remote-port)
                  (usocket:socket-receive s buffer 1024)
                (handler-bind
                    ((error (lambda (c)
                               (format t "socket-send error: ~A~%" c)
                               (break))))                             
                  (when buffer 
                    (usocket:socket-send s (subseq buffer 0 count) count
                                         :host remote-host
                                         :port remote-port)))))))
      (usocket:socket-close s))))

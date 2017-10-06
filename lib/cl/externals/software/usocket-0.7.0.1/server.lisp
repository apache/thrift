(in-package :usocket)

(defun socket-server (host port function &optional arguments
                      &key in-new-thread (protocol :stream)
                           ;; for udp
                           (timeout 1) (max-buffer-size +max-datagram-packet-size+)
                           ;; for tcp
                           element-type reuse-address multi-threading
                           name)
  (let* ((real-host (or host *wildcard-host*))
         (socket (ecase protocol
                   (:stream
                    (apply #'socket-listen
                           `(,real-host ,port
                             ,@(when element-type `(:element-type ,element-type))
                             ,@(when reuse-address `(:reuse-address ,reuse-address)))))
                   (:datagram
                    (socket-connect nil nil :protocol :datagram
                                    :local-host real-host
                                    :local-port port)))))
    (labels ((real-call ()
               (ecase protocol
                 (:stream
                  (tcp-event-loop socket function arguments
                                  :element-type element-type
                                  :multi-threading multi-threading))
                 (:datagram
                  (udp-event-loop socket function arguments
                                  :timeout timeout
                                  :max-buffer-size max-buffer-size)))))
      (if in-new-thread
	  (values (portable-threads:spawn-thread (or name "USOCKET Server") #'real-call) socket)
	  (real-call)))))

(defvar *remote-host*)
(defvar *remote-port*)

(defun default-udp-handler (buffer) ; echo
  (declare (type (simple-array (unsigned-byte 8) *) buffer))
  buffer)

(defun udp-event-loop (socket function &optional arguments
                       &key timeout max-buffer-size)
  (let ((buffer (make-array max-buffer-size :element-type '(unsigned-byte 8) :initial-element 0))
        (sockets (list socket)))
    (unwind-protect
        (loop do
          (multiple-value-bind (return-sockets real-time)
              (wait-for-input sockets :timeout timeout)
            (declare (ignore return-sockets))
            (when real-time
              (multiple-value-bind (recv n *remote-host* *remote-port*)
                  (socket-receive socket buffer max-buffer-size)
                (declare (ignore recv))
                (if (plusp n)
                    (progn
                      (let ((reply
                             (apply function (subseq buffer 0 n) arguments)))
                        (when reply
                          (replace buffer reply)
                          (let ((n (socket-send socket buffer (length reply)
                                                :host *remote-host*
                                                :port *remote-port*)))
                            (when (minusp n)
                              (error "send error: ~A~%" n))))))
                  (error "receive error: ~A" n))))
            #+scl (when thread:*quitting-lisp* (return))
            #+(and cmu mp) (mp:process-yield)))
      (socket-close socket)
      (values))))

(defun default-tcp-handler (stream) ; null
  (declare (type stream stream))
  (terpri stream))

(defun echo-tcp-handler (stream)
  (loop
     (when (listen stream)
       (let ((line (read-line stream nil)))
	 (write-line line stream)
	 (force-output stream)))))

(defun tcp-event-loop (socket function &optional arguments
                       &key element-type multi-threading)
  (let ((real-function #'(lambda (client-socket &rest arguments)
                           (unwind-protect
                               (multiple-value-bind (*remote-host* *remote-port*) (get-peer-name client-socket)
                                 (apply function (socket-stream client-socket) arguments))
                             (close (socket-stream client-socket))
                             (socket-close client-socket)
                             nil))))
    (unwind-protect
        (loop do
          (let* ((client-socket (apply #'socket-accept
                                       `(,socket ,@(when element-type `(:element-type ,element-type)))))
                 (client-stream (socket-stream client-socket)))
            (if multi-threading
                (apply #'portable-threads:spawn-thread "USOCKET Client" real-function client-socket arguments)
              (prog1 (apply real-function client-socket arguments)
                (close client-stream)
                (socket-close client-socket)))
            #+scl (when thread:*quitting-lisp* (return))
            #+(and cmu mp) (mp:process-yield)))
      (socket-close socket)
      (values))))

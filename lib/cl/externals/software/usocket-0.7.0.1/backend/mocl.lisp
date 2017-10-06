;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (declare (ignore socket))
  (signal condition))

(defun socket-connect (host port &key (protocol :stream) (element-type 'character)
                       timeout deadline (nodelay t nodelay-specified)
		       (local-host nil local-host-p)
		       (local-port nil local-port-p))
  (when (and nodelay-specified 
             (not (eq nodelay :if-supported)))
    (unsupported 'nodelay 'socket-connect))
  (when deadline (unsupported 'deadline 'socket-connect))
  (when timeout (unimplemented 'timeout 'socket-connect))
  (when local-host-p
     (unimplemented 'local-host 'socket-connect))
  (when local-port-p
     (unimplemented 'local-port 'socket-connect))

  (let (socket)
    (ecase protocol
      (:stream
       (setf socket (rt::socket-connect host port))
       (let ((stream (rt::make-socket-stream socket :binaryp (not (eq element-type 'character)))))
	 (make-stream-socket :socket socket :stream stream)))
      (:datagram
       (error 'unsupported
              :feature '(protocol :datagram)
              :context 'socket-connect)))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  (unimplemented 'socket-listen 'mocl))

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (unimplemented 'socket-accept 'mocl))

;; Sockets and their associated streams are modelled as
;; different objects. Be sure to close the socket stream
;; when closing stream-sockets; it makes sure buffers
;; are flushed and the socket is closed correctly afterwards.
(defmethod socket-close ((usocket usocket))
  "Close socket."
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (rt::socket-shutdown usocket)
  (rt::c-fclose usocket))

(defmethod socket-close ((usocket stream-usocket))
  "Close socket."
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (close (socket-stream usocket)))

;; (defmethod socket-close :after ((socket datagram-usocket))
;;   (setf (%open-p socket) nil))

(defmethod socket-shutdown ((usocket stream-usocket) direction)
  (declare (ignore usocket direction))
  ;; sure would be nice if there was some documentation for mocl...
  (unimplemented "shutdown" 'socket-shutdown))

;; (defmethod socket-send ((usocket datagram-usocket) buffer size &key host port)
;;   (let ((s (socket usocket))
;; 	(host (if host (host-to-hbo host)))
;; 	(real-buffer (if (zerop offset)
;; 			 buffer
;; 			 (subseq buffer offset (+ offset size)))))
;;     (multiple-value-bind (result errno)
;; 	(ext:inet-socket-send-to s real-buffer size
;; 				 :remote-host host :remote-port port)
;;       (or result
;; 	  (mocl-map-socket-error errno :socket usocket)))))

;; (defmethod socket-receive ((socket datagram-usocket) buffer length &key)
;;   (declare (values (simple-array (unsigned-byte 8) (*)) ; buffer
;; 		   (integer 0)                          ; size
;; 		   (unsigned-byte 32)                   ; host
;; 		   (unsigned-byte 16)))                 ; port
;;   (let ((s (socket socket)))
;;     (let ((real-buffer (or buffer
;; 			   (make-array length :element-type '(unsigned-byte 8))))
;; 	  (real-length (or length
;; 			   (length buffer))))
;;       (multiple-value-bind (result errno remote-host remote-port)
;; 	  (ext:inet-socket-receive-from s real-buffer real-length)
;; 	(if result
;; 	    (values real-buffer result remote-host remote-port)
;; 	    (mocl-map-socket-error errno :socket socket))))))

;; (defmethod get-local-name ((usocket usocket))
;;   (multiple-value-bind (address port)
;;       (with-mapped-conditions (usocket)
;;         (ext:get-socket-host-and-port (socket usocket)))
;;     (values (hbo-to-vector-quad address) port)))

;; (defmethod get-peer-name ((usocket stream-usocket))
;;   (multiple-value-bind (address port)
;;       (with-mapped-conditions (usocket)
;;         (ext:get-peer-host-and-port (socket usocket)))
;;     (values (hbo-to-vector-quad address) port)))

;; (defmethod get-local-address ((usocket usocket))
;;   (nth-value 0 (get-local-name usocket)))

;; (defmethod get-peer-address ((usocket stream-usocket))
;;   (nth-value 0 (get-peer-name usocket)))

;; (defmethod get-local-port ((usocket usocket))
;;   (nth-value 1 (get-local-name usocket)))

;; (defmethod get-peer-port ((usocket stream-usocket))
;;   (nth-value 1 (get-peer-name usocket)))


;; (defun get-host-by-address (address)
;;   (multiple-value-bind (host errno)
;;       (ext:lookup-host-entry (host-byte-order address))
;;     (cond (host
;;            (ext:host-entry-name host))
;;           (t
;;            (let ((condition (cdr (assoc errno +unix-ns-error-map+))))
;;              (cond (condition
;;                     (error condition :host-or-ip address))
;;                    (t
;;                     (error 'ns-unknown-error :host-or-ip address
;;                            :real-error errno))))))))

(defun get-hosts-by-name (name)
  (rt::lookup-host name))

;; (defun get-host-name ()
;;   (unix:unix-gethostname))


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
  (unimplemented 'wait-for-input-internal 'mocl))

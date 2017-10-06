;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.
(in-package :usocket-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *wait-for-input-timeout* 2))

(deftest wait-for-input.1
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect *common-lisp-net* 80))
          (time (get-universal-time)))
      (unwind-protect
          (progn (usocket:wait-for-input sock :timeout *wait-for-input-timeout*)
            (- (get-universal-time) time))
        (usocket:socket-close sock))))
  #.*wait-for-input-timeout*)

(deftest wait-for-input.2
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect *common-lisp-net* 80))
          (time (get-universal-time)))
      (unwind-protect
          (progn (usocket:wait-for-input sock :timeout *wait-for-input-timeout* :ready-only t)
            (- (get-universal-time) time))
        (usocket:socket-close sock))))
  #.*wait-for-input-timeout*)

(deftest wait-for-input.3
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect *common-lisp-net* 80)))
      (unwind-protect
          (progn
            (format (usocket:socket-stream sock)
                    "GET / HTTP/1.0~2%")
            (force-output (usocket:socket-stream sock))
            (usocket:wait-for-input sock :timeout *wait-for-input-timeout*)
            (subseq (read-line (usocket:socket-stream sock)) 0 15))
        (usocket:socket-close sock))))
  "HTTP/1.1 200 OK")

;;; Advanced W-F-I tests by Elliott Slaughter <elliottslaughter@gmail.com>

(defvar *socket-server-port* 0)
(defvar *socket-server-listen* nil)
(defvar *socket-server-connection*)
(defvar *socket-client-connection*)
(defvar *output-p* t)

(defun stage-1 ()
  (unless *socket-server-listen*
    (setf *socket-server-listen*
	  (socket-listen *wildcard-host* 0 :element-type '(unsigned-byte 8)))
    (setf *socket-server-port* (get-local-port *socket-server-listen*)))

  (setf *socket-server-connection*
	(when (wait-for-input *socket-server-listen* :timeout 0 :ready-only t)
	  (socket-accept *socket-server-listen*)))
  
  (when *output-p* ; should be NIL
    (format t "First time (before client connects) is ~s.~%"
	    *socket-server-connection*))

  *socket-server-connection*)

;; TODO: original test code have addition (:TIMEOUT 0) when doing the SOCKET-CONNECT,
;; it seems cannot work on SBCL/Windows, need to investigate, but here we ignore it.

(defun stage-2 ()
  (setf *socket-client-connection*
        (socket-connect "localhost" *socket-server-port* :protocol :stream
			 :element-type '(unsigned-byte 8)))
  (setf *socket-server-connection*
	(when (wait-for-input *socket-server-listen* :timeout 0 :ready-only t)
	  #+(and win32 (or lispworks ecl sbcl))
          (when *output-p*
            (format t "%READY-P: ~D~%" (usocket::%ready-p *socket-server-listen*)))
	  (socket-accept *socket-server-listen*)))

  (when *output-p* ; should be a usocket object
    (format t "Second time (after client connects) is ~s.~%"
	    *socket-server-connection*))

  *socket-server-connection*)

(defun stage-3 ()
  (setf *socket-server-connection*
	(when (wait-for-input *socket-server-listen* :timeout 0 :ready-only t)
	  #+(and win32 (or lispworks ecl sbcl))
          (when *output-p*
            (format t "%READY-P: ~D~%" (usocket::%ready-p *socket-server-listen*)))
	  (socket-accept *socket-server-listen*)))

  (when *output-p* ; should be NIL again
    (format t "Third time (before second client) is ~s.~%"
	    *socket-server-connection*))

  *socket-server-connection*)

(deftest elliott-slaughter.1
    (let ((*output-p* nil))
      (let* ((s-1 (stage-1)) (s-2 (stage-2)) (s-3 (stage-3)))
	(prog1 (and (null s-1) (usocket::usocket-p s-2) (null s-3))
	  (socket-close *socket-server-listen*)
	  (setf *socket-server-listen* nil))))
  t)

#|

Issue elliott-slaughter.2 (WAIT-FOR-INPUT/win32 on TCP socket)

W-F-I correctly found the inputs, but :READY-ONLY didn't work.

|#
(defun receive-each (connections)
  (let ((ready (usocket:wait-for-input connections :timeout 0 :ready-only t)))
    (loop for connection in ready
       collect (read-line (usocket:socket-stream connection)))))

(defun receive-all (connections)
  (loop for messages = (receive-each connections)
     then (receive-each connections)
     while messages append messages))

(defun send (connection message)
  (format (usocket:socket-stream connection) "~a~%" message)
  (force-output (usocket:socket-stream connection)))

(defun server ()
  (let* ((listen (usocket:socket-listen usocket:*wildcard-host* 12345))
         (connection (usocket:socket-accept listen)))
    (loop for messages = (receive-all connection) then (receive-all connection)
       do (format t "Got messages:~%~s~%" messages)
       do (sleep 1/50))))

(defun client ()
  (let ((connection (usocket:socket-connect "localhost" 12345)))
    (loop for i from 0
       do (send connection (format nil "This is message ~a." i))
       do (sleep 1/100))))

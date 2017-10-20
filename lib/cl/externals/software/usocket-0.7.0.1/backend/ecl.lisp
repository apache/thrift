;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; $URL$

;;;; Foreign functions defined by ECL's DFFI, used for #+ecl-bytecmp only.
;;;; See LICENSE for licensing information.

(in-package :usocket)

#+(and ecl-bytecmp windows)
(eval-when (:load-toplevel :execute)
  (ffi:load-foreign-library "ws2_32.dll" :module "ws2_32"))

#+(and ecl-bytecmp windows)
(progn
  (ffi:def-function ("gethostname" c-gethostname)
    ((name (* :unsigned-char))
     (len :int))
    :returning :int
    :module "ws2_32")

  (defun get-host-name ()
    "Returns the hostname"
    (ffi:with-foreign-object (name '(:array :unsigned-char 256))
      (when (zerop (c-gethostname (ffi:char-array-to-pointer name) 256))
        (ffi:convert-from-foreign-string name))))

  (ffi:def-foreign-type ws-socket :unsigned-int)
  (ffi:def-foreign-type ws-dword :unsigned-long)
  (ffi:def-foreign-type ws-event :unsigned-int)

  (ffi:def-struct wsa-network-events
    (network-events :long)
    (error-code (:array :int 10)))

  (ffi:def-function ("WSACreateEvent" wsa-event-create)
    ()
    :returning ws-event
    :module "ws2_32")

  (ffi:def-function ("WSACloseEvent" c-wsa-event-close)
    ((event-object ws-event))
    :returning :int
    :module "ws2_32")

  (defun wsa-event-close (ws-event)
    (not (zerop (c-wsa-event-close ws-event))))

  (ffi:def-function ("WSAEnumNetworkEvents" wsa-enum-network-events)
    ((socket ws-socket)
     (event-object ws-event)
     (network-events (* wsa-network-events)))
    :returning :int
    :module "ws2_32")

  (ffi:def-function ("WSAEventSelect" wsa-event-select)
    ((socket ws-socket)
     (event-object ws-event)
     (network-events :long))
    :returning :int
    :module "ws2_32")

  (ffi:def-function ("WSAWaitForMultipleEvents" c-wsa-wait-for-multiple-events)
    ((number-of-events ws-dword)
     (events (* ws-event))
     (wait-all-p :int)
     (timeout ws-dword)
     (alertable-p :int))
    :returning ws-dword
    :module "ws2_32")

  (defun wsa-wait-for-multiple-events (number-of-events events wait-all-p timeout alertable-p)
    (c-wsa-wait-for-multiple-events number-of-events
                                    events
                                    (if wait-all-p -1 0)
                                    timeout
                                    (if alertable-p -1 0)))

  (ffi:def-function ("ioctlsocket" wsa-ioctlsocket)
    ((socket ws-socket)
     (cmd :long)
     (argp (* :unsigned-long)))
    :returning :int
    :module "ws2_32")

  (ffi:def-function ("WSAGetLastError" wsa-get-last-error)
    ()
    :returning :int
    :module "ws2_32")

  (defun maybe-wsa-error (rv &optional socket)
    (unless (zerop rv)
      (raise-usock-err (wsa-get-last-error) socket)))

  (defun bytes-available-for-read (socket)
    (ffi:with-foreign-object (int-ptr :unsigned-long)
      (maybe-wsa-error (wsa-ioctlsocket (socket-handle socket) fionread int-ptr)
                       socket)
      (let ((int (ffi:deref-pointer int-ptr :unsigned-long)))
        (prog1 int
          (when (plusp int)
            (setf (state socket) :read))))))

  (defun map-network-events (func network-events)
    (let ((event-map (ffi:get-slot-value network-events 'wsa-network-events 'network-events))
          (error-array (ffi:get-slot-pointer network-events 'wsa-network-events 'error-code)))
      (unless (zerop event-map)
        (dotimes (i fd-max-events)
          (unless (zerop (ldb (byte 1 i) event-map))
            (funcall func (ffi:deref-array error-array '(:array :int 10) i)))))))

  (defun update-ready-and-state-slots (sockets)
    (dolist (socket sockets)
      (if (%ready-p socket)
          (progn
            (setf (state socket) :READ))
        (ffi:with-foreign-object (network-events 'wsa-network-events)
          (let ((rv (wsa-enum-network-events (socket-handle socket) 0 network-events)))
            (if (zerop rv)
                (map-network-events
                 #'(lambda (err-code)
                     (if (zerop err-code)
                         (progn
                           (setf (state socket) :READ)
                           (when (stream-server-usocket-p socket)
                             (setf (%ready-p socket) t)))
                       (raise-usock-err err-code socket)))
                 network-events)
              (maybe-wsa-error rv socket)))))))

  (defun os-wait-list-%wait (wait-list)
    (ffi:deref-pointer (wait-list-%wait wait-list) 'ws-event))

  (defun (setf os-wait-list-%wait) (value wait-list)
    (setf (ffi:deref-pointer (wait-list-%wait wait-list) 'ws-event) value))

  (defun free-wait-list (wl)
    (when (wait-list-p wl)
      (unless (null (wait-list-%wait wl))
        (wsa-event-close (os-wait-list-%wait wl))
        (ffi:free-foreign-object (wait-list-%wait wl))
        (setf (wait-list-%wait wl) nil))))

  (defun %setup-wait-list (wait-list)
    (setf (wait-list-%wait wait-list)
          (ffi:allocate-foreign-object 'ws-event))
    (setf (os-wait-list-%wait wait-list)
          (wsa-event-create))
    (ext:set-finalizer wait-list #'free-wait-list))

  (defun os-socket-handle (usocket)
    (socket-handle usocket))

) ; #+(and ecl-bytecmp windows)

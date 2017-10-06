;;;; -*- Mode: Lisp -*-
;;;; $Id: usocket-test.asd 46 2006-02-06 20:50:07Z ehuelsmann $
;;;; $URL: svn+ssh://common-lisp.net/project/usocket/svn/usocket/trunk/test/usocket-test.asd $

;;;; See the LICENSE file for licensing information.

(defsystem usocket-test
    :name "usocket test"
    :author "Erik Enge"
    :maintainer "Chun Tian (binghe)"
    :version "0.2.0"
    :licence "MIT"
    :description "Tests for usocket"
    :depends-on (:usocket-server
                 :rt)
    :components ((:module "test"
		  :serial t
		  :components ((:file "package")
			       (:file "test-usocket")
			       (:file "test-condition")
			       (:file "test-datagram")
			       (:file "wait-for-input")))))

(defmethod perform ((op test-op) (c (eql (find-system :usocket-test))))
  (funcall (intern "DO-TESTS" "USOCKET-TEST")))

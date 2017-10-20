(in-package :cl-user)

(require "asdf")
(load (merge-pathnames "../../lib/cl/load-locally.lisp" *load-truename*))
(load (merge-pathnames "externals/bundle.lisp" *load-truename*))
(asdf:load-system :net.didierverna.clon)
(asdf:load-system :fiasco)
(asdf:load-asd (merge-pathnames "gen-cl/ThriftTest/thrift-gen-ThriftTest.asd"))
(asdf:load-system :thrift-gen-thrifttest)

(net.didierverna.clon:nickname-package)

(defpackage :thrift-cross
  (:use :common-lisp :fiasco)
  (:export :cross-test))

(in-package :thrift-cross)

(defparameter *prot* nil)

(load (merge-pathnames "tests.lisp" *load-truename*))

(clon:defsynopsis ()
  (text :contents "The Common Lisp client for Thrift's cross-language test suite.")
  (group (:header "Allowed options:")
    (flag :short-name "h" :long-name "help"
          :description "Print this help and exit.")
    (stropt :long-name "host"
	    :description "The host to connect to."
	    :default-value "localhost"
	    :argument-name "ARG")
    (stropt :long-name "port"
	    :description "Number of the port to listen for connections on."
	    :default-value "9090"
	    :argument-name "ARG"
	    :argument-type :optional)
    (stropt :long-name "transport"
	    :description "Transport: transport to use; one of: \"buffered\", \"framed\""
	    :default-value "buffered"
	    :argument-name "ARG")
    (stropt :long-name "protocol"
	    :description "Protocol: currently only \"binary\""
	    :default-value "binary"
	    :argument-name "ARG")))

(defun main ()
  "Entry point for our standalone application."
  (clon:make-context)
  (when (clon:getopt :short-name "h")
    (clon:help)
    (clon:exit))
  (let ((port "9090")
	(host "localhost")
        (framed nil))
    (clon:do-cmdline-options (option name value source)
      (print (list option name value source))
      (if (string= name "host")
	  (setf host value))
      (if (string= name "port")
	  (setf port value))
      (if (string= name "transport")
          (cond ((string= value "buffered") (setf framed nil))
                ((string= value "framed") (setf framed t))
                (t (error "Unsupported transport.")))))
    (terpri)
    (setf *prot* (thrift.implementation::client (puri:parse-uri
                                                 (concatenate 'string "thrift://" host ":" port))
                                                :framed framed))
    (let ((result (cross-test)))
      (thrift.implementation::close *prot*)
      (clon:exit result))))

(clon:dump "TestClient" main)

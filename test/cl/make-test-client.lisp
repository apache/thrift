(in-package :cl-user)

(require "asdf")
(load (merge-pathnames "../../lib/cl/load-locally.lisp" *load-truename*))
(load (merge-pathnames "externals/bundle.lisp" *load-truename*))
(asdf:load-system :net.didierverna.clon)
(asdf:load-system :fiasco)
(asdf:load-asd (merge-pathnames "gen-cl/ThriftTest/thrift-gen-ThriftTest.asd"))
(asdf:load-system :thrift-gen-thrifttest)

(net.didierverna.clon:nickname-package)

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
	    :description "Transport: currently only \"buffered\""
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
	(host "localhost"))
    (clon:do-cmdline-options (option name value source)
      (print (list option name value source))
      (if (string= name "host")
	  (setf host value))
      (if (string= name "port")
	  (setf port value)))
    (terpri)
    (thrift:with-client (prot (puri:parse-uri (concatenate 'string
							   "thrift://"
							   host
							   ":"
							   port)))
      ))
  (clon:exit))

(clon:dump "TestClient" main)

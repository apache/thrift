(in-package :cl-user)

(require "asdf")
(load (merge-pathnames "../../lib/cl/load-locally.lisp" *load-truename*))
(load (merge-pathnames "externals/bundle.lisp" *load-truename*))
(asdf:load-system :net.didierverna.clon)
(asdf:load-asd (merge-pathnames "gen-cl/ThriftTest/thrift-gen-ThriftTest.asd"))
(asdf:load-system :thrift-gen-thrifttest)
(load (merge-pathnames "implementation.lisp"))

(net.didierverna.clon:nickname-package)

(clon:defsynopsis ()
  (text :contents "The Common Lisp server for Thrift's cross-language test suite.")
  (group (:header "Allowed options:")
    (flag :short-name "h" :long-name "help"
          :description "Print this help and exit.")
    (stropt :long-name "port"
	    :description "Number of the port to listen for connections on."
	    :default-value "9090"
	    :argument-name "ARG"
	    :argument-type :optional)
    (stropt :long-name "server-type"
	    :description "The type of server, currently only \"simple\" is available."
	    :default-value "simple"
	    :argument-name "ARG")
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
  (let ((port "9090"))
    (clon:do-cmdline-options (option name value source)
      (print (list option name value source))
      (if (string= name "port")
	  (setf port value)))
    (terpri)
    (thrift:serve (puri:parse-uri (concatenate 'string
					       "thrift://127.0.0.1:"
					       port))
		  thrift.test:thrift-test))
  (clon:exit))

(clon:dump "TestServer" main)

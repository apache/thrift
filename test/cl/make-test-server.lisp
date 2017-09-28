(in-package :cl-user)

(require "asdf")
(load (merge-pathnames "../../lib/cl/load-locally.lisp" *load-truename*))
(load (merge-pathnames "externals/bundle.lisp" *load-truename*))
(asdf:load-system :net.didierverna.clon)

(net.didierverna.clon:nickname-package)

(clon:defsynopsis (:postfix "FILES...")
  (text :contents "A very short program.")
  (group (:header "Immediate exit options:")
    (flag :short-name "h" :long-name "help"
          :description "Print this help and exit.")
    (flag :short-name "v" :long-name "version"
          :description "Print version number and exit.")))

(defun main ()
  "Entry point for our standalone application."
  (clon:make-context)
  (when (clon:getopt :short-name "h")
    (clon:help)
    (clon:exit))
  (clon:do-cmdline-options (option name value source)
    (print (list option name value source)))
  (terpri)
  (clon:exit))

(clon:dump "TestServer" main)

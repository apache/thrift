(in-package #:cffi-example)

(defcfun "puts" :int
  "Put a string to standard output, return non-negative length output, or EOF"
  (string :string))

(defun check-groveller ()
  (assert (equal (list +a0+ +a1+ +a2+ +a3+ +a4+) '(2 4 8 16 32)))
  (assert (equal (bn 1) 32)))

(defun entry-point ()
  (when uiop:*command-line-arguments*
    (uiop:format! t "Arguments: ~A~%" (uiop:escape-command uiop:*command-line-arguments*)))
  (puts "hello, world!")
  (check-groveller)
  (uiop:finish-outputs)
  (uiop:quit 0))

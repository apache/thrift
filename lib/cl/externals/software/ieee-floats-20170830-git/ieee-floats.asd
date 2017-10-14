(defsystem :ieee-floats
  :description "Convert floating point values to IEEE 754 binary representation"
  :author "Marijn Haverbeke <marijnh@gmail.com>"
  :license "BSD"
  :components ((:file "ieee-floats"))
  :in-order-to ((test-op (test-op "ieee-floats-tests"))))

(defsystem :ieee-floats-tests
  :description "Test suite for ieee-floats"
  :author "Marijn Haverbeke <marijnh@gmail.com>"
  :license "BSD"
  :depends-on (:ieee-floats :fiveam)
  :components ((:file "tests"))
  :perform (test-op (o s) (uiop:symbol-call :fiveam '#:run! :ieee-floats)))

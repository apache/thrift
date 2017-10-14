(defsystem "alexandria-tests"
  :licence "Public Domain / 0-clause MIT"
  :description "Tests for Alexandria, which is a collection of portable public domain utilities."
  :author "Nikodemus Siivola <nikodemus@sb-studio.net>, and others."
  :depends-on (:alexandria #+sbcl :sb-rt #-sbcl :rt)
  :components ((:file "tests"))
  :perform (test-op (o c)
             (flet ((run-tests (&rest args)
                      (apply (intern (string '#:run-tests) '#:alexandria-tests) args)))
               (run-tests :compiled nil)
               (run-tests :compiled t))))

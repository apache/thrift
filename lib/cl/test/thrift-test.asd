(in-package :common-lisp-user)

(asdf:defsystem :thrift-test
  :depends-on (:thrift
               :bordeaux-threads)
  :description "tests for com.apache.thrift"
  :serial t
  :components ((:file "package")
               (:file "vector-protocol")
               (:file "test")
               (:file "conditions")
               (:file "definition-operators")
               (:file "protocol")
               #+(or)
               (:module :gen-cl
                :serial t
                :components ((:file "AnnotationTest-types")
                             (:file "AnnotationTest-vars")
                             (:file "AnnotationTest")
                             ; (:file "ConstantsDemo")
                             ; (:file "DebugProtoTest")
                             ; (:file "DenseLinkingTest")
                             ; (:file "DocTest")
                             ;(:file "JavaBeansTest")
                             (:file "ManyTypedefs-types")
                             (:file "ManyTypedefs-vars")
                             (:file "ManyTypedefs")
                             ;(:file "OptionalRequiredTest")
                             (:file "SmallTest-types")
                             (:file "SmallTest-vars")
                             (:file "SmallTest")
                             (:file "StressTest-types.lisp")
                             (:file "StressTest-vars.lisp")
                             (:file "StressTest.lisp")
                             (:file "ThriftTest-types")
                             (:file "ThriftTest-vars") empty
                             (:file "ThriftTest")
                             ))))


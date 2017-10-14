(asdf:defsystem #:closer-mop
  :name "Closer to MOP"
  :description "Closer to MOP is a compatibility layer that rectifies many of the absent or incorrect CLOS MOP features across a broad range of Common Lisp implementations."
  :author "Pascal Costanza"
  :version "1.0.0"
  :licence "MIT-style license"
  :serial t
  :components
  ((:file "closer-mop-packages")
   (:file "closer-mop-shared")
   (:file "closer-abcl"      :if-feature :abcl)
   (:file "closer-allegro"   :if-feature :allegro)
   (:file "closer-clasp"     :if-feature :clasp)
   (:file "closer-clisp"     :if-feature :clisp)
   (:file "closer-clozure"   :if-feature :clozure)
   (:file "closer-cmu"       :if-feature :cmu)
   (:file "closer-ecl"       :if-feature :ecl)
   (:file "closer-lispworks" :if-feature :lispworks)
   (:file "closer-mcl"       :if-feature :mcl)
   (:file "closer-sbcl"      :if-feature :sbcl)
   (:file "closer-scl"       :if-feature :scl)))

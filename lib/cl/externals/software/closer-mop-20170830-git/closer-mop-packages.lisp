(in-package :cl-user)

(defpackage #:closer-mop
  (:use #:common-lisp #+lispworks #:lispworks)
  (:nicknames #:c2mop)

  #+(or allegro clozure lispworks mcl)
  (:shadow #:standard-class)

  #+(or allegro clisp clozure ecl clasp lispworks sbcl)
  (:shadow #:defgeneric #:defmethod #:standard-generic-function)

  #+clozure (:shadow standard-method)

  #+(or cmu mcl) (:shadow #:typep subtypep)

  #+lispworks5.1
  (:import-from #:system #:with-hash-table-locked)
  #+(or lispworks6 lispworks7)
  (:import-from #:hcl #:with-hash-table-locked)

  #-(or clisp scl)
  (:import-from
   #+abcl      #:ext
   #+allegro   #:excl
   #+clozure   #:ccl
   #+cmu       #:pcl
   #+ecl       #:clos
   #+clasp     #:clos
   #+lispworks #:clos
   #+mcl       #:ccl
   #+sbcl      #:sb-pcl

   #:classp)

  (:import-from
   #+abcl      #:mop
   #+allegro   #:mop
   #+clisp     #:clos
   #+clozure   #:ccl
   #+cmu       #:clos-mop
   #+ecl       #:clos
   #+clasp     #:clos
   #+lispworks #:clos
   #+mcl       #:ccl
   #+sbcl      #:sb-mop
   #+scl       #:clos

   #:direct-slot-definition
   #:effective-slot-definition
   #-lispworks #:eql-specializer
   #:forward-referenced-class
   #-lispworks #:funcallable-standard-class
   #-lispworks4 #:funcallable-standard-object
   #:metaobject
   #:slot-definition
   #-(or lispworks4 lispworks5 scl) #:specializer
   #:standard-accessor-method
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   #:standard-reader-method
   #:standard-slot-definition
   #:standard-writer-method

   #-lispworks4.3 #:accessor-method-slot-definition
   #-scl #:add-dependent
   #-scl #:add-direct-method
   #:add-direct-subclass
   #-scl #:class-default-initargs
   #-scl #:class-direct-default-initargs
   #:class-direct-slots
   #:class-direct-subclasses
   #:class-direct-superclasses
   #:class-finalized-p
   #:class-precedence-list
   #:class-prototype
   #:class-slots
   #-(or clozure lispworks mcl) #:compute-applicable-methods-using-classes
   #:compute-class-precedence-list
   #-(or lispworks4 lispworks5) #:compute-default-initargs
   #-clozure #:compute-discriminating-function
   #-(or clozure scl) #:compute-effective-method
   #:compute-effective-slot-definition
   #:compute-slots
   #:direct-slot-definition-class
   #:effective-slot-definition-class
   #:ensure-class
   #:ensure-class-using-class
   #:ensure-generic-function-using-class
   #-lispworks #:eql-specializer-object
   #:extract-lambda-list
   #:extract-specializer-names
   #:finalize-inheritance
   #-lispworks #:find-method-combination
   #-(or lispworks scl) #:funcallable-standard-instance-access
   #-allegro #:generic-function-argument-precedence-order
   #:generic-function-declarations
   #:generic-function-lambda-list
   #:generic-function-method-class
   #:generic-function-method-combination
   #:generic-function-methods
   #:generic-function-name
   #-lispworks #:intern-eql-specializer
   #-(or allegro clisp clozure lispworks mcl scl) #:make-method-lambda
   #-scl #:map-dependents
   #-clozure #:method-function
   #:method-generic-function
   #:method-lambda-list
   #:method-specializers
   #-lispworks4.3 #:reader-method-class
   #-scl #:remove-dependent
   #-scl #:remove-direct-method
   #:remove-direct-subclass
   #:set-funcallable-instance-function
   #:slot-boundp-using-class
   #:slot-definition-allocation
   #:slot-definition-initargs
   #:slot-definition-initform
   #:slot-definition-initfunction
   #:slot-definition-location
   #:slot-definition-name
   #:slot-definition-readers
   #:slot-definition-writers
   #:slot-definition-type
   #:slot-makunbound-using-class
   #:slot-value-using-class
   #-lispworks #:specializer-direct-generic-functions
   #:specializer-direct-methods
   #-lispworks #:standard-instance-access
   #-scl #:update-dependent
   #:validate-superclass
   #-lispworks4.3 #:writer-method-class)

  (:export
   #:built-in-class
   #:class
   #:direct-slot-definition
   #:effective-slot-definition
   #:eql-specializer
   #+lispworks #:eql-specializer*
   #:forward-referenced-class
   #:funcallable-standard-class
   #:funcallable-standard-object
   #:generic-function
   #:metaobject
   #:method
   #:method-combination
   #:slot-definition
   #:specializer
   #:standard-accessor-method
   #:standard-class
   #:standard-generic-function
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   #:standard-method
   #:standard-object
   #:standard-reader-method
   #:standard-slot-definition
   #:standard-writer-method

   #:defclass
   #:defgeneric
   #:define-method-combination
   #:defmethod

   #:classp
   #:ensure-finalized
   #:ensure-method
   #:fix-slot-initargs
   #:required-args
   #:subclassp

   #:accessor-method-slot-definition
   #:add-dependent
   #:add-direct-method
   #:add-direct-subclass
   #:class-default-initargs
   #:class-direct-default-initargs
   #:class-direct-slots
   #:class-direct-subclasses
   #:class-direct-superclasses
   #:class-finalized-p
   #:class-precedence-list
   #:class-prototype
   #:class-slots
   #:compute-applicable-methods-using-classes
   #:compute-class-precedence-list
   #:compute-default-initargs
   #:compute-discriminating-function
   #:compute-effective-method
   #:compute-effective-method-function
   #:compute-effective-slot-definition
   #:compute-slots
   #:direct-slot-definition-class
   #:effective-slot-definition-class
   #:ensure-class
   #:ensure-class-using-class
   #:ensure-generic-function
   #:ensure-generic-function-using-class
   #:eql-specializer-object
   #:extract-lambda-list
   #:extract-specializer-names
   #:finalize-inheritance
   #:find-method-combination
   #:funcallable-standard-instance-access
   #:generic-function-argument-precedence-order
   #:generic-function-declarations
   #:generic-function-lambda-list
   #:generic-function-method-class
   #:generic-function-method-combination
   #:generic-function-methods
   #:generic-function-name
   #:intern-eql-specializer
   #+lispworks #:intern-eql-specializer*
   #:make-method-lambda
   #:map-dependents
   #:method-function
   #:method-generic-function
   #:method-lambda-list
   #:method-specializers
   #:reader-method-class
   #:remove-dependent
   #:remove-direct-method
   #:remove-direct-subclass
   #:set-funcallable-instance-function
   #:slot-boundp-using-class
   #:slot-definition-allocation
   #:slot-definition-initargs
   #:slot-definition-initform
   #:slot-definition-initfunction
   #:slot-definition-location
   #:slot-definition-name
   #:slot-definition-readers
   #:slot-definition-writers
   #:slot-definition-type
   #:slot-makunbound-using-class
   #:slot-value-using-class
   #:specializer-direct-generic-functions
   #:specializer-direct-methods
   #:standard-instance-access
   #:subtypep
   #:typep
   #:update-dependent
   #:validate-superclass
   #:writer-method-class

   #:warn-on-defmethod-without-generic-function))

(in-package :closer-mop)

(macrolet ((define-closer-common-lisp-package ()
             (loop with symbols = (nunion (loop for sym being the external-symbols of :common-lisp
                                                if (find-symbol (symbol-name sym) :c2mop)
                                                collect it
                                                else collect sym)
                                          (loop for sym being the external-symbols of :c2mop
                                                collect sym))
                   with map = '()
                   for symbol in symbols do
                   (push (symbol-name symbol)
                         (getf map (symbol-package symbol)))
                   finally (return 
                            `(defpackage #:closer-common-lisp
                               (:nicknames #:c2cl)
                               (:use)
                               ,@(loop for (package symbols) on map by #'cddr
                                       collect `(:import-from ,(package-name package) ,@symbols))
                               (:export ,@(mapcar #'symbol-name symbols)))))))
  (define-closer-common-lisp-package))

(defpackage #:closer-common-lisp-user
  (:nicknames #:c2cl-user)
  (:use #:closer-common-lisp))

(in-package :cl-user)

#|
  :name "Closer to MOP"
  :author "Pascal Costanza"
  :version "1.0.0"
  :licence "MIT-style license"
|#

(defsystem :closer-mop ()
  (:serial
   "closer-mop-packages"
   "closer-mop-shared"
   "closer-allegro"))

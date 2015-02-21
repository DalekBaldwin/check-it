(in-package :cl-user)

(defpackage :check-it
  (:use :cl :alexandria :optima :clometa.c)
  (:export #:shrink
           
           #:*size*
           
           #:generator
           #:generate
           
           #:int-generator
           #:real-generator
           #:list-generator
           #:tuple-generator
           #:or-generator
           #:guard-generator
           #:object-generator
           #:generate
           
           #:tuple
           
           #:element
           #:elements
           #:guard
           #:constructor
           #:slot-names
           #:slot-generators))

(in-package :check-it)

(defparameter *system-directory*
  (make-pathname
   :directory
   (pathname-directory
    (asdf:system-definition-pathname "check-it"))))

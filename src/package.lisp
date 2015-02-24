(in-package :cl-user)

(defpackage :check-it
  (:use :cl :alexandria :optima)
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
           #:struct
           
           #:cached-value
           #:cached-generator
           #:element
           #:elements
           #:guard
           #:constructor
           #:slot-names
           #:slot-generators
           
           #:defgenerator))

(in-package :check-it)

(defparameter *system-directory*
  (make-pathname
   :directory
   (pathname-directory
    (asdf:system-definition-pathname "check-it"))))

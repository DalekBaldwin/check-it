(in-package :cl-user)

(defpackage :check-it
  (:use :cl :alexandria :optima)
  (:export #:shrink
           
           #:*size*
           #:*list-size*
           #:*list-size-decay*
           #:*num-trials*
           #:*bias-sensitivity*
           #:*recursive-bias-decay*
           
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
           
           #:defgenerator
           
           #:*check-it-output*
           #:regression-case
           #:check-it))

(in-package :check-it)

(defparameter *system-directory*
  (make-pathname
   :directory
   (pathname-directory
    (asdf:system-definition-pathname "check-it"))))

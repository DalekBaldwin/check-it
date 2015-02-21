(in-package :cl-user)

(defpackage :check-it
  (:use :cl :alexandria)
  (:export #:shrink))

(in-package :check-it)

(defparameter *system-directory*
  (make-pathname
   :directory
   (pathname-directory
    (asdf:system-definition-pathname "check-it"))))

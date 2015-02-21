(in-package :cl-user)

(defpackage :check-it-test
  (:use :cl :check-it :stefil)
  (:export
   #:test-all))

(in-package :check-it-test)

(defparameter *system-directory* check-it::*system-directory*)

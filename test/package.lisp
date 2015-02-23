(in-package :cl-user)

(defpackage :check-it-test
  (:use :cl :check-it :stefil :alexandria)
  (:export
   #:run-all-tests
   #:deterministic-tests
   #:randomized-tests))

(in-package :check-it-test)

(defparameter *system-directory* check-it::*system-directory*)

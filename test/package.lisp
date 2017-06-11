(in-package :cl-user)

(fiasco:define-test-package :check-it/test
  (:use :cl
        :check-it
        :fiasco
        :alexandria)
  #+nil
  (:export
   #:run-all-tests
   #:deterministic-tests
   #:randomized-tests))

(in-package :check-it/test)

(defparameter *system-directory* check-it::*system-directory*)

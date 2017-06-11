(in-package :cl-user)

(fiasco:define-test-package :check-it/test
  (:use :cl
        :check-it
        :fiasco
        :alexandria))

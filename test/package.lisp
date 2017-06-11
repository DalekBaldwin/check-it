(in-package :cl-user)

(defpackage :check-it/test
  (:use :fiasco
        :cl)
  (:use :cl
        :check-it
        :fiasco
        :alexandria))

(fiasco:defsuite
    (fiasco-suites::check-it/test :bind-to-package :check-it/test
                                  :in fiasco-suites::all-tests))

(in-package :check-it/test)

(defparameter *system-directory* (asdf:system-source-directory "check-it/test"))

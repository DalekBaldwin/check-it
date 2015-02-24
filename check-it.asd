;;;; check-it.asd

(defpackage :check-it-system
  (:use :cl :asdf))
(in-package :check-it-system)

(defsystem :check-it
  :name "check-it"
  :serial t
  :components
  ((:static-file "check-it.asd")
   (:module :src
            :components ((:file "package")
                         (:file "util")
                         (:file "generators")
                         (:file "shrink")
                         (:file "check-it"))
            :serial t))
  :depends-on (:alexandria :closer-mop :optima))

(defsystem :check-it-test
  :name "check-it-test"
  :serial t
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "check-it-test")
                         (:file "deterministic-tests")
                         (:file "randomized-tests")
                         (:file "for-travis"))))
  :depends-on (:check-it :stefil))

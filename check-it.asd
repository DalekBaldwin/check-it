;;;; check-it.asd

(defsystem "check-it"
  :name "check-it"
  :serial t
  :author "Kyle Littler"
  :license "LLGPL"
  :description "A randomized property-based testing tool for Common Lisp."
  :homepage "https://github.com/DalekBaldwin/check-it"
  :version "0.1.0"
  :components
  ((:static-file "check-it.asd")
   (:module "src"
            :components ((:file "package")
                         (:file "util")
                         (:file "generators")
                         (:file "regenerate")
                         (:file "shrink")
                         (:file "check-it"))
            :serial t))
  :depends-on ("alexandria"
               "closer-mop"
               "optima")
  :in-order-to ((test-op (test-op "check-it/test"))))

(defsystem "check-it/test"
  :serial t
  :author "Kyle Littler"
  :license "LLGPL"
  :version "0.1.0"
  :description "Tests for check-it."
  :components
  ((:module "test"
            :components ((:file "package")
                         (:file "system")
                         (:file "check-it-test")
                         (:file "deterministic-tests")
                         (:file "randomized-tests")
                         (:file "destructive-tests")
                         (:file "for-travis"))))
  :depends-on ("check-it"
               "fiasco")
  :perform (test-op (op c) (symbol-call :check-it/test '#:run-all-tests)))

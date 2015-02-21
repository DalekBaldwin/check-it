(in-package :check-it-test)

(in-root-suite)

(defsuite* test-all)

(defun int-tester (int)
  (< int 5))

(deftest test-shrink ()
  (loop for int in (list 5 20 100 300)
     do
       (is (= (shrink int #'int-tester) 5))))

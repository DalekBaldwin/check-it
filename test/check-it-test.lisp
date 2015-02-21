(in-package :check-it-test)

(in-root-suite)

(defsuite* test-all)

(defun int-tester (int)
  (< int 5))

(deftest test-int-shrink ()
  (loop for int in (list 5 20 100 300)
     do
       (is (= (shrink int #'int-tester) 5))))

(defun list-tester (list)
  (< (length list) 5))

(deftest test-list-shrink ()
  (loop for size in (list 5 20 50 100)
       do
       (is (equal (shrink (make-list size :initial-element nil) #'list-tester)
                  '(nil nil nil nil nil)))))

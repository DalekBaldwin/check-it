(in-package :check-it-test)

(in-root-suite)

(defsuite* deterministic-tests)

(deftest test-int-shrink ()
  (loop for int in (list 5 20 100 300)
     do
       (is (= (shrink int #'int-tester) 5))))

(deftest test-list-shrink ()
  (loop for size in (list 5 20 50 100)
       do
       (is (equal (shrink (make-list size :initial-element nil) #'list-tester)
                  '(nil nil nil nil nil)))))

(deftest test-struct-slot-names ()
  (let ((test-struct (make-a-struct :a-slot 5 :another-slot 5)))
    (is (equalp (check-it::struct-slot-names test-struct)
                (list 'a-slot 'another-slot)))))

#-(or abcl allegro)
(deftest test-copy-structure-and-slots ()
  (let ((test-struct (make-a-struct :a-slot 5 :another-slot 5)))
    (is (equalp (check-it::copy-structure-and-slots
                 test-struct
                 (list 'a-slot 'another-slot))
                test-struct))))

(deftest test-struct-shrink ()
  (let ((test-struct (make-a-struct :a-slot 5 :another-slot 5)))
    (loop for i in (list 5 20 100 300)
       for j in (list 5 20 100 300)
       do
         (is (equalp (shrink (make-a-struct
                              :a-slot i
                              :another-slot j)
                             #'struct-tester)
                     test-struct)))))

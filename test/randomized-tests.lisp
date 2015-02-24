(in-package :check-it-test)

(in-root-suite)

(defsuite* randomized-tests)

(deftest test-generator ()
  (is (every #'identity
       (mapcar (lambda (x y) (subtypep (type-of x) y))
               (generate (generator (tuple (real) (integer) (list (integer)))))
               '(single-float integer
                 #-abcl (or cons null)
                 #+abcl t ;; ridiculous
                 )))))

(deftest test-int-generate-shrink ()
  (let ((generator (generator (guard #'positive-integer-p (integer)))))
    (loop for i from 0 to 100
       do
         (is (= (shrink (generate generator) (constantly nil)) 0)))))

(deftest test-struct-generate-shrink ()
  (let ((generator (generator (struct a-struct
                                      #+(or abcl allegro) make-a-struct
                                      :a-slot (integer)
                                      :another-slot (integer))))
        (test-struct (make-a-struct :a-slot 0 :another-slot 0)))
    (loop for i from 1 to 10
       do
         (is (equalp (shrink (generate generator) (constantly nil))
                     test-struct)))))

(deftest test-tuple-generator-shrink ()
  (let ((generator (generator (tuple (integer) (integer) (integer)))))
    (loop for i from 1 to 10
       do
         (progn
           (generate generator)
           (is (equal (shrink generator (constantly nil))
                      (list 0 0 0))))))
  (let ((generator (generator (tuple
                               (guard #'greater-than-5 (integer))
                               (guard #'greater-than-5 (integer))
                               (guard #'greater-than-5 (integer))))))
    (loop for i from 1 to 10
       do
         (progn
           (generate generator)
           (is (every (lambda (x) (= (abs x) 6))
                      (shrink generator #'tuple-tester)))))))

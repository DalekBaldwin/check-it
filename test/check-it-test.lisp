(in-package :check-it-test)

(in-root-suite)

(defsuite* test-all)

(deftest test-generator ()
  (is (every #'identity
       (mapcar (lambda (x y) (subtypep (type-of x) y))
               (generate (generator (tuple (real) (integer) (list (integer)))))
               '(single-float integer cons)))))

(defun int-tester (int)
  (< int 5))

(deftest test-int-shrink ()
  (loop for int in (list 5 20 100 300)
     do
       (is (= (shrink int #'int-tester) 5))))

(deftest test-int-generate-shrink ()
  (let ((generator (generator (guard #'positive-integer-p (integer)))))
    (loop for i from 0 to 100
       do
         (is (= (shrink (generate generator) (constantly nil)) 0)))))

(defun list-tester (list)
  (< (length list) 5))

(deftest test-list-shrink ()
  (loop for size in (list 5 20 50 100)
       do
       (is (equal (shrink (make-list size :initial-element nil) #'list-tester)
                  '(nil nil nil nil nil)))))

(defstruct a-struct
  a-slot
  another-slot)

(defun struct-tester (struct)
  (or (< (a-struct-a-slot struct) 5)
      (< (a-struct-another-slot struct) 5)))

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



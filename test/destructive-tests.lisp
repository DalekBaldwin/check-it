(in-package :check-it/test)

;;;; Test pattern: copy generated value, mutate value in cache, regenerate, and compare

(deftest test-list-generator-mutation ()
  (let ((g (generator (list (integer) :min-length 2)))
        (filler (generator (list (integer) :min-length 3))))
    (loop repeat 10
       do
         (let* ((test-value (generate g))
                (copied (copy-list test-value)))
           (setf (car test-value) :derp)
           (setf (cdr test-value) (generate filler))
           (is (equal (regenerate g) copied))))))

(deftest test-tuple-generator-mutation ()
  (let ((g (generator (tuple (integer) (integer))))
        (filler (generator (tuple (integer) (integer)))))
    (loop repeat 10
       do
         (let* ((test-value (generate g))
                (copied (copy-list test-value))
                (fill-value (generate filler)))
           (setf (first test-value) (first fill-value)
                 (cdr test-value) fill-value)
           (is (equal (regenerate g) copied))))))

(deftest test-string-generator-mutation ()
  (let ((g (generator (string :min-length 2))))
    (loop repeat 10
       do
         (let* ((test-value (generate g))
                (copied (copy-seq test-value)))
           (setf (aref test-value 1) #\x)
           (is (equal (regenerate g) copied))))))

(deftest test-struct-generator-mutation ()
  (let ((g (generator (struct a-struct
                              :a-slot (integer)
                              :another-slot (real)))))
    (loop repeat 10
       do
         (let* ((test-value (generate g))
                (copied (copy-structure test-value)))
           (setf (slot-value test-value 'a-slot) :derp
                 (slot-value test-value 'another-slot) :herp)
           (is (equalp (regenerate g) copied)))))
  (let ((g (generator (struct a-struct
                              :a-slot (list (integer) :min-length 2)
                              :another-slot (list (real) :min-length 2)))))
    (loop repeat 10
       do
         (let* ((test-value (generate g))
                (copied
                 (make-a-struct
                  :a-slot
                  (copy-list (slot-value test-value 'a-slot))
                  :another-slot
                  (copy-list (slot-value test-value 'another-slot)))))
           (setf (car (slot-value test-value 'a-slot)) :derp
                 (car (slot-value test-value 'another-slot)) :herp)
           (is (equalp (regenerate g) copied))))))

(deftest test-mapped-generator-mutation ()
  (let ((g (generator
            (map (lambda (x y) (list y x y x))
                 (list (integer) :min-length 2)
                 (list (integer) :min-length 3)))))
    (loop repeat 10
       do
         (let* ((test-value (generate g))
                (copied (copy-tree test-value)))
           (setf (caar test-value) :derp
                 (cddr test-value) (list 1 2 3 :herp))
           (is (equal (regenerate g) copied))))))

(deftest test-chained-generator-mutation ()
  (let ((g (generator
            (chain ((x (integer 10 20))
                    (y (integer 21 30)))
              (generator (list (integer x y) :min-length x :max-length y))))))
    (loop repeat 10
       do
         (let* ((test-value (generate g))
                (copied (copy-list test-value)))
           (setf (car test-value) :derp
                 (cddr test-value) (list 1 2 3 :herp))
           (is (equal (regenerate g) copied))))))

(deftest test-guard-generator-mutation ()
  (let ((g (generator
            (guard (lambda (x) (> (length x) 4))
                   (list (integer))))))
    (loop repeat 10
         do
         (let* ((test-value (generate g))
                (copied (copy-list test-value)))
           (setf (car test-value) :derp
                 (cddr test-value) (list 1 2 3 :herp))
           (is (equal (regenerate g) copied))))))

(deftest test-or-generator-mutation ()
  (let ((g (generator
            (or (list (integer 5) :min-length 2)
                (tuple (integer) (real))))))
    (loop repeat 10
       do
         (let* ((test-value (generate g))
                (copied (copy-tree test-value)))
           (setf (car test-value) :derp
                 (cdr test-value) (list 1 2 3 :herp))
           (is (equal (regenerate g) copied))))))

(deftest test-custom-generator-mutation ()
  (let ((g (generator (big-custom-generator))))
    (loop repeat 10
       do
         (let* ((test-value (generate g))
                (copied (if (listp test-value)
                            (copy-tree test-value)
                            (make-a-struct
                             :a-slot
                             (slot-value test-value 'a-slot)
                             :another-slot
                             (slot-value test-value 'another-slot)))))
           (if (listp test-value)
               (setf (car test-value) :derp
                     (cdr test-value) (list 1 2 3 :herp))
               (setf (slot-value test-value 'a-slot) :herp
                     (slot-value test-value 'another-slot) :derp))
           (is (equalp (regenerate g) copied))))))

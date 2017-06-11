(in-package :check-it/test)

(defun run-all-tests ()
  (format t "~&Running deterministic tests:~%")
  (deterministic-tests)
  (format t "~&Running randomized tests:~%")
  (randomized-tests))

(defun int-tester (int)
  (< int 5))

(defun list-tester (list)
  (< (length list) 5))

(defun struct-tester (struct)
  (or (< (a-struct-a-slot struct) 5)
      (< (a-struct-another-slot struct) 5)))

(defun tuple-tester (tuple)
  (some (lambda (x) (< (abs x) 5)) tuple))

(defun greater-than-5 (num)
  (> (abs num) 5))

(defstruct a-struct
  a-slot
  another-slot)

(in-package :check-it-test)

(defun run-tests-for-travis ()
  (let ((*debugger-hook*
         (lambda (c h)
           (declare (ignore h))
           (print c t)
           (force-output t)
           (sleep 1)
           (uiop:quit -1))))
    (run-all-tests)))

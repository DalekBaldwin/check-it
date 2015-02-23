(in-package :check-it-test)

(defun run-tests-for-travis ()
  (let ((*debugger-hook*
         (lambda (c h)
           (declare (ignore h))
           (format t "~&~A~%" c)
           (force-output t)
           (sleep 1)
           (uiop:quit -1))))
    (handler-case
        (run-all-tests)
      (stefil::assertion-failed (c)
        (format t "~&Test assertion failedz:~%~%")
        (describe (stefil::failure-description-of c) t)
        (force-output t)
        (sleep 1)
        (uiop:quit -1)))))

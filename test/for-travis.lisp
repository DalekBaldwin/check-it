(in-package :check-it/test)

(defun run-tests-for-travis ()
  (let ((*debugger-hook*
         (lambda (c h)
           (declare (ignore h))
           (format t "~&~A~%" c)
           (force-output t)
           (sleep 1)
           (uiop:quit -1))))
    (fiasco:run-package-tests :package :check-it/test)
    #+nil
    (handler-case
        (run-all-tests)
      ;; some Lisps appear not to use the condition's :report value,
      ;; so we reimplement it here explicitly
      (stefil::assertion-failed (c)
        (force-output t)
        (format t "~&Test assertion failed:~%~%")
        (describe (stefil::failure-description-of c) t)
        (format t "~%")
        (force-output t)
        (sleep 1)
        (uiop:quit -1)))))

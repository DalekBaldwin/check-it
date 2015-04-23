(in-package :check-it)

(defmacro regression-case (name datum)
  `(push
    ,datum
    (get ',name 'regression-cases)))

(defun check-it% (test-form generator test
                  &key (random-state t) regression-id regression-file)
  (block trial-run
    (when regression-id
      (loop for regression-case in (get regression-id 'regression-cases)
         do
           (let ((passed (funcall test regression-case)))
             (unless passed
               (format t "~&Test ~A failed regression ~A with arg ~A"
                       test-form
                       regression-id
                       regression-case)
               (return-from trial-run nil)))))
    (let ((*random-state* (make-random-state random-state)))
      (loop repeat *num-trials*
         do
           (progn
             (generate generator)
             (let ((passed (funcall test (cached-value generator))))
               (unless passed
                 (format t "~&Test ~A ~A failed with random state:~%~S~%with arg ~A~%"
                         test-form
                         test
                         *random-state*
                         (cached-value generator))
                 (let ((shrunk (shrink generator test)))
                   (format t "~&Shrunken failure case:~%~A~%" shrunk)
                   (when regression-file
                     (push shrunk (get regression-id 'regression-cases))
                     (with-open-file (s regression-file
                                        :direction :output
                                        :if-exists :append
                                        :if-does-not-exist :error)
                       (format s "~%~S~%"
                               `(regression-case
                                 ,regression-id
                                 (eval (read-from-string
                                        ,(format nil "~S" shrunk))))))))
                 (return-from trial-run nil))))))
    (return-from trial-run t)))

(defmacro check-it (generator test
                    &key
                      (random-state t random-state-supplied)
                      (regression-id nil regression-id-supplied)
                      (regression-file nil regression-file-supplied))
  `(check-it% ',test ,generator ,test
              ,@(when random-state-supplied `(:random-state ,random-state))
              ,@(when regression-id-supplied `(:regression-id ',regression-id))
              ,@(when regression-file-supplied `(:regression-file ,regression-file))))

(in-package :check-it)

(defparameter *test-output-template*
  (lambda (test-form datum &optional package)
    `(deftest ,(gentemp "TEST" package) ()
       (is (funcall ,test-form ,datum)))))

(defmacro check-it (generator-form test-form
                    &key (random-state t)
                      gen-output-file gen-output-package gen-output-template)
  (with-gensyms (gen test trial-run passed state shrunk stream)
    `(let ((,gen ,generator-form)
           (,test ,test-form))
       (block ,trial-run
         (setf *random-state* (make-random-state ,random-state))
         (loop repeat *num-trials*
            do
              (progn
                (generate ,gen)
                (let ((,state *random-state*)
                      (,passed (funcall ,test (cached-value ,gen))))
                  (unless ,passed
                    (format t "~&Test ~A failed with random state:~%~S~%with arg ~A"
                            ',test-form
                            ,state
                            (cached-value ,gen))
                    (let ((,shrunk (shrink ,gen ,test)))
                      (format t "~&Shrunken failure case:~%~A" ,shrunk)
                      (when ,gen-output-file
                        (with-open-file (,stream ,gen-output-file
                                                 :direction :output
                                                 :if-exists :append
                                                 :if-does-not-exist :error)
                          (format ,stream "~%~S~%"
                                  (funcall ,gen-output-template
                                           ,test-form
                                           ,shrunk
                                           ,gen-output-package)))))
                    (return-from ,trial-run)))))))))

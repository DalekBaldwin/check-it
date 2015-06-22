(in-package :check-it)

(defclass reified-error ()
  ((wrapped-error
    :initarg :wrapped-error
    :accessor wrapped-error)))

(defmacro regression-case (&key name datum timestamp)
  `(regression-case% ',name ,datum ,timestamp))

(defclass regression-case ()
  ((name
    :initarg :name
    :reader name)
   (datum
    :initarg :datum
    :reader datum)
   (timestamp
    :initarg :timestamp
    :reader timestamp)))

(defun regression-case% (name datum timestamp)
  (push
   (make-instance 'regression-case
                  :name name
                  ;; Some external representations can't be dumped to FASL
                  ;; files, so for now it's simplest to delay loading serialized
                  ;; forms until runtime
                  :datum (read-from-string datum)
                  :timestamp timestamp)
   (get name 'regression-cases)))

(defun write-regression-case (name datum)
  `(regression-case
    :name ,name
    :datum ,(format nil "~S" datum)
    :timestamp ,(get-universal-time)))

(defparameter *check-it-output* *standard-output*)

(defparameter *package-regression-files* (make-hash-table))

(defun register-package-regression-file (package regression-file)
  (setf (gethash
         (find-package package)
         *package-regression-files*)
        regression-file))

(defgeneric errored (result)
  (:method (result) nil)
  (:method ((result reified-error)) t))

(defun wrap-test-for-error-reporting (test)
  (lambda (arg)
    (handler-case
        (funcall test arg)
      (error (c)
        (make-instance 'reified-error c)))))

(defun wrap-test-for-shrinking (test)
  (lambda (arg)
    (handler-case
        (funcall test arg)
      (error (c) nil))))

(defun check-it% (test-form generator test
                  &key
                    examples
                    (random-state t)
                    (regression-id nil regression-id-supplied)
                    (regression-file (when regression-id-supplied
                                       (gethash
                                        (symbol-package regression-id)
                                        *package-regression-files*))))
  (let ((error-reporting-test
         (wrap-test-for-error-reporting test))
        (shrink-test
         (wrap-test-for-shrinking test)))
    (block trial-run
      (loop for example in examples
         do
           (let ((result (funcall error-reporting-test example)))
             ;; to-do: DRY this up
             (cond
               ((null result)
                (format *check-it-output* "~&Test ~A failed on example arg ~A~%"
                        test-form
                        example)
                (return-from trial-run nil))
               ((errored result)
                (format *check-it-output* "~&Test ~A signaled error ~A on example arg ~A~%"
                        test-form
                        (wrapped-error result)
                        example)
                (return-from trial-run nil)))))
      (when regression-id
        (loop for regression-case in (get regression-id 'regression-cases)
           do
             (let ((result (funcall error-reporting-test regression-case)))
               (cond
                 ((null result)
                  (format *check-it-output* "~&Test ~A failed regression ~A with arg ~A~%"
                          test-form
                          regression-id
                          regression-case)
                  (return-from trial-run nil))
                 ((errored result)
                  (format *check-it-output* "~&Test ~A signaled error ~A on regression ~A with arg ~A~%"
                          test-form
                          (wrapped-error result)
                          regression-id
                          regression-case)
                  (return-from trial-run nil))))))
      (let ((*random-state* (make-random-state random-state)))
        (loop repeat *num-trials*
           do
             (progn
               (generate generator)
               (let ((result (funcall error-reporting-test (cached-value generator))))
                 (flet ((do-shrink ()
                          (let ((shrunk (shrink generator shrink-test)))
                            (format *check-it-output* "~&Shrunken failure case:~%~A~%" shrunk)
                            (when regression-file
                              (push shrunk (get regression-id 'regression-cases))
                              (with-open-file (s regression-file
                                                 :direction :output
                                                 :if-exists :append
                                                 :if-does-not-exist :error)
                                (format s "~&~S~%"
                                        (write-regression-case regression-id shrunk)))))))
                   (cond
                     ((null result)
                      (format *check-it-output*
                              "~&Test ~A ~A failed with random state:~%~S~%with arg ~A~%"
                              test-form
                              test
                              *random-state*
                              (cached-value generator))
                      (do-shrink)
                      (return-from trial-run nil))
                     ((errored result)
                      (format *check-it-output*
                              "~&Test ~A ~A signaled error ~A with random state:~%~S~%with arg ~A~%"
                              test-form
                              test
                              (wrapped-error result)
                              *random-state*
                              (cached-value generator))
                      (do-shrink)
                      (return-from trial-run nil))))))))
      (return-from trial-run t))))

(defmacro check-it (generator test
                    &key
                      examples
                      (random-state t random-state-supplied)
                      (regression-id nil regression-id-supplied)
                      (regression-file nil regression-file-supplied))
  `(check-it% ',test ,generator ,test
              :examples ,examples
              ,@(when random-state-supplied `(:random-state ,random-state))
              ,@(when regression-id-supplied `(:regression-id ',regression-id))
              ,@(when regression-file-supplied `(:regression-file ,regression-file))))

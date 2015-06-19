(in-package :check-it)

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

(defun check-it% (test-form generator test
                  &key (random-state t)
                    (regression-id nil regression-id-supplied)
                    (regression-file (when regression-id-supplied
                                       (gethash
                                        (symbol-package regression-id)
                                        *package-regression-files*))))
  (block trial-run
    (when regression-id
      (loop for regression-case in (get regression-id 'regression-cases)
         do
           (let ((passed (funcall test regression-case)))
             (unless passed
               (format *check-it-output* "~&Test ~A failed regression ~A with arg ~A~%"
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
                 (format *check-it-output*
                         "~&Test ~A ~A failed with random state:~%~S~%with arg ~A~%"
                         test-form
                         test
                         *random-state*
                         (cached-value generator))
                 (let ((shrunk (shrink generator test)))
                   (format *check-it-output* "~&Shrunken failure case:~%~A~%" shrunk)
                   (when regression-file
                     (push shrunk (get regression-id 'regression-cases))
                     (with-open-file (s regression-file
                                        :direction :output
                                        :if-exists :append
                                        :if-does-not-exist :error)
                       (format s "~&~S~%" (write-regression-case regression-id shrunk)))))
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

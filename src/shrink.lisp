(in-package :check-it)

(defgeneric shrink (value test))

(defmethod shrink (value test)
  (declare (ignore test))
  value)

(defun copy-structure-and-slots (structure slot-names)
  (let ((copy (make-instance (class-of structure))))
    (loop for slot-name in slot-names
         do (setf (slot-value copy slot-name)
                  (slot-value structure slot-name)))
    copy))

;;;; Value shrinkers

(defmethod shrink ((value structure-object) test)
  (let ((slot-names (struct-slot-names value)))
    (loop for slot-name in slot-names
       do
         (let ((shrunk-elem
                (shrink (slot-value value slot-name)
                        (lambda (x)
                          (handler-case
                              (funcall test
                                       (let ((test-struct
                                              (copy-structure value)))
                                         (setf (slot-value test-struct slot-name) x)
                                         test-struct))
                            (error () nil))))))
           (setf (slot-value value slot-name) shrunk-elem))))
  value)

(defmethod shrink ((value integer) test)
  (shrink-int value test 0 value))

(defun smaller (num1 num2)
  (< (abs num1) (abs num2)))

(defun shrink-int (value test prev best)
  (cond ((= value prev)
         (if (funcall test value)
             best
             value))
        (t
         (multiple-value-bind (small big)
             (if (smaller value prev)
                 (values value prev)
                 (values prev value))
           (let ((test-value (truncate (/ (+ small big) 2))))
             (cond ((funcall test test-value)
                    ;; success; search away from zero
                    (if (zerop test-value)
                        best ;; can't get smaller than zero
                        (shrink-int
                         (case (signum test-value)
                           (1 (1+ test-value))
                           (-1 (1- test-value)))
                         test big best)))
                 ;;failure; search toward zero
                   (t
                    (shrink-int small test test-value test-value))))))))

(defmethod shrink ((value real) test)
  (declare (ignore test))
  ;; can't shrink over non-discrete search space
  value)

(defmethod shrink ((value list) test)
  (shrink-list value test))

(defun shrink-list (value test)
  (flet ((elem-wise-shrink ()
           (loop for i from 0
              for elem in value
              do
                (let ((shrunk-elem
                       (shrink elem
                               (lambda (x)
                                 ;; test if elem can be replaced with a
                                 ;; particular value and still fail
                                 (handler-case
                                     (funcall test
                                              (let ((test-list (copy-list value)))
                                                (setf (nth i test-list) x)
                                                test-list))
                                   (error () nil))))))
                  ;; now actually replace it with the best value
                  (setf (nth i value) shrunk-elem)))
           value))
    (cond
      ((endp value)
       ;; can't shrink nil!
       value)
      (t
       (let ((can-shrink-lengthwise
              (block remove-one-elem
                (loop for i from 0 below (length value)
                   do
                     (let ((shrunk-list (remove-nth i value)))
                       (unless (funcall test shrunk-list)
                         (setf value shrunk-list)
                         (return-from remove-one-elem t))))
                (return-from remove-one-elem nil))))
         (if (not can-shrink-lengthwise)
             ;; there were no failures for lists of length-1, so start shrinking
             ;; elements instead
             (elem-wise-shrink)
             (shrink value test)))))))

;;;; Generator shrinkers

(defmethod shrink ((value string-generator) test)
  (let ((shrunken (shrink-list (cached-str-list value) test)))
    (setf (cached-value value) (join-list shrunken))))

(defmethod shrink ((value int-generator) test)
  (with-obvious-accessors (cached-value shrinker-predicate) value
    (setf cached-value (shrink cached-value
                               (funcall shrinker-predicate test)))))

(defmethod shrink ((value real-generator) test)
  (declare (ignore test))
  ;; can't shrink over non-discrete search space
  (cached-value value))

(defmethod shrink ((value or-generator) test)
  "If any of the untried alternatives are constant, they can be trivially
considered to constitute a search space of a complexity smaller than or equal to
that of the alternative that was originally tried."
  (with-obvious-accessors (cached-value cached-generator sub-generators) value
    (let ((shrunk-cached-value (shrink cached-generator test)))
      (cond
        ((eql shrunk-cached-value cached-value)
         ;; original gen couldn't shrink, so try constant alternatives
         (let ((potential-generators
                (remove-if (lambda (g)
                             (or (eql g cached-generator)
                                 (closer-mop:subclassp
                                  (class-of g)
                                  (find-class 'generator))))
                           sub-generators)))
           (loop for gen in potential-generators
              do
                (unless (funcall test gen)
                  ;; use first failing alternative
                  (setf cached-value gen
                        cached-generator gen)
                  (return-from shrink gen)))
           ;; if nothing shrank, return original cache
           cached-value))
        (t
         (setf cached-value shrunk-cached-value))))))

(defun remove-nth (n list)
  (append (subseq list 0 n)
          (subseq list (1+ n))))

(defmethod shrink ((value list-generator) test)
  (shrink-list-generator value test))

;; we could almost store a single subgenerator on a `list` generator and reset
;; its cached-value for every shrink attempt, but `or` generators must also keep
;; track of which one of their subgenerators they used to shrink correctly
(defun shrink-list-generator (value test)
  (with-obvious-accessors (cached-value sub-generators min-length) value
    (flet ((elem-wise-shrink ()
             (loop
                for sub-generator in sub-generators
                for i from 0
                do
                ;; this is wrong... need separate generators for each value
                ;;(setf (cached-value sub) elem)
                  (let ((shrunk-elem
                         (shrink sub-generator
                                 (lambda (x)
                                   ;; test if elem can be replaced with a
                                   ;; particular value and still fail
                                   (handler-case
                                       (funcall test
                                                (let ((test-list
                                                       (copy-list cached-value)))
                                                  (setf (nth i test-list) x)
                                                  test-list))
                                     (error () nil))))))
                    ;; now actually replace it with the best value
                    (setf (nth i cached-value) shrunk-elem)))
             cached-value))
      (cond
        ((endp cached-value)
         ;; can't shrink nil!
         cached-value)
        ((= (length cached-value) min-length)
         (elem-wise-shrink))
        (t
         (let ((can-shrink-lengthwise
                (block remove-one-elem
                  (loop for i from 0 below (length cached-value)
                     do
                       (let ((shrunk-list (remove-nth i cached-value))
                             (shrunk-subs (remove-nth i sub-generators)))
                         (unless (funcall test shrunk-list)
                           (setf cached-value shrunk-list
                                 sub-generators shrunk-subs)
                           (return-from remove-one-elem t))))
                  (return-from remove-one-elem nil))))
           (if (not can-shrink-lengthwise)
               ;; there were no failures for lists of length-1, so start shrinking
               ;; elements instead
               (elem-wise-shrink)
               (shrink-list-generator value test))))))))

(defmethod shrink ((value tuple-generator) test)
  (with-obvious-accessors (cached-value sub-generators) value
    (loop
       for sub-generator in sub-generators
       for i from 0
       do
         (let ((shrunk-elem
                (shrink sub-generator
                        (lambda (x)
                          (handler-case
                              (funcall test
                                       (let ((test-tuple
                                              (copy-list cached-value)))
                                         (setf (nth i test-tuple) x)
                                         test-tuple))
                            (error () nil))))))
           (setf (nth i cached-value) shrunk-elem)))
    cached-value))

(defmethod shrink ((value struct-generator) test)
  (with-obvious-accessors (cached-value
                           struct-type
                           slot-names
                           slot-generators) value
    (let ((struct
           #-(or abcl allegro)
           (make-instance struct-type)
           #+(or abcl allegro)
           (make-struct-from-type struct-type)))
      (loop for name in slot-names
         for gen in slot-generators
         do
           (let ((shrunk-elem
                  (shrink gen
                          (lambda (x)
                            (handler-case
                                (funcall test
                                         (let ((test-struct
                                                (copy-structure cached-value)))
                                           (setf (slot-value test-struct name) x)
                                           test-struct))
                              (error () nil))))))
             (setf (slot-value struct name) shrunk-elem)))
      (setf cached-value struct))))

(defmethod shrink ((value guard-generator) test)
  (with-obvious-accessors (cached-value guard sub-generator) value
    (let ((result
           (shrink sub-generator
                   (lambda (x)
                     (or (funcall test x)
                         (not (funcall guard x)))))))
      (setf cached-value result))))

(defmethod shrink ((value mapped-generator) test)
  (with-obvious-accessors (cached-value sub-generator mapping) value
    (let ((result
           (funcall mapping
                    (shrink sub-generator
                            (lambda (x)
                              (funcall (compose test mapping) x))))))
      (setf cached-value result))))

(defmethod shrink ((value chained-generator) test)
  (with-obvious-accessors (cached-value cached-generator) value
    (let ((result
           (shrink cached-generator test)))
      (setf cached-value result))))

(defmethod shrink ((value custom-generator) test)
  (shrink (sub-generator value) test))

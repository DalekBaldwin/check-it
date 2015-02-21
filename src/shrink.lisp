(in-package :check-it)

(defgeneric shrink (value test))

(defmethod shrink (value test)
  (declare (ignore test))
  value)

(defun struct-slot-names (struct)
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of struct))))

(defmethod shrink ((value structure-object) test)
  (let ((slot-names (struct-slot-names value)))
    (loop for slot-name in slot-names
         do
         (let ((shrunk-elem
                (shrink (slot-value value slot-name)
                        (lambda (x)
                          (handler-case
                              (funcall test
                                       (let ((test-struct (copy-structure value)))
                                         (setf (slot-value test-struct slot-name) x)
                                         test-struct))
                            (error () nil))))))
           (setf (slot-value value slot-name) shrunk-elem)))
    value))

(defmethod shrink ((value integer) test)
  (shrink-int value test 0 value))

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
             (if (funcall test test-value)
                 ;; success; search away from zero
                 (if (zerop test-value)
                     best ;; can't get smaller than zero
                     (shrink-int
                      (case (signum test-value)
                        (1 (1+ test-value))
                        (-1 (1- test-value)))
                      test big best))
                 ;;failure; search toward zero
                 (shrink-int small test test-value test-value)))))))

(defmethod shrink ((value list) test)
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
       (cond
         ((funcall test value)
          (elem-wise-shrink))
         (t
          ;; can't shrink nil!
          value)))
      (t
       (block comb
         (map-combinations
          (lambda (x)
            (unless (funcall test x)
              (return-from comb
                (shrink x test value x))))
          value
          :length (1- (length value))
          :copy nil)
         ;; there were no failures for lists of length-1, so start shrinking
         ;; elements instead
         (elem-wise-shrink))))))

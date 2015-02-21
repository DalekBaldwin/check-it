(in-package :check-it)

(defparameter *size* 10)

(defclass generator () ())

(defclass int-generator (generator) ())

(defclass real-generator (generator) ())

(defclass list-generator (generator)
  ((element
    :initarg :element
    :accessor element)))

(defclass tuple-generator (generator)
  ((elements
    :initarg :elements
    :accessor elements)))

(defclass or-generator (generator)
  ((elements
    :initarg :elements
    :accessor elements)))

(defclass guard-generator (generator)
  ((guard
    :initarg :guard
    :accessor guard)
   (element
    :initarg :element
    :accessor element)))

(defclass struct-generator (generator)
  ((constructor
    :initarg :constructor
    :accessor constructor)
   (slot-names
    :initarg :slot-names
    :accessor slot-names)
   (slot-generators
    :initarg :slot-generators
    :accessor slot-generators)))

(defgeneric generate (generator))

(defmethod generate (generator)
  generator)

(defmethod generate ((generator list-generator))
  (loop repeat (random *size*)
     collect (generate (element generator))))

(defmethod generate ((generator tuple-generator))
  (mapcar #'generate (elements generator)))

(defun random-element (list)
  (nth (random (length list)) list))

(defmethod generate ((generator or-generator))
  (generate (random-element (elements generator))))

(defmethod generate ((generator int-generator))
  (- (random (+ *size* *size* 1)) *size*))

(defmethod generate ((generator real-generator))
  (- (random (float (* 2 *size*)))
     *size*))

(defmethod generate ((generator guard-generator))
  (let ((try (generate (element generator))))
    (if (funcall (guard generator) try)
        try
        (generate generator))))

(defmethod generate ((generator struct-generator))
  (apply (constructor generator)
         (loop for name in (slot-names generator)
            for gen in (slot-generators generator)
            collect name
            collect (generate gen))))

(defmacro generator (exp)
  (cond
    ((atom exp) exp)
    ((symbolp (first exp))
     (case (first exp)
       (integer
        `(make-instance 'int-generator))
       (real
        `(make-instance 'real-generator))
       (list
        `(make-instance 'list-generator :element (generator ,(second exp))))
       (tuple
        `(make-instance 'tuple-generator
                        :elements (list ,@(loop for elem in (rest exp)
                                             collect `(generator ,elem)))))
       (or
        `(make-instance 'or-generator
                        :elements (list ,@(loop for elem in (rest exp)
                                             collect `(generator ,elem)))))
       (guard
        `(make-instance 'guard-generator
                        :guard ,(second exp)
                        :element (generator ,(third exp))))
       (struct
        (loop for (name gen) on (cddr exp) by #'cddr
           collect name into slot-names
           collect `(generator ,gen) into slot-generators
           finally
             (return `(make-instance 'struct-generator
                                     :constructor ,(second exp)
                                     :slot-names ,slot-names
                                     :slot-generators ,slot-generators))))))
    (t exp)))

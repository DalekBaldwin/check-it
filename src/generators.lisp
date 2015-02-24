(in-package :check-it)

(defparameter *size* 10)

(defclass generator ()
  ((cached-value
    :initarg :cached-value
    :accessor cached-value))
  (:documentation
   "Abstract base class for all generators. Not meant to be instantiated."))

(defclass simple-generator (generator) ()
  (:documentation
   "Abstract base class for non-compound generators. Not meant to be instantiated."))

(defclass int-generator (simple-generator) ())

(defclass real-generator (simple-generator) ())

(defclass symbol-generator (simple-generator)
  ((element
    :initarg :element
    :accessor element)))

(defclass list-generator (generator)
  ((sub-generator
    :initarg :sub-generator
    :accessor sub-generator)))

(defclass tuple-generator (generator)
  ((sub-generators
    :initarg :sub-generators
    :accessor sub-generators)))

(defclass or-generator (generator)
  ((cached-generator
    :initarg :cached-generator
    :accessor cached-generator)
   (sub-generators
    :initarg :sub-generators
    :accessor sub-generators)))

(defclass guard-generator (generator)
  ((guard
    :initarg :guard
    :accessor guard)
   (sub-generator
    :initarg :sub-generator
    :accessor sub-generator)))

(defclass struct-generator (generator)
  ((struct-type
    :initarg :struct-type
    :accessor struct-type)
   #+(or abcl allegro)
   (constructor
    :initarg :constructor
    :accessor constructor)
   (slot-names
    :initarg :slot-names
    :accessor slot-names)
   (slot-keywords
    :initarg :slot-keywords
    :accessor slot-keywords)
   (slot-generators
    :initarg :slot-generators
    :accessor slot-generators)))

(defgeneric generate (generator))

(defmethod generate (generator)
  "Treat non-generators as constants."
  generator)

(defmethod generate :around ((generator generator))
  (setf (cached-value generator) (call-next-method)))

(defmethod generate ((generator list-generator))
  (loop repeat (random *size*)
     collect (generate (sub-generator generator))))

(defmethod generate ((generator tuple-generator))
  (mapcar #'generate (sub-generators generator)))

(defun random-element (list)
  (nth (random (length list)) list))

(defmethod generate ((generator or-generator))
  (let ((chosen-generator (random-element (sub-generators generator))))
    (setf (cached-generator generator) chosen-generator))
  (generate chosen-generator))

(defmethod generate ((generator int-generator))
  (- (random (+ *size* *size* 1)) *size*))

(defmethod generate ((generator real-generator))
  (- (random (float (* 2 *size*)))
     *size*))

(defmethod generate ((generator guard-generator))
  (let ((try (generate (sub-generator generator))))
    (if (funcall (guard generator) try)
        try
        (generate generator))))

(defun slot-definition-name (slot)
  #-abcl (closer-mop:slot-definition-name slot)
  #+abcl (aref slot 1))

(defun struct-slot-names (struct)
  (mapcar #'slot-definition-name
          (closer-mop:class-slots (class-of struct))))

(defun struct-type-slot-names (struct-type)
  (mapcar #'slot-definition-name
          (closer-mop:class-slots (find-class struct-type))))

(defmethod generate ((generator struct-generator))
  (let* ((struct
          #-(or abcl allegro)
          (make-instance (struct-type generator))
          #+(or abcl allegro)
          (funcall (constructor generator))))
    (loop for name in (slot-names generator)
       for gen in (slot-generators generator)
       do (setf (slot-value struct name)
                (generate gen)))
    struct))

(defmacro generator (exp)
  (cond
    ((atom exp) exp)
    ((symbolp (first exp))
     (case (first exp)
       (quote
        `',(second exp))
       (integer
        `(make-instance 'int-generator))
       (real
        `(make-instance 'real-generator))
       (list
        `(make-instance 'list-generator :sub-generator (generator ,(second exp))))
       (tuple
        `(make-instance 'tuple-generator
                        :sub-generators (list ,@(loop for elem in (rest exp)
                                             collect `(generator ,elem)))))
       (or
        `(make-instance 'or-generator
                        :sub-generators (list ,@(loop for elem in (rest exp)
                                             collect `(generator ,elem)))))
       (guard
        `(make-instance 'guard-generator
                        :guard ,(second exp)
                        :sub-generator (generator ,(third exp))))
       (struct
        (let* ((struct-type (second exp))
               (slot-names (struct-type-slot-names struct-type))
               #+(or abcl allegro) (constructor (third exp)))
          (loop for (keyword gen) on (#-(or abcl allegro) cddr
                                      #+(or abcl allegro) cdddr
                                      exp) by #'cddr
             collect (list keyword gen) into keywords-and-gens
             finally
               (return
                 (let ((sorted-slots
                        (sort keywords-and-gens #'<
                              :key (lambda (keyword-and-gen)
                                     (position (first keyword-and-gen) slot-names
                                               :test (lambda (key sym)
                                                       (equal (symbol-name key)
                                                              (symbol-name sym))))))))
                   `(make-instance
                     'struct-generator
                     :struct-type ',struct-type
                     #+(or abcl allegro) ,@(list :constructor `',constructor)
                     :slot-names (list ,@(loop for slot-name in slot-names
                                            collect `(quote ,slot-name)))
                     :slot-keywords (list ,@(mapcar #'first sorted-slots))
                     :slot-generators
                     (list ,@(loop for slot in sorted-slots
                                collect `(generator ,(second slot))))))))))))
    (t exp)))

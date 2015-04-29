(in-package :check-it)

(defparameter *size* 10)
(defparameter *list-size* 20)
(defparameter *list-size-decay* 0.8)
(defparameter *num-trials* 100)
(defparameter *bias-sensitivity* 6.0)
(defparameter *recursive-bias-decay* 1.5)

(defclass generator ()
  ((cached-value
    :initarg :cached-value
    :accessor cached-value))
  (:documentation
   "Abstract base class for all generators. Not meant to be instantiated."))

(defclass simple-generator (generator) ()
  (:documentation
   "Abstract base class for non-compound generators. Not meant to be instantiated."))

(defclass int-generator (simple-generator)
  ((lower-limit
    :initarg :lower-limit
    :accessor lower-limit
    :initform '*)
   (upper-limit
    :initarg :upper-limit
    :accessor upper-limit
    :initform '*)
   (generator-function
    :accessor generator-function)
   (shrinker-predicate
    :accessor shrinker-predicate)))

(defmethod initialize-instance
    :after ((instance int-generator) &rest initargs)
  (declare (ignore initargs))
  (with-obvious-accessors
      (lower-limit upper-limit generator-function shrinker-predicate) instance
      (setf generator-function (int-generator-function lower-limit upper-limit)
            shrinker-predicate (int-shrinker-predicate lower-limit upper-limit))))

(defclass real-generator (simple-generator)
  ((lower-limit
    :initarg :lower-limit
    :accessor lower-limit
    :initform '*)
   (upper-limit
    :initarg :upper-limit
    :accessor upper-limit
    :initform '*)
   (generator-function
    :accessor generator-function)))

(defmethod initialize-instance
    :after ((instance real-generator) &rest initargs)
  (declare (ignore initargs))
  (with-obvious-accessors
      (lower-limit upper-limit generator-function) instance
      (setf generator-function (real-generator-function lower-limit upper-limit))))

(defclass char-generator (simple-generator)
  ((lower-limit
    :initarg :lower-limit
    :accessor lower-limit
    :initform '*)
   (upper-limit
    :initarg :upper-limit
    :accessor upper-limit
    :initform '*)
   (generator-function
    :accessor generator-function)))

(defmethod initialize-instance
    :after ((instance char-generator) &rest initargs)
  (declare (ignore initargs))
  (with-obvious-accessors
      (lower-limit upper-limit generator-function) instance
    (setf generator-function (char-generator-function lower-limit upper-limit))))

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

(defclass custom-generator (generator)
  ((kind
    :initarg :kind
    :accessor kind)
   (sub-generator
    :accessor sub-generator)
   (recursive-depth
    :initform 0
    :accessor recursive-depth)))

(defgeneric generate (generator))

(defmethod generate (generator)
  "Treat non-generators as constants."
  generator)

(defmethod generate :around ((generator generator))
  (setf (cached-value generator) (call-next-method)))

(defmethod generate ((generator list-generator))
  (loop repeat (random *list-size*)
     collect
       (let ((*list-size* (floor (* *list-size* *list-size-decay*))))
         (generate (sub-generator generator)))))

(defmethod generate ((generator tuple-generator))
  (mapcar #'generate (sub-generators generator)))

(defun random-element (list)
  (nth (random (length list)) list))

(defun compute-weights (numbers sensitivity)
  (let* ((len (length numbers))
         (total (reduce #'+ numbers))
         (proportions (loop for number in numbers
                         collect (/ number total)))
         (weights (loop for number in proportions
                     collect
                       (expt
                        (- (* len (log number)))
                        sensitivity)))
         (total-weights (reduce #'+ weights))
         (normalized-weights (loop for number in weights
                                collect (/ number total-weights))))
    normalized-weights))

(defmethod bias (generator)
  1.0)

(let ((double-float-most-positive-fixnum
       (coerce most-positive-fixnum 'double-float)))
  (defun random-uniform ()
    (/ (random most-positive-fixnum) double-float-most-positive-fixnum)))

(defun choose-generator (generators)
  (let* ((len (length generators))
         (total-bias (loop for generator in generators
                        sum (bias generator)))
         (weights (compute-weights
                   (loop for generator in generators
                      collect (/ (bias generator) total-bias))
                   *bias-sensitivity*))
         (thresholds (loop for weight in weights
                        sum weight into total-weight
                        collect total-weight))
         (rand (random-uniform))
         (selected-position
          (or
           (position-if (lambda (threshold) (< rand threshold)) thresholds)
           (1- len)))
         (chosen-generator (elt generators selected-position)))
    #+nil
    (format t "~&gens: ~A~%weights: ~A~%thresholds: ~A~%pos: ~A~%gen: ~A~%~%"
            generators weights thresholds selected-position chosen-generator)
    chosen-generator))

(defmethod generate ((generator or-generator))
  (let ((chosen-generator ;;(random-element (sub-generators generator))
         (choose-generator (sub-generators generator))))
    (setf (cached-generator generator) chosen-generator)
    (generate chosen-generator)))

(defun int-generator-function (low high)
  (match (cons low high)
    ((cons '* '*)
     (lambda () (- (random (+ *size* *size* 1)) *size*)))
    ((cons '* _)
     (lambda ()
       (let ((new-high (* (min (abs high) *size*) (signum high))))
         (- (random (+ new-high *size* 1)) *size*))))
    ((cons _ '*)
     (lambda () (let ((new-low (* (min (abs low) *size*) (signum low))))
                  (+ (random (- (1+ *size*) new-low)) new-low))))
    (_
     (lambda ()
       (let ((new-high (* (min (abs high) *size*) (signum high)))
             (new-low (* (min (abs low) *size*) (signum low))))
         (+ (random (- (1+ new-high) new-low)) new-low))))))

(defun real-generator-function (low high)
  (match (cons low high)
    ((cons '* '*)
     (lambda () (- (random (float (* 2 *size*))) *size*)))
    ((cons '* _)
     (lambda ()
       (let ((new-high (* (min (abs high) *size*) (signum high))))
         (- (random (float (+ new-high *size*))) *size*))))
    ((cons _ '*)
     (lambda ()
       (let ((new-low (* (min (abs low) *size*) (signum low))))
         (+ (random (float (- *size* new-low))) new-low))))
    (_
     (lambda ()
       (let ((new-high (* (min (abs high) *size*) (signum high)))
             (new-low (* (min (abs high) *size*) (signum low))))
         (+ (random (float (- new-high new-low))) new-low))))))

(defun char-generator-function (low high)
  (match (cons low high)
    ((cons '* '*)
     (lambda () (code-char (random 128))))
    ((cons '* _)
     (lambda ()
       (let ((new-high (min high 128)))
             (code-char (random new-high)))))
    ((cons _ '*)
     (lambda ()
       (let ((new-low (max low 0)))
         (code-char (+ (random (- 128 new-low)) new-low)))))
    (_
     (lambda ()
       (let ((new-high (min high 128))
             (new-low (max low 0)))
         (code-char (+ (random (- new-high new-low)) new-low)))))))

(defun int-shrinker-predicate (low high)
  (match (cons low high)
    ((cons '* '*)
     (lambda (test) (lambda (x) (funcall test x))))
    ((cons '* _)
     (lambda (test) (lambda (x) (or (< high x) (funcall test x)))))
    ((cons _ '*)
     (lambda (test) (lambda (x) (or (< x low) (funcall test x)))))
    (_
     (lambda (test) (lambda (x) (or (< x low) (< high x) (funcall test x)))))))

(defmethod generate ((generator int-generator))
  (funcall (generator-function generator)))

(defmethod generate ((generator real-generator))
  (funcall (generator-function generator)))

(defmethod generate ((generator char-generator))
  (funcall (generator-function generator)))

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

(defmethod generate ((generator custom-generator))
  (generate (sub-generator generator)))

;; should probably look into special slots in ContextL
(defmethod generate :around ((generator custom-generator))
  (with-obvious-accessors (recursive-depth) generator
    (let ((old-depth recursive-depth))
      (incf recursive-depth)
      (unwind-protect
           (call-with-adjusted-bias generator
                                    (lambda () (call-next-method)))
        (setf recursive-depth old-depth)))))

(defgeneric call-with-adjusted-bias (generator proceed)
  (:documentation "Strategies for managing growth of recursive generators."))

(defmethod call-with-adjusted-bias ((generator custom-generator) proceed)
  (with-obvious-accessors (bias) generator
    (let ((old-bias bias))
      (setf bias (* bias *recursive-bias-decay*))
      (unwind-protect
           (funcall proceed)
        (setf bias old-bias)))))

(defun expand-generator (exp)
  (cond
    ((atom exp) exp)
    ((symbolp (first exp))
     (case (first exp)
       (quote
        `',(second exp))
       (integer
        `(make-instance 'int-generator
                        ,@(when (second exp)
                                (append
                                 (list :lower-limit
                                       (if (eql (second exp) '*)
                                           ''*
                                           (second exp)))
                                 (when (third exp)
                                   (list :upper-limit
                                         (if (eql (third exp) '*)
                                             ''*
                                             (third exp))))))))
       (real
        `(make-instance 'real-generator
                        ,@(when (second exp)
                                (append
                                 (list :lower-limit
                                       (if (eql (second exp) '*)
                                           ''*
                                           (second exp)))
                                 (when (third exp)
                                   (list :upper-limit
                                         (if (eql (third exp) '*)
                                             ''*
                                             (third exp))))))))
       (character
        `(make-instance 'char-generator
                        ,@(when (second exp)
                                (append
                                 (list :lower-limit
                                       (if (eql (second exp) '*)
                                           ''*
                                           (second exp)))
                                 (when (third exp)
                                   (list :upper-limit
                                         (if (eql (third exp) '*)
                                             ''*
                                             (third exp))))))))
       (list
        `(make-instance 'list-generator :sub-generator ,(expand-generator (second exp))))
       (tuple
        `(make-instance 'tuple-generator
                        :sub-generators (list ,@(loop for elem in (rest exp)
                                                   collect (expand-generator elem)))))
       (or
        `(make-instance 'or-generator
                        :sub-generators (list ,@(loop for elem in (rest exp)
                                                   collect (expand-generator elem)))))
       (guard
        `(make-instance 'guard-generator
                        :guard ,(second exp)
                        :sub-generator ,(expand-generator (third exp))))
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
                                collect (expand-generator (second slot))))))))))
       (otherwise
        (cond
          ((get (first exp) 'generator)
           (let* ((gen-name (first exp)))
             `(make-instance ',gen-name)))
          (t exp)))))
    (t exp)))

(defmacro generator (exp)
  (expand-generator exp))

(defmacro defgenerator (name params &body body)
  (declare (ignorable params))
  (with-gensyms (gen-form)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,name (custom-generator)
         ((bias
           :initform 1.0
           :accessor bias
           :allocation :class)))
       (setf (get ',name 'generator) t)
       (defmethod generate ((generator ,name))
         (generate
          (if (slot-boundp generator 'sub-generator)
              (sub-generator generator)
              (setf (sub-generator generator)
                    (macrolet ((,gen-form ()
                                 `(progn ,@(list ,@body))))
                      (,gen-form)))))))))

(defgenerator alpha ()
  `(generator (or (character 65 91)
                  (character 97 123))))

(defgenerator alphanumeric ()
  `(generator (or (alpha)
                  (character 48 58))))

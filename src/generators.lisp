(in-package :check-it)

(defparameter *size* 10)
(defparameter *num-trials* 100)
(defparameter *bias-sensitivity* 6.0)
(defparameter *recursive-bias-decay* 1.5)

(defclass generator ()
  ((cached-value
    :initarg :cached-value
    :accessor cached-value)
   (bias
    :initform 1.0
    :accessor bias))
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

(define-layered-function generate (generator))

(define-layered-method generate
  (generator)
  "Treat non-generators as constants."
  generator)

(define-layered-method generate
  :around ((generator generator))
  (setf (cached-value generator) (call-next-method)))

(define-layered-method generate ((generator list-generator))
  (loop repeat (random *size*)
     collect (generate (sub-generator generator))))

(define-layered-method generate ((generator tuple-generator))
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

(define-layered-method bias (generator)
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

(define-layered-method generate ((generator or-generator))
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

(define-layered-method generate ((generator int-generator))
  (funcall (generator-function generator)))

(define-layered-method generate ((generator real-generator))
  (- (random (float (* 2 *size*))) *size*))

(define-layered-method generate ((generator guard-generator))
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

(define-layered-method generate ((generator struct-generator))
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

(define-layered-method generate ((generator custom-generator))
  (generate (sub-generator generator)))

;; should probably look into special slots in ContextL
(define-layered-method generate :around ((generator custom-generator))
  (with-obvious-accessors (recursive-depth) generator
    (let ((old-depth recursive-depth))
      (incf recursive-depth)
      (unwind-protect
           (call-with-adjusted-bias generator
                                    (lambda () (call-next-method)))
        (setf recursive-depth old-depth)))))

(define-layered-function call-with-adjusted-bias (generator proceed)
  (:documentation "Strategies for managing growth of recursive generators."))

(define-layered-method call-with-adjusted-bias ((generator custom-generator) proceed)
  (with-obvious-accessors (bias) generator
    (let ((old-bias bias))
      (setf bias (* bias *recursive-bias-decay*))
      (unwind-protect
           (funcall proceed)
        (setf bias old-bias)))))

(defgeneric propagate-bias-adjustment (generator))

(define-layered-method propagate-bias-adjustment (generator))

(define-layered-method propagate-bias-adjustment ((generator list-generator))
  (propagate-bias-adjustment (sub-generator generator)))

(define-layered-method propagate-bias-adjustment ((generator tuple-generator))
  (mapc #'propagate-bias-adjustment (sub-generators generator)))

(define-layered-method propagate-bias-adjustment ((generator or-generator)))

(define-layered-method propagate-bias-adjustment ((generator custom-generator)))

(defmacro generator (exp &environment env)
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
                                collect `(generator ,(second slot))))))))))
       (otherwise
        (cond
          ((get (first exp) 'generator-fun)
           (let* ((gen-name (first exp))
                  (gen-rule (get gen-name 'generator-fun)))
             (multiple-value-bind (expansion expanded-p)
                 ;; I can't believe I finally found a legitimate use for this hack
                 (macroexpand-1 'generator-context env)
               (if (and expanded-p
                        (assoc gen-name expansion))
                   `(symbol-value ',(cdr (assoc gen-name expansion)))
                   (let ((gen-var (gensym (symbol-name gen-name))))
                     `(let ((,gen-var
                             (make-instance 'custom-generator :kind ',gen-name)))
                        (declare (special ,gen-var))
                        (symbol-macrolet
                            ((generator-context
                              ,(if expanded-p (cons `(,gen-name . ,gen-var)
                                                    expansion)
                                   (list `(,gen-name . ,gen-var)))))
                          (setf (sub-generator ,gen-var)
                                (generator
                                 ,(funcall gen-rule (rest exp)))))))))))
          (t exp)))))
    (t exp)))

(defmacro defgenerator (name params &body body)
  (with-gensyms (exp)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,name (custom-generator) ())
       (setf (get ',name 'generator-fun)
             (lambda (,exp)
                     (destructuring-bind ,params ,exp
                       ,@body))))))

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

(defclass bool-generator (generator) ())

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
    (when (characterp lower-limit)
      (setf lower-limit (char-code lower-limit)))
    (when (characterp upper-limit)
      (setf upper-limit (char-code upper-limit)))
    (setf generator-function (char-generator-function lower-limit upper-limit))))

(defclass list-generator (generator)
  ((generator-function
    :initarg :generator-function
    :accessor generator-function)
   (sub-generators
    :initarg :sub-generators
    :accessor sub-generators)
   (min-length
    :initarg :min-length
    :accessor min-length
    :initform 0)
   (max-length
    :initarg :max-length
    :accessor max-length
    :initform (1- most-positive-fixnum))))

(defclass string-generator (list-generator)
  ((cached-str-list
    :accessor cached-str-list)))

(defun string-generator-function ()
  (make-instance 'or-generator
                 :sub-generators (list (make-instance 'char-generator
                                                      :lower-limit
                                                      #.(char-code #\A)
                                                      :upper-limit
                                                      #.(char-code #\Z))
                                       (make-instance 'char-generator
                                                      :lower-limit
                                                      #.(char-code #\a)
                                                      :upper-limit
                                                      #.(char-code #\z))
                                       (make-instance 'char-generator
                                                      :lower-limit
                                                      #.(char-code #\0)
                                                      :upper-limit
                                                      #.(char-code #\9)))))

(defmethod initialize-instance :after ((generator string-generator) &rest initargs)
  (declare (ignore initargs))
  (setf (generator-function generator) #'string-generator-function))

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
   (slot-names
    :initarg :slot-names
    :accessor slot-names)
   (slot-keywords
    :initarg :slot-keywords
    :accessor slot-keywords)
   (slot-generators
    :initarg :slot-generators
    :accessor slot-generators)))

(defclass mapped-generator (generator)
  ((sub-generator
    :initarg :sub-generator
    :accessor sub-generator)
   (mapping
    :initarg :mapping
    :accessor mapping)))

(defclass chained-generator (generator)
  ((pre-generators
    :initarg :pre-generators
    :accessor pre-generators)
   (generator-function
    :initarg :generator-function
    :accessor generator-function)
   (cached-generator
    :initarg :cached-generator
    :accessor cached-generator)))

(defclass custom-generator (generator)
  ((kind
    :initarg :kind
    :accessor kind)
   (sub-generator
    :accessor sub-generator)
   (recursive-depth
    :initform 0
    :accessor recursive-depth)))

(defgeneric generate (generator)
  (:documentation
   "Produce and cache a random value from a generator object."))

(defmethod generate (generator)
  "Treat non-generators as constants."
  generator)

(defmethod generate :around ((generator generator))
  (setf (cached-value generator) (call-next-method)))

(defmethod generate ((generator list-generator))
  (with-obvious-accessors (generator-function
                           sub-generators
                           min-length
                           max-length) generator
    (let ((rand-length
           (+ min-length
              (random (- (min (1+ max-length) *list-size*) min-length)))))
      (setf sub-generators (loop repeat (max rand-length min-length)
                              collect (funcall generator-function)))
      (loop for sub-generator in sub-generators
         collect
           (let ((*list-size* (floor (* *list-size* *list-size-decay*))))
             (generate sub-generator))))))

(defmethod generate ((generator string-generator))
  (let ((chars (call-next-method)))
    (setf (cached-str-list generator) chars)
    (join-list chars)))

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
  "Make a weighted choice to pick one of a number of generators.
This function, along with COMPUTE-WEIGHTS, uses an algorithm that basically
generalizes a sigmoidal probabilistic activation function from 2 to N possible outputs."
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
  "Return a function for producing random integers uniformly with appropriate bounds."
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
  "Return a function for producing random reals uniformly with appropriate bounds."
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

(let ((top-char 127))
  (defun char-generator-function (low high)
    "Return a function for producing random chars uniformly with appropriate bounds."
    (match (cons low high)
      ((cons '* '*)
       (lambda () (code-char (random (1+ top-char)))))
      ((cons '* _)
       (lambda ()
         (let ((new-high (min high top-char)))
           (code-char (random (1+ new-high))))))
      ((cons _ '*)
       (lambda ()
         (let ((new-low (max low 0)))
           (code-char (+ (random (- (1+ top-char) new-low)) new-low)))))
      (_
       (lambda ()
         (let ((new-high (min high top-char))
               (new-low (max low 0)))
           (code-char (+ (random (1+ (- new-high new-low))) new-low))))))))

(defun int-shrinker-predicate (low high)
  "Return a function for testing the validity of shrunking integers with appropriate bounds."
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

(defmethod generate ((generator bool-generator))
  (case (random 2)
    (0 nil)
    (1 t)))

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

#+(or abcl allegro)
(defun make-struct-from-type (type-name)
  (with-input-from-string
      (s (format nil "(~A::~A)"
                 (package-name (symbol-package type-name))
                 (symbol-name type-name)))
    (funcall (get-dispatch-macro-character #\# #\S)
             s #\S nil)))

(defmethod generate ((generator struct-generator))
  (let* ((struct
          #-(or abcl allegro)
          (make-instance (struct-type generator))
          #+(or abcl allegro)
          (make-struct-from-type (struct-type generator))))
    (loop for name in (slot-names generator)
       for gen in (slot-generators generator)
       do (setf (slot-value struct name)
                (generate gen)))
    struct))

(defmethod generate ((generator mapped-generator))
  (with-obvious-accessors (sub-generator mapping) generator
    (funcall mapping (generate sub-generator))))

(defmethod generate ((generator chained-generator))
  (with-obvious-accessors (pre-generators generator-function cached-generator) generator
    (setf cached-generator (apply generator-function (mapcar #'generate pre-generators)))
    (generate cached-generator)))

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
       (boolean
        `(make-instance 'bool-generator))
       (integer
        `(make-instance
          'int-generator
          ,@(destructuring-bind
             (&optional lower upper) (rest exp)
             (append
              (when lower
                (list :lower-limit
                      (if (eql lower '*)
                          ''*
                          lower)))
              (when upper
                (list :upper-limit
                      (if (eql upper '*)
                          ''*
                          upper)))))))
       (real
        `(make-instance
          'real-generator
          ,@(destructuring-bind
             (&optional lower upper) (rest exp)
             (append
              (when lower
                (list :lower-limit
                      (if (eql lower '*)
                          ''*
                          lower)))
              (when upper
                (list :upper-limit
                      (if (eql upper '*)
                          ''*
                          upper)))))))
       (character
        `(make-instance
          'char-generator
          ,@(destructuring-bind
             (&optional lower upper) (rest exp)
             (append
              (when lower
                (list :lower-limit
                      (if (eql lower '*)
                          ''*
                          lower)))
              (when upper
                (list :upper-limit
                      (if (eql upper '*)
                          ''*
                          upper)))))))
       (alpha
        `(make-instance 'or-generator
                        :sub-generators
                        (list ,(expand-generator '(character
                                                   #.(char-code #\A)
                                                   #.(char-code #\Z)))
                              ,(expand-generator '(character
                                                   #.(char-code #\a)
                                                   #.(char-code #\z))))))
       (alphanumeric
        `(make-instance 'or-generator
                        :sub-generators
                        (list ,(expand-generator '(alpha))
                              ,(expand-generator '(character
                                                   #.(char-code #\0)
                                                   #.(char-code #\9))))))
       (list
        (when (null (second exp))
          (error "LIST generator requires a subgenerator"))
        (destructuring-bind (sub-generator &rest keys) (rest exp)
          `(make-instance
            'list-generator
            :generator-function (lambda () ,(expand-generator sub-generator))
            ,@keys)))
       (string
        `(make-instance 'string-generator))
       (tuple
        `(make-instance 'tuple-generator
                        :sub-generators (list ,@(loop for elem in (rest exp)
                                                   collect (expand-generator elem)))))
       (or
        (when (endp (rest exp))
          (error "OR generator requires at least one subgenerator"))
        `(make-instance 'or-generator
                        :sub-generators (list ,@(loop for elem in (rest exp)
                                                   collect (expand-generator elem)))))
       (guard
        `(make-instance 'guard-generator
                        :guard ,(second exp)
                        :sub-generator ,(expand-generator (third exp))))
       (struct
        (let* ((struct-type (second exp))
               (slot-names (struct-type-slot-names struct-type)))
          (loop for (keyword gen) on (cddr exp) by #'cddr
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
                     :slot-names (list ,@(loop for slot-name in slot-names
                                            collect `(quote ,slot-name)))
                     :slot-keywords (list ,@(mapcar #'first sorted-slots))
                     :slot-generators
                     (list ,@(loop for slot in sorted-slots
                                collect (expand-generator (second slot))))))))))
       (map
        `(make-instance 'mapped-generator
                        :mapping ,(second exp)
                        :sub-generator ,(expand-generator (third exp))))
       (chain
           (destructuring-bind (params &rest body) (rest exp)
             (let ((binding-vars
                    (loop for param in params
                       collect (cond
                                 ((listp param) (first param))
                                 (t param))))
                   (binding-gens
                    (loop for param in params
                       collect (cond
                                 ((listp param) (expand-generator (second param)))
                                 (t param)))))
               `(make-instance 'chained-generator
                               :pre-generators (list ,@binding-gens)
                               :generator-function (lambda ,binding-vars ,@body)))))
       (otherwise
        (case (get (first exp) 'genex-type)
          (generator
           (let* ((gen-name (first exp)))
             `(funcall ,(get gen-name 'generator-form) ,@(rest exp))))
          (macro
           (expand-generator
            (funcall (get (first exp) 'genex-macro)
                     (rest exp))))
          (otherwise
           exp)))))
    (t exp)))

(defmacro generator (exp)
  "Macro to establish the beginning of a section of code in the generator DSL."
  (expand-generator exp))

(defmacro def-generator (name lambda-list &body body)
  "Define a new, possibly recursive, generator type."
  (let ((slots (extract-params-from-lambda-list lambda-list)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,name (custom-generator)
         ((bias
           :initform 1.0
           :accessor bias
           :allocation :class)
          ,@(loop for slot in slots
               collect `(,slot
                         :initarg ,(make-keyword slot)))))
       (setf (get ',name 'genex-type) 'generator)
       (setf (get ',name 'generator-form)
             `(lambda ,',lambda-list
                (make-instance ',',name
                               ,@',(loop for slot in slots
                                      collect (make-keyword slot)
                                      collect slot))))
       (defmethod generate ((generator ,name))
         (generate
          (if (slot-boundp generator 'sub-generator)
              (sub-generator generator)
              (setf (sub-generator generator)
                    (let (,@(loop for slot in slots
                               collect `(,slot (slot-value generator ',slot))))
                      ,@body))))))))

(defmacro def-genex-macro (name lambda-list &body body)
  "Define a code template to expand wherever a generator expression could appear."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ',name 'genex-type) 'macro)
     (setf (get ',name 'genex-macro)
           (destructuring-lambda ,lambda-list
             ,@body))))

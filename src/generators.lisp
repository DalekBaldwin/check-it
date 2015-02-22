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

(defun struct-slot-names (struct)
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of struct))))

(defun struct-type-slot-names (struct-type)
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (find-class struct-type))))

(defmethod generate ((generator struct-generator))
  (let* ((struct (make-instance (struct-type generator))))
    (loop for name in (slot-names generator)
       for gen in (slot-generators generator)
       do (setf (slot-value struct name)
                (generate gen)))
    struct))

(defmacro seq<< (&rest clauses)
  "Collect the result of each clause in a seq form into a list."
  (let ((gensyms (loop for clause in clauses collect (gensym))))
    `(seq
      ,@(loop for clause in clauses
           for sym in gensyms
           collect `(bind ,sym ,clause))
      :-> (append ,@(loop for clause in clauses
                       for sym in gensyms
                       collect `(list ,sym))))))

(defgrammar generator-grammar ()
  (sym ()
       (bind s _)
       :->? (and (symbolp s) (not (keywordp s)))
       :-> s)
  (sat (pred)
       (bind s _)
       :->? (funcall pred s)
       :-> s)
  (gen ()
       (or (int-gen)
           (real-gen)
           (list-gen)
           (tuple-gen)
           (or-gen)
           (guard-gen)
           (struct-gen)
           (atom-gen)))
  (atom-gen () (sat #'atom))
  (int-gen ()
           (list 'integer)
           :-> `(make-instance 'int-generator))
  (real-gen ()
            (list 'real)
            :-> `(make-instance 'real-generator))
  (list-gen ()
            (list 'list (bind element (gen)))
            :-> `(make-instance 'list-generator :element ,element))
  (tuple-gen ()
             (list 'tuple (bind elements (* (gen))))
             :-> `(make-instance 'tuple-generator :elements (list ,@elements)))
  (or-gen ()
          (list 'or (bind elements (+ (gen))))
          :-> `(make-instance 'tuple-generator :elements (list ,@elements)))
  (guard-gen ()
             (list 'guard (bind guard _) (bind element (gen)))
             :-> `(make-instance 'guard-generator
                                 :guard ,guard
                                 :element ,element))
  (struct-gen ()
              (list 'struct
                    (bind struct-type (sym))
                    (bind keys-and-gens (* (seq<< (sat #'keywordp) (gen)))))
              :-> (let* ((slot-names (struct-type-slot-names struct-type))
                         (sorted-slots
                          (sort keys-and-gens #'<
                                :key (lambda (key-and-gen)
                                       (position (first key-and-gen) slot-names
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
                      (list ,@(mapcar #'second sorted-slots))))))

(defmacro generator (exp)
  (let ((gen (gomatch generator-grammar gen () (list exp))))
    (if (eql gen clometa.c::failure-value)
        (error "Bad generator form ~A" exp)
        gen))
  #+nil
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
                     :struct-type ',(second exp)
                     :slot-names (list ,@(loop for slot-name in slot-names
                                            collect `(quote ,slot-name)))
                     :slot-keywords (list ,@(mapcar #'first sorted-slots))
                     :slot-generators
                     (list ,@(loop for slot in sorted-slots
                                collect `(generator ,(second slot))))))))))))
    (t exp)))

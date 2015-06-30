(in-package :check-it)

(defgeneric regenerate (generator)
  (:documentation
   "Regenerate a value equivalent to the previous value created with GENERATE."))

(defmethod regenerate (generator)
  "Treat non-generators as constants."
  generator)

(defmethod regenerate :around ((generator generator))
  (setf (cached-value generator) (call-next-method)))

(defmethod regenerate ((generator list-generator))
  (mapcar #'regenerate (sub-generators generator)))

(defmethod regenerate ((generator tuple-generator))
  (mapcar #'regenerate (sub-generators generator)))

(defmethod regenerate ((generator or-generator))
  (regenerate (cached-generator generator)))

(defmethod regenerate ((generator int-generator))
  (cached-value generator))

(defmethod regenerate ((generator real-generator))
  (cached-value generator))

(defmethod regenerate ((generator char-generator))
  (cached-value generator))

(defmethod regenerate ((generator string-generator))
  (let ((chars (call-next-method)))
    (setf (cached-str-list generator) chars)
    (join-list chars)))

(defmethod regenerate ((generator struct-generator))
  (let ((struct (make-struct-from-type (struct-type generator))))
    (loop for name in (slot-names generator)
       for gen in (slot-generators generator)
       do (setf (slot-value struct name)
                (regenerate gen)))
    struct))

(defmethod regenerate ((generator mapped-generator))
  (with-obvious-accessors (sub-generators mapping) generator
    (apply mapping (mapcar #'regenerate sub-generators))))

(defmethod regenerate ((generator chained-generator))
  (regenerate (cached-generator generator)))

(defmethod regenerate ((generator guard-generator))
  (regenerate (sub-generator generator)))

(defmethod regenerate ((generator custom-generator))
  (regenerate (sub-generator generator)))

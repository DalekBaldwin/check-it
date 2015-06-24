(in-package :check-it)

(defmacro destructuring-lambda (params &body body)
  (with-gensyms (shallow-params)
    `(lambda (&rest ,shallow-params)
       (destructuring-bind (,params) ,shallow-params
         ,@body))))

(defun extract-params-from-lambda-list (lambda-list)
  (multiple-value-bind (required optional rest keys allow-other-keys aux keyp)
      (parse-ordinary-lambda-list lambda-list)
    (declare (ignore allow-other-keys keyp))
    (append required
            (mapcar #'first optional)
            (when rest (list rest))
            (mapcar #'cadar keys)
            (mapcar #'first aux))))

(defmacro with-obvious-accessors (accessors instance &body body)
  "Like WITH-ACCESSORS but with the brevity of WITH-SLOTS."
  `(with-accessors
         (,@(loop for accessor in accessors
               collect `(,accessor ,accessor)))
       ,instance
     ,@body))

(defun join-list (char-list)
  "Join the given CHAR-LIST of characters to a string. '(#\a #\b #\b) => \"abc\""
  (if (null char-list)
      ""
      (reduce (lambda (str char)
                   (concatenate 'string str (princ-to-string char)))
                 (rest char-list)
                 :initial-value (princ-to-string (first char-list)))))

(in-package :check-it)

(defmacro destructuring-lambda (params &body body)
  "Pass the arguments of a destructuring lambda list to a function body."
  (with-gensyms (shallow-params)
    `(lambda (&rest ,shallow-params)
       (destructuring-bind (,params) ,shallow-params
         ,@body))))

(defun extract-params-from-lambda-list (lambda-list)
  "Return a list of the names of all parameters in an ordinary lambda list."
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

(defun make-struct-from-type (type-name)
  #-(or abcl allegro)
  (make-instance type-name)
  #+(or abcl allegro)
  (with-input-from-string
      (s (format nil "(~A::~A)"
                 (package-name (symbol-package type-name))
                 (symbol-name type-name)))
    (funcall (get-dispatch-macro-character #\# #\S)
             s #\S nil)))

(defun update-alist (item value alist)
  "Non-destructively replace cdr of the cons whose car matches ITEM in ALIST
with VALUE, or insert a new cons if no car matches ITEM."
  (destructuring-bind (replacedp new-alist)
      (reduce (lambda (accum item)
                (destructuring-bind (replacedp new-alist) accum
                  (cond
                    (replacedp
                     (list replacedp (list* item new-alist)))
                    ((eql item (car item))
                     (list t (list* (cons item value) new-alist)))
                    (t
                     (list replacedp (list* item new-alist))))))
              alist
              :initial-value (list nil nil))
    (cond
      (replacedp
       (reverse new-alist))
      (t
       (list* (cons item value) alist)))))

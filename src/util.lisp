(in-package :check-it)

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

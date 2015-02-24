(in-package :check-it)

(defmacro with-obvious-accessors (accessors instance &body body)
  "Like WITH-ACCESSORS but with the brevity of WITH-SLOTS."
  `(with-accessors
         (,@(loop for accessor in accessors
               collect `(,accessor ,accessor)))
       ,instance
     ,@body))

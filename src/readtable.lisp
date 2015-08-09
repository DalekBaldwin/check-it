(in-package :check-it)

(defun generator-reader (stream char numarg)
  (declare (ignore char numarg))
  `(generator ,(read stream t nil t)))

(named-readtables:defreadtable check-it
  (:merge :standard)
  (:dispatch-macro-char #\# #\G #'generator-reader))

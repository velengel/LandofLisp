(defparameter foo (list 1 (make-hash-table) 3))
(setf (gethash 'my-key (nth 1 foo)) 77)
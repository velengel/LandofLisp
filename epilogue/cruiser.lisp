(defun unique-letters (name)
  (concatenate 'string
	       "Hello "
	       (coerce (remove-duplicates name) 'string)))

(defun ask-and-respond ()
  (princ "What is your name?")
  (princ (unique-letters (read-line))))
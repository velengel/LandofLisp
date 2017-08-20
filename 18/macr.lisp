(defmacro lazy (&body body)
  (let ((forced (gensym))
	(value (gensym)))
    `(let ((,for
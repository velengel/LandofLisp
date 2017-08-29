(defun raise-widget-prices (widgets)
  (when widgets
    (loop (restart-case (progn (raise-price (car widgets))
			  (return))
			(try-again () (princ "trying again"))))
    (raise-widget-prices (cdr widgets))))
(defclass widget ()
	  ((color :accessor widget-color
		  :initarg :color)))
(defmethod describe-widget ((w widget))
	   (format t "This is a ~a widget" (widget-color w)))

(defmethod describe-widget :before ((w widget))
	   (add-to-log "Somebody is checking on a widget"))
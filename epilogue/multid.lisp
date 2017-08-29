(defclass color () ())
(defclass red (color) ())
(defclass blue (color) ())
(defclass yellow (color) ())

(defmethod mix ((c1 color) (c2 color))
	   "I don't know what color that makes")

(defmethod mix ((c1 blue) (c2 yellow))
	   "you made green!")

(defmethod mix ((c1 yellow) (c2 red))
	   "You made orange!")
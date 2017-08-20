(with-open-file (my-stream
		"testfile.txt"
		:direction :output
		:if-exists :supersede)
	(princ "Hello File!" my-stream))

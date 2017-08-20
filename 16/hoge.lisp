(defun add (a b)
  (let ((x (+ a b)))
    (format t "The sum is ~a" x)
    x))

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defun add (a b)
  (let1 x (+ a b)
	(format t "The sum is ~a" x)
	x))

#|(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
	     (tail (cdr ,val)))
	 ,yes)
     ,no))

(defmacro split2 (val yes no)
  `(let1 x ,val
	 (if x
	     (let ((head (car x))
		   (tail (cdr x)))
	       ,yes)
	   ,no)))|#

(defmacro split (val yes no)
  (let1 g (gensym)
	`(let1 ,g ,val
	       (if ,g
		   (let ((head (car ,g))
			 (tail (cdr ,g)))
		     ,yes)
		 ,no))))

(defun my-length (lst)
  (labels ((f (lst acc)
	     (split lst
		    (f tail (1+ acc))
		    acc)))
    (f lst 0)))

(defun pairs (lst)
  (labels ((f (lst acc)
	     (split lst
		    (if tail
			(f (cdr tail) (cons (cons head (car tail)) acc))
		      (reverse acc))
		    (reverse acc))))
    (f lst nil)))

(defmacro recurse (cars &body body)
  (let1 p (pairs vars)
	`(labels ((self ,(mapcar #'car p)
		    ,@body))
	   (self ,@(mapcar #'cdr p)))))

(defun my-length (lst)
  (recurse (lst lst
		acc 0)
	   (split lst
		  (self tail (1+ acc))
		  acc)))

(defun my-length (lst)
  (reduce (lambda (x i)
	    (1+ x))
	  lst
	  :initial-value 0))
(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))

(defun foo (a b &optional c d)
  "a and b are required, c and are optional"
  (list a b c d))

(defun foo-with-defaults (a &optional (b 10))
  "a is required, b is optional, with a default"
  (list a b))

(defun make-rect (width &optional (height width))
  "defaults can refer to previous arguments"
  (list width height))

(defun foo-with-default-indicator (a &optional (b 10 b-p))
  "b-p will be t or nill if the arg was passed or not"
  (if b-p
	  (print "b was passed"))
  (list a b b-p))

(defun foo-with-rest (a &rest others)
  "everything after the first goes into others"
  (list a others))

(defun foo-with-key (&key a (b "foo" b-p))
  "keywords arguments"
  (list a b b-p))

(defun using-return-from (n)
  "showing how to use return-from"
  (dotimes (i 10)
	(dotimes (j 10)
	  (when (> (* i j) n)
		(return-from using-return-from (list i j))))))

(defun plot (fn min max step)
  "using funcall"
  (loop for i from min to max by step do
		(loop repeat (funcall fn i) do (format t "*"))
		(format t "~%")))

;; `apply` takes a function some args, and a list, and
;; appies the function to the args


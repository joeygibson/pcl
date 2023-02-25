(defun hello-world ()
  (format t "Hello, World!"))


(list 1 2 3)
(list :a 1 :b 2 :c 3)
(getf (list :a 1 :b 2 :c 3) :b)

(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))

(defun foo (a b &optional c d)
  (list a b c d))

(defun foo-with-defaults (a &optional (b 10) (c 23))
  (list a b c))

(defun make-rectangle (width &optional (height width))
  (list width height))

(defun foo-with-defaults-2 (a &optional (b 10 b-supplied-p) (c 20 c-supplied-p))
  (list a b b-supplied-p c c-supplied-p))

(defun foo-with-named-params (&key a b c)
  (list b c a))

(defun foo-with-several (&key (a 0)
						   (b 23 b-supplied-p)
						   (c (+ a b)))
  (list a b c))

(defun foo-with-opt-and-key (x &optional y &key z)
  (list x y z))

(defun ret-fro (n)
  (dotimes (i 10)
	(dotimes (j 10)
	  (when (> (* i j) n)
		(return-from ret-fro (list i j))))))

(defun plot (fn min max step)
  "shows usage of funcall"
  (loop for i from min to max by step do
		(loop repeat (funcall fn i) do (format t "*"))
		(format t "~%")))

; (plot #'exp 0 4 1/2)
; (apply #'plot '(#'exp 0 4 1/2))
; (apply #'plot '(exp 0 4 1/2))

; (plot #'(lambda (x) (* 2 x)) 0 10 1)












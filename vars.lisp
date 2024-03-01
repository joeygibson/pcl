(defun foo (x)
  (format t "param: ~a~%" x)
  (let ((x 2)) 
	(format t "outer let: ~a~%" x)
	(let ((x 3))
	  (format t "inner let: ~a~%" x))
	(format t "outer let: ~a~%" x))
  (format t "param: ~a~%" x))

;; `let*` allows you to refer to params within the init,
;; not just in the body
(print (let* ((x 10) (y (* x 2)))
		 (list x y)))

(defvar *bar* 23)

(defun print-bar ()
  (print *bar*))

;; locally redefine a global variable
(let ((*bar* 99))
  (print-bar))

(print *bar*)

(incf *bar*)
(print *bar*)

(let ((x 10) (y 20))
  (rotatef x y)
  (print x)
  (print y))

(let ((x 10) (y 20) (z 30))
  (shiftf x y z 99)
  (format t "~a, ~a, ~a~%" x y z))

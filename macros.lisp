(defmacro my-when (condition &rest body)
  `(if ,condition (progn ,@body)))

(defmacro my-unless (condition &rest body)
  `(if (not ,condition)
	   (progn ,@body)))

;; writing your own macros

(defun prime-p (number)
  (when (> number 1)
	(loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (prime-p n) return n))

(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
   `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
		 (,ending-value-name ,end))
		((> ,var ,end))
	  ,@body)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
	 ,@body))

(defmacro do-primes-1 ((var start end) &body body)
  (with-gensyms (ending-value-name)
	`(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
		  (,ending-value-name ,end))
		 ((> var ,end))
	   ,@body)))

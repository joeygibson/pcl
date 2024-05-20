(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

(defmacro check-orig (form)
  `(report-result ,form ',form))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
	 ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
	`(let ((,result t))
	   ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
	   ,result)))

(defmacro check (&body forms)
  `(combine-results
	 ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test-+ ()
  (check
	(= (+ 2 3) 5)
	(= (+ 3 1) 4)))

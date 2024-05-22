(defvar *test-name* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
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

;; (defun test-+ ()
;;   (let ((*test-name* 'test-+))
;; 	(check
;; 	  (= (+ 2 3) 5)
;; 	  (= (+ 3 1) 4))))

;; (defun test-* ()
;;   (let ((*test-name* 'test-*))
;; 	(check
;; 	  (= (* 3 2) 6)
;; 	  (= (* 3 3) 7))))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
	 (let ((*test-name* (append *test-name* (list ',name))))
	   ,@body)))

(deftest test-+ ()
  (check
	(= (+ 2 3) 5)
	(= (+ 3 2) 4)))

(deftest test-* ()
  (= (* 3 2) 6)
  (= (* 3 3) 9))

(deftest test-/ ()
  (check
	(= (/ 4 2) 2)
	(= (/ 5 2) 99)))

(deftest test-arithmetic ()
  (combine-results
	(test-+)
	(test-*)
	(test-/)))

(deftest test-math ()
  (test-arithmetic))

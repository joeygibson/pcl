(quote (1 2 3)) ; (1 2 3)

; flet and labels
(defun count-versions (dir)
  (let ((versions (mapcar #'(lambda (x) (cons x 0)) '(2 3 4))))
	(flet ((count-version (file)
			 (incf (cdr (assoc (major-version (read-id3 file)) versions)))))
	  (walk-directory dir #'count-version :test #'mp3-p))
	versions))

(defun collect-leaves (tree)
  (let ((leaves ()))
	(labels ((walk (tree)
			   (cond
				 ((null tree))
				 ((atom tree) (push tree leaves))
				 (t (walk (car tree))
					(walk (cdr tree))))))
	  (walk tree))
	(nreverse leaves)))

;; block, return
(dotimes (i 10)
  (let ((answer (random 100)))
	(print answer)
	(if (> answer 50)
		(return))))

; unwind-protect is like try/finally
(defmacro with-database-connection ((var &rest open-args) &body body)
  `(let ((,var (open-connection ,@open-args)))
	 (unwind-protect (progn ,@body)
	   (close-connection ,var))))

; using it
(with-database-connection (conn :host "foo" :user "scott" :pwd "tiger")
  (do-stuff conn)
  (do-more-stuff conn))





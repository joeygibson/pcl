(define-condition malformed-log-entry-error (error)
  ((text :initarg :text :reader :text)))

(defun parse-log-entry (text)
  (if (well-formed-log-entry-p text)
	  (make-instance ...)
	  (error 'malformed-log-entry-error :text text)))

(defun parse-log-file (file)
  (with-open-file (in file :direction :input)
	(loop for text = (read-line in nil nil) while text
		  for entry = (handler-case (parse-log-entry text)
						(malformed-log-entry-error () nil))
		  when entry collect it)))

; better version, with restarts
(defun parse-log-file (file)
  (with-open-file (in file :direction :input)
	(loop for text = (read-line in nil nil) while text
		  for entry = (restart-case (parse-log-entry text)
						(skip-log-entry () nil))
		  when entry collect it)))

; handler-bind registers handlers for conditions
(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error
				   #'(lambda (c)
					   (invoke-restart 'skip-log-entry))))
	(dolist (log (find-all-logs))
	  (analyze-log log))))

;; clearer version, without the lambda
(defun skip-log-entry (c)
  (invoke-restart 'skip-log-entry))

(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error #'skip-log-entry))
	(dolist (log (find-all-logs))
	  (analyze-log log))))

; this one finds a restart, wherever it was bound
(defun skip-log-entry (c)
  (let ((restart (find-restart 'skip-log-entry)))
	(when restart (invoke-restart restart))))

;; multiple restarts
(defun parse-log-entry (text)
  (if (well-formed-log-entry-p text)
	  (make-instance 'log-entry ...)
	  (restart-case (error 'malformed-log-entry-error :text text)
		(use-value (value) value)
		(reparse-entry (fixed-text) (parse-log-entry fixed-text)))))

; this version would create an instance of malformed-log-entry
; when a line is unparsable
(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error
                   #'(lambda (c)
                       (use-value
						(make-instance 'malformed-log-entry :text (text c))))))
    (dolist (log (find-all-logs))
      (analyze-log log))))








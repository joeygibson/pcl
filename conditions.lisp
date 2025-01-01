;; https://gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts

(define-condition malformed-log-entry-error (error)
  ((text :initarg :text :reader text)))

(defun parse-log-entry (text)
  (if (well-formed-log-entry-p text)
      (make-instance 'log-entry ...)
      (error 'malformed-log-entry-error :text text)))

;; skip the malformed entry
(defun parse-log-file (file)
  (with-open-file (in file :direction :input)
    (loop for text = (read-line in nil nil) while text
          for entry = (handler-case (parse-log-entry text)
                        (malformed-log-entry-error () nil))
          when entry collect it)))

;; this one uses restarts
(defun parse-log-file (file)
  (with-open-file (in file :direction :input)
    (loop for text = (read-line in nil nil) while text
          for entry = (restart-case (parse-log-entry text)
                        (skip-log-entry () nil))
          when entry collect it)))

(defun log-analyzer ()
  (dolist (log (find-all-logs))
    (analyze-log log)))

(defun analyze-log (log)
  (dolist (entry (parse-log-file log))
    (analyze-entry entry)))

;; setup handler at top-level
(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error
                   #'(lambda (c)
                       (invoke-restart 'skip-log-entry))))
    (dolist (log (file-all-logs))
      (analyze-log log))))

;; use a restart function, instead of a lamda
(defun skip-log-entry (c)
  (invoke-restart 'skip-log-entry))

(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error #'skip-log-entry))
    (dolist (log (find-all-logs))
      (analyze-log log))))

;; this one can deal with no restart being registered
(defun skip-log-entry (c)
  (let ((restart (find-restart 'skip-log-entry)))
    (when restart (invoke-restart restart))))

;; support multiple restarts
(defun parse-log-entry (text)
  (if (well-formed-log-entry-p text)
      (make-instance 'log-entry ...)
      (restart-case (error 'malformed-log-entry-error :text text)
        (use-value (value) value)
        (reparse-entry (fixed-text) (parse-log-entry fixed-text)))))

;; support returning a different object for malformed entries
(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error ))))









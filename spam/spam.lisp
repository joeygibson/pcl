(in-package :com.joeygibson.spam)

(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)

(defun classification (score)
  (cond
	((<= score *max-ham-score* score) 'ham)
	((>= score *min-spam-score* score) 'spam)
	(t 'unsure)))

(defclass word-feature ()
  ((word
	:initarg :word
	:accessor word
	:initform (error "Must specify :word")
	:documentation "The word this feature represents.")
   (spam-count
	:initarg :spam-count
	:accessor spam-count
	:initform 0
	:documentation "Number of spams we've seen this word in.")
   (ham-count
	:initarg :han-count
	:accessor ham-count
	:initform 0
	:documentation "Number of hams we have seen this feature in.")))

(defvar *feature-database* (make-hash-table :test #'equal))

(defun clear-database ()
  (setf *feature-database* (make-hash-table :test #'equal)))

(defun intern-feature (word)
  (or (gethash word *feature-database*)
	  (setf (gethash word *feature-database*)
			(make-instance 'word-feature :word word))))

(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))

(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))

(defun score (features)
  (print features))

(defun classify (text)
  (classification (score (extract-features text))))








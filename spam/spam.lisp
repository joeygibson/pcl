(in-package :com.joeygibson.spam)

(defvar *feature-database* (make-hash-table :test #'equal))

(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)
(defparameter *max-chars* (* 10 1024))

(defvar *total-spams* 0)
(defvar *total-hams* 0)

;; (defun classification (score)
;;   (cond
;; 	((<= score *max-ham-score* score) 'ham)
;; 	((>= score *min-spam-score* score) 'spam)
;; 	(t 'unsure)))

(defun classification (score)
  (values
   (cond
	 ((<= score *max-ham-score*) 'ham)
	 ((>= score *min-spam-score*) 'spam)
	 (t 'unsure))
   score))

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

(defmethod print-object ((object word-feature) stream)
  "like a toString method in Java"
  (print-unreadable-object (object stream :type t)
	(with-slots (word ham-count spam-count) object
	  (format stream "~s: :hams ~d :spams ~d" word ham-count spam-count))))

(defun clear-database ()
  (setf
   *feature-database* (make-hash-table :test #'equal)
   *total-spams* 0
   *total-hams* 0))

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

;; (defun score (features)
;;   (print features))

(defun classify (text)
  (classification (score (extract-features text))))

(defun train (text type)
  (dolist (feature (extract-features text))
	(increment-count feature type))
  (increment-total-count type))

(defun increment-count (feature type)
  (ecase type
	(ham (incf (ham-count feature)))
	(spam (incf (spam-count feature)))))

(defun increment-total-count (type)
  (ecase type
	(ham (incf *total-hams*))
	(spam (incf *total-spams*))))

(defun naive-spam-probability (feature)
  (with-slots (spam-count ham-count) feature
	(/ spam-count (+ spam-count ham-count))))

(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
	(let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
		  (ham-frequency (/ ham-count (max 1 *total-hams*))))
	  (/ spam-frequency (+ spam-frequency ham-frequency)))))

(defun bayesian-spam-probability (feature &optional
											(assumed-frequency 1/2)
											(weight 1))
  (let ((basic-probability (spam-probability feature))
		(data-points (+ (spam-count feature)
						(ham-count feature))))
	(/ (+ (* weight assumed-frequency)
		  (* data-points basic-probability))
	   (+ weight data-points))))

(defun score (features)
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
	(and (zerop spam-count)
		 (zerop ham-count))))

(defun fisher (probs number-of-probs)
  "Fisher computation described by Robinson."
  (inverse-chi-square
   (* -2 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))

(defun inverse-chi-square (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min
   (loop with m = (/ value 2)
		 for i below (/ degrees-of-freedom 2)
		 for prob = (exp (- m)) then (* prob (/ m i))
		 summing prob)
   1.0))

;; testing

(defparameter *corpus* (make-array 1000 :adjustable t :fill-pointer 0))

(defun add-file-to-corpus (filename type corpus)
  (vector-push-extend (list filename type) corpus))

(defun add-directory-to-corpus (dir type corpus)
  (let ((dir-name (if (str:ends-with-p "*" dir)
					  dir
					  (format nil "~a/*" dir))))
	(dolist (filename (directory dir-name))
	  (add-file-to-corpus filename type corpus))))

(defun test-classifier (corpus testing-fraction)
  (clear-database)
  (let* ((shuffled (shuffle-vector corpus))
		 (size (length corpus))
		 (train-on (floor (* size (- 1 testing-fraction)))))
	(train-from-corpus shuffled :start 0 :end train-on)
	(test-from-corpus shuffled :start train-on)))

(defun train-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) do
		(destructuring-bind (file type) (aref corpus idx)
		  (train (start-of-file file *max-chars*) type))))

(defun test-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) collecting
		(destructuring-bind (file type) (aref corpus idx)
		  (multiple-value-bind (classification score)
			  (classify (start-of-file file *max-chars*))
			(list
			 :file file
			 :type type
			 :classification classification
			 :score score)))))

;; utility functions

(defun nshuffle-vector (vector)
  (loop for idx downfrom (1- (length vector)) to 1
		for other = (random (1+ idx))
		do (unless (= idx other)
			 (rotatef (aref vector idx)
					  (aref vector other))))
  vector)

(defun vector-shuffle (vector)
  (nshuffle-vector (copy-seq vector)))

(defun start-of-file (file max-chars)
  (with-open-file (in file)
	(let* ((length (min (file-length in) max-chars))
		   (text (make-string length))
		   (read (read-sequence text in)))
	  (if (< read length)
		  (subseq text 0 read)
		  text))))

(defun result-type (result)
  (destructuring-bind (&key type classification &allow-other-keys) result
	(ecase type
	  (ham
	   (ecase classification
		 (ham 'correct)
		 (spam 'false-positive)
		 (unsure 'missed-ham)))
	  (spam
	   (ecase classification
		 (ham 'false-negative)
		 (spam 'correct)
		 (unsure 'missed-spam))))))

(defun false-positive-p (result)
  (eql (result-type result) 'false-positive))

(defun false-negative-p (result)
  (eql (result-type result) 'false-negative))

(defun missed-ham-p (result)
  (eql (result-type result) 'missed-ham))

(defun missed-spam-p (result)
  (eql (result-type result) 'missed-spam))

(defun correct-p (result)
  (eql (result-type result) 'correct))

(defun analyze-results (results)
  (let* ((keys '(total correct false-positive
				 false-negative missed-ham missed-spam))
		 (counts (loop for x in keys collecting (cons x 0))))
	(dolist (item results)
	  (incf (cdr (assoc 'total counts)))
	  (incf (cdr (assoc (result-type item) counts))))
	(loop with total = (cdr (assoc 'total counts))
		  for (label . count) in counts
		  do (format t "~&~@(~a~):~20t~5d~,5t: ~6,2f%~%"
					 label count (* 100 (/count total))))))



















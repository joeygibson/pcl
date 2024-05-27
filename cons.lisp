;; ch13, other uses for cons cells

;; trees

(defun make-tree (item)
  "Create a tree node with the given item"
  (cons (cons item nil) nil))

(defun add-child (tree child)
  "Add a child node to the given tree"
  (setf (car tree) (append (car tree) (list child)))
  tree)

(defun first-child (tree)
  "Return the first child of the tree node, or nil if none"
  (if (null tree)
	  nil
	  (cdr (car tree))))

(defun next-sibling (tree)
  "Return the next siblib of the tree node, or nil if none"
  (cdr tree))

(defun data (tree)
  (car (car tree)))

(defparameter *tree* (make-tree 60))

(add-child (car *tree*) 23)

;; sets

;; adjoin adds an item to a list-as-a-set

(defparameter *set* ())
(adjoin 1 *set*) ; (1) - but it didn't modify the list
(setf *set* (adjoin 1 *set*))
(pushnew 2 *set*) ; (2 1) - *does* modify the list in place

(member 2 *set*) ; (2 1)
(member 23 *set*) ; nil
;; also member-if, member-if-not

;; also intersection, union, set-difference, set-exclusive-or

(intersection '(1 2 3) '(2 3 4)) ; (3 2)
(union '(1 2 3) '(2 3 4)) ; (1 2 3 4)
(set-difference '(1 2 3) '(2 3 4)) ; (1) - items in first set missing from second
(set-exclusive-or '(1 2 3) '(2 3 4)) ; (4 1)

;; each of those has a recycling version, starting with 'n'

(subsetp '(3 2 1) '(1 2 3 4)) ; T
(subsetp '(1 2 3 4) '(1 2 3)) ; nil

;; Lookup Tables: Alists and Plists

;;; alists

(assoc 'a '((a . 1) (b . 2) (c . 3))) ; (a . 1)
(assoc 'b '((a . 1) (b . 2) (c . 3))) ; (b . 2)
(assoc 'c '((a . 1) (b . 2) (c . 3))) ; (c . 3)
(assoc 'd '((a . 1) (b . 2) (c . 3))) ; nil

(cdr (assoc 'a '((a . 1) (b . 2)))) ; 1

;; use string for keys
(assoc "a" '(("a" . 1) ("b" . 2)) :test #'string=) ; ("a" . 1)

;; assoc scans front to back
(assoc 'a '((a . 10) (b . 2) (a . 1))) ; (a . 10)

;; add to front of list
(defparameter *alist* '((a . 1) (b . 2)))
(cons (cons 'new-key 'new-value) *alist*)

;; or
(acons 'd 23 *alist*)

;; both need to have result saved
(setf *alist* (acons 'd 23 *alist*))
(push (cons 'd 23) *alist*)

(assoc-if #'(lambda (x) (eql x 'a)) *alist*) ; (a . 1)
(assoc-if-not #'(lambda (x) (string= x 'a)) *alist*) ; (d . 23)

;; rassoc, rassoc-if, rassoc-if-not are similar, but use
;; the value in the cdr, instead of car

;; build alist from two lists
(pairlis '(a b c) '(1 2 3)) ; ((c . 3) (b . 2) (a . 1))

;;; plists

(defparameter *plist* '(:c 23))
;(defparameter *plist* ())

(setf (getf *plist* :a) 1) ; 1
(setf (getf *plist* :b) 2) ; 2
(setf (getf *plist* :a) 10) ; 10
(getf *plist* :a) ; 10

(defun process-properties (plist keys)
  (loop while plist do
		(multiple-value-bind (key value tail)
			(get-properties plist keys)
		  (when key (format t "~a = ~a~%" key value))
		  (setf plist (cddr tail)))))

(process-properties *plist* '(:a :b))

;;; remove a property

(remf *plist* :a)

;; DESTRUCTURING-BIND

(destructuring-bind (x y z) (list 1 2 3)
  (format t "~a ~a ~a~%" x y z)) ; (1 2 3)

(destructuring-bind (x y z) '(1 (2 20) 3)
  (format t "~a ~a ~a~%" x y z)) ; 1 (2 20) 3

(destructuring-bind (x (y1 y2) z) '(1 (2 20) 3)
  (format t "~a ~a ~a ~a~%" x y1 y2 z)) ; 1 2 20 3

;;; &whole assigns the whole thing, too
(destructuring-bind (&whole whole &key x y z)
	'(:z 1 :y 2 :x 3)
  (format t "~a, ~a, ~a, ~a~%"
		  whole x y z)) ; (Z 1 Y 2 X 3), 3, 2, 1














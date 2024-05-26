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












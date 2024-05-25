;; ch12: lists

(cons 1 2) ; (1 . 2)
(car (cons 1 2)) ; 1
(cdr (cons 1 2)) ; 2

(defparameter *cons* (cons 1 2)) ; (1 . 2)
*cons* ; (1 . 2)

(setf (car *cons*) 10) ; 10
*cons* ; (10 . 2)

(setf (cdr *cons*) 20) ; 20
*cons* ; (10 . 20)

(cons 1 nil) ; (1)
(cons 1 (cons 2 nil)) ; (1 2)
(cons 1 (cons 2 (cons 3 nil))) ; (1 2 3)

(list 1) ; (1)
(list 1 2) ; (1 2)
(list 1 2 3) ; (1 2 3)

(first (list 1 2 3)) ; 1
(rest (list 1 2 3)) ; (2 3)

;; functional programming and lists

(defparameter *list-1* (list 1 2))
(defparameter *list-2* '(3 4))
(defparameter *list-3* (append *list-1* *list-2*))
*list-3*

(reverse *list-3*) ; (4 3 2 1)
(nreverse *list-3*) ; (4 3 2 1)
*list-3* ; (1)

;; recycling versions of append, and substitute
(defparameter *x* '(1 2 3 4))
(nconc *x* '(4 5 6)) ; (1 2 3 4 4 5 6)

;; (nsubstitute )

;; Combining Recycling with Shared Structure

(defun upto (max)
  (let ((result nil))
	(dotimes (i max)
	  (push i result))
	(nreverse result)))

(upto 10) ; (0 1 2 3 4 5 6 7 8 9)

;; (delete) is the recycling version of (remove)

;; (setf foo (delete nil foo))

;; sort, stable-sort, and merge are recycling, without
;; non-recycling versions. pass a copy with copy-list
(sort (copy-list '(1 0 9 8 3)) #'<) ; (0 1 3 8 9)

;; List-Manipulation Functions

;; first, rest == car, cdr
;; first, second, third ... tenth

(nth 2 '(1 3 2 4 0)) ; 2
(nthcdr 2 '(1 3 2 4 0)) ; (2 4 0)

;; (caar '(1 3 2 4 0)) ; error
(caar (list (list 1 2) 3)) ; 1
(cadr (list (list 1 2) (list 3 4))) ; (3 4)
(caadr (list (list 1 2) (list 3 4))); 3

(last '(1 2 3 4)) ; (4)
(butlast '(1 2 3 4)) ; (1 2 3) , nbutlast recycling

(consp 23) ; nil
(consp (cons 1 2)) ; T

(atom 23) ; T
(atom (cons 1 2)) ; nil

(null 23) ; nil
(null '()) ; T

;; Mapping

(mapcar #'(lambda (x) (* 2 x))
		'(1 2 3)) ; (2 4 6)

(mapcar #'+ '(1 2 3)
		'(10 20 30)) ; (11 22 33)

;; maplist is like mapcar, but passes in the cons cells

;; MAPCAN and MAPCON work like MAPCAR and MAPLIST except for the way they build
;; up their result. While MAPCAR and MAPLIST build a completely new list to
;; hold the results of the function calls, MAPCAN and MAPCON build their
;; result by splicing together the results--which must be lists--as if by
;; NCONC. Thus, each function invocation can provide any number of elements
;; to be included in the result.14 MAPCAN, like MAPCAR, passes the elements
;; of the list to the mapped function while MAPCON, like MAPLIST, passes the
;; cons cells.










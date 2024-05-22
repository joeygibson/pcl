(vector)
(vector 1)
(vector 1 2 3)

;; fixed-size array
(make-array 5)
(make-array 5 :initial-element nil)

;; create resizable array
(make-array 5 :fill-pointer 0)

;; resizable array, with a maximum of 5 items
(let ((arr (make-array 5 :fill-pointer 0)))
  (vector-push 'a arr)
  (vector-push 'r arr)
  (vector-push 'r arr)
  (vector-pop arr)
  (print arr))

;; adjustable _and_ resizable array
(let ((arr (make-array 5 :fill-pointer 0 :adjustable t)))
  (vector-push-extend 'a arr)
  (vector-push-extend 'b arr)
  (vector-pop arr)
  (print arr))

;; strings are just specialized vectors

;; create a resizable string
(make-array 5 :fill-pointer 0 :adjustable t
			:element-type 'character)

(defparameter *x* (vector 1 2 3))

;; sequence functions

(length *x*); 3
(elt *x* 0) ; 1
(elt *x* 1) ; 2

;; `elt` returns a `SETFable` place
(setf (elt *x* 0) 10)
(print *x*)

;; `count` the number of times item appears
(count 3 #(1 2 3 1 2 3)) ; 2

;; `remove` item from the sequence
(remove 3 #(1 2 3 1 2 3)) ; #(1 2 1 2)

;; works on lists, too
(remove 3 '(1 2 3 1 2 3)) ; (1 2 1 2)

;; and strings
(remove #\a "foobarbaz") ; "foobrbz"

;; replace item
(substitute 10 1 #(1 2 3 1 2 3)) ; #(10 2 3 10 2 3)

;; lists...
(substitute 10 1 '(1 2 3 1 2 3)) ; (10 2 3 10 2 3)

;; strings...
(substitute #\x #\a "foobarbaz") ; "foobxrbxz"

;; returns the first element of the sequence that satisfies the given test
(find 2 #(1 2 3 1 2 3)) ; 2
(find 23 #(1 2 3 1 2 3)) ; NIL

;; returns the index of the first element satisfying the test
(position 1 #(1 2 3 1 2 3))

;; specify test function
(count "foo" #("foo" "bar" "baz") :test #'string=) ; 1

;; specify key to extract from nested sequences
(find 'c #((a 10) (b 20) (c 30)) :key #'first) ; (c 30)

;; :start, and :end limit the search range
;; non-nill :from-end causes search in reverse
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first) ; (a 10)
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t) ; (a 30)




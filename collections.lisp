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


;; Higher order function versions
(count-if #'evenp #(1 2 3 4 5)) ; 2
(count-if-not #'evenp #(1 2 3 4 5)) ; 3
(position-if #'digit-char-p "abcd0001") ; 4

(remove-if-not #'(lambda (x) (char= (elt x 0) #\f))
			   #("foo" "bar" "baz" "foom")) ; #("foo" "foom")

(count-if #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e))
		  :key #'first) ; 2

(count-if-not #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e))
			  :key #'first) ; 3

(remove-if-not #'alpha-char-p #("foo" "bar" "1baz")
			   :key #'(lambda (x) (elt x 0))) ; #("foo" "bar")

(remove-duplicates #(1 2 1 2 3 1 2 3 4)) ; #(1 2 3 4)

(copy-seq #(1 2 3)) ; #(1 2 3)
(reverse #(1 2 3)) ; #(3 2 1)

(concatenate 'vector #(1 2 3) '(4 5 6)) ; #(1 2 3 4 5 6)
(concatenate 'list #(1 2 3) '(4 5 6)) ; '(1 2 3 4 5 6)
(concatenate 'string "abc" '(#\d #\e #\f)) ; "abcdef"

(defparameter *lst* '("foo" "bar" "baz" "quux"))

(setf *lst* (sort *lst* #'string<))
(setf *lst* (stable-sort *lst* #'string<))

(sort #((a 99) (b 23) (c 1))
	  #'<
	  :key #'(lambda (x) (elt x 1))) ; ((c 1) (b 23) (a 99))

;; merge combines two sequences in order
(merge 'vector
	   #(1 3 5)
	   #(2 4 6)
	   #'<) ; #(1 2 3 4 5 6)

(merge 'list
	   #(1 3 5)
	   #(2 4 6)
	   #'<) ; '(1 2 3 4 5 6)

(merge 'vector
	   #((a 99) (b 23) (c 1))
	   #((z 104) (p 55) (q 2))
	   #'<
	   :key #'cadr)

;; subsequences

(subseq "foobarbaz" 3) ; "barbaz"
(subseq "foobarbaz" 3 6) ; "bar"

(search "bar" "foobarbaz") ; 3

(mismatch "foobarbaz" "foom") ; 3

;; sequence predicates

(every #'evenp #(1 2 3 4 5)) ; nil
(some #'evenp #(1 2 3 4 5)) ; T
(notany #'evenp #(1 2 3 4 5)) ; nil
(notevery #'evenp #(1 2 3 4 5)) ; T

;; sequence mapping functions

(map 'vector
	 #'*
	 #(1 2 3 4 5)
	 #(10 9 8 7 6)) ; #(10 18 24 28 30)

(map 'vector
	 #'1+
	 #(1 2 3 4 5)) ; #(2 3 4 5 6)

(defparameter *bar* #(1 2 3 4 5))

(map-into *bar*
		  #'+
		  *bar*
		  #(1 2 3 4 5)) ; #(2 4 6 8 10)

(reduce #'+
		#(1 2 3 4 5)) ; 15

;; hash tables

(make-hash-table) ; creates a hash, not suitable for strings, since it uses EQL
(make-hash-table :test #'equal) ; hash suitable for strings

(defparameter *h* (make-hash-table))
(gethash 'foo *h*) ; nil, nil
(setf (gethash 'foo *h*) 'quux)
(gethash 'foo *h*) ; quux, T

;; use multiple-value-bind to access both values
(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
	(if present
		(format nil "Value ~a actually present" value)
		(format nil "Value ~a because key not found" value))))

(setf (gethash 'bar *h*) nil)

(show-value 'foo *h*) ; Value quux actually present
(show-value 'bar *h*) ; Value nil actually present
(show-value 'baz *h*) ; Value nil because key not found

;; remove a key
(remhash 'bar *h*) ; T

;; clear a hash table
(clrhash *h*)

;; hash table iteration

(maphash #'(lambda (k v)
			 (format t "~a => ~a~%" k v))
		 *h*)

;; remove all keys whose value is < 10
;; (maphash #'(lambda (k v)
;; 			 (when (< v 10)
;; 			   (remhash k *h*))) *h*)

(loop for k being the hash-keys in *h* using (hash-value v)
	  do (format t "~a => ~a~%" k v))




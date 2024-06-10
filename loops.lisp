(loop for i upto 10 collect i)
(loop as i upto 10 collect i)  ; `as` is a synonym of `for`

(loop for i from 0 downto -10 collect i)

(loop for i from 20 downto 10 collect i)
(loop for i downfrom 20 to 10 collect i) ; both are the same

(loop for i upto 10 do (print 23))
(loop repeat 10 do (print 23)) ; same, but no iteration variable

(loop for i in '(10 20 30) collect i)

; `by` clause specifies function to walk the list
; default is `cdr`
(loop for i in '(10 20 30 40)
	  by #'cddr
	  collect i) ; (10 30)

; `on` iterates the cons cells
(loop for i on '(10 20 30)
	  collect i) ; ((10 20 30) (20 30) (30))

; `on` with `by`
(loop for i on '(10 20 30 40)
	  by #'cddr
	  collect i) ; ((10 20 30 40) (30 40))

; `across` is used to loop over a vector (string, etc)
(loop for i across "abcd" collect i) ; (#\a #\b #\c #\d)

; for hashtables and packages, use this form:
; `(loop for var being the things in hash-or-package ...)`

(defparameter *hash* (make-hash-table))
(setf (gethash 'foo *hash*) "flarb")
(setf (gethash 'bar *hash*) "quux")

(loop for k being the hash-keys in *hash* collect k)   ; (foo bar)
(loop for v being the hash-values in *hash* collect v) ; ("flarb" "quux")

; keys _and_ values
(loop for k being the hash-keys in *hash*
	  using (hash-value v)
	  collect (list k v)) ; ((FOO "flarb") (BAR "quux"))

(loop for v being the hash-values in *hash*
	  using (hash-key k)
	  collect (list v k)) ; (("flarb" FOO) ("quux" BAR))

;; equals-then-iteration

;; (loop for x = 0
;; 		then (+ x 3)
;; 	  collect x)

(loop repeat 5
	  for x = 0 then y
	  for y = 1 then (+ x y)
	  collect y) ; (1 2 4 8 16)

(loop repeat 5
	  for x = 0 then y
	  and y = 1 then (+ x y)
	  collect y) ; (1 1 2 3 5)

(loop for i in '(10 20 30)
	  with foo = 23
	  collect (+ i foo)) ; (33 43 53)

; destructuring variables

(loop for (a b) in '((1 2) (3 4) (5 6))
	  do (format t "a: ~al b: ~b~%" a b))

(loop for (item . rest) on '(10 20 30 40)
	  do (format t "~a" item)
	  when rest do (format t ", "))

; ignore a value when destructuring
(loop for (a nil) in '((1 2) (3 4) (5 6))
	  do (format t "~a~%" a))

(loop for (nil b) in '((1 2) (3 4) (5 6))
	  do (format t "~a~%" b))

; participle forms of verbs are available
; collect/collecting append;appending, etc.

(defparameter *random* (loop repeat 100 collecting (random 10000)))

; most of the verbs at once
(loop for i in *random*
	  counting (evenp i) into evens
	  counting (oddp i) into odds
	  summing i into total
	  maximizing i into max
	  minimizing i into min
	  finally (return (list min max total evens odds)))

; unconditional execution

(loop for i from 1 to 10 do (print i))
(loop for i from 1 to 10 doing (print i))

(block outer
  (loop for i from 0 return 100)
  (print "This will print")
  200) ; 200

(block outer
  (loop for i from 0 do (return-from outer 100))
  (print "This won't print")
  200) ; 100

; conditional execution

; only print evens
(loop for i from 1 to 10
	  do (when (evenp i) (print i)))

(loop for i from 1 to 10
	  when (evenp i) summing i) ; 30

; conditions are `if`, `when`, `unless`

; `it` is the result of a condition check
(loop for key in '(foo blarb)
	  when (gethash key *hash*)
		collect it) ; ("flarb")

; they all have an optional `else` clause
(loop for key in '(foo blarb)
	  when (gethash key *hash*)
		collect it
	  else
		collect key) ; ("flarb" BLARB)

(loop for i from 1 to 100
	  if (evenp i)
		minimize i into min-even and
	  maximize i into max-even and
	  unless (zerop (mod i 4))
		sum i into even-not-fours-total
	  end
	  and sum i into even-total
	  else
		minimize i into min-odd and
	  maximize i into max-odd and
	  when (zerop (mod i 5))
		sum i into fives-total
	  end
	  and sum i into odd-total
	  do (return (list min-even
                       max-even
                       min-odd
                       max-odd
                       even-total
                       odd-total
                       fives-total
                       even-not-fours-total)))

; setting up and tearing down
; `initially` and `finally` clauses run before, and after, loop
(loop for i in '(1 2 3 4 5)
	  initially (print "Starting loop")
	  summing i into total
	  finally (format t "End of loop: ~a~% " total)
			  (return total)) ; 15

; break out of nested loop using loop names
(loop named outer for list in lists do
	  (loop for item in list do
			(if (what-i-am-looking-for-p item)
				(return-from outer item))))

; termination clauses: while, until, always, never, and thereis
(if (loop for n in numbers always (evenp n))
	(print "All even"))

(if (loop for n in numbers never (oddp n))
	(print "All even")) ; same thing

















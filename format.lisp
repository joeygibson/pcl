(defparameter *list* '(1 2 3 4 5))

(loop for cons on *list*
	  do (format t "~a" (car cons))
	  when (cdr cons) do (format t ", "))

(format t "~{~a~^, ~}~%" *list*)

(let ((foo (format nil "Foo bar baz")))
  (print foo))

; ~$ prints two decimal places
(format t "~$~%" pi)

; ~5$ prints five decimals
(format t "~5$~%" pi)

; take the 3 as the format argument
(format t "~v$~%" 3 pi)

; ???
(format t "~#$~%" pi)

; ~f prints floating point
(format t "~,5f~%" pi)

; integer
(format t "~d~%" 1000000)

; commas every 3
(format t "~:d~%" 1000000)

; pos/neg
(format t "~@d~%" 1000000)

; both
(format t "~@:d~%" 1000000)

; aesthetic form
(format t "The value is: ~a~%" 10)
(format t "The value is: ~a~%" "foo")
(format t "The value is: ~a~%" (list 3 2 1))

; ~% is forced newline, ~& is "fresh line"
(format t "~&~a~%" "foo")

; ~~ is a literal tilde








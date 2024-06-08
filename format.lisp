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

(format t "~&~d~%" -12345)

(format t "~&~12d~%" 1000000)  ;      1000000
(format t "~&~12,'0d" 1000000) ; 000001000000
(format t "~&~4,'0d-~2,'0d-~2,'0d" 2024 6 7) ; 2024-06-07

(format t "~:d" 100000000)       ; 100,000,000
(format t "~,,'.,4:d" 100000000) ; 1.0000.0000

(format t "~&~x~%" 1000000) ; F4240
(format t "~&~o~%" 1000000) ; 3641100
(format t "~&~b~%" 1000000) ; 11110100001001000000

(format t "~&~r~%" 1000000) ; one million

(format t "~&~36r~%" 1000000) ; LFLS

(format t "~&~f~%" pi)   ; 3.141592653589793
(format t "~&~,4f~%" pi) ; 3.1416
(format t "~&~e~%" pi)   ; 3.141592653589793d+0
(format t "~&~,4e~%" pi) ; 3.1416d+0

; ~$ is equivalent to ~,2f

;; English-Language Directives

(format t "~&~r~%" 1234)  ; one thousand two hundred thirty-four
(format t "~&~:r~%" 1234) ; one thousand two hundred thirty-fourth

; Roman fucking numerals! 😮

(format t "~&~@r~%" 1234)  ; MCCXXXIV
(format t "~&~:@r~%" 1234) ; MCCXXXIIII

; plurals

(format t "~&~r file~:p~%" 1)  ; one file
(format t "~&~r file~:p~%" 10) ; ten files
(format t "~&~r file~:p~%" 0)  ; zero files

(format t "~&~r famil~:@p~%" 1)  ; one family
(format t "~&~r famil~:@p~%" 10) ; ten families
(format t "~&~r famil~:@p~%" 0)  ; zero families

; things between ~( and ~) will be lowercase

(format t "~&~a~(~a~)~a~%" "FOO" "BAR" "BAZ") ; FOObarBAZ
(format t "~&~(~@r~)~%" 1234) ; mccxxxiv

(format t "~&~(~a~)~%" "tHe Quick BROWN foX")   ; the quick brown fox
(format t "~&~@(~a~)~%" "tHe Quick BROWN foX")  ; The quick brown fox
(format t "~&~:(~a~)~%" "tHe Quick BROWN foX")  ; The Quick Brown Fox
(format t "~&~:@(~a~)~%" "tHe Quick BROWN foX") ; THE QUICK BROWN FOX

;; Conditional Formatting

(format t "~&~[cero~;uno~;dos~]" 0) ; cero
(format t "~&~[cero~;uno~;dos~]" 1) ; uno
(format t "~&~[cero~;uno~;dos~]" 2) ; dos
(format t "~&~[cero~;uno~;dos~]" 3) ; ""

(format t "~&~[cero~;uno~;dos~:;mucho~]" 3)   ; mucho
(format t "~&~[cero~;uno~;dos~:;mucho~]" 100) ; mucho

(defparameter *list-etc*
  "~#[NONE~;~a~;~a and ~a~:;~a, ~a~]~#[~; and ~a~:;, ~a, etc~].")

(format nil *list-etc*)                ; "NONE."
(format nil *list-etc* 'a)             ; "A."
(format nil *list-etc* 'a 'b)          ; "A and B."
(format nil *list-etc* 'a 'b 'c)       ; "A, B and C."
(format nil *list-etc* 'a 'b 'c 'd)    ; "A, B, C, etc."
(format nil *list-etc* 'a 'b 'c 'd 'e) ; "A, B, C, etc."

; with a ~:[...~], it's false or true
(format t "~&~:[NO~;YES~]~%" (< 10 23)) ; YES
(format t "~&~:[NO~;YES~]~%" (< 55 23)) ; NO

(format nil "~@[x = ~a ~]~@[y = ~a~]" 10 20)   ; "x = 10 y = 20"
(format nil "~@[x = ~a ~]~@[y = ~a~]" 10 nil)  ; "x = 10 "
(format nil "~@[x = ~a ~]~@[y = ~a~]" nil 20)  ; "y = 20"
(format nil "~@[x = ~a ~]~@[y = ~a~]" nil nil) ; ""

;; Iteration

(format t "~&~{~a~%~}~%" '(1 2 3)) ; 1\n2\n3\n
(format t "~&~{~a, ~}~%" '(1 2 3)) ; 1, 2, 3, 

; the ^ skips the rest of the formatting after the last element
(format t "~&~{~a~^, ~}~%" '(1 2 3)) ; 1, 2, 3

; the @ processes the rest of the args as a list
(format t "~&~@{~a~^, ~}~%" 1 2 3) ; 1, 2, 3

(format t "~&~{~a~#[~;, and ~;, ~]~}" '(1 2 3)) ; 1, 2, and 3

; ~* skips next; ~:* skips back (repeats)
(format t "~&~r ~:*(~d)~%" 4) ; four (4)

(format nil "I saw ~r el~:*~[ves~;f~:;ves~]." 0) ; "I saw zero elves."
(format nil "I saw ~r el~:*~[ves~;f~:;ves~]." 1) ; "I saw one elf."
(format nil "I saw ~r el~:*~[ves~;f~:;ves~]." 2) ; "I saw two elves."










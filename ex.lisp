;; Define a new package
(defpackage :my-package
  (:use :cl)                 ; Use symbols from the CL package
  (:export :my-function))    ; Export the my-function symbol

;; Switch to the new package
(in-package :my-package)

;; Define a function in this package
(defun my-function (x)
  (format t "The value is: ~a~%" x))

;; Back in the CL-USER package
(in-package :cl-user)

;; Call the function using the package prefix
(my-package:my-function 42)  ; Prints "The value is: 42"


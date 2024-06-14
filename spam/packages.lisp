;; spam filter example
(in-package :cl-user)

(ql:quickload :cl-ppcre)
(ql:quickload :str)

(defpackage :com.joeygibson.pathnames
  (:use :common-lisp))

(load "pathnames.lisp")

(defpackage :com.joeygibson.spam
  (:use :common-lisp))

(load "spam.lisp")

(in-package :com.joeygibson.spam)

(use-package :str)







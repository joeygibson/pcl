;; spam filter example
(in-package :cl-user)

(ql:quickload :cl-ppcre)

(load "pathnames.lisp")

(defpackage :com.joeygibson.spam
  (:use :common-lisp
   :cl-ppcre
   :com.joeygibson.pathnames))

(load "spam.lisp")

(in-package :com.joeygibson.spam)











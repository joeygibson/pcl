;; (defpackage :com.joeygibson.email-d
;;   (:use :common-lisp
;; 		:com.joeygibson.text-db
;; 		:com.acme.text)
;;   (:import-from :com.acme.email :parse-email-address)
;;   (:shadow :build-index)
;;   (:shadowing-import-from :com.gigamonkeys.text-db :save))

(in-package :com.joeygibson.email-db)

(defun hello-world ()
  (print (open-db))
  (print "Hello, World 99"))










(in-package :common-lisp-user)

(defpackage :com.joeygibson.text-db
  (:use :common-lisp)
  (:export :open-db
		   :save
		   :store))

(defpackage :com.joeygibson.email-db
  (:use :common-lisp
		:com.joeygibson.text-db))

(in-package :com.joeygibson.email-db)

(hello-world)




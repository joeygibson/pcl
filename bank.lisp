(defclass bank-account ()
  ((customer-name
	:initarg :customer-name
	:initform (error "Must specify a customer name")
	:reader customer-name
	:documentation "Customer's name")
   (account-number
	:initform (random 999999999)
	:reader account-number
	:documentation "Account number, unique within bank")
   (balance
	:initarg :balance
	:initform 0
	:accessor balance
	:documentation "Current account balance")
   (account-type
	:reader account-type
	:documentation "Type of account")))

(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (slot-value account 'balance)))
	(setf (slot-value account 'account-type)
		  (cond
			((>= balance 100000) :gold)
			((>= balance 50000) :silver)
			(t :bronze)))))

(defmethod initialize-instance :after ((account bank-account)
									   &key opening-bonus-percentage)
  (when opening-bonus-percentage
	(incf (slot-value account 'balance)
		  (* (slot-value account 'balance)
			 (/ opening-bonus-percentage 100)))))

(defparameter *account*
  (make-instance 'bank-account :customer-name "John Doe"
				 :balance 23.00))

(defparameter *account2*
  (make-instance 'bank-account :customer-name "Billy Bob"
				 :balance 1000000
				 :opening-bonus-percentage 5))


(print (slot-value *account* 'customer-name))
(print (slot-value *account* 'balance))
(setf (slot-value *account* 'balance) 99.00)
(print (slot-value *account* 'account-number))
(print (slot-value *account* 'account-type))

;; Accessor Functions
;;
;; If not using the :reader, :writer, :accessor options,
;;  you have to write these by hand.
;;
;; (defgeneric balance (account))
;; (defmethod balance ((account bank-account))
;;   (slot-value account 'balance))

;; (defgeneric customer-name (account))
;; (defmethod customer-name ((account bank-account))
;;   (slot-value account 'customer-name))

;; (defgeneric (setf customer-name) (value account))
;; (defmethod (setf customer-name) (value (account bank-account))
;;   (setf (slot-value account 'customer-name) value))

;; (print (customer-name *account*))
;; (setf (customer-name *account*) "Linda Lou")

(print (customer-name *account*))
(print (account-number *account*))
(print (account-type *account*))
(print (balance *account*))

(defparameter *minimum-balance* 12345)

(defgeneric assess-penalty (account))
(defmethod assess-penalty ((account bank-account))
  (with-slots (balance) account
	(when (< balance *minimum-balance*)
	  (decf balance (* balance 0.01)))))

;; and again, using with-accessors
(defmethod assess-penalty ((account bank-account))
  (with-accessors ((bal balance)) account
	(when (< bal *minimum-balance*)
	  (decf bal (* bal 0.01)))))

(assess-penalty *account*)

(defgeneric merge-accounts (account1 account2))
(defmethod merge-accounts ((account1 bank-account)
						   (account2 bank-account))
  (with-accessors ((bal1 balance)) account1
	(with-accessors ((bal2 balance)) account2
	  (incf bal1 bal2)
	  (setf bal2 0))))

(merge-accounts *account* *account2*)
(print (balance *account*))
(print (balance *account2*))










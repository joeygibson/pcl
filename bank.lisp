(defclass bank-account ()
  ((customer-name
	:initarg :customer-name
	:initform (error "Must specify a customer name"))
   (account-number
	:initform (random 999999999))
   (balance
	:initarg :balance
	:initform 0)
   account-type))

(defparameter *account*
  (make-instance 'bank-account :customer-name "John Doe"
				 :balance 23.00))

(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (slot-value account 'balance)))
	(setf (slot-value account 'account-type)
		  (cond
			((>= balance 100000) :gold)
			((>= balance 50000) :silver)
			(t :bronze)))))

;; initializer with &key arguments
;; (defmethod initialize-instance :after ((account bank-account)
;;                                        &key opening-bonus-percentage)
;;   (when opening-bonus-percentage
;;     (incf (slot-value account 'balance)
;;           (* (slot-value account 'balance) (/ opening-bonus-percentage 100)))))

(print (slot-value *account* 'customer-name))
(print (slot-value *account* 'balance))
(setf (slot-value *account* 'balance) 99.00)
(print (slot-value *account* 'account-number))
(print (slot-value *account* 'account-type))






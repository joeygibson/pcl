(defgeneric withdraw (account amount)
  (:documentation "Withdraw the specified amount from account.
Signal an error if the balance is less than amount."))

(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
	(error "Account overdrawn."))
  (decf (balance account) amount))

(defmethod withdraw ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
	(when (plusp overdraft)
	  (withdraw (overdraft-account account) overdraft)
	  (incf (balance account) overdraft)))
  (call-next-method))

(defmethod withdraw ((account (eql *account-of-bank-president*)) amount)
  (let ((overdraft (- amount (balance account))))
	(when (plusp overdraft)
	  (incf (balance account) (embezzle *bank* overdraft)))
	(call-next-method)))





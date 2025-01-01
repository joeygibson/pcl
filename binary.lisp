(defun naive-read-u2 (in)
  (+ (* (read-byte in) 256)
	 (read-byte in)))

(defun read-u2 (in)
  (let ((u2 0))
	(setf (ldb (byte 8 8) u2) (read-byte in))
	(setf (ldb (byte 8 0) u2) (read-byte in))))

(defun write-u2 (out value)
  (write-byte (ldb (byte 8 8) value) out)
  (write-byte (ldb (byte 8 0) value) out))










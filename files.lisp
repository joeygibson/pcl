;; files and file IO

(let ((in (open "./funcs.lisp")))
  (format t "~a~%" (read-line in))
  (close in))

(let ((in (open "foo.lisp" :if-does-not-exist nil)))
  (when in
	(format t "~a~%" (read-line in))
    (close in)))

(let ((in (open "/some/file/name.txt" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
         while line do (format t "~a~%" line))
    (close in)))

; read will read a single sexp, ignoring whitespace and comments
(let ((in (open "funcs.lisp")))
  (format t "~a~%" (read in))
  (close in)); reads a single sexp

(let ((out (open "/tmp/dkdkdkkd.txt"
				 :direction :output :if-exists :supersede)))
  (close out))

;; read-char, read-line, read, read-sequence
;; write-char, write-line, write-string, write-sequence
;; terpri - writes a newline
;; fresh-line; only writes a newline if necessary

;; print, prints an sexp, preceded by newline, followed by space
;; prin1, just prints the sexp
;; pprint, tried to print sexp in pleasing way
;; princ, prints sexp in human-readable way
;; write-byte to write to binary file

(with-open-file (in "funcs.lisp")
  (with-open-file (out "/tmp/funcs.lisp"
					   :direction :output
					   :if-exists :supersede)
	(let ((line (read in)))
	  (print line out))))

;;; pathnames

(pathname "/tmp/foo") ; #P"/tmp/foo"
(pathname "./foo") ; #P"./foo"
(pathname-directory (pathname "/tmp/foo")) ; (:absolute "tmp")
(pathname-name (pathname "/tmp/foo")) ; "foo"
(pathname-type (pathname "/tmp/foo")) ; nil
(pathname-type (pathname "/tmp/foo.txt")) ; "txt

(namestring (pathname "/tmp/foo")) ; "/tmp/foo"
(directory-namestring (pathname "/tmp/foo")) "/tmp/"
(file-namestring (pathname "/tmp/foo")) ; "foo"

(make-pathname :device "c" :directory '(:absolute "foo" "bar")
			   :name "baz")

(make-pathname :type "html" :defaults (pathname "/tmp/foo"))

(merge-pathnames #p"foo/bar.html" #p"html/") ; #p"html/foo/bar.html"

(enough-namestring #p"/www/html/foo/bar.html" #p"/www/") ; "html/foo/bar.html"

;; Interacting with the File System

(probe-file "/tmp/sjsjsjsj") ; nil
(probe-file "funcs.lisp") ; #P".../funcs.lisp"

(directory ".") ; #P".../pcl"

; delete-file, rename-file
; ensure-directories-exist - creates path structure

;; (with-open-file (out (ensure-directories-exist "name") :direction :output)
;;   ...)

(file-write-date "funcs.lisp") ; file mod date
(file-author "funcs.lisp") ; jgibson
;; file-length takes a stream, gives length

;; for most accurate count, use binary stream
;; (with-open-file (in filename :element-type '(unsigned-byte 8))
;;   (file-length in))

;; file-position gives read/write position in stream

;;; Other Kinds of I/O





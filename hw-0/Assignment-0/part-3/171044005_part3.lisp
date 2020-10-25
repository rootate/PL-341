(defun collatz (n)
  (progn
   (push n l)
   (cond
     ((= n 1) n)
     ((evenp n) (collatz (/ n 2)))
     ((oddp n) (collatz (+ 1 (* n 3)))))))

(defun get-file-as-a-string (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun string-to-list (str)
  (if (not (streamp str))
    (string-to-list (make-string-input-stream str))
    (if (listen str)
      (cons (read str) (string-to-list str))
      nil)))

(defun write-list-to-file(filename l)
  (with-open-file (out filename :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~d : " (first l))
    (dolist (segment l)
      (format out "~d " segment))
    (format out "~%")))

(defun doit()
  (dolist (x (string-to-list (get-file-as-a-string "integer_inputs.txt")))
    (setq l '()) ( collatz x)
    (write-list-to-file "collatz_outputs.txt" (reverse l))))

(doit)





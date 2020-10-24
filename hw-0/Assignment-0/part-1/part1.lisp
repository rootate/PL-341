(defun get-file-as-a-string (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))
(defun flatten (L)
  (if (null L)
    nil
    (if(atom (first L))
      (cons (first L) (flatten (rest L)))
      (append (flatten (first L)) (flatten (rest L))))))

(defun write-list-to-file(filename l)
  (with-open-file (out filename :direction :output :if-exists :append :if-does-not-exist :create)
    (dolist (segment l)
      (format out "~d " segment))
    (format out "~%")))

(defun string-to-list (str)
  (if (not (streamp str))
    (string-to-list (make-string-input-stream str))
    (if (listen str)
      (cons (read str) (string-to-list str))
      nil)))


(write-list-to-file "out.txt" (flatten(string-to-list(get-file-as-a-string "nested_list.txt"))))
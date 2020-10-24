(defun primep (n) 
  (prog ((d 2))    
        CNT 
        (cond 
          (( >( expt d 2) n)  (return t))  
          (( = (mod n d) 0) (return NIL)))
        (cond 
          ((eq d 2) (incf d 1))
          (t  (incf d 2))) 
        (go CNT)))


(defun semi-primep(n)
  (prog ((d 2))
        CNT 
        (cond 
          (( >( expt d 2) n)  (return nil))  
          (( = (mod n d) 0) (return (and (not (eq 1 (/ n d))) (primep(/ n d))))))
        (cond 
          ((eq d 2) (incf d 1))
          (t  (incf d 2)))
        (go CNT)))


(defun what-is-it (x)
  (with-open-file (out "primedistribution.txt" :direction :output :if-exists :append :if-does-not-exist :create)
    (cond ((primep x) (format out "~d is prime ~%" x))
      ((semi-primep x) (format out "~d is semi-prime ~%" x)))))


(defun read-boundries()
  (with-open-file (in "boundries.txt" :direction :input)
    (loop repeat 2 
      collect (read in))))

(setq bound (read-boundries))
(loop for a from (car bound) to  (cadr bound)
  do (what-is-it a))




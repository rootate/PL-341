



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


(print (primep (read)))


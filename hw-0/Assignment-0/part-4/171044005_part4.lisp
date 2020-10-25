
(defun create-frequency-list (keys &optional (fl '())) 
  (cond ((endp keys) 
         fl)
    
    (t 
     (create-frequency-list (cdr keys) (inc-key-frequency fl (car keys))))))

(defun inc-key-frequency (fl key)
  (cond
    ((endp fl) 
     (list (new-frequency-list key 1))) 
    ((equal (get-key (car fl)) key) 
     (cons (new-frequency-list key (1+ (get-frequency (car fl)))) (cdr fl)))
    (t 
     (cons (car fl) (inc-key-frequency (cdr fl) key)))))



(defun new-frequency-list (key freq)
  (list (list key) freq))



(defun get-frequency (freq-list-node)
  (second  freq-list-node))



(defun get-key (freq-list-node)
  (car ( car freq-list-node)))



(defun lessp (first-tree second-tree)
  (<  (get-weight first-tree)  (get-weight second-tree)))



(defun get-keys (htree)
  (car (get-root htree)))



(defun get-weight (htree)
  (second (get-root htree)))



(defun get-root (htree)
  (car htree))


(defun sort-tree (htrees)
  (sort htrees #'lessp))


(defun create-huffman (keys)
  (create-huffman-helper 
   
   (sort-tree (map 'list 
                   #'list (create-frequency-list keys)))))


(defun create-huffman-helper (htrees)
  (if (equal (length htrees) 1) 
    (car htrees) 
    (create-huffman-helper 
     (sort-tree (cons 
                 (merge-trees (car htrees) (nth 1 htrees))
                 (cdr (cdr htrees)))))))



(defun merge-trees (first-tree second-tree)
  (list 
   (list (append (get-keys first-tree) (get-keys second-tree))
         (+ (get-weight first-tree) (get-weight second-tree)))
   first-tree second-tree))


(defun leafp (htree)
  (endp (cdr htree)))




(defun get-left-tree (htree)
  (second htree))



(defun get-right-tree (htree)
  (third htree))


(defun encode (keys huffman-tree)
  
  (if (endp keys) 
    nil
    
    (append (encode-key (car keys) huffman-tree)
            (encode (cdr keys) huffman-tree))))



(defun encode-key (key huffman-tree)
  (cond ((leafp huffman-tree) 
         nil)
    
    ((null (member key (get-keys (get-left-tree huffman-tree))))
     (cons '1 (encode-key key (get-right-tree huffman-tree))))
    (t 
     (cons '0 (encode-key key (get-left-tree huffman-tree))))))



(defun decode (bits huffman-tree)
  
  (decode-helper bits huffman-tree huffman-tree))



(defun decode-helper (bits main-tree tree-iter)
  
  (cond ((leafp tree-iter) 
         (cons (car (get-keys tree-iter)) 
               (decode-helper bits main-tree main-tree)))
    ((endp bits)  
     nil)
    
    ((equal '1 (car bits)) 
     (decode-helper (cdr bits) main-tree (get-right-tree tree-iter)))
    (T 
     (decode-helper (cdr bits) main-tree (get-left-tree tree-iter)))))

(defun string-to-list (s)
  (coerce s 'list))
(defun get-file-as-a-string (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))


(defvar keys (string-to-list(get-file-as-a-string "paragraph.txt")))
(defvar tree (create-huffman (string-to-list keys)))
(defvar encoded-keys (encode (string-to-list keys) tree))

(with-open-file (out "huffman_codes.txt" :direction :output :if-exists :append :if-does-not-exist :create)
  (loop for x  in (get-keys tree)
    do(format out "~a: ~d ~%" x (encode (list x )tree))))
;; binary file uyarisi alsanizda dosyayi acin
;; maalesef sirali olarak basmayi beceremedim







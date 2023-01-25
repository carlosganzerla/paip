(defparameter *suffixes* '(Jr MD PhD))

(defun last-name (name &optional acc)
  (if (member (first name) *suffixes*)
      (nreverse acc)
      (last-name (rest name) (cons (first name) acc))))

(defun naive-power (base pow)
  (destructuring-bind (pow-op acc-op) (if (minusp pow)
                                        (list #'/ #'+)
                                        (list #'* #'-))
    (labels ((rec (acc n)
                  (if (= n 0)
                    acc
                    (rec (funcall pow-op acc base)
                         (funcall acc-op n 1)))))
      (rec 1 pow))))

(defun count-atoms (lst &optional (acc 0))
  (if (not lst)
      acc   
      (count-atoms (cdr lst) (if (atom (car lst)) 
                                 (+ acc 1)
                                 (+ acc (count-atoms (car lst)))))))


(defun count-anywhere (expr lst &optional (acc 0))
  (if (not lst)
      acc   
      (let ((found (if (equal (car lst) expr) 1 0)))
       (count-anywhere expr (cdr lst)
                       (if (atom (car lst))
                           (+ acc found)
                           (+ acc found (count-anywhere expr (car lst))))))))

(defun dot-product (seq1 seq2)
  (apply #'+ (mapcar #'* seq1 seq2)))

;; NOTE: It's the same thing as mapcan
(defun mappend (fn the-list)
  "Apply fn to each element of lis t and append the results."
  (if (null the-list)
      nil
      (append (funcall fn (first the-list))
              (mappend fn (rest the-list))))) 

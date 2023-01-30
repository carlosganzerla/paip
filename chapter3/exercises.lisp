;; 3.1
(defmacro my-let ((&rest vars) &body body)
  (reduce (lambda (def acc)
            `(funcall (lambda (,(car def))
                        ,acc)
                      ,(cadr def)))
          vars
          :initial-value `(progn ,@body)
          :from-end t))
(a) 
("true" "k")


;; 3.2
;; Push but without mutation

;; 3.3 ??
(defun princ-dotted (expr)
  (if (listp expr)
      (princ `(,@(subseq expr 0 (1- (length expr))) . ,(first (last expr))))
      expr))

;; 3.5 
;; Store all dictionary words and use binary search. Since 2^20 > |Dictionary|,
;; gg

;; 3.6
(setf a 'global-a)
(defvar *b* 'global-b)
(defun fn () *b*)
(let ((a 'local-a)
      (*b* 'local-b))
  (list a *b* (fn) (symbol-value 'a) (symbol-value '*b*))) 


;; 3.7 to avoid errors and assumes that the second is faulty. The other key
;; goes into allow other keys

;; 3.8. Use whole key word. Get the right most on the list lmao

;; 3.9
(defun redlen (l)
  (reduce #'+ l :initial-value 0 :key (lambda (&rest args) 1)))

;; 3.10
;;  LCM - least common multiple of integers
;; Nconcs the first list reversed

;; 3.11
(union '((a . 3)) '((a . 1) (b . 3)) :key #'first)
(union '((c . 6)) '((a . 1) (b . 3)) :key #'first)

;; 3.12
(format t "~@(~{~a~^ ~}.~)" '(this is a test))

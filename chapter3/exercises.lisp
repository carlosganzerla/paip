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

;; 3.5 (run with chapter2/grammar.lisp)
;; Create question grammar and guesses. Generate all possiblities.
;; If can't find answer, add it to response

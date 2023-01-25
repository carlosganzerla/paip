(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(defparameter *runlang-grammar* 
  '((workout -> (white steps white))
    (steps -> (workout-step (plus white workout-step)))
    (workout-step* -> () (workout-step workout-step*))
    (workout-step -> (repetition) (step))
    (repeat -> (integer x ))
    (x -> x)))

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially , this is
   *simple-grammar*, but we can switch to other grammars.") 

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for thi s category."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (let ((rewrite (rewrites phrase)))
    (cond ((listp phrase) (mapcan #'generate phrase))
          (rewrite (generate (random-elt rewrite)))
          (t (list phrase)))))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
   with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

(defun cross-product (fn xlist ylist)
  "Return a list of all (fn x y) values."
  (mapcan (lambda (y)
            (mapcar (lambda (x) (funcall fn x y)) xlist))
          ylist))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x"
  (cross-product #'append xlist ylist)) 

(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
        ((listp phrase) (combine-all (generate-all (first phrase))
                                     (generate-all (rest phrase))))

        ((rewrites phrase)
         (mapcan #'generate-all (rewrites phrase)))      
        (t (list (list phrase))))) 

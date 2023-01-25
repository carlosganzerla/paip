(defvar *grammar*)

(defun ensure-list (l)
  (if (listp l)
      l
      (list l)))

(defun validate-choices (choices)
  (dolist (choice choices)
    (dolist (item (ensure-list choice))
      (assert (typep item '(or integer symbol string)) nil
              "Bad grammar item choice. Expected integer, symbol or string,
               got ~s" item))))


(defun validate-grammar (grammar)
  (assert (listp grammar) (grammar) "Bad grammar type ~A" (type-of grammar))
  (dolist (item grammar grammar)
    (destructuring-bind (sym choices) item
      (assert (listp choices) nil  "Bad grammar choices ~A" (type-of choices))
      (assert (symbolp sym) nil 
              "Bad definition for grammar item. Expected symbol, got ~A" sym)
      (validate-choices choices))))

(defmacro defgrammar (name &body items)
  "Defines a grammar using DEFPARAMETER NAME.
   The grammar has the format (RULE (CHOICES))*,
   where RULE is a symbol. CHOICE has the format
   {ELEM-CHOICE | (ELEM-CHOICE*)} and ELEM-CHOICE
   follows {RULE | STRING | INTEGER}"
  `(defparameter ,name ',(validate-grammar items)))


(defun get-rule (rule)
  (assert (symbolp rule) (rule) "Bad rule ~A" rule)
  (let ((found (assoc (symbol-name rule) *grammar* :key #'symbol-name)))
    (assert found (rule) "Rule ~A not found" rule)
    (cadr found)))

(defun choose (lst)
  (elt lst (random (length lst))))

(defun generate (rule &optional (stream *standard-output*))
  (typecase rule
    (list (mapcan (lambda (r) 
                    (generate r stream)) rule))
    (symbol (generate (choose (get-rule rule)) stream))
    (string (list (write-string rule stream)))
    (integer (list (write rule :stream stream)))))


(defgrammar *test*
  (number (0 1 2 3 4 5 6 7 8 9))
  (integer (number (number integer))))

(defgrammar *runlang-grammar*
  (range ("->"))
  (times ("x"))
  (plus  ("+" "\\"))
  (separator ("." ","))
  (min ("min"))
  (s ("s"))
  (h ("h"))
  (colon (":"))
  (m ("m"))
  (km ("km"))
  (ws (" ")) 
  (term  ("CL" "CA" "CV" "TR" "LVS" "LE" "MO" "FO" "FTS" "MAX"))
  (ws* (() (ws ws*)))
  (ws1 ((ws ws*)))
  (zero-to-five (0 1 2 3 4 5))
  (six-to-nine (6 7 8 9))
  (digit (six-to-nine zero-to-five))
  (integer (digit (digit integer)))
  (decimal ((integer separator digit) integer))
  (watch-digits ((zero-to-five digit)))
  (base60 (watch-digits digit))
  (distance ((integer m) (decimal km)))
  (watch-time ((base60 colon watch-digits)
               (integer colon watch-digits colon watch-digits)))
  (time (watch-time
          (base60 colon watch-digits)
          (base60 min)
          (base60 s)
          (base60 min base60 s)
          (integer h base60 min base60 s)
          (integer h base60 min)
          (integer h base60 s)))
  (pace (term (watch-time "/" km)))
  (progression ((pace range pace colon distance)))
  (step ((time ws1 distance)
         (time ws1 pace)
         (distance ws1 pace)
         (distance ws1 progression)))
  (repeat ((integer times "(" ws* workout ws* ")")))
  (workout-step ((step ws*) (repeat ws*)))
  (workout-step* (() (plus ws* workout-step)))
  (workout ((workout-step workout-step*)))) 

(let ((*grammar* *test*))
  (generate 'integer))

(let ((*grammar* *runlang-grammar*))
  (generate 'workout)
  nil)

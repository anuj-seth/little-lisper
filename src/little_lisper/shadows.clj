(ns little-lisper.shadows
  (:require [little-lisper.toys :as toys]
            [little-lisper.numbers-games :as n-g]
            [little-lisper.do-it-again :as d-i-a]))

;; numbered? determines whether a representation of an arithmetic expression only contains +, x and p (for power operation)
;; and the operands on which these operators will work. 
;; This function only accepts properly parenthesised expressions i.e. of the form 1 + (3 + 5) and not 1 + 3 + 5.
;; We could also say that it accepts expressions consisting of only one operator and two operands; the parentheses in 1 + (3 + 5) acts to
;; make this an expression with operator + and two operands 1 and (3 + 5).
;; We could also do away with the check for the operators by just assuming that we would be getting only valid expressions as per our
;; little language.
(defn numbered? [aexp]
  (cond
   (toys/atom? aexp) (number? aexp)
   (or (n-g/equan? '+ (toys/car (toys/cdr aexp)))
       (n-g/equan? 'x (toys/car (toys/cdr aexp)))
       (n-g/equan? 'p (toys/car (toys/cdr aexp))))
   (and (numbered? (toys/car aexp)) (numbered? (toys/car (toys/cdr (toys/cdr aexp)))))))


;; value finds out the value of an arithmetic expression.
(defn value [aexp]
  (cond
   (number? aexp) aexp
   (n-g/equan? '+ (toys/car (toys/cdr aexp))) (n-g/+ (value (toys/car aexp)) (value (toys/car (toys/cdr (toys/cdr aexp)))))
   (n-g/equan? 'x (toys/car (toys/cdr aexp))) (n-g/x (value (toys/car aexp)) (value (toys/car (toys/cdr (toys/cdr aexp)))))
   (n-g/equan? 'p (toys/car (toys/cdr aexp))) (n-g/pow (value (toys/car aexp)) (value (toys/car (toys/cdr (toys/cdr aexp)))))))

;; first-sub-exp returns the first sub-expression of a prefix arithmetic expression
(defn first-sub-exp [aexp]
  (toys/car (toys/cdr aexp))) 

;; second-sub-exp returns the second sub-expression of a prefix arithmetic expression
(defn second-sub-exp [aexp]
  (toys/car (toys/cdr (toys/cdr aexp)))) 

;; third-sub-exp returns the third sub-expression of a prefix arithmetic expression
(defn third-sub-exp [aexp]
  (toys/car (toys/cdr (toys/cdr (toys/cdr aexp))))) 

;; fourth-sub-exp returns the fourth sub-expression of a prefix arithmetic expression
(defn fourth-sub-exp [aexp]
  (toys/car (toys/cdr (toys/cdr (toys/cdr (toys/cdr aexp)))))) 


;; operator returns the operator of a prefix arithmetic expression
(defn operator [aexp]
  (toys/car aexp))

;; value-prefix finds out the value of a prefix arithmetic expression; the operators are represented by the atoms `plus`, `times`
;; and `expt`.
(defn value-prefix [aexp]
  (cond
   (number? aexp) aexp
   (n-g/equan? 'plus (operator aexp)) (n-g/+ (value-prefix (first-sub-exp aexp)) (value-prefix (second-sub-exp aexp)))
   (n-g/equan? 'times (operator aexp)) (n-g/x (value-prefix (first-sub-exp aexp)) (value-prefix (second-sub-exp aexp)))
   (n-g/equan? 'expt (operator aexp)) (n-g/pow (value-prefix (first-sub-exp aexp)) (value-prefix (second-sub-exp aexp)))))

;; mk+exp takes two or three arithmetic expressions- `aexp1`, `aexp2` and `aexp3` - and returns an 
;; expression joined by the plus, `+`, operator.
(defn mk+exp 
  ([aexp1 aexp2]
   (cons aexp1 (cons '+ (cons aexp2 ()))))
  ([aexp1 aexp2 aexp3]
   (cons aexp1 (cons '+ (cons aexp2 (cons '+ (cons aexp3 ())))))))

;; mkxexp takes two arithmetic expressions- `aexp1`, `aexp2` - and returns an 
;; expression joined by the multiply, `x`, operator.
(defn mkxexp [aexp1 aexp2]
  (cons aexp1 (cons 'x (cons aexp2 ()))))

;; mkpexp takes two arithmetic expressions- `aexp1`, `aexp2` - and returns an 
;; expression joined by the power, `p`, operator.
(defn mkpexp [aexp1 aexp2]
  (cons aexp1 (cons 'p (cons aexp2 ()))))

;; aexp? determines whether a representation of an arithmetic expression only contains +, x and p (for power operation)
;; and the operands on which these operators will work. 
;; This function only accepts properly parenthesised expressions i.e. of the form 1 + (3 + 5) and not 1 + 3 + 5.
;; We could also say that it accepts expressions consisting of only one operator and two operands; the parentheses in 1 + (3 + 5) acts to
;; make this an expression with operator + and two operands 1 and (3 + 5).
(defn aexp? [aexp]
  (cond
   (toys/atom? aexp) (number? aexp)
   (or (n-g/equan? '+ (toys/car (toys/cdr aexp)))
       (n-g/equan? 'x (toys/car (toys/cdr aexp)))
       (n-g/equan? 'p (toys/car (toys/cdr aexp)))) 
   (and (aexp? (toys/car aexp)) (aexp? (toys/car (toys/cdr (toys/cdr aexp)))))
   :else false))

;; count-op counts the number of operators in an arithmetic expression `aexp`
;;
(defn count-op [aexp]
  (cond
   (number? aexp) 0
   (or (n-g/equan? '+ (toys/car (toys/cdr aexp)))
       (n-g/equan? 'x (toys/car (toys/cdr aexp)))
       (n-g/equan? 'p (toys/car (toys/cdr aexp)))) (n-g/+ 1 
                                                          (n-g/+ (count-op (toys/car aexp))
                                                                 (count-op (toys/car (toys/cdr (toys/cdr aexp))))))))

;; count-one-op counts the number of times the given operator, `op`,  appears in an arithmetic expression `aexp`
(defn count-one-op [op aexp]
  (cond
   (number? aexp) 0
   (or (n-g/equan? op (toys/car (toys/cdr aexp)))) (n-g/+ 1 
                                                          (n-g/+ (count-one-op op (toys/car aexp)) 
                                                                 (count-one-op op (toys/car (toys/cdr (toys/cdr aexp))))))
   :else (n-g/+ (count-one-op op (toys/car aexp)) 
                (count-one-op op (toys/car (toys/cdr (toys/cdr aexp)))))))

;; count+ counts the number of times the plus - `+` - operator appears in an arithmetic expression `aexp`
(defn count+ [aexp]
  (count-one-op '+ aexp))

;; countp counts the number of times the power - `p` - operator appears in an arithmetic expression `aexp`
(defn countp [aexp]
  (count-one-op 'p aexp))

;; countx counts the number of times the multiplication - `x` - operator appears in an arithmetic expression `aexp`
(defn countx [aexp]
  (count-one-op 'x aexp))

;; count-numbers counts the number of numbers in an arithmetic expression `aexp`
(defn count-numbers [aexp]
  (if (number? aexp) 
    1
    (n-g/+ (count-numbers (toys/car aexp)) 
           (count-numbers (toys/car (toys/cdr (toys/cdr aexp)))))))

;; cnt-aexp counts the number of arithmetic sub-expressions in a list containing a prefix expression.
(defn cnt-aexp [aexp]
  (if (toys/null? aexp) 
    0
    (n-g/sub1 (n-g/length aexp))))

;; value-prefix-4 finds out the value of a prefix arithmetic expression; the operators are represented by the atoms `+`, `x`
;; and `p`.
;; It can handle a maximum of 4 operands.
(defn value-prefix-4 [aexp]
  (cond
   (number? aexp) aexp
   (= 2 (cnt-aexp aexp)) (cond 
                          (n-g/equan? '+ (operator aexp)) (n-g/+ (value-prefix-4 (first-sub-exp aexp)) 
                                                                 (value-prefix-4 (second-sub-exp aexp)))
                          (n-g/equan? 'x (operator aexp)) (n-g/x (value-prefix-4 (first-sub-exp aexp)) 
                                                                 (value-prefix-4 (second-sub-exp aexp)))
                          (n-g/equan? 'p (operator aexp)) (n-g/pow (value-prefix-4 (first-sub-exp aexp)) 
                                                                   (value-prefix-4 (second-sub-exp aexp))))
   (= 3 (cnt-aexp aexp)) (cond 
                          (n-g/equan? '+ (operator aexp)) (n-g/+ (n-g/+ (value-prefix-4 (first-sub-exp aexp)) 
                                                                        (value-prefix-4 (second-sub-exp aexp)))
                                                                 (value-prefix-4 (third-sub-exp aexp)))
                          (n-g/equan? 'x (operator aexp)) (n-g/x (n-g/x (value-prefix-4 (first-sub-exp aexp)) 
                                                                        (value-prefix-4 (second-sub-exp aexp)))
                                                                 (value-prefix-4 (third-sub-exp aexp))))
   (= 4 (cnt-aexp aexp)) (cond 
                          (n-g/equan? '+ (operator aexp)) (n-g/+ (n-g/+ (n-g/+ (value-prefix-4 (first-sub-exp aexp)) 
                                                                               (value-prefix-4 (second-sub-exp aexp)))
                                                                        (value-prefix-4 (third-sub-exp aexp)))
                                                                 (value-prefix-4 (fourth-sub-exp aexp)))
                          (n-g/equan? 'x (operator aexp)) (n-g/x (n-g/x (n-g/x (value-prefix-4 (first-sub-exp aexp)) 
                                                                               (value-prefix-4 (second-sub-exp aexp)))
                                                                        (value-prefix-4 (third-sub-exp aexp)))
                                                                 (value-prefix-4 (fourth-sub-exp aexp))))))

;; lexp? checks whether the S-expression `lexp` is a logical expression i.e. L-expression or not.
;; An L-expression consists of the atoms AND, OR or NOT and other L-expressions.
;; The L-expression is assumed to be in prefix notation
(defn lexp? [lexp]
  (cond
   (toys/atom? lexp) true
   (or (= (toys/car lexp) 'AND) (= (toys/car lexp) 'OR))  (and (lexp? (first-sub-exp lexp)) (lexp? (second-sub-exp lexp)))
   (= (toys/car lexp) 'NOT) (lexp? (first-sub-exp lexp))
   :else false))

;; covered? checks whether all the variables in `lexp` are in `lat` or not.
(defn covered? [lexp lat]
  (cond
   (toys/atom? lexp) (d-i-a/member? lexp lat)
   (or (= (toys/car lexp) 'AND) (= (toys/car lexp) 'OR))  (and (covered? (first-sub-exp lexp) lat) (covered? (second-sub-exp lexp) lat))
   (= (toys/car lexp) 'NOT) (covered? (first-sub-exp lexp) lat)))

;; lookup returns the value of `var`, as found in the `alist`.
;; The `alist` is a list of lists where each element is of the form `(var value)`.
;; The function will match the `var` with the first element of each pair in `alist`.
(defn lookup [var alist]
  (cond
   (toys/null? alist) nil
   (n-g/equan? var (toys/car (toys/car alist))) (toys/car (toys/cdr (toys/car alist)))
   :else (lookup var (toys/cdr alist))))

;; Mlexp evaluates the `lexp` by finding the value of each variable in the L-exp in `alist`.
(defn Mlexp [lexp alist]
  (cond
   (toys/atom? lexp) (n-g/equan? 1 (lookup lexp alist))
   (= (toys/car lexp) 'AND) (and (Mlexp (first-sub-exp lexp) alist) (Mlexp (second-sub-exp lexp) alist))
   (= (toys/car lexp) 'OR)  (or (Mlexp (first-sub-exp lexp) alist) (Mlexp (second-sub-exp lexp) alist))
   (= (toys/car lexp) 'NOT) (not (Mlexp (first-sub-exp lexp) alist))))

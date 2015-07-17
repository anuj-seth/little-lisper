(ns little-lisper.its-full-of-stars
  (:require [little-lisper.toys :as toys]
            [little-lisper.numbers-games :as n-g]))

;; leftmost returns the leftmost atom in the list.
;; If the first element is not an atom then it recursively drills down peeling off each layer of parentheses
;; until none are left.
(defn leftmost [l]
  (if (toys/atom? (toys/car l))
    (toys/car l)
    (leftmost (toys/car l))))


;; not negates any input supplied
(defn not [a]
  (if a
    nil
    true))

;; non-atom? reverses the result of toys/atom? function
(defn non-atom? [a]
  (not (toys/atom? a)))

;; rember* removes all the occurences of `a` from the list `l`.
;; When `l` contains other lists as elements then rember* will drill down to the atoms contained in each list 
;; and compare their value against `a`.
(defn rember* [a l]
  (cond
   (toys/null? l) ()
   (toys/atom? (toys/car l)) (cond
                              (= a (toys/car l)) (rember* a (toys/cdr l))
                              :else (cons (toys/car l) (rember* a (toys/cdr l))))
   :else (cons (rember* a (toys/car l)) (rember* a (toys/cdr l)))))

;; insertR* inserts the atom `new` to the right of the atom `old` for all occurences of `old` in the list `l`.
;; If the `l` contains other lists as elements then insertR* drills down to the atoms in the nested lists.
(defn insertR* [new old l]
  (cond
   (toys/null? l) ()
   (toys/atom? (toys/car l)) (cond
                              (= old (toys/car l)) (cons old (cons new (insertR* new old (toys/cdr l))))
                              :else (cons (toys/car l) (insertR* new old (toys/cdr l))))
   :else (cons (insertR* new old (toys/car l)) (insertR* new old (toys/cdr l)))))

;; insertL* inserts the atom `new` to the left of the atom `old` for all occurences of `old` in the list `l`.
;; If the `l` contains other lists as elements then insertL* drills down to the atoms in the nested lists.
(defn insertL* [new old l]
  (cond
   (toys/null? l) ()
   (toys/atom? (toys/car l)) (cond
                              (= old (toys/car l)) (cons new (cons old (insertL* new old (toys/cdr l))))
                              :else (cons (toys/car l) (insertL* new old (toys/cdr l))))
   :else (cons (insertL* new old (toys/car l)) (insertL* new old (toys/cdr l)))))


;; occur* counts the number of times the atom `a` occurs in the list `l`.
;; `l` may contain nested lists.
(defn occur* [a l]
  (cond
   (toys/null? l) 0
   (toys/atom? (toys/car l)) (cond 
                              (= a (toys/car l)) (n-g/+ 1 (occur* a (toys/cdr l)))
                              :else (occur* a (toys/cdr l)))
   :else (n-g/+ (occur* a (toys/car l)) (occur* a (toys/cdr l)))))



;; subst* replaces the atom `new` in place of the atom `old` for all occurences of `old` in the list `l`.
;; If the `l` contains other lists as elements then subst* drills down to the atoms in the nested lists.
(defn subst* [new old l]
  (cond
   (toys/null? l) ()
   (toys/atom? (toys/car l)) (cond
                              (= old (toys/car l)) (cons new (subst* new old (toys/cdr l)))
                              :else (cons (toys/car l) (subst* new old (toys/cdr l))))
   :else (cons (subst* new old (toys/car l)) (subst* new old (toys/cdr l)))))

;; member* returns true if the list `l` contains the atom `a`.
;; If the `l` contains other lists as elements then member* drills down to the atoms in the nested lists.
(defn member* [a l]
  (cond
   (toys/null? l) nil
   (toys/atom? (toys/car l)) (cond
                              (= a (toys/car l)) true
                              :else (member* a (toys/cdr l)))
   :else (or (member* a (toys/car l)) (member* a (toys/cdr l)))))


;;eqlist? determines if two lists `l1` and `l2` are structurally the same.
(defn eqlist? [l1 l2]
  (cond
   (and  (toys/null? l1) (toys/null? l2)) true
   (and (toys/atom? (toys/car l1)) (toys/atom? (toys/car l2))) (if (n-g/equan? (toys/car l1) (toys/car l2)) 
                                                                 (eqlist? (toys/cdr l1) (toys/cdr l2))
                                                                 false)
   :else (and (eqlist? (toys/car l1) (toys/car l2)) (eqlist? (toys/cdr l1) (toys/cdr l2)))))

;; equal? checks if two S-expressions are structurally the same.
;; The two expressions can be atoms or lists themselves.
(defn equal? [l1 l2]
  (cond
   (and (toys/atom? l1) (toys/atom? l2)) (n-g/equan? l1 l2)
   (and (non-atom? l1) (non-atom? l2)) (eqlist? l1 l2)
   :else false))

;; down* puts every atom in `l` in a list of it's own.
;; For nested lists, down* drills down to the atom and cons'es the atom on to a list.
(defn down* [l]
  (cond
   (toys/null? l) ()
   (toys/atom? (toys/car l)) (cons (cons (toys/car l) ()) (down* (toys/cdr l)))
   :else (cons (down* (toys/car l)) (down* (toys/cdr l)))))

;; occurN* counts all the atoms that are common to `lat` and `l`, `l` may contain nested lists.
;; Used occur* for each atom in lat and l.
(defn occurN* [lat l]
  (if (toys/null? lat) 
    0
    (n-g/+ (occur* (toys/car lat) l) (occurN* (toys/cdr lat) l))))

;; double* doubles each occurence of `a` in `l`.
;; We will use the function insertL* with `old` and `new` both set to `a`.
(defn double* [a l]
  (insertL* a a l))

;; rightmost returns the rightmost atom in the list.
(defn rightmost [l]
  (cond
   (and (non-atom? (toys/car l)) (n-g/equan? '() (toys/cdr l))) (rightmost (toys/car l))
   (and (toys/atom? (toys/car l)) (n-g/equan? '() (toys/cdr l))) (toys/car l)
   :else (rightmost (toys/cdr l))))

;; memberR* returns true if the list `l` contains the atom `a`. 
;; It tries to find the rightmost `a` in `l` by recurring on the cdr of `l`.
;; If the `l` contains other lists as elements then memberL* drills down to the atoms in the nested lists.
(defn memberR* [a l]
  ;; uncomment the println below to see that on finding the last `chips` in the list the function does not recur any further,
  ;; when run with following input
  ;; (little-lisper.its-full-of-stars/memberR* 'chips '((potato) (chips ((with) fish) (chips))))
  ;; (println l)
  (cond
   (and (non-atom? (toys/car l)) (n-g/equan? '() (toys/cdr l))) (memberR* a (toys/car l))
   (and (toys/atom? (toys/car l)) (n-g/equan? '() (toys/cdr l))) (if (n-g/equan? a (toys/car l))
                                                                   true
                                                                   false)
   :else (or (memberR* a (toys/cdr l)) (memberR* a (toys/car l)))))

;; list+ adds all the numbers in a general list, recursing down to atoms.
(defn list+ [l]
  (cond
   (toys/null? l) 0
   (number? (toys/car l)) (n-g/+ (toys/car l) (list+ (toys/cdr l)))
   :else (n-g/+ (list+ (toys/car l)) (list+ (toys/cdr l)))))

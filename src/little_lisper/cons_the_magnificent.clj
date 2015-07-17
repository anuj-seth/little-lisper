(ns little-lisper.cons-the-magnificent
  (:require [little-lisper.toys :as toys]
            [little-lisper.do-it-again :as d-i-a]))

;; rember removes the first occurence of atom `a` from the lat.
;; Only the first occurence will be removed any subsequent occurences of the atom will 
;; not be removed.
(defn rember [a lat]
  (cond
   (toys/null? lat) ()
   (= a (toys/car lat)) (toys/cdr lat)
   :else (cons (toys/car lat) (rember a (toys/cdr lat)))))

;; rember2 removes the second occurence of atom `a` from the lat.
;; Only the second occurence will be removed any previous or subsequent occurences of the atom will 
;; not be removed.
(defn rember2 [a lat]
  (cond
   (toys/null? lat) ()
   (= a (toys/car lat)) (cons a (rember a (toys/cdr lat)))
   :else (cons (toys/car lat) (rember2 a (toys/cdr lat)))))

;; firsts extracts the first elements from a list of lists i.e. each member of the list must 
;; also be a list or it is a null list.
(defn firsts [lol]
  (if (toys/null? lol)
    ()
    (cons (toys/car (toys/car lol)) (firsts (toys/cdr lol)))))

;; seconds extracts the second elements from a list of lists i.e. each member of the list must 
;; also be a list or it is a null list.
(defn seconds [lol]
  (if (toys/null? lol)
    ()
    (cons (toys/car (toys/cdr (toys/car lol))) (seconds (toys/cdr lol)))))


;; insertR inserts the atom `new` the right of the first occurence of atom `old` in the list `lat`
(defn insertR [new old lat]
  (cond
   (toys/null? lat) ()
   (= old (toys/car lat)) (cons old (cons new (toys/cdr lat)))
   :else (cons (toys/car lat) (insertR new old (toys/cdr lat)))))

;; insertL inserts the atom `new` the left of the first occurence of atom `old` in the list `lat`
(defn insertL [new old lat]
  (cond
   (toys/null? lat) ()
   (= old (toys/car lat)) (cons new lat)
   :else (cons (toys/car lat) (insertL new old (toys/cdr lat)))))

;; subst replaces the first occurence of atom `old` in the list `lat` with the atom `new`
(defn subst [new old lat]
  (cond
   (toys/null? lat) ()
   (= old (toys/car lat)) (cons new (toys/cdr lat))
   :else (cons (toys/car lat) (subst new old (toys/cdr lat)))))


;; subst2 replaces either the first occurence of atom `o1` or the first occurence of the atom `o2`
;; in the list `lat` with the atom `new`
(defn subst2 [new o1 o2 lat]
  (cond
   (toys/null? lat) ()
   (= o1 (toys/car lat)) (cons new (toys/cdr lat))
   (= o2 (toys/car lat)) (cons new (toys/cdr lat))
   :else (cons (toys/car lat) (subst2 new o1 o2 (toys/cdr lat)))))

;; dupla duplicates the `a`s as many times as the number of elements in l
(defn dupla [a l]
  (if (toys/null? l)
    ()
    (cons a (dupla a (toys/cdr l)))))

;; double repeats the first `a` in the list so that we have 2 instances of `a` in the list
(defn double [a l]
  (cond
   (toys/null? l) ()
   (= a (toys/car l)) (cons a l)
   :else (cons (toys/car l) (double a (toys/cdr l)))))

;; subst-sauce substitutes the first occurence of the atom `sauce` in the `lat` with atom `new`
(defn subst-sauce [new lat]
  (subst new 'sauce lat))

;; subst3 replaces the first occurence of either `o1`, `o2` or `o3` with `new` in `lat`
(defn subst3 [new o1 o2 o3 lat]
  (cond
   (toys/null? lat) ()
   (or (= o1 (toys/car lat)) (= o2 (toys/car lat)) (= o3 (toys/car lat))) (cons new (toys/cdr lat))
   :else (cons (toys/car lat) (subst3 new o1 o2 o3 (toys/cdr lat)))))

;; substN replaces the first occurence of an atom in `lat` that also occurs in `slat` with the atom `new`
(defn substN [new slat lat]
  (cond
   (toys/null? lat) ()
   (d-i-a/member? (toys/car lat) slat) (cons new (toys/cdr lat))
   :else (cons (toys/car lat) (substN new slat (toys/cdr lat))))) 

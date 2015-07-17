(ns little-lisper.the-multichapter-chapter
  (:require [little-lisper.toys :as toys]
            [little-lisper.numbers-games :as n-g]
            [little-lisper.do-it-again :as d-i-a]))

;; multirember removes all occurences of the atom `a` from the list of atoms `lat`
(defn multirember [a lat]
  (cond
   (toys/null? lat) ()
   (= a (toys/car lat)) (multirember a (toys/cdr lat))
   :else (cons (toys/car lat) (multirember a (toys/cdr lat)))))

;; multiinsertR inserts the atom `new` to the right of all occurences of atom `old` in the `lat`
(defn multiinsertR [new old lat]
  (cond 
   (toys/null? lat) ()
   (= old (toys/car lat)) (cons old (cons new (multiinsertR new old (toys/cdr lat))))
   :else (cons (toys/car lat) (multiinsertR new old (toys/cdr lat)))))

;; multiinsertL inserts the atom `new` to the left of all occurences of atom `old` in the `lat`
(defn multiinsertL [new old lat]
  (cond 
   (toys/null? lat) ()
   (= old (toys/car lat)) (cons new (cons old (multiinsertL new old (toys/cdr lat))))
   :else (cons (toys/car lat) (multiinsertL new old (toys/cdr lat)))))

;; multisubst substitutes the atom `new` for all occurences of atom `old` in the `lat`
(defn multisubst [new old lat]
  (cond 
   (toys/null? lat) ()
   (= old (toys/car lat)) (cons new (multisubst new old (toys/cdr lat)))
   :else (cons (toys/car lat) (multisubst new old (toys/cdr lat)))))

;; occur counts the number of times an atom `a` occurs in the list of atoms `lat`
(defn occur [a lat]
  (cond
   (toys/null? lat) 0
   (= a (toys/car lat)) (n-g/add1 (occur a (toys/cdr lat)))
   :else (occur a (toys/cdr lat))))

;; multisubst2 replaces all occurences of either `o1` or `o2` with `new` in `lat`
(defn multisubst2 [new o1 o2 lat]
  (cond 
   (toys/null? lat) ()
   (= o1 (toys/car lat)) (cons new (multisubst2 new o1 o2 (toys/cdr lat)))
   (= o2 (toys/car lat)) (cons new (multisubst2 new o1 o2 (toys/cdr lat)))
   :else (cons (toys/car lat) (multisubst2 new o1 o2 (toys/cdr lat)))))

;; multidown replaces each atom in `lat` with a list containing that atom
(defn multidown [lat]
  (if (toys/null? lat)
    ()
    (cons (cons (toys/car lat) ()) (multidown (toys/cdr lat)))))

;; occurN counts how many times an atom of `alat` occurs in `lat`
(defn occurN [alat lat]
  (if (toys/null? alat)
    0
    (n-g/+ (occur (toys/car alat) lat) (occurN (toys/cdr alat) lat))))

;; I returns the first atom of `lat2` that is also in `lat1`
(defn I [lat1 lat2]
  (cond
   (toys/null? lat2) ()
   (d-i-a/member? (toys/car lat2) lat1) (toys/car lat2)
   :else (I lat1 (toys/cdr lat2))))

;; multiI returns the list of atoms of `lat2` that are also in `lat1`
(defn multiI [lat1 lat2]
  (cond
   (toys/null? lat2) ()
   (d-i-a/member? (toys/car lat2) lat1) (cons (toys/car lat2) (multiI lat1 (toys/cdr lat2)))
   :else (multiI lat1 (toys/cdr lat2))))

;; multiup replaces every lat of length one in `l` by the atom in that list and 
;; also removes every empty list
(defn multiup [l]
  (cond
   (toys/null? l) ()
   (n-g/= 1 (n-g/length (toys/car l))) (cons (toys/car (toys/car l)) (multiup (toys/cdr l)))
   (toys/null? (toys/car l)) (multiup (toys/cdr l))
   :else (cons (toys/car l) (multiup (toys/cdr l)))
   
   ))

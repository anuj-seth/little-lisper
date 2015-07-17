(ns little-lisper.do-it-again
  (:require [little-lisper.toys :as toys]))
;; Chapter 2 - Do It, Do It Again, and Again, and Again...

;; lat? - list of atoms ? - checks if a list contains only atoms.
;; An atom is an empty list or anything that is not a list.
;; Returns true or nil.
(defn lat? [l]
  (cond
   (toys/null? l) true
   (toys/atom? (toys/car l)) (lat? (toys/cdr l))
   :else nil))

;; member? checks if the given atom 'a' belongs in a list of atoms.
;; Returns true or false.
(defn member? [a lat]
  (cond
   (toys/null? lat) false
   (= a (toys/car lat)) true
   :else (member? a (toys/cdr lat))))

(defn member2? [a lat]
  (cond
   (toys/null? lat) false
   :else (or (member? a (toys/cdr lat)) 
             (= a (toys/car lat)))))

;; member-twice? checks if an atom `a` appears at least twice in list
(defn member-twice? [a lat]
  (cond
   (toys/null? lat) false
   (= a (toys/car lat)) (member? a (toys/cdr lat))
   :else (member-twice? a (toys/cdr lat))))

;; nonlat? checks if a given list contains any non-atoms.
;; Uses lat? and inverts it's result.
;; Returns true or false.
(defn nonlat? [l]
  (if (lat? l)
    false
    true))

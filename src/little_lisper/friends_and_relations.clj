(ns little-lisper.friends-and-relations
  (:require [little-lisper.toys :as toys]
            [little-lisper.do-it-again :as d-i-a]
            [little-lisper.the-multichapter-chapter :as t-mc-c]
            [little-lisper.cons-the-magnificent :as c-t-m]))

;; set? returns true if a `lat` contains each atom only once, otherwise false.
(defn set? [lat]
  (cond
   (toys/null? lat) true
   (d-i-a/member? (toys/car lat) (toys/cdr lat)) false
   :else (set? (toys/cdr lat))))

;; makeset takes a `lat` as input and returns a set, removing all duplicates.
(defn makeset [lat]
  (cond 
   (toys/null? lat) ()
   (d-i-a/member? (toys/car lat) (toys/cdr lat)) (makeset (toys/cdr lat))
   :else (cons (toys/car lat) (makeset (toys/cdr lat)))))

;; makeset-multirember takes a `lat` and returns a set, removing all duplicates.
;; It uses the function the-multichapter-chapter/multirember to remove duplicates.
(defn makeset-multirember [lat]
  (cond
   (toys/null? lat) ()
   :else (cons (toys/car lat) (makeset-multirember (t-mc-c/multirember (toys/car lat) lat)))))

;; subset? returns true if each atom in `set1` is also in `set2`, otherwise false.
(defn subset? [set1 set2]
  (if (toys/null? set1)
    true
    (and (d-i-a/member? (toys/car set1) set2) (subset? (toys/cdr set1) set2))))

;; eqset? returns true if `set1` is equal to `set2` i.e. both have exactly the same elements.
;; We know that if two sets A and B are subsets of each other i.e. A is subset of B and B is subset of A, then the two sets are equal.
(defn eqset? [set1 set2]
  (and (subset? set1 set2) (subset? set2 set1)))
 
;; intersect? returns true if at least one element in `set1` is also present in `set2`, otherwise false.
(defn intersect? [set1 set2]
  (cond
   (toys/null? set1) false
   :else (or (d-i-a/member? (toys/car set1) set2) (intersect? (toys/cdr set1) set2))))

;; intersect returns the elements that are common to both `set1` and `set2`.
(defn intersect [set1 set2]
  (cond
   (toys/null? set1) ()
   (d-i-a/member? (toys/car set1) set2) (cons (toys/car set1) (intersect (toys/cdr set1) set2))
   :else (intersect (toys/cdr set1) set2)))

;; union returns the set consisting of all the elements of `set1` and `set2`.
;; This function creates a lat of all the elements of both sets and then calls makeset to remove duplicates, hence it
;; may not be very efficient for large sets.
(defn union [set1 set2]
  (cond 
   (toys/null? set1) set2
   :else (makeset (cons (toys/car set1) (union (toys/cdr set1) set2)))))

;; intersectall takes a list of sets, `l-set`, and returns the intersection of all the sets.
(defn intersectall [l-set]
  (cond 
   (toys/null? (toys/cdr l-set)) (toys/car l-set)
   :else (intersect (toys/car l-set) (intersectall (toys/cdr l-set)))))

;; first returns the first atom in a pair of atoms
(defn first [pair]
  (toys/car pair))

;; second returns the second atom in a pair of atoms.
(defn second [pair]
  (toys/car (toys/cdr pair)))

;; build builds a pair from two atoms `a1` and `a2`.
(defn build [a1 a2]
  (cons a1 (cons a2 ())))

;; fun? tests if a set of pairs is a function or not, where function is defined in the algebraic sense as being a relation between
;; 2 elements. A set of pairs is a function if the first elements of the pairs are unique.
(defn fun? [rel]
  (set? (c-t-m/firsts rel)))

;; revrel reverses the pairs in `rel`
(defn revrel [rel]
  (if (toys/null? rel)
    ()
    (cons (build (second (toys/car rel)) (first (toys/car rel))) (revrel (toys/cdr rel)))))

;; one-to-one returns true if the `fun` also has the second elements as a set.
(defn one-to-one [fun]
  (fun? (revrel fun)))

;; domset returns the domain of discourse of the relation `rel`.
;; The domain of discourse is the list of all the atoms in `rel`.
(defn domset [rel]
  (cond
   (toys/null? rel) ()
   :else (union (union (toys/car rel) (toys/car (toys/cdr rel))) (domset (toys/cdr (toys/cdr rel))))))

;; idrel of `s` makes a relation of all pairs of from (d, d) where d is an atom of the set `s`.
;; It is also called the identity relation on s.
(defn idrel [s]
  (if (toys/null? s)
    ()
    (cons (cons (toys/car s) (cons (toys/car s) '())) (idrel (toys/cdr s)))))

;; reflexive? returns true if the relation `r` is reflexive i.e. it contains all pairs of the form (d, d)
;; where d is an element in the domain of discourse of `r`.
(defn reflexive? [r]
  (eqset? (idrel (domset r)) 
          (intersect r (idrel (domset r)))))

;; symmetric? tests whether a relation is equal to it's reversed relation
(defn symmetric? [r]
  (eqset? r (revrel r)))

;; antisymmetric? tests whether the intersection of the relation `r` with it's reverse relation is a
;; subset of the identity relation on it's domain of discourse.
(defn antisymmetric? [r]
  (subset? (intersect r (revrel r)) 
           (idrel (domset r))))

;; Fapply returns the value of `f` at `x` i.e. the second of the pair whose first value if `x`.
(defn Fapply [f x]
  (cond
   (toys/null? f) nil
   (= (toys/car (toys/car f)) x) (toys/car (toys/cdr (toys/car f)))
   :else (Fapply (toys/cdr f) x)))

;; Fcomp composes two functions `f` and `g` so that if `g` contains an element (x, y) and `f` contains an element (y, z)
;; then Fcomp will contain (x, z).
(defn Fcomp [f g]
  (cond
   (toys/null? g) ()
   (nil? (Fapply f (second (toys/car g)))) (Fcomp f (toys/cdr g))
   :else (cons (cons (first (toys/car g)) (cons (Fapply f (second (toys/car g))) ())) (Fcomp f (toys/cdr g))))) 

;; Rapply returns the value set of `rel` at place `x`.
;; The value set is the set of second components of all pairs in `rel` whose first value is `x`.
(defn Rapply [rel x]
  (cond
   (toys/null? rel) ()
   (nil? (Fapply rel x)) (Rapply (toys/cdr rel) x)
   :else (cons (Fapply rel x) (Rapply (toys/cdr rel) x))))

;; Rin of `x` and set `s` produces a relation of pairs (x d) where d belongs to `s`.
(defn Rin [x s]
  (cond
   (toys/null? s) ()
   :else (cons (cons x (cons (toys/car s) ())) (Rin x (toys/cdr s)))))

;; Rcomp composes relation `rel1` and `rel2` i.e. for each pair (x y) in `rel1` if a pair (y z) exists in `rel2` it returns
;; (x z).
(defn Rcomp [rel1 rel2]
  (cond
   (toys/null? rel1) ()
   :else (union (Rin (first (toys/car rel1))
                     (Rapply rel2 (second (toys/car rel1))))
                (Rcomp (toys/cdr rel1) rel2))))

;; transitive? checks if the composition of `rel` with `rel` is a subset of `rel`.
(defn transitive? [rel]
  (subset? (Rcomp rel rel) rel))

(ns little-lisper.toys)
;; Chapter 1 - Toys
;; The functions in this namespace are only defined to provide the equivalent
;; functions for the ones defined in the book and present in Scheme.

;; Get the first element of a list
(defn car [l]
  (first l))

;; Get the list consisting of all elements of a list 
;; except the first one
(defn cdr [l]
  (rest l))

;; Takes a list and return true if the list is empty and false otherwise.
;; If an atom is passed as the argument then it throws an exception.
;; This could also be defined in term of the atom? function
(defn null? [l]
  (nil? (seq l)))

;; An atom is also an S-expression.
;; Any symbol or empty list is considered an atom.
;; Everything else is not an atom.
(defn atom? [x]
  (if (and (seq? x) (= (count x) 0))
    true
    (not (seq? x))))

;; eq? only takes 2 atoms as arguments
(defn eq? [a b]
  (if (not (and (atom? a) (atom? b)))
    (throw (Exception. "One of the arguments is not an atom"))
    (= a b)))


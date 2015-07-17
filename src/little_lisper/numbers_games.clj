(ns little-lisper.numbers-games
  (:require [little-lisper.toys :as toys]))

;; add1 increments a given number by 1
(defn add1 [n]
  (inc n))

;; sub1 decrements a given number by 1
(defn sub1 [n]
  (dec n))

;; + adds 2 numbers
(defn + [n m]
  (if (zero? m)
    n
    (+ (add1 n) (sub1 m))))

;; - subtracts 2 numbers
(defn - [n m]
  (if (zero? m)
    n
    (- (sub1 n) (sub1 m))))

;; addvec takes a list of numbers and adds them
(defn addvec [l]
  (if (toys/null? l) 
    0
    (+ (toys/car l) (addvec (toys/cdr l)))))

;; x multiplies 2 numbers n and m
;; or in other words it adds n up m times
(defn x [n m]
  (if (zero? m)
    0
    (+ n (x n (sub1 m)))))

;; vec+ adds the corresponding elements of two vectors
(defn vec+ [vec1 vec2]
  (cond
   (toys/null? vec1) vec2
   (toys/null? vec2) vec1
   :else (cons (+ (toys/car vec1) (toys/car vec2)) (vec+ (toys/cdr vec1) (toys/cdr vec2)))))

;; > returns true if n > m, false otherwise
(defn > [n m]
  (cond
   (zero? n) false
   (zero? m) true
   :else (> (sub1 n) (sub1 m))))

;; < returns true if n < m, false otherwise
(defn < [n m]
  (cond
   (zero? m) false
   (zero? n) true
   :else (< (sub1 n) (sub1 m))))

;; = returns true if n = m, false otherwise
(defn = [n m]
  (cond
   (zero? m) (zero? n)
   (zero? n) false
   :else (= (sub1 n) (sub1 m))))

;; pow gives n raised to the power m
;; or n multiplied by itself m times
(defn pow [n m]
  (if (zero? m)
    1
    (x n (pow n (sub1 m)))))

;; length counts the number of atoms in `lat`
(defn length [lat]
  (if (toys/null? lat)
    0
    (add1 (length (toys/cdr lat)))))

;; pick extracts the atom at position `n` in the `lat`
(defn pick [n lat]
  (cond
   (toys/null? lat) nil
   (= 0 (sub1 n)) (toys/car lat)
   :else (pick (sub1 n) (toys/cdr lat))))

;; rempick removes the atom at position `n` in the `lat`
;; and returns the lat
(defn rempick [n lat]
  (cond
   (toys/null? lat) ()
   (= 0 (sub1 n)) (toys/cdr lat)
   :else (cons (toys/car lat) (rempick (sub1 n) (toys/cdr lat)))))

;; no-nums takes a `lat` and removes all the numbers from it
;; and returns a new lat
(defn no-nums [lat]
  (cond
   (toys/null? lat) ()
   (number? (toys/car lat)) (no-nums (toys/cdr lat))
   :else (cons (toys/car lat) (no-nums (toys/cdr lat)))))

;; all-nums takes a `lat` and removes all the non-numbers from it
;; and returns a new lat
(defn all-nums [lat]
  (cond
   (toys/null? lat) ()
   (number? (toys/car lat)) (cons (toys/car lat) (all-nums (toys/cdr lat)))
   :else (all-nums (toys/cdr lat))))


;; duplicate takes a number `n` and an object `obj` and returns a list containing `obj` 
;; duplicated `n` times
(defn duplicate [n obj]
  (if (zero? n)
    ()
    (cons obj (duplicate (sub1 n) obj))))

;; multivec multiplies all the numbers in a vector and returns the result
(defn multivec [vec]
  (if (toys/null? vec)
    1
    (x (toys/car vec) (multivec (toys/cdr vec)))))

;; equan? just redefines clojure.core/=.
;; We have to use the fully qualified namespace of = since we have defined our own
;; function of the same name in this file.
(def equan? clojure.core/=)

;; index returns the place of the atom `a` in the `lat`.
;; The index numbering starts at 1.
;; The assumption is that `a` is always a part of `lat`.
(defn index [a lat]
  (println lat)
  (cond
   (equan? a (toys/car lat)) 1
   :else (add1 (index a (toys/cdr lat)))))

;; product takes 2 vectors of number, `vec1` and `vec2`, and returns a vector consisting of the products
;; of corresponding numbers in the inputs.
;; If one vector is longer then rest of the elements of that vector should be returned.
(defn product [vec1 vec2]
  (cond
   (toys/null? vec1) vec2
   (toys/null? vec2) vec1
   :else (cons (x (toys/car vec1) (toys/car vec2)) (product (toys/cdr vec1) (toys/cdr vec2)))))

;; dot-product multiplies corresponding numbers in `vec1` and `vec2` and sums them all up.
(defn dot-product [vec1 vec2]
  (addvec (product vec1 vec2)))

;; / divides two non-negative integers
(defn / [n m]
  (if (< n m)
    0
    (add1 (/ (- n m) m))))

;; remainder returns the number left over after dividing n by m
(defn remainder [n m]
  (if (< n m)
    n
    (+ 0 (remainder (- n m) m))))

;; <= returns true if n is less than or equal to m, false otherwise
(defn <= [n m]
  (cond
   (zero? m) (if (zero? n) 
               true
               false)
   (zero? n) true
   :else (<= (sub1 n) (sub1 m))))

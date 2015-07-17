(ns little-lisper.lambda-the-ultimate
  (:require [little-lisper.toys :as toys]))

;; rember-f is a function of `test?` that returns a function which removes all elements, elt, of `l` for which the function `test?` with arguments `a` and elt returns true.
(defn rember-f [test?] 
  (fn [a l]
    (cond
     (toys/null? l) ()
     (true? (test? a (toys/car l))) ((rember-f test?) a (toys/cdr l))
     :else (cons (toys/car l) ((rember-f test?) a (toys/cdr l))))))

(defn seqR [new old l]
  (cons old (cons new l)))
(defn seqL [new old l]
  (cons new (cons old l)))

;; insert-g inserts the element `new` to the left or right of `old` in list l.
;; The function `seq-f` decides whether to insert to left or right.
(defn insert-g [seq-f]
  (fn [new old l]
    (cond
     (toys/null? l) ()
     (= old (toys/car l)) (seq-f new old l)
     :else (cons (toys/car l) ((insert-g seq-f) new old (toys/cdr l))))))

(defn M [recfun]
  (fn [l]
    (cond
     (toys/null? l) ()
     (= (toys/car l) 'curry) (recfun (toys/cdr l))
     :else (cons (toys/car l) (recfun (toys/cdr l))))))

(defn function-maker [future]
  (M (fn [arg] ((future future) arg))))

(def Mrember-curry 
  ((fn [future]
     (M (fn [arg] ((future future) arg))))
   (fn [future]
     (M (fn [arg] ((future future) arg))))))

(defn Y [M]
  ((fn [future]
     (M (fn [arg] ((future future) arg))))
   (fn [future] (M (fn [arg] ((future future) arg))))))

(defn L [recfun]
  (fn [l]
    (cond
     (toys/null? l) 0
     :else (inc (recfun (toys/cdr l))))))

((Y L) [1 2 3])
;;(Mrember-curry '(a b c curry e curry g curry))

;;((M (fn [arg] ((future future) arg))) '(a b c curry e curry g curry))

(defn map [f l]
  (if (toys/null? l) 
    ()
    (cons (f (toys/car l)) (map f (toys/cdr l)))))

(defn assq-sf [a l sk fk]
  (cond
   (toys/null? l) (fk a)
   (= a (toys/car (toys/car l))) (sk (toys/car l))
   :else (assq-sf a (toys/cdr l) sk fk)))

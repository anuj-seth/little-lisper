(ns little-lisper.shadows-test
  (:require [clojure.test :refer :all] 
            [little-lisper.shadows :as s]))

(def aexp1 '(1 + (3 x 4)))
(def aexp2 '((3 p 4) + 5))
(def aexp3 '(3 x (4 x (5 x 6))))
(def aexp4 5)
(def l1 ())
(def l2 '(3 + (66 6)))
(def lexp1 '(AND (OR x y) y))
(def lexp2 '(AND (NOT y) (OR u v)))
(def lexp3 '(OR x y))
(def lexp4 'z)

(deftest exercise-7.2
  (is (true? (s/aexp? aexp1)))
  (is (true? (s/aexp? aexp2)))
  (is (false? (s/aexp? l1)))
  (is (false? (s/aexp? l2))))

(deftest exercise-7.3
  (is (= 2 (s/count-op aexp1)))
  (is (= 3 (s/count-op aexp3)))
  (is (= 0 (s/count-op aexp4))))

(deftest exercise-7.3_2
  (is (= 1 (s/count+ aexp1)))
  (is (= 1 (s/countx aexp1)))
  (is (zero? (s/countp aexp1))))

(deftest exercise-7.4
  (is (= 3 (s/count-numbers aexp1)))
  (is (= 4 (s/count-numbers aexp3)))
  (is (= 1 (s/count-numbers aexp4))))

(deftest exercise-7.5
  (let [aexp1 '(+ 3 2 (x 7 8))
        aexp2 '(x 3 4 5 6)
        aexp3 '(p aexp1 aexp2)]
    (is (= 3 (s/cnt-aexp aexp1)))
    (is (= 4 (s/cnt-aexp aexp2)))
    (is (= 2 (s/cnt-aexp aexp3)))))

(deftest exercise-7.6
  (is (true? (s/lexp? lexp1)))
  (is (true? (s/lexp? lexp2)))
  (is (true? (s/lexp? lexp3)))
  (is (false? (s/lexp? aexp1)))
  (is (false? (s/lexp? l2))))

(deftest exercise-7.7
  (let [l1 '(x y z u)]
    (is (true? (s/covered? lexp1 l1)))
    (is (false? (s/covered? lexp2 l1)))
    (is (true? (s/covered? lexp4 l1)))))

(deftest exercise-7.8
  (let [l1 '((x 1) (y 0))
        l2 '((u 1) (v 1))
        l3 ()
        a 'y
        b 'u]
    (is (zero? (s/lookup a l1)))
    (is (= 1 (s/lookup b l2)))
    (is (nil? (s/lookup a l3)))))

(deftest exercise-7.9
  (let [l1 '((x 1) (y 0) (z 0))
        l2 '((y 0) (u 0) (v 1))]
    (is (false? (s/Mlexp lexp1 l1)))
    (is (true? (s/Mlexp lexp2 l2)))
    (is (false? (s/Mlexp lexp4 l1))))
  )

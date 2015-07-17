(ns little-lisper.its-full-of-stars-test
  (:require [clojure.test :refer :all]
            [little-lisper.its-full-of-stars :as i-f-o-s]))

(def l1 '((fried potatoes) (baked (fried)) tomatoes))
(def l2 '(((chili) chili (chili))))
(def l3 ())
(def lat1 '(chili and hot))
(def lat2 '(baked fried))
(def a 'fried)

(deftest exercise-6.1
  (is (= '((((chili)) (chili) ((chili)))) (i-f-o-s/down* l2)))
  (is (= () (i-f-o-s/down* l3)))
  (is (= '((chili) (and) (hot)) (i-f-o-s/down* lat1))))

(deftest exercise-6.2
  (is (= 3 (i-f-o-s/occurN* lat1 l2)))
  (is (= 3 (i-f-o-s/occurN* lat2 l1)))
  (is (= 0 (i-f-o-s/occurN* lat1 l3))))

(deftest exercise-6.3
  (is (= '((fried fried potatoes) (baked (fried fried)) tomatoes) (i-f-o-s/double* a l1)))
  (is (= '(((chili) chili (chili))) (i-f-o-s/double* a l2)))
  (is (= '(baked fried fried) (i-f-o-s/double* a lat2))))

(deftest exercise-6.5
  ;; i-f-o-s/memberR* finds the rightmost atom matching the given atom
  (i-f-o-s/memberR* 'chips '((potato) (chips ((with) fish) (chips)))))

(deftest exercise-6.6
  (let [l1 '((1 (6 6 ())))
        l2 '((1 2 (3 6)) 1)]
    (is (= 13 (i-f-o-s/list+ l1)))
    (is (= 13 (i-f-o-s/list+ l2)))
    (is (zero? (i-f-o-s/list+ l3)))))


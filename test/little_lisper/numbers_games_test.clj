(ns little-lisper.numbers-games-test
  (:require [clojure.test :refer :all]
            [little-lisper.numbers-games :as n-g]))

(def vec1 '(1 2))
(def vec2 '(3 2 4))
(def vec3 '(2 1 3))
(def vec4 '(6 2 1))
(def l ())
(def zero 0)
(def one 1)
(def three 3)
(def obj '(x y))

(deftest exercise-4.1
  (is (= '((x y) (x y) (x y)) (n-g/duplicate three obj)))
  (is (= () (n-g/duplicate zero obj)))
  (is (= '((1 2)) (n-g/duplicate one vec1))))

(deftest exercise-4.2
  (is (= 24 (n-g/multivec vec2)))
  (is (= 6 (n-g/multivec vec3)))
  (is (= 1 (n-g/multivec l))))

(deftest exercise-4.5
  (let [a 'car
        lat1 '(cons cdr car null? eq?)
        b 'motor
        lat2 '(car engine auto motor)]
    (is (= 3 (n-g/index a lat1)))
    (is (= 1 (n-g/index a lat2)))
    (is (= 4 (n-g/index b lat2)))))

(deftest exercise-4.6
  (is (= '(3 4 4) (n-g/product vec1 vec2)))
  (is (= '(6 2 12) (n-g/product vec2 vec3)))
  (is (= '(12 2 3) (n-g/product vec3 vec4))))

(deftest exercise-4.7
  (is (= 29 (n-g/dot-product vec2 vec2)))
  (is (= 26 (n-g/dot-product vec2 vec4)))
  (is (= 17 (n-g/dot-product vec3 vec4))))

(deftest exercise-4.8
  (is (= 1 (n-g// 7 5)))
  (is (= 4 (n-g// 8 2)))
  (is (= 0 (n-g// 2 3))))

(deftest exercise-4.10
  (is (true? (n-g/<= zero one)))
  (is (true? (n-g/<= one one)))
  (is (false? (n-g/<= three one))))

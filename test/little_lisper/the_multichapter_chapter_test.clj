(ns little-lisper.the-multichapter-chapter-test
  (:require [clojure.test :refer :all]
            [little-lisper.the-multichapter-chapter :as t-mc-c]))

(def x 'comma)
(def y 'dot)
(def a 'kiwis)
(def b 'plums)
(def lat1 '(bananas kiwis))
(def lat2 '(peaches apples bananas))
(def lat3 '(kiwis pears plums bananas cherries))
(def lat4 '(kiwis mangoes kiwis guavas kiwis))
(def l1 '((curry) () (chicken) ()))
(def l2 '((peaches) (and cream)))
(def l3 '((plums) and (ice) and cream))
(def l4 ())

(deftest exercise-5.1
  (let [multisubst-kiwis (fn [new lat] (t-mc-c/multisubst new 'kiwis lat))]
    (is (= '(bananas plums) (multisubst-kiwis b lat1)))
    (is (= '(peaches apples bananas) (multisubst-kiwis y lat2)))
    (is (= '(dot mangoes dot guavas dot) (multisubst-kiwis y lat4)))
    (is (= () (multisubst-kiwis y l4)))))

(deftest exercise-5.2
  (is (= '(bananas comma) (t-mc-c/multisubst2 x a b lat1)))
  (is (= '(dot pears dot bananas cherries) (t-mc-c/multisubst2 y a b lat3)))
  (is (= '(bananas kiwis) (t-mc-c/multisubst2 a x y lat1))))

(deftest exercise-5.3
  (is (= '((bananas) (kiwis))) (t-mc-c/multidown lat1))
  (is (= '((peaches) (apples) (bananas)) (t-mc-c/multidown lat2)))
  (is (= () (t-mc-c/multidown l4))))

(deftest exercise-5.4
  (is (zero? (t-mc-c/occurN lat1 l4)))
  (is (= 1 (t-mc-c/occurN lat1 lat2)))
  (is (= 2 (t-mc-c/occurN lat1 lat3))))

(deftest exercise-5.5
  (is (= () (t-mc-c/I lat1 l4)))
  (is (= 'bananas (t-mc-c/I lat1 lat2)))
  (is (= 'kiwis (t-mc-c/I lat1 lat3)))
  (is (= () (t-mc-c/multiI lat1 l4)))
  (is (= '(bananas) (t-mc-c/multiI lat1 lat2)))
  (is (= '(kiwis bananas) (t-mc-c/multiI lat1 lat3))))

(deftest exercise-5.9
  (is (= () (t-mc-c/multiup l4)))
  (is (= '(curry chicken) (t-mc-c/multiup l1)))
  (is (= '(peaches (and cream)) (t-mc-c/multiup l2))))




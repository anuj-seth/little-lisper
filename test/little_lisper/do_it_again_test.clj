(ns little-lisper.do-it-again-test
  (:require [clojure.test :refer :all]
            [little-lisper.do-it-again :as d-i-a]))

(def l1 '(german chocolate cake))
(def l2 '(poppy seed cake))
(def l3 '((linzer) (torte) ()))
(def l4 '((bleu cheese) (and) (red) (wine)))
(def l5 '(() ()))
(def a1 'coffee)
(def a2 'seed)
(def a3 'poppy)

(deftest exercise-2.1
  (is (true? (d-i-a/lat? l1)))
  (is (true? (d-i-a/lat? l2)))
  (is (nil? (d-i-a/lat? l3))))

(deftest exercise-2.2
  (is (false? (d-i-a/member? a1 l1)))
  (is (true? (d-i-a/member? a2 l2))))

(deftest exercise-2.5
  (is (false? (d-i-a/nonlat? l1)))
  (is (false? (d-i-a/nonlat? l2)))
  (testing "As per The Little Lisper, 3rd Edition, (nonlat? l3) should be false but I believe that is wrong."
      (is (false? (d-i-a/nonlat? l3))))
  (is (true? (d-i-a/nonlat? l4)))
  (is (false? (d-i-a/nonlat? l5))))

(deftest exercise-2.6
  (let [member-cake? (fn [l] (d-i-a/member? 'cake l))]
    (is (true? (member-cake? l1)))
    (is (true? (member-cake? l2)))
    (is (false? (member-cake? l5)))))

(deftest exercise-2.7
  (is (false? (d-i-a/member2? a1 l1)))
  (is (true? (d-i-a/member2? a2 l2))))

(deftest exercise-2.10
  (is (true? (d-i-a/member-twice? a2 '(poppy seed cake poppy seed cake seed))))
  (is (false? (d-i-a/member-twice? a2 '(poppy cake)))))

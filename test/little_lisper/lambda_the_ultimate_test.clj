(ns little-lisper.lambda-the-ultimate-test
  (:require [little-lisper.toys :as toys]
            [little-lisper.friends-and-relations :as f-a-r]
            [little-lisper.lambda-the-ultimate :as l-t-u]
            [clojure.test :refer :all]))

(deftest exercise-9.2
  (let [a 'apple
        b1 ()
        b2 '((apple 1) (plum 2))
        b3 '((peach 3))
        sk (fn [p] (f-a-r/build (first p) (inc (second p))))
        fk (fn [name] (cons name '(not-in-list)))]
    (is (= '(apple not-in-list) (l-t-u/assq-sf a b1 sk fk)))
    (is (= '(apple 2) (l-t-u/assq-sf a b2 sk fk)))
    (is (= '(apple not-in-list) (l-t-u/assq-sf a b3 sk fk)))))



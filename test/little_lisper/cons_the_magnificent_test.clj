(ns little-lisper.cons-the-magnificent-test
  (:require [clojure.test :refer :all]
            [little-lisper.cons-the-magnificent :as c-t-m]))

(def l1 '((paella spanish) (wine red) (and beans)))
(def l2 ())
(def l3 '(cincinnati chili))
(def l4 '(texas hot chili))
(def l5 '(soy sauce and tomato sauce))
(def l6 '((spanish) () (paella)))
(def l7 '((and hot) (but dogs)))
(def a1 'chili)
(def a2 'hot)
(def a3 'spicy)
(def a4 'sauce)
(def a5 'soy)

(deftest exercise-3.1
  (is (= '(spanish red beans) (c-t-m/seconds l1)))
  (is (= () (c-t-m/seconds l2)))
  (is (= '(hot dogs) (c-t-m/seconds l7))))

(deftest exercise-3.2
  (is (= '(hot hot hot) (c-t-m/dupla a2 l4)))
  (is (= () (c-t-m/dupla a2 l2)))
  (is (= '(chili chili chili chili chili) (c-t-m/dupla a1 l5))))

(deftest exercise-3.3
  (is (= () (c-t-m/double a2 l2)))
  (is (= '(cincinnati chili chili) (c-t-m/double a1 l3)))
  (is (= '(texas hot hot chili) (c-t-m/double a2 l4))))

(deftest exercise-3.4
  (is (= '(texas hot chili) (c-t-m/subst-sauce a1 l4)))
  (is (= '(soy chili and tomato sauce) (c-t-m/subst-sauce a1 l5)))
  (is (= ( ) (c-t-m/subst-sauce a4 l2))))

(deftest exercise-3.5
  (is (= '(soy soy and tomato sauce) (c-t-m/subst3 a5 a1 a2 a4 l5)))
  (is (= '(texas sauce chili) (c-t-m/subst3 a4 a1 a2 a3 l4)))
  (is (= () (c-t-m/subst3 a3 a1 a2 a5 l2))))

(deftest exercise-3.6
  (is (= '(texas hot hot) (c-t-m/substN a2 l3 l4)))
  (is (= '(soy sauce and tomato sauce) (c-t-m/substN a4 l3 l5)))
  (is (= () (c-t-m/substN a4 l3 l2))))

(deftest exercise-3.9
  (is (= '(cincinnati chili) (c-t-m/rember2 a1 l3)))
  (is (= '(soy sauce and tomato) (c-t-m/rember2 a4 l5)))
  (is (= '() (c-t-m/rember2 a4 l2))))

(ns little-lisper.friends-and-relations-test
  (:require [little-lisper.friends-and-relations :as f-a-r]
            [clojure.test :refer :all]))

(def r1 '((a b) (a a) (b b)))
(def r2 '((c c)))
(def r3 '((a c) (b c)))
(def r4 '((a b) (b a)))
(def f1 '((a 1) (b 2) (c 2) (d 1)))
(def f2 ())
(def f3 '((a 2) (b 1)))
(def f4 '((1 $) (3 *)))
(def d1 '(a b))
(def d2 '(c d))
(def x 'a)

(deftest exercise-8.1
  (is (= '(a b) (f-a-r/domset r1)))
  (is (= '(c) (f-a-r/domset r2)))
  (is (= '(a b c) (f-a-r/domset r3)))
  (is (= '((a a) (b b)) (f-a-r/idrel d1)))
  (is (= '((c c) (d d)) (f-a-r/idrel d2)))
  (is (= () (f-a-r/idrel f2))))

(deftest exercise-8.2
  (is (true? (f-a-r/reflexive? r1)))
  (is (true? (f-a-r/reflexive? r2)))
  (is (false? (f-a-r/reflexive? r3))))

(deftest exercise-8.3
  (is (false? (f-a-r/symmetric? r1)))
  (is (true? (f-a-r/symmetric? r2)))
  (is (true? (f-a-r/symmetric? f2))))

(deftest exercise-8.3-part-2
  (is (true? (f-a-r/antisymmetric? r1)))
  (is (true? (f-a-r/antisymmetric? r2)))
  (is (false? (f-a-r/antisymmetric? r4))))

(deftest exercise-8.4
  (is (= 1 (f-a-r/Fapply f1 x)))
  (is (nil? (f-a-r/Fapply f2 x)))
  (is (= 2 (f-a-r/Fapply f3 x))))


(deftest exercise-8.5
  (is (= () (f-a-r/Fcomp f1 f4)))
  (is (= () (f-a-r/Fcomp f1 f3)))
  (is (= '((a $) (d $)) (f-a-r/Fcomp f4 f1)))
  (is (= '((b $) (f-a-r/Fcomp f4 f3)))))

(deftest exercise-8.6
  (is (= '(1) (f-a-r/Rapply f1 x)))
  (is (= '(b a) (f-a-r/Rapply r1 x)))
  (is (= () (f-a-r/Rapply f2 x))))

(deftest exercise-8.7
  (is (= '((a a) (a b) (f-a-r/Rin x d1))))
  (is (= '((a c) (a d)) (f-a-r/Rin x d2)))
  (is (= () (f-a-r/Rin x f2))))

(deftest exercise-8.8
  (is (= '((a c) (b c))) (f-a-r/Rcomp r1 r3))
  (is (= '((a 2) (a 1) (b 2)) (f-a-r/Rcomp r1 f1)))
  (is (= '((a b) (a a) (b b)) (f-a-r/Rcomp r1 r1))))

(deftest exercise-8.9
  (is (true? (f-a-r/transitive? r1)))
  (is (true? (f-a-r/transitive? r3)))
  (is (true? (f-a-r/transitive? f1))))




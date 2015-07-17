(ns little-lisper.toys-test
  (:require [clojure.test :refer :all]
            [little-lisper.toys :as toys]))

(deftest exercise-1.3
  (let [a 'all
        b 'these
        c 'problems
        d ()]
    (is (= '(all (these problems)) (cons a (cons (cons b (cons c d)) d))))
    (is (= '(all (these) problems) (cons a (cons (cons b d) (cons c d)))))
    (is (= '((all these) problems) (cons (cons a (cons b d)) (cons c d))))
    (is (= '((all these problems)) (cons (cons a (cons b (cons c d))) d)))))

(deftest exercise-1.4
  (let [a 'french
        l '(fries)]
    (is (= a (toys/car (cons a l)))))
  (let [a 'oranges
        l '(apples and peaches)]
    (is (= l (toys/cdr (cons a l))))))

(deftest exercise-1.5
  (let [x 'lisp
        y 'lisp]
    (is (toys/eq? x y))))

(deftest exercise-1.6
  (let [a 'atom
        l '()]
    (testing "There is no list that when cons'ed with an atom will return an empty list, hence the test will always fail"
      (is (= true (toys/null? (cons a l)))))))

(deftest exercise-1.7
  (is (= '(()) (cons () ())))
  (is (nil? (toys/car ())))
  (is (= () (toys/cdr ()))))

(deftest exercise-1.8
  (let [l '((meatballs) and spaghetti)]
    (is (false? (toys/atom? (toys/car l)))))
  (let [l '((meatballs))]
    (is (true? (toys/null? (toys/cdr l)))))
  (let [l '(two meatballs)]
    (is (false? (toys/eq? (toys/car l) (toys/car (toys/cdr l))))))
  (let [l '(ball)
        a 'meat]
    (is (false? (toys/atom? (cons a l))))))

(deftest exercise-1.9
  (let [l '((kiwis mangoes lemons) and (more))]
    (is (= 'lemons (toys/car (toys/cdr (toys/cdr (toys/car l)))))))
  (let [l '(() (eggs and (bacon)) (for) (breakfast))]
    (is (= 'and (toys/car (toys/cdr (toys/car (toys/cdr l)))))))
  (let [l '(() () () (and (coffee) please))]
    (is (= '(and (coffee) please) (toys/car (toys/cdr (toys/cdr (toys/cdr l))))))))

(deftest exercise-1.10
  (let [l '(apples in (Harry has a backyard))]
    (is (= 'Harry (toys/car (toys/car (toys/cdr (toys/cdr l)))))))
  (let [l '(apples and Harry)]
    (is (= 'Harry (toys/car (toys/cdr (toys/cdr l))))))
  (let [l '(((apples) and ((Harry))) in his backyard)]
    (is (= 'Harry (toys/car (toys/car (toys/car (toys/cdr (toys/cdr (toys/car l))))))))))



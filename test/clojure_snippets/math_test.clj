(ns clojure-snippets.math-test
  (:use clojure.test
        clojure-snippets.math))

(deftest make-fib-test
  (is (= '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610)
         (take 16 ((make-fib 1 1) 0 1))))
  (is (= '(1 2 1 2 1 2 1 2)
         (take 8 ((make-fib 1 0) 1 2))))
  (is (= '(1 4 14 50)
         (take 4 ((make-fib 2 3) 1 4))))
  (is (= '(-1 1 2 1 -1 -2 -1 1 2 1 -1 -2 -1 1 2 1)
         (take 16 ((make-fib -1 1) -1 1)))))

(deftest binom-test
  (let [pascal-row (fn [n] (for [k (range (inc n))] (binom n k)))]
    (is (= '(1) (pascal-row 0)))
    (is (= '(1 1) (pascal-row 1)))
    (is (= '(1 2 1) (pascal-row 2)))
    (is (= '(1 3 3 1) (pascal-row 3)))
    (is (= '(1 4 6 4 1) (pascal-row 4)))
    (is (= '(1 5 10 10 5 1) (pascal-row 5)))
    (is (= '(1 6 15 20 15 6 1) (pascal-row 6)))
    (is (= '(1 7 21 35 35 21 7 1) (pascal-row 7)))))
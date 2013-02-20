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

(deftest coprimes-test
  (is (= #{[1 2] [1 3] [1 4] [1 5] [1 6] [1 7] [1 8] [1 9] 
           [2 3] [2 5] [2 7] [2 9]
           [3 4] [3 5] [3 7] [3 8]
           [4 5] [4 7] [4 9]
           [5 6] [5 7] [5 8] [5 9]
           [6 7]
           [7 8] [7 9]
           [8 9]}
         (set (coprimes (fn [_ m] (> 10 m)))))))

(deftest coprimes-test
  (let [ps '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 
             47 53 59 61 67 71 73 79 83 89 97 101)]
    (is (= ps (primes 101)))
    (is (= ps (primes 102)))))
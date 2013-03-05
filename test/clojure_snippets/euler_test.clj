(ns clojure-snippets.euler-test
  (:use clojure.test
        clojure-snippets.euler))

(deftest test-solve-1
  (is (= 23 (solve-1 10))))

(deftest test-solve-1-sample
  (is (= 23 (solve-1-sample 10))))

(deftest test-solve-2
  (is (= (+ 2 8 34) (solve-2 89))))

(deftest test-solve-7
  (is (= '(2 3 5 7 11 13) (map solve-7 (range 1 7)))))

(deftest test-solve-10
  (is (= (+ 2 3 5 7) (solve-10 10))))

(deftest test-solve-15
  (is (= 6 (solve-15 2))))

(deftest test-solve-351-slow
  (is (= 30 (solve-351-slow 5)))
  (is (= 138 (solve-351-slow 10)))
  (is (= 1177848 (solve-351-slow 1000))))

(deftest test-solve-351
  (is (= 30 (solve-351 5)))
  (is (= 138 (solve-351 10)))
  (is (= 1177848 (solve-351 1000))))
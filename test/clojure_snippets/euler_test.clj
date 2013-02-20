(ns clojure-snippets.euler-test
  (:use clojure.test
        clojure-snippets.euler))

(deftest test-solve-1
  (is (= 23 (solve-1 10))))

(deftest test-solve-1-sample
  (is (= 23 (solve-1-sample 10))))

(deftest test-solve-2
  (is (= (+ 2 8 34) (solve-2 89))))

(deftest test-solve-15
  (is (= 6 (solve-15 2))))
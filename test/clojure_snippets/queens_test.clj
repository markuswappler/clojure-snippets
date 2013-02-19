(ns clojure-snippets.queens-test
  (:use clojure.test
        clojure-snippets.queens))

(deftest test-conflict?
  (is (conflict? [2 3] [2 5]))
  (is (conflict? [2 3] [5 6]))
  (is (conflict? [2 3] [5 3]))
  (is (conflict? [2 3] [3 2]))
  (is (conflict? [2 3] [0 3]))
  (is (conflict? [2 3] [0 1]))
  (is (conflict? [2 3] [2 2]))
  (is (conflict? [2 3] [4 1]))
  (is (not (conflict? [2 3] [3 5])))
  (is (not (conflict? [2 3] [1 0])))
  (is (not (conflict? [2 3] [5 11])))
  (is (not (conflict? [2 3] [0 0]))))

(deftest test-solve
  (is (= #{[0]} (set (solve 1))))
  (is (= #{} (set (solve 2))))
  (is (= #{} (set (solve 3))))
  (is (= #{[1 3 0 2] [2 0 3 1]} (set (solve 4))))
  (is (= 10 (count (solve 5))))
  (is (= #{[1 3 5 0 2 4] [4 2 0 5 3 1] [2 5 1 4 0 3] [3 0 4 1 5 2]}
         (set (solve 6))))
  (is (= 40 (count (solve 7))))
  (is (= 92 (count (solve 8)))))
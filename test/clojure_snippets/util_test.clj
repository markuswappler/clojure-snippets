(ns clojure-snippets.util-test
  (:use clojure.test
        clojure-snippets.util))

(deftest dbg-test
  (is (= 7 (dbg (+ 3 4))))
  (is (= "dbg: (+ 3 4) -> 7\r\n" 
         (with-out-str (dbg (+ 3 4))))))

(deftest interleave-all-test
  (is (= '(1 4 2 5 3 6)
         (interleave-all [1 2 3] [4 5 6])))
  (is (= '(1 4 2 5 6)
         (interleave-all [1 2] [4 5 6])))
  (is (= '(1 4 2 3)
         (interleave-all [1 2 3] [4])))
  (is (= '(1 2)
         (interleave-all [] [1 2])))
  (is (= '(1)
         (interleave-all [1] [])))
  (is (= '()
         (interleave-all [] []))))

(deftest test-queue
  (let [q0 (queue)
        q1 (conj q0 1 2 3)
        q2 (queue 1 2 3)]
    (is (empty? q0))
    (is (= 1 (peek q1)))
    (is (= 1 (peek q2)))
    (is (= 2 (peek (pop q1))))
    (is (= 2 (peek (pop q2))))
    (is (= 3 (peek (pop (pop q1)))))
    (is (= 3 (peek (pop (pop q2)))))
    (is (empty? (pop (pop (pop q1)))))
    (is (empty? (pop (pop (pop q2)))))))

(deftest test-post-cons
  (is (= '(1 2 3 4) (post-cons 4 [1 2 3])))
  (is (= '(1 2 3 4) (post-cons 4 '(1 2 3)))))
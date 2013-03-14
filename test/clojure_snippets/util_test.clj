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

(deftest test-make-tree-walk
  (testing "binary tree"
           (is (= '([5 6] 6 5 [[1 2] [3 4]] [3 4] 4 3 [1 2] 2 1)
                  ((make-treewalk :depth :pre vector? identity identity identity)
                    [[1 2] [3 4]] [5 6])))
           (is (= '([[1 2] [3 4]] [1 2] 1 2 [3 4] 3 4 [5 6] 5 6)
                  ((make-treewalk :depth :pre vector? identity identity reverse)
                    [5 6] [[1 2] [3 4]])))
           (is (= '(1 2 [1 2] 3 4 [3 4] [[1 2] [3 4]] 5 6 [5 6])
                  ((make-treewalk :depth :post vector? identity identity identity)
                    [[1 2] [3 4]] [5 6])))
           (is (= '([[1 2] [3 4]] [5 6] [1 2] [3 4] 5 6 1 2 3 4)
                  ((make-treewalk :breadth :pre vector? identity identity identity)
                    [[1 2] [3 4]] [5 6])))
           (is (= '(4 3 2 1 6 5 [3 4] [1 2] [5 6] [[1 2] [3 4]])
                  ((make-treewalk :breadth :post vector? identity identity identity)
                    [[1 2] [3 4]] [5 6]))))
  (testing "range"
           (let [walk (make-treewalk :depth :pre (constantly true) 
                                 identity nil (comp vector inc))]
             (is (= (range 1 11) 
                    (take 10 (walk 1))))
             (is (= (range 42 4711) 
                    (take (- 4711 42) (walk 42))))))
  (testing "fibonacci"
           (let [walk (make-treewalk :depth :pre (constantly true)
                                 first nil (fn [[a b]] [[b (+ a b)]]))]
             (is (= '(0 1 1 2 3 5 8 13 21 34 55 89) 
                    (take 12 (walk [0 1]))))
             (is (= '(2 6 8 14 22 36 58 94)
                    (take 8 (walk [2 6])))))))

(deftest test-make-dijkstra
  (testing "unique distances"
           (let [dijkstra (make-dijkstra (fn [k] [(dec k) (inc k) (* 2 k)])
                                         (constantly 1))]
             (is (= [{:node 16 :dist 4}
                     {:node 8 :dist 3}
                     {:node 4 :dist 2}
                     {:node 2 :dist 1}
                     {:node 1 :dist 0}]
                    (dijkstra [1] [16])))
             (is (= [{:node 16 :dist 4}
                     {:node 8 :dist 3}
                     {:node 4 :dist 2}
                     {:node 2 :dist 1}
                     {:node 1 :dist 0}]
                    (dijkstra [1] #(= 16 %))))
             (is (= [{:node 16 :dist 2}
                     {:node 8 :dist 1}
                     {:node 9 :dist 0}]
                    (dijkstra [1 9] [16])))
             (is (= [{:node 16 :dist 2}
                     {:node 8 :dist 1}
                     {:node 9 :dist 0}]
                    (dijkstra [1 9] #(= 16 %))))
             (is (= [{:node 18 :dist 1}
                     {:node 9 :dist 0}]
                    (dijkstra [1 9] [16 18])))
             (is (= [{:node 18 :dist 1}
                     {:node 9 :dist 0}]
                    (dijkstra [1 9] #(or (= 16 %) (= 18 %)))))
             (is (= [{:node 1 :dist 0}]
                    (dijkstra [1 9] [1 16 18])))
             (is (= [{:node 1 :dist 0}]
                    (dijkstra [1 9] #(or (= 1 %) (= 16 %) (= 18 %)))))))
  (testing "variable distances"
           (let [dijkstra (make-dijkstra (fn [k] [(inc k) (+ 3 k)])
                                         (fn [k m]
                                           (let [diff (- m k)]
                                             (if (odd? k)
                                               diff
                                               (* 2 diff)))))]
             (is (= [{:node 4 :dist 5}
                     {:node 1 :dist 2}
                     {:node 0 :dist 0}] 
                    (dijkstra [0] [4])))
             (is (= [{:node 8 :dist 10}
                     {:node 5 :dist 7}
                     {:node 4 :dist 5}
                     {:node 1 :dist 2}
                     {:node 0 :dist 0}] 
                    (dijkstra [0] [8])))))
  (testing "not connected"
           (let [dijkstra (make-dijkstra (fn [k]
                                           (if (> 16 k)
                                             [(* 2 k)]))
                                         (constantly 1))]
             (is (= [{:node 16 :dist 4}
                     {:node 8 :dist 3}
                     {:node 4 :dist 2}
                     {:node 2 :dist 1}
                     {:node 1 :dist 0}]
                    (dijkstra [1] [16])))
             (is (nil? (dijkstra [1] [32]))))))
(ns clojure-snippets.mastermind-test
  (:refer-clojure :exclude [compare])
  (:use clojure.test
        clojure-snippets.mastermind))

(deftest test-compare
  (testing "four equals"
           (is (= {:exact 0 :present 0} 
                  (compare [0 0 0 0] [1 2 3 4])))
           (is (= {:exact 1 :present 0}
                  (compare [0 0 0 0] [0 1 2 3])))
           (is (= {:exact 2 :present 0}
                  (compare [0 0 0 0] [0 1 2 0])))
           (is (= {:exact 3 :present 0}
                  (compare [0 0 0 0] [0 0 1 0])))
           (is (= {:exact 4 :present 0}
                  (compare [0 0 0 0] [0 0 0 0]))))
  (testing "three equals"
           (is (= {:exact 3 :present 0}
                  (compare [0 0 0 1] [0 0 1 1])))
           (is (= {:exact 4 :present 0}
                  (compare [0 0 0 1] [0 0 0 1])))
           (is (= {:exact 2 :present 2}
                  (compare [0 0 0 1] [0 0 1 0]))))
  (testing "two times two equals"
           (is (= {:exact 4 :present 0}
                  (compare [0 0 1 1] [0 0 1 1])))
           (is (= {:exact 2 :present 2}
                  (compare [0 0 1 1] [0 1 0 1])))
           (is (= {:exact 1 :present 1}
                  (compare [0 0 1 1] [0 1 2 3]))))
  (testing "two equals"
           (is (= {:exact 4 :present 0}
                  (compare [1 2 0 0] [1 2 0 0])))
           (is (= {:exact 0 :present 2}
                  (compare [1 2 0 0] [0 0 3 3])))
           (is (= {:exact 0 :present 2}
                  (compare [1 2 0 0] [3 4 1 2])))
           (is (= {:exact 1 :present 1}
                  (compare [1 2 0 0] [3 1 0 4])))
           (is (= {:exact 1 :present 1}
                  (compare [1 2 0 0] [0 2 3 4]))))
  (testing "no equals"
           (is (= {:exact 4 :present 0}
                  (compare [1 2 3 4] [1 2 3 4])))
           (is (= {:exact 2 :present 2}
                  (compare [1 2 3 4] [4 2 3 1])))
           (is (= {:exact 2 :present 1}
                  (compare [1 2 3 4] [0 2 1 4])))
           (is (= {:exact 1 :present 2}
                  (compare [1 2 3 4] [3 2 4 0])))
           (is (= {:exact 0 :present 0}
                  (compare [1 2 3 4] [0 0 5 5])))))

(deftest test-exclude
  (testing "no codes"
           (is (= []
                  (exclude {:guess [0 1 2 3]
                            :result {:exact 4 :present 0}}
                           []))))
  (testing "length 2"
           (let [codes (for [x (range 4) y (range 4)] [x y])
                 =set #(= (set %1) (set %2))]
             (is (=set [[0 0]] 
                       (exclude {:guess [0 0]
                                 :result {:exact 2 :present 0}} 
                                codes)))
             (is (=set [] 
                       (exclude {:guess [0 0]
                                 :result {:exact 1 :present 1}}
                                codes)))
             (is (=set [[0 1] [0 2] [0 3] [1 0] [2 0] [3 0]] 
                       (exclude {:guess [0 0]
                                 :result {:exact 1 :present 0}}
                                codes)))
             (is (=set []
                       (exclude {:guess [0 0]
                                 :result {:exact 0 :present 1}}
                                codes)))
             (is (=set [[1 1] [1 2] [1 3] [2 1] [2 2] [2 3] [3 1] [3 2] [3 3]]
                       (exclude {:guess [0 0]
                                 :result {:exact 0 :present 0}}
                                codes)))
             (is (=set [[0 1]] 
                       (exclude {:guess [0 1]
                                 :result {:exact 2 :present 0}} 
                                codes)))
             (is (=set [] 
                       (exclude {:guess [0 1]
                                 :result {:exact 1 :present 1}}
                                codes)))
             (is (=set [[0 0] [0 2] [0 3] [1 1] [2 1] [3 1]] 
                       (exclude {:guess [0 1]
                                 :result {:exact 1 :present 0}}
                                codes)))
             (is (=set [[2 0] [3 0] [1 2] [1 3]]
                       (exclude {:guess [0 1]
                                 :result {:exact 0 :present 1}}
                                codes)))
             (is (=set [[2 2] [2 3] [3 2] [3 3]]
                       (exclude {:guess [0 1]
                                 :result {:exact 0 :present 0}}
                                codes))))))

(deftest test-five-guess
  (let [run (fn [code]
              (let [history (five-guess (make-query code))]
                [(last history) (dec (count history))]))]
    (let [[solution guesses] (run [:blue :blue :green :yellow])]
      (is (= [:blue :blue :green :yellow] solution)
      (is (>= 5 guesses))))
    (let [[solution guesses] (run [:green :orange :red :yellow])]
      (is (= [:green :orange :red :yellow] solution))
      (is (>= 5 guesses)))
    (let [[solution guesses] (run [:green :green :green :green])]
      (is (= [:green :green :green :green] solution))
      (is (>= 5 guesses)))))

(deftest test-six-guess
  (let [run (fn [code] (six-guess (make-query code)))]
    (is (= [:blue :blue :green :yellow] (run [:blue :blue :green :yellow])))
    (is (= [:green :orange :red :yellow] (run [:green :orange :red :yellow])))
    (is (= [:green :green :green :green] (run [:green :green :green :green])))))
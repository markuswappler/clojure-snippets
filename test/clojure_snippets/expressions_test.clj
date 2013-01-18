(ns clojure-snippets.expressions-test
  (:refer-clojure :exclude [char])
  (:use clojure.test 
        the.parsatron
        clojure-snippets.expressions))

(deftest test-optional
  (let [p (optional (char \a))
        q (>> p (char \b))]
    (is (= \a (run p "a")))
    (is (nil? (run p "x")))
    (is (= \b (run q "ab")))
    (is (= \b (run q "b")))
    (is (thrown? RuntimeException (run q "xb")))))

(deftest test-number
  (is (= {:type :number
          :value "42"}
         (run (number) "42")))
  (is (= {:type :number
          :value "47.11"}
         (run (number) "47.11"))))

(deftest test-operator
  (let [p (operator "+" :plus)]
    (is (= {:type :operator
            :name :plus}
           (run p "+")))
    (is (thrown? RuntimeException (run p "*")))))

(deftest test-operators
  (let [p (operators "+" :plus "*" :mult)
        q (operators "++" :plusplus "+" :plus)
        r (operators "+" :plus "++" :plusplus)]
    (is (= {:type :operator
            :name :plus}
           (run p "+")))
    (is (= {:type :operator
            :name :mult}
           (run p "*")))
    (is (thrown? RuntimeException (run p "-")))
    (is (= {:type :operator
            :name :plus}
           (run q "+42")))            
    (is (= {:type :operator
            :name :plusplus}
           (run q "++")))
    (is (not= {:type :operator
               :name :plusplus}
              (run r "++")))))

(deftest test-whitespace
  (is (= [] (run (whitespace) "")))
  (is (= [] (run (whitespace) "x")))
  (is (= [\space] (run (whitespace) " ")))
  (is (= [\tab] (run (whitespace) "\t")))
  (is (= [\space \space \tab \tab \space \space] (run (whitespace) "  \t\t  x")))
  (is (not= [\newline] (run (whitespace) "\n"))))

(deftest test-in-parens
  (is (= {:type :in-parens
          :content "hello world"}
         (run (in-parens (string "hello world")) "(  hello world  )"))))

(deftest test-expr
  (is (= {:type :expr
          :tokens [{:type :in-parens
                    :content {:type :expr
                              :tokens [{:type :number
                                        :value "1"}
                                       {:type :operator
                                        :name :plus}
                                       {:type :number
                                        :value "2"}]}}
                   {:type :operator
                    :name :mult}
                   {:type :in-parens
                    :content {:type :expr
                              :tokens [{:type :number
                                        :value "3"}
                                       {:type :operator
                                        :name :plus}
                                       {:type :number
                                        :value "4"}
                                       {:type :operator
                                        :name :plus}
                                       {:type :number
                                        :value "5"}]}}]}
         (run (expr) "(1 + 2) * (3 + 4 + 5)"))))

(deftest test-interpret
  (is (= 42.0 (interpret "42")))
  (is (= 7.0 (interpret "3+4")))
  (is (= 12.0 (interpret "3*4")))
  (is (= 10.0 (interpret "2 * 3 + 4")))
  (is (= 14.0 (interpret "2 * (3 + 4)")))
  (is (= 7.5 (interpret "2 * 2.5 + 5 / 2")))
  (is (= 65536.0 (interpret "2 ** 2 ** 2 ** 2")))
  (is (= 256.0 (interpret "((2 ** 2) ** 2) ** 2")))
  (is (= 3.0 (interpret "(4-2) * (4+2) / 2**2"))))
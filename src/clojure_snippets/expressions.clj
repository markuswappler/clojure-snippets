(ns clojure-snippets.expressions
  (:refer-clojure :exclude [char])
  (:use the.parsatron))

(defparser optional [p]
  (either 
    (attempt p)
    (always nil)))          

(defparser number []
  (let->> [int (many1 (digit))
           frac (optional
                  (let->> [_ (char \.) 
                           ds (many1 (digit))]
                    (always (str \. (apply str ds)))))]
    (always {:type :number
             :value (str (apply str int) frac)})))

(defparser operator [symb name]
  (>> 
    (attempt (string symb))
    (always {:type :operator
             :name name})))

(defparser operators [& op-args]
  (->> op-args
    (partition 2)
    (map (partial apply operator))
    (apply choice)))

(defparser whitespace []
  (many (choice (char \space) (char \tab))))

(defparser in-parens [content-parser]
  (let->> [_ (char \()
           _ (whitespace)
           content content-parser
           _ (whitespace)
           _ (char \))]
    (always {:type :in-parens
             :content content})))

(defparser expr []
  (let->> [left (choice 
                  (number) 
                  (in-parens (expr)))           
           cont (optional
                  (let->> [_ (whitespace)
                           op (operators "+" :plus
                                         "-" :minus
                                         "**" :power
                                         "*" :mult
                                         "/" :div)
                           _ (whitespace)
                           right (expr)]
                    (always (concat [op] (right :tokens)))))]
    (always {:type :expr
             :tokens (concat [left] cont)})))

(defmulti interpret :type)

(defmethod interpret :default [arg]
  (if (string? arg)
    (interpret (run (expr) arg))))

(defmethod interpret :number [{:keys [value]}]
  (Double/parseDouble value))

(defmethod interpret :in-parens [{:keys [content]}]
  (interpret content))

(defmethod interpret :expr [{:keys [tokens]}]
  (let [ops [[:plus + :left]
             [:minus - :left]
             [:mult * :left]
             [:div / :left]
             [:power (fn [bas exp] (Math/pow bas exp)) :right]]
        expr-ops (->> tokens
                   (filter #(= :operator (% :type)))
                   (map :name))
        in-expr? (fn [op] (some #{(first op)} expr-ops))
        prec-op (->> ops
                  (filter in-expr?)
                  (first))]
    (if (nil? prec-op)
      (interpret (first tokens))
      (let [[op-name op-exec op-assoc] prec-op
            order (if (= :left op-assoc)
                    reverse
                    identity)
            [x-toks y-toks] (->> tokens
                              (order)         
                              (split-with (complement #{{:type :operator
                                                         :name op-name}})))
            operands (order [{:type :expr
                              :tokens (order x-toks)}
                             {:type :expr
                              :tokens (order (rest y-toks))}])]
        (op-exec 
          (interpret (first operands))
          (interpret (second operands)))))))
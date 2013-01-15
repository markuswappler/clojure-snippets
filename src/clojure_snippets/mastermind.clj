;; Implementation of the Mastermind game
;; http://en.wikipedia.org/wiki/Mastermind_(board_game)
;; http://mathworld.wolfram.com/Mastermind.html

(ns clojure-snippets.mastermind
  (:refer-clojure :exclude [compare])
  (:use [clojure.math.combinatorics :only (selections)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions 
;; independent of code length and number of colors

(defn compare [code guess]
  (let [exact (->> (interleave code guess)
                (partition 2)
                (filter (partial apply =))
                (count))
        matches (->> (for [cf (frequencies code)
                           gf (frequencies guess)
                           :when (= (key cf) (key gf))]
                       (min (val cf) (val gf)))
                  (reduce +))]
    {:exact exact
     :present (- matches exact)}))

(defn exclude [{:keys [guess result]} codes]
  (filter #(= (compare % guess) result) codes))

(defn minimax [codes]
  (let [guess-scores (for [guess codes]
                       [guess (->> codes
                                (group-by #(compare % guess))
                                (map val)
                                (map count)
                                (apply max))])
        best-guess (->> guess-scores
                     (apply min-key second)
                     (first))]
    best-guess))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; concrete implementation with 6 colors and code length of 4
;; create a new puzzle:
;; (def query (make-query)) random code
;; (def query (make-query [:blue :blue :cyan :green])) given code for testing
;; evaluate a guess:
;; (query [:orange :red :yellow :yellow]) how good does the guess match? 
;; (query) tells the hidden code
;; solve the puzzle:
;; (five-guess query) at most 5 calls of query, by Donald Knuth
;; (six-guess query) 6 static calls of query, by Don Greenwell

(def colors #{:blue :cyan :green :orange :red :yellow})

(defn make-query
  ([]
    (let [choose-color (partial rand-nth (vec colors))
          code (take 4 (repeatedly choose-color))]
      (make-query code)))
  ([code]
    (fn
      ([] code)
      ([guess] (compare code guess)))))

(defn five-guess [query]
  (loop [guess [:blue :blue :cyan :cyan]
         codes (selections colors 4)         
         history []]
    (let [result {:guess guess :result (query guess)}                        
          codes (exclude result codes)
          history (conj history result)]
      (if (= 1 (count codes))
        (conj history (first codes))
        (recur (minimax codes) codes history)))))

(defn six-guess [query]
  (loop [results (map (fn [guess] {:guess guess :result (query guess)})
                      [[:blue :cyan :cyan :blue]
                       [:cyan :green :red :orange]
                       [:green :green :blue :blue]
                       [:orange :red :cyan :orange]
                       [:red :yellow :red :yellow]
                       [:yellow :yellow :orange :green]])
         codes (selections colors 4)]  
    (if (empty? results)
      (first codes)
      (recur (rest results) (exclude (first results) codes)))))
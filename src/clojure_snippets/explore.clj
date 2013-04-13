(ns clojure-snippets.explore)

;; effect of range being a chunked sequence
(def chunks
  (map #(count (take-while (partial >= %) chunks)) 
       (range)))

(def unchunks
  (map #(count (take-while (partial >= %) unchunks)) 
       (reductions (fn [_ x] x) (range))))

(defn thread-cond [ops x]
  (cond-> x
          (some #{:inc} ops) (inc)
          (some #{:dbl} ops) (* 2)))

(defn thread-as [x]
  (as-> x name
        (+ name 3)
        (+ 4 name)))

(defn thread-some [x]
  (let [half (fn [x] (when (even? x) (/ x 2)))]
    (some-> x
            (half)
            (half)
            (half))))
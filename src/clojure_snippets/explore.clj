(ns clojure-snippets.explore)

;; effect of range being a chunked sequence
(def chunks
  (map #(count (take-while (partial >= %) chunks)) 
       (range)))

(def unchunks
  (map #(count (take-while (partial >= %) unchunks)) 
       (reductions (fn [_ x] x) (range))))
(ns clojure-snippets.queens)

(defn conflict? [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (or 
      (zero? dx) 
      (zero? dy) 
      (= dx dy) 
      (= dx (- dy)))))

(defn solve [n]
  (let [next-queens-1 (fn [ys]
                        (let [qs (map-indexed #(vector %1 %2) ys)
                              x (count ys)]
                          (for [y (range n)
                                :when (not-any? (partial conflict? [x y]) qs)]
                            (conj ys y))))
        next-queens (fn [yss] (mapcat next-queens-1 yss))]
    (nth (iterate next-queens [[]]) n)))        
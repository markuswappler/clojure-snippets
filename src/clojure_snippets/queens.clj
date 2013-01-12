(ns clojure-snippets.queens)

(defn conflict? [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (or 
      (zero? dx) 
      (zero? dy) 
      (= dx dy) 
      (= dx (- dy)))))

(defn add-queen [n ys]
  (let [qs (map-indexed #(vector %1 %2) ys)
        x (count ys)
        possible (for [y (range n)
                       :when (not-any? (partial conflict? [x y]) qs)]
                   y)]
    (map (partial conj ys) possible)))

(defn solve [n]
  (let [solve-one-more (fn [qs] (mapcat (partial add-queen n) qs))]
    (nth (iterate solve-one-more [[]]) n)))
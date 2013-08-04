(ns clojure-snippets.date)

(defn before? [[xd xm xy] [yd ym yy]]
  (or (< xy yy)
      (and (= xy yy)
           (or (< xm ym)
               (and (= xm ym)
                    (< xd yd))))))

(defn leap-year? [y]
  (and (zero? (mod y 4))
       (or (not (zero? (mod y 100)))
           (zero? (mod y 400)))))

(defn leap-days [[xd xm xy :as x] [yd ym yy :as y]]
  (let [y0 (if (before? x [1 3 xy])
             xy
             (inc xy))
        y1 (if (before? [29 2 yy] y)
             (inc yy)
             yy)]
    (->> (range y0 y1)
      (filter leap-year?)
      count)))

(def month-lengths [31 28 31 30 31 30 31 31 30 31 30 31])

(defn days [[xd xm xy :as x] [yd ym yy :as y]]
  (cond
    (before? y x) (- (days y x))
    (< xy yy) (+ (days x [31 12 xy])
                 1                 
                 (days [1 1 (inc xy)] y))
    :else (let [dd (- yd xd)
                dm (->> (range xm ym)
                     (map dec)
                     (map month-lengths)
                     (reduce +))]
            (+ dd dm (leap-days x y)))))

(def day-names [:monday :tuesday :wednesday :thursday :friday :saturday :sunday])

(defn weekday [date]
  (day-names (mod (days [1 1 1900] date) 7)))
(ns clojure-snippets.date)

(defn days
  ([[d m y]]
    (let [month-lengths [31 28 31 30 31 30 31 31 30 31 30 31]
          yleap (if (> 3 m) (dec y) y)
          leap-years (+ (quot yleap 4)
                        (- (quot yleap 100))
                        (quot yleap 400))
          from-year (+ (* 365 (dec y)) leap-years)
          from-month (reduce + (take (dec m) month-lengths))
          from-day (dec d)]
      (+ from-year from-month from-day)))
  ([x y]
    (- (days y) (days x))))

(defn weekday [date]
  ([:monday :tuesday :wednesday :thursday :friday :saturday :sunday] 
    (mod (days [1 1 1900] date) 7)))
(ns clojure-snippets.date-test
  (:use clojure.test
        clojure-snippets.date))

(deftest test-before?
  (is (before? [15 7 1985] [10 5 1987]))
  (is (not (before? [5 3 1985] [10 5 1983])))
  (is (before? [15 3 1985] [10 5 1985]))
  (is (not (before? [5 7 1985] [10 5 1985])))
  (is (before? [5 5 1985] [10 5 1985]))
  (is (not (before? [15 5 1985] [10 5 1985])))
  (is (not (before? [10 5 1985] [10 5 1985]))))

(deftest test-leap-year?
  (is (not (leap-year? 2007)))
  (is (leap-year? 2008))
  (is (not (leap-year? 2009)))
  (is (not (leap-year? 2010)))
  (is (not (leap-year? 2011)))
  (is (leap-year? 2012))
  (is (not (leap-year? 2013)))
  (is (not (leap-year? 1800)))
  (is (not (leap-year? 1900)))
  (is (leap-year? 2000))
  (is (not (leap-year? 2100))))

(deftest test-leap-days
  (is (= 1 (leap-days [1 3 1599] [5 3 1601])))
  (is (= 0 (leap-days [1 3 1699] [5 3 1701])))
  (is (= 2 (leap-days [28 2 1988] [1 3 1992])))
  (is (= 2 (leap-days [29 2 1988] [1 3 1992])))
  (is (= 1 (leap-days [1 3 1988] [1 3 1992])))
  (is (= 0 (leap-days [1 3 1988] [29 2 1992]))))

(deftest test-days
  (is (= 0 (days [24 1 1977] [24 1 1977])))
  (is (= 7 (days [24 1 1977] [31 1 1977])))
  (is (= -7 (days [31 1 1977] [24 1 1977])))
  (is (= 2 (days [28 2 1992] [1 3 1992])))
  (is (= 1 (days [28 2 1993] [1 3 1993])))
  (is (= 367 (days [28 2 1992] [1 3 1993])))
  (is (= 1 (days [31 12 1899] [1 1 1900])))
  (is (= -1 (days [1 1 1900] [31 12 1899])))
  (is (= 334 (days [10 2 2013] [10 1 2014])))
  (is (= 337 (days [10 3 2013] [10 2 2014])))
  (is (= 335 (days [10 5 2013] [10 4 2014]))))

(deftest test-weekday
  (is (= :monday (weekday [1 1 1900])))
  (is (= :sunday (weekday [31 12 1899])))
  (is (= :saturday (weekday [30 12 1899])))
  (is (= :tuesday (weekday [2 1 1900])))
  (is (= :wednesday (weekday [3 1 1900])))
  (is (= :thursday (weekday [4 1 1900])))
  (is (= :friday (weekday [5 1 1900])))
  (is (= :sunday (weekday [4 8 2013])))
  (is (= :monday (weekday [24 1 1977]))))
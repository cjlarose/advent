(ns advent.day2
  (:require [clojure.string :refer [split]]))

(defn day2-parse [input]
  (let [parse-d (fn [d] (->> (split d #"x")
                             (mapv #(. Integer parseInt %))))]
    (->> (split input #"\n")
         (map parse-d))))

(defn day2-wrapping-paper [input]
  (let [surface-area (fn [[l w h]]
                       (* 2 (+ (* l w) (* w h) (* h l))))
        area-smallest-face (fn [[l w h]]
                             (min (* l w) (* w h) (* h l)))
        wrapping-paper (fn [ds]
                         (+ (surface-area ds) (area-smallest-face ds)))]
    (->> input
         (day2-parse)
         (map wrapping-paper)
         (reduce + 0))))

(defn day2-ribbon [input]
  (let [p (fn [[w h]] (+ (* 2 w) (* 2 h)))
        shortest-perimeter (fn [[l w h]]
                             (min (p [l w]) (p [w h]) (p [h l])))
        volume (fn [[l w h]]
                 (* l w h))
        ribbon (fn [ds]
                 (+ (shortest-perimeter ds) (volume ds)))]
    (->> input
         (day2-parse)
         (map ribbon)
         (reduce + 0))))

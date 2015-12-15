(ns advent.core
  (:require [clojure.string :refer [split]])
  (:gen-class))

(defn day1-floor-number [parens]
  (let [to-i #(if (= % \() 1 -1)]
    (reduce + 0 (map to-i parens))))

(defn day1-basement-position [parens]
  (let [nums (map #(if (= % \() 1 -1) parens)]
    (loop [position 0
           floor 0
           curr (first nums)
           remaining (rest nums)]
      (if (< floor 0)
        position
        (recur (inc position)
               (+ floor curr)
               (first remaining)
               (rest remaining))))))

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

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

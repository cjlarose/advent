(ns advent.day6
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [union difference]]))

(defn parse-command [s]
  (let [read-pair (fn [x] (mapv #(. Integer parseInt %) (split x #",")))
        read-parts (fn [[_ c start end]] [c (read-pair start) (read-pair end)])]
    (->> s
         (re-matches #"(.*) ([\d,]+) through ([\d,]+)")
         (read-parts))))

(defn parse [input]
  (->> (split input #"\n")
       (map parse-command)))

(defn houses-in-block [[start-i start-j] [end-i end-j]]
  (set (for [i (range start-i (inc end-i))
             j (range start-j (inc end-j))]
         [i j])))

(defn flip-lights [lit-houses [instruction start end]]
  (let [affected-houses (houses-in-block start end)
        [turn-on turn-off] (case instruction
                             "turn on" [affected-houses #{}]
                             "turn off" [#{} affected-houses]
                             "toggle" [(set (filter (complement lit-houses) affected-houses))
                                       (set (filter lit-houses affected-houses))])]
    (-> lit-houses
        (difference turn-off)
        (union turn-on))))

(defn count-lit-homes [input]
  (->> input
       (parse)
       (reduce flip-lights #{})
       (count)))

(defn change-brightness [brightnesses [instruction start end]]
  (let [affected-houses (houses-in-block start end)
        change-f (case instruction
                   "turn on" inc
                   "turn off" #(max 0 (dec %))
                   "toggle" (partial + 2))
        handle-home (fn [state home]
                      (let [v (or (state home) 0)]
                        (assoc state home (change-f v))))]
    (->> (houses-in-block start end)
         (reduce handle-home brightnesses))))

(defn total-brightness [input]
  (->> input
       (parse)
       (reduce change-brightness {})
       (vals)
       (reduce + 0)))

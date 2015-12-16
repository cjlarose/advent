(ns advent.day6
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [union difference]]))

(defn parse-command [s]
  (let [read-pair (fn [x] (mapv #(. Integer parseInt %) (split x #",")))
        read-parts (fn [[_ c start end]] [c (read-pair start) (read-pair end)])]
    (->> s
         (re-matches #"(.*) ([\d,]+) through ([\d,]+)")
         (read-parts))))

(defn houses-in-block [[start-i start-j] [end-i end-j]]
  (set (for [i (range start-i (inc end-i))
             j (range start-j (inc end-j))]
         [i j])))

(defn handle-command [lit-houses [instruction start end]]
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
  (->> (split input #"\n")
       (map parse-command)
       (reduce handle-command #{})
       (count)))

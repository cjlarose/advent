(ns advent.day3
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [union]]))

(defn parse [input]
  (->> (split input #"\n")
       (first)
       (map {\^ [-1 0]
             \v [1 0]
             \> [0 1]
             \< [0 -1]})))

(defn houses-visited [dirs]
  (loop [current-position [0 0]
         visited #{[0 0]}
         directions dirs]
    (if-let [dir (first directions)]
      (let [new-pos (mapv + current-position dir)]
        (recur new-pos
               (conj visited new-pos)
               (rest directions)))
      visited)))

(defn at-least-one-present [input]
  (-> (parse input)
      (houses-visited)
      (count)))

(defn robo-santa [input]
  (let [dirs (parse input)
        santa-dirs (take-nth 2 dirs)
        robo-dirs (take-nth 2 (rest dirs))]
    (->> (houses-visited santa-dirs)
         (union (houses-visited robo-dirs))
         (count))))

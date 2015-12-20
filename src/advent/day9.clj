(ns advent.day9
  (:require [instaparse.core :as insta]
            [clojure.string :refer [split]]))

(def input-parser
  (insta/parser
    "S = (leg <'\n'>)*
     leg = city <' to '> city <' = '> distance
     <city> = #'[A-Za-z]+'
     <distance> = #'[0-9]+'"))

(def adjacency-list
  (let [to-int #(. Integer parseInt %)
        f (fn [l [_ src dest distance-str]]
                      (assoc-in l [src dest] (to-int distance-str)))]
    (partial reduce f {})))

(defn permutations [xs]
  (if (<= (count xs) 1)
    xs
    (map (partial conj (first xs)) (permuatations (rest xs)))))

(defn possible-routes [legs]
  (let [cities (reduce (fn [cx [_ src dest _]]
                         (conj cx src dest))
                       #{} legs)]
    cities))

(ns advent.day5
  (:require [clojure.string :refer [split replace-first]]))

(defn has-at-least-three-vowels? [s]
  (->> s
       (filter #{\a \e \i \o \u})
       (count)
       (<= 3)))

(defn contains-repeat-char? [s]
  (loop [remaining s
         last-char nil]
    (if-let [c (first remaining)]
      (if (= c last-char)
        true
        (recur (rest remaining) c)))))

(defn contains-forbidden-strings? [s]
  (let [forbidden #{"ab" "cd" "pq" "xy"}]
    (some #(.contains s %) forbidden)))

(defn is-nice? [s]
  (and (has-at-least-three-vowels? s)
       (contains-repeat-char? s)
       (not (contains-forbidden-strings? s))))

(defn count-nice [f input]
  (->> (split input #"\n")
       (filter f)
       (count)))

(defn contains-double-pair? [s]
  (let [pairs (->> (partition 2 1 s)
                   (map (partial apply str)))
        appears-twice? (fn [pair]
                         (let [idx (.indexOf s pair)
                               s1 (subs s 0 idx)
                               s2 (subs s (+ idx 2))]
                           (or (.contains s1 pair) (.contains s2 pair))))]
    (some appears-twice? pairs)))

(defn contains-interrupted-pair? [s]
  (let [triples (->> (partition 3 1 s))
        is-pair? (fn [[a _ c]]
                   (= a c))]
    (some is-pair? triples)))

(defn is-nice-with-new-rules? [s]
  (and (contains-double-pair? s) (contains-interrupted-pair? s)))

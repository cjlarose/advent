(ns advent.day1)

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

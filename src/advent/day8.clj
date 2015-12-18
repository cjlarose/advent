(ns advent.day8
  (:require [instaparse.core :as insta]
            [clojure.string :refer [split]]))

(def input-parser
  (insta/parser
    "S = (string-literal <'\n'>)*
     string-literal = <'\"'> character* <'\"'>
     character = literal-character | escape-sequence | hex-character-literal
     literal-character = #'[a-z]'
     escape-sequence = <'\\\\'> ('\\\\' | '\"')
     hex-character-literal = <'\\\\x'> hex-character hex-character
     <hex-character> = #'[0-9a-f]'"))

(defn code-size [input]
  (->> (split input #"\n")
       (map count)
       (reduce + 0)))

(defn read-char [[_ char-ast]]
  (let [->dec #(. Integer parseInt % 16)
        [tag & char-desc] char-ast]
    (if (#{:literal-character :escape-sequence} tag)
      (first (first char-desc))
      (->> char-desc
           (apply str)
           (->dec)
           (char)))))

(defn read-str [string-literal-ast]
  (let [char-asts (rest string-literal-ast)]
    (apply str (map read-char char-asts))))

(defn parse-strings [input]
  (->> input
       (input-parser)
       (rest)
       (map read-str)))

(defn encoding-overhead [input]
  (- (code-size input) (apply + (map count (parse-strings input)))))

(defn encode-char [c]
  (condp = c
    \" "\\\""
    \\ "\\\\"
    c))

(defn encode-string [s]
  (apply str "\"" (apply str (map encode-char s)) "\""))

(defn re-encode-strings [input]
  (->> (split input #"\n")
       (map encode-string)))

(defn re-encoding-overhead [input]
  (let [encoded-strings (re-encode-strings input)]
    (- (apply + (map count encoded-strings)) (code-size input))))

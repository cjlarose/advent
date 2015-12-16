(ns advent.day7
  (:require [instaparse.core :as insta]
            [clojure.core.async :as async :refer [chan <! >! go <!! mult tap]]))

(def input-parser
  (insta/parser
    "S = (instruction <newline>)*
     instruction = (integer | binary-op | shift-op | unary-op | wire) <whitespace> <'->'> <whitespace> wire
     binary-op = (wire | integer) <whitespace> 'AND' <whitespace> (wire | integer)
               | (wire | integer) <whitespace> 'OR' <whitespace> (wire | integer)
     shift-op = wire <whitespace> 'LSHIFT' <whitespace> integer
              | wire <whitespace> 'RSHIFT' <whitespace> integer
     unary-op = 'NOT' <whitespace> wire
     newline = '\n'
     whitespace = #'\\s+'
     integer = #'[0-9]+'
     wire = #'[a-z]+'"))

(defn bin-op-chan [op a-chan b-chan result-chan]
  (go (let [merge-chan (async/merge [a-chan b-chan])
            a (<! merge-chan)
            b (<! merge-chan)]
        (>! result-chan (op a b)))))

(defn unary-chan [f input-chan result-chan]
  (go (->> (<! input-chan)
           (f)
           (>! result-chan))))

(defn handle-instruction [result-chans mults instruction]
  (let [[_ lhs [_ result-wire]] instruction
        lhs-type (first lhs)
        result-chan (result-chans result-wire)]
    (case lhs-type
      :wire     (let [[_ input-wire] lhs
                      input-chan (chan)]
                  (tap (mults input-wire) input-chan)
                  (unary-chan identity input-chan result-chan))
      :unary-op (let [[_ _ [_ input-wire]] lhs
                      input-chan (chan)]
                  (tap (mults input-wire) input-chan)
                  (unary-chan bit-not input-chan result-chan))
      :shift-op (let [[_ [_ input-wire] op-type [_ shift-amount]] lhs
                      input-chan (chan)
                      shift-fn (if (= op-type "LSHIFT")
                                 bit-shift-left
                                 unsigned-bit-shift-right)
                      shift-amount-int (. Integer parseInt shift-amount)
                      op #(shift-fn % shift-amount-int)]
                  (tap (mults input-wire) input-chan)
                  (unary-chan op input-chan result-chan))
      :binary-op (let [[_ op1 op-type op2] lhs
                       to-chan (fn [[tag v]] (let [c (chan)]
                                               (if (= tag :wire)
                                                 (tap (mults v) c)
                                                 (go (>! c (. Integer parseInt v))))
                                               c))
                       a-chan (to-chan op1)
                       b-chan (to-chan op2)
                       op (if (= op-type "AND") bit-and bit-or)]
                     (bin-op-chan op a-chan b-chan result-chan)))))

(defn value-of-wire-a [input]
  (let [instructions (rest (input-parser input))
        result-chans (into {} (map (fn [[_ _ [_ result-wire]]] [result-wire (chan)])
                                   instructions))
        mults (into {} (map (fn [[k c]] [k (mult c)]) result-chans))
        seed-instruction? (fn [[_ [tag _] _]] (= tag :integer))]
    (doseq [instruction (remove seed-instruction? instructions)]
      (handle-instruction result-chans mults instruction))
    (doseq [[_ [_ v] [_ wire]] (filter seed-instruction? instructions)]
      (go (>! (result-chans wire) (. Integer parseInt v))))
    (let [c (chan)]
      (tap (mults "a") c)
      (<!! c))))

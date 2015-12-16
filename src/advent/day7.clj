(ns advent.day7
  (:require [instaparse.core :as insta]))

(def input-parser
  (insta/parser
    "S = (instruction <newline>)*
     instruction = (integer | binary-op | unary-op | wire) <whitespace> <'->'> <whitespace> wire
     operand  = wire | integer
     binary-op = operand <whitespace> 'AND' <whitespace> operand
               | operand <whitespace> 'OR' <whitespace> operand
               | wire <whitespace> 'LSHIFT' <whitespace> integer
               | wire <whitespace> 'RSHIFT' <whitespace> integer
     unary-op = 'NOT' <whitespace> wire
     newline = '\n'
     whitespace = #'\\s+'
     integer = #'[0-9]+'
     wire = #'[a-z]+'"))

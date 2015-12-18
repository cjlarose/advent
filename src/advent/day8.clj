(ns advent.day8
  (:require [instaparse.core :as insta]))

(def input-parser
  (insta/parser
    "S = (string-literal <'\n'>)*
     string-literal = <'\"'> character* <'\"'>
     character = literal-character | escape-sequence 
     literal-character = #'[a-z]'
     escape-sequence = <'\\\\'> (escaped-character | hex-character-literal)
     escaped-character = '\\\\' | '\"'
     hex-character-literal = <'x'> hex-character hex-character
     hex-character = #'[0-9a-f]'"))

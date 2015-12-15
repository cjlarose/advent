(ns advent.day4)

(defn md5 [token]
  (let [hash-bytes (doto (java.security.MessageDigest/getInstance "MD5")
                     (.reset)
                     (.update (.getBytes token)))
        digest (.digest hash-bytes)
        bi (java.math.BigInteger. 1 digest)]
    (format (str "%0" (* 2 (count digest)) "X") bi)))

(defn find-hash [leading-zeros k]
  (loop [i 1]
    (let [input (str k i)
          h     (md5 input)]
      (if (every? #(= % \0) (take leading-zeros h))
        i
        (recur (inc i))))))

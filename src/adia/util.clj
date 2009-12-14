(ns adia.util
  (:use [clojure.contrib.duck-streams :only (slurp*)]))

(defn even-items [lst]
  (loop [evens []
         r     lst]
    (if (< (count r) 2)
      evens
      (recur (conj evens (first r)) (rest (rest r))))))

(defn odd-items [lst]
  (loop [odds []
         r     lst]
    (if (< (count r) 2)
      odds
      (recur (conj odds (second r)) (rest (rest r))))))

(defn- silent-read
  [s]
  (try
    (let [r (-> s java.io.StringReader. java.io.PushbackReader.)]
      [(read r) (slurp* r)])
    (catch Exception e))) ; this indicates an invalid form -- s is just string data

(defn- interpolate
  ([s atom?]
   (lazy-seq
     (if-let [[form rest] (silent-read (subs s (if atom? 2 1)))]
       (cons form (interpolate (if atom? (subs rest 1) rest)))
       (cons (subs s 0 2) (interpolate (subs s 2))))))
  ([#^String s]
   (let [start (max (.indexOf s "~{") (.indexOf s "~("))]
     (if (== start -1)
       [s]
       (lazy-seq (cons
                   (subs s 0 start)
                   (interpolate (subs s start) (= \{ (.charAt s (inc start))))))))))

(defmacro <<
  [string]
  `(str ~@(interpolate string)))

(defn md5 [str]
  (let [md (java.security.MessageDigest/getInstance "MD5")]
    (.update md (.getBytes str))
    (.toString (BigInteger. 1 (.digest md)) 16)))

(ns adia.util
  (:use [clojure.contrib.duck-streams :only (slurp*)]))

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

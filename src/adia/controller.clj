(ns adia.controller
  (:use compojure)
  (:use [adia.model :as model]))

(def *uri-mapping* (ref {}))
(def *uri-list* (ref []))

(def *request* nil)
(def *form* nil)

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

(defn convert-type [val type]
  (cond
    (= type :string) val
    (= type :int) (Integer/parseInt val)
    (keyword? type) (model/retrieve type val)
    :else (throw (RuntimeException. (str "Could not convert to type: " type)))))

(defn input-string 
  ([attrs name] [:input (assoc attrs :name name
                                      :value (*form* name))])
  ([name] (input-string {} name)))

(defmacro defact
  ([name doc-str attrs args body]
   (let [controller-parts (.split (str (ns-name *ns*)) "\\.")
         controller-name  (aget controller-parts (- (alength controller-parts) 1))
         action-name      (if (= name 'index)
                            ""
                            (str name))
         uri (if (contains? attrs :uri)
               (:uri attrs)
               (if (= controller-name "index")
                 action-name
                 (str controller-name (if (empty? action-name)
                                        ""
                                        (str "/" action-name)))))]
     `(do
        (def ~name {:name      (str (quote ~name))
                    :arg-names (quote ~(even-items args))
                    :arg-types (quote ~(odd-items args))
                    :doc       ~doc-str
                    :attrs     ~attrs
                    :fn        (fn [~@(even-items args)] ~body) })
        (dosync 
          (ref-set *uri-list*  (sort (fn [e1# e2#] (> (.length e1#) (.length e2#))) (conj @*uri-list* ~uri)))
          (commute *uri-mapping* assoc ~uri ~name)))))
  ([name doc-or-attrs args body] 
    (if (string? doc-or-attrs) ; doc
     `(defact ~name ~doc-or-attrs {} ~args ~body)
     `(defact ~name nil ~doc-or-attrs ~args ~body)))
  ([name args body] `(defact ~name nil {} ~args ~body)))

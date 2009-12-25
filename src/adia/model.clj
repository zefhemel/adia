(ns adia.model
  (:use adia.util)
  (:require [somnium.congomongo :as mongo])
  (:gen-class))

(def *db-entites* (ref {}))

(defn- mongo->adia
  [kind ent]
  (if ent
    (assoc (dissoc ent :_ns) :_kind kind)))

(defn- adia->mongo
  [kind ent]
  (if ent
    (assoc (dissoc ent :_kind) :_ns (kind :tblname))))

(defn sync-database-metadata 
  "Nothing for now"
  [name]
  true)

(defn persist!
  "Persists a given entity to the database"
  [ent]
  (let [kind       (:_kind ent)
        clean-ent  (adia->mongo kind ent)]
    (mongo/save! (kind :tblname) clean-ent)))

(defn delete!
  [ent]
  (mongo/destroy! ((:_kind ent) :tblname) {:_id (:_id ent)}))

(defn update-inc!
  "Add one to counter"
  [ent field n]
  (mongo/update! ((:_kind ent) :tblname) (adia->mongo (:_kind ent) ent) {:$inc {field n}})
  (assoc ent field (+ (if (field ent)
                        (field ent)
                        0) n)))

(defn query [kind & args]
  (map mongo->adia (cycle [kind]) (apply mongo/fetch (kind :tblname) args)))

(defn query-one [kind & args]
  (let [result (apply query kind args)]
    (if-not (empty? result)
      (first result)
      nil)))

(defn retrieve
  "Retrieves an entity from the database"
  [kind id]
  (query-one kind :where {:_id id}))

(defn retrieve-all
  "Retrieves an entity from the database"
  [kind]
  (query kind))

(def *db-config* (ref {}))

(defn set-db-config! [& config]
  (dosync
    (ref-set *db-config* config)))

(defn connect-db []
  (apply mongo/mongo! @*db-config*))

(defn- bind-property [ent value spec]
  (let [column-type (second spec)]
    (condp = column-type
      :string  (if (= (count spec) 3)
                 (let [attrs (nth spec 2)]
                   (if (and (:length attrs)
                            (> (count value) (:length attrs)))
                     (throw (RuntimeException. (str "Value '" value "' is too, long, maximum length: " (:length attrs)))))
                   (if (:unique attrs)
                     (if-let [duplicate (query-one (:_kind ent) :where {(first spec) value})]
                       (if-not (= (:_id ent) (:_id duplicate))
                         (throw (RuntimeException. (str "Value '" value "' is not unique."))))))
                   value)
                 value)
      :int      (try
                  (Integer/parseInt value)
                  (catch NumberFormatException nfe (RuntimeException. (str "Value '" value "' is not a number."))))
      :password (md5 value)
      :text      value
      :email    (if (re-matches #".+@.+\.[a-z]+" value)
                  value
                  (throw (RuntimeException. (str "'" value "' is not a valid e-mail address."))))
      ; entity type
      value ; for now, no checking
      )))

(defn- lookup-property [kind property-name]
  (let [properties (filter #(= (first %1) property-name) (kind :properties))]
    (if (empty? properties)
      nil
      (first properties))))

(defn databind 
  ([ent values-map selected-properties] 
   (apply assoc ent 
          (mapcat (fn [k] [k (bind-property ent (values-map k) (lookup-property (:_kind ent) k))])
                  selected-properties)))
  ([ent values-map] (databind ent values-map (keys values-map))))

(defmacro defent [name & properties]
  `(do
     (defn ~name
       ([] {:_kind ~name})
       ([key#] (if (string? key#)
                 (retrieve ~name (mongo/object-id key#))
                 ({:tblname    (str (quote ~name))
                   :properties [~@properties]} key#)))
       ([k# v# & kvs# ] (apply assoc {:_kind ~name} k# v# kvs#)))
     (dosync 
       (commute *db-entites* assoc (quote ~name) ~name))
     (sync-database-metadata (quote ~name))))

(ns adia.model
  (:use [clojure.contrib.sql :as sql])
  (:gen-class))

(def *db-config* (ref {}))

(def fk-type "varchar(64)")

(defn- query [query]
  (with-query-results 
    rs query
    (doall rs)))

(defn- kind->table
  [kind]
  (.substring (str kind) 1))

(defn- keyword->str
  [kw]
  (.substring (str kw) 1))

(defn create-entity-type
  [kind & columns]
  (apply sql/create-table
         kind
         [:id "varchar(64)" "PRIMARY KEY"]
         columns))

(defn entity 
  "Create a new entity instance of the supplied kind"
  [kind & kvs]
  (with-meta
    (apply assoc {:kind kind
                  :id (str (java.util.UUID/randomUUID))}
           kvs)
    {:persisted false}))

(defn persist
  "Persists a given entity to the database"
  [ent]
  (let [kind       (:kind ent)
        clean-ent  (dissoc ent :kind)
        persisted  (:persisted ^ent)]
    (if persisted
      (sql/update-values
        kind
        ["id = ?" (:id ent)]
        (dissoc clean-ent :id))
      (sql/insert-values
        kind (keys clean-ent) (vals clean-ent))))
  (println "Persisted it!")
  (with-meta ent {:persisted true}))

(defn retrieve
  "Retrieves an entity from the database"
  [kind id]
  (if-let [rs (query [(str "select * from " (kind->table kind) " where id = ?") id])]
    (first rs)
    nil))

(defn find-by [kind prop value]
  (if-let [rs (query [(str "select * from " (kind->table kind) " where `" prop "` = ?") value])]
    (first rs)
    nil))

(defn find-all-by [kind prop value]
  (query [(str "select * from " (kind->table kind) " where `" (keyword->str prop) "` = ?") value]))

(defn retrieve-all
  "Retrieves an entity from the database"
  [kind]
  (query [(str "select * from " (kind->table kind))]))

(defn set-db-config! [config]
  (dosync
    (ref-set *db-config* config)))

(defmacro with-conn [& body]
  `(sql/with-connection
     @*db-config*
     (sql/transaction
       ~@body)))

(ns adia.model
  (:use [clojure.contrib.sql :as sql])
  (:use adia.util)
  (:gen-class))

(def *db-config* (ref {}))

(def *db-entites* (ref {}))

(defmacro with-conn [& body]
  `(sql/with-connection
     @*db-config*
     (sql/transaction
       ~@body)))

(defn- query [query]
  (with-query-results 
    rs query
    (doall rs)))

(defn col-def-to-sql [[name & col-def]]
  (condp = (first col-def)
    :string (if (= (count col-def) 2)
              (let [attrs (second col-def)]
                [name (str "VARCHAR(" 
                           (if (:length attrs)
                             (:length attrs)
                             "255")
                           ")")
                 (if (:unique (second col-def))
                   "UNIQUE"
                   "")])
              [name "VARCHAR(255)"])
    :int      [name "INT" "DEFAULT 0"]
    :password [name "VARCHAR(62)"]
    :email    [name "VARCHAR(80)"]
    :text     [name "MEDIUMTEXT"]
    ; else, entity reference
    [name "VARCHAR(36)"]))

(defn sync-database-metadata [name]
  (with-conn
    (let [all-tables (mapcat vals (query ["SHOW TABLES"]))]
      (if-not (some #(= %1 (str name)) all-tables)
        (apply sql/create-table
               (keyword (str name))
               [:id "varchar(64)" "PRIMARY KEY"]
               (map col-def-to-sql ((*db-entites* name) :properties)))))))

(defn persist!
  "Persists a given entity to the database"
  [ent]
  (let [kind       (:kind ent)
        clean-ent  (dissoc ent :kind)
        persisted  (:persisted ^ent)]
    (if persisted
      (sql/update-values
        (kind :tblname)
        ["id = ?" (:id ent)]
        clean-ent)
        ;(dissoc clean-ent :id))
      (sql/insert-values
        (kind :tblname)
        (keys clean-ent) (vals clean-ent))))
  (with-meta ent {:persisted true}))

(defn retrieve
  "Retrieves an entity from the database"
  [kind id]
  (if-let [rs (query [(str "select * from " (kind :tblname) " where id = ?") id])]
    (do 
      (println (with-meta (assoc (first rs) :kind kind) {:persisted true}))
      (with-meta (assoc (first rs) :kind kind) {:persisted true}))
    nil))

(defn find-all-by [kind & prop-value-pairs]
  (query (apply vector 
                (str "select * from " (kind :tblname) " where "
                     (reduce #(str %1 " AND " %2)
                             (map #(str "`" (keyword->str %1) "` = ?") (even-items prop-value-pairs))))
                (odd-items prop-value-pairs))))

(defn find-by [kind & prop-value-pairs]
  (if-let [results (apply find-all-by kind prop-value-pairs)]
    (first results)
    nil))

(defn retrieve-all
  "Retrieves an entity from the database"
  [kind]
  (query [(str "select * from " (kind :tblname))]))

(defn set-db-config! [config]
  (dosync
    (ref-set *db-config* config)))

(defn- bind-property [ent value spec]
  (let [column-type (second spec)]
    (condp = column-type
      :string  (if (= (count spec) 3)
                 (let [attrs (nth spec 2)]
                   (if (and (:length attrs)
                            (> (count value) (:length attrs)))
                     (throw (RuntimeException. (str "Value '" value "' is too, long, maximum length: " (:length attrs)))))
                   (if (:unique attrs)
                     (if-let [duplicate (find-by (:kind ent) (first spec) value)]
                       (if-not (= (:id ent) (:id duplicate))
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
          (mapcat (fn [k] [k (bind-property ent (values-map k) (lookup-property (:kind ent) k))])
                  selected-properties)))
  ([ent values-map] (databind ent values-map (keys values-map))))

(defmacro defent [name & properties]
  `(do
     (defn ~name
       ([] (with-meta {:kind ~name
                       :id (str (java.util.UUID/randomUUID))}
                      {:persisted false}))
       ([key#] ({:tblname    (str (quote ~name))
                 :properties [~@properties]} key#))
       ([k# v# & kvs# ] (with-meta
                          (apply assoc {:kind ~name
                                        :id (str (java.util.UUID/randomUUID))}
                                 k# v# kvs#)
                          {:persisted false})))
     (dosync 
       (commute *db-entites* assoc (quote ~name) ~name))
     (sync-database-metadata (quote ~name))))

(ns adia.servlet
  (:use compojure)
  (:use [clojure.contrib.sql :as sql])
  (:use adia.model)
  (:use adia.web)
  (:gen-class
     :extends javax.servlet.http.HttpServlet))

(defn find-prefix-match [coll uri]
  (cond
    (empty? coll) nil
    (.startsWith uri (first coll)) (let [controller-name (first coll)
                                         controller      (@*uri-mapping* controller-name)]
                                     (if (= controller-name uri) ; no params
                                       controller-name
                                       (if (= (count (.split (.substring uri (+ (.length controller-name) 1)) "/"))
                                              (count (controller :arg-names)))
                                         controller-name
                                         (find-prefix-match (rest coll) uri))))
    :else (find-prefix-match (rest coll) uri)))

(defn convert-kind [val kind]
  (try
    (kind val)
    (catch Exception e
      (throw (RuntimeException. (str "Invalid argument: " val))))))

(defn handler [request]
  (connect-db)
  (if (nil? @*localhost*)
    (dosync (ref-set *localhost* ((:headers request) "host"))))
  (let [uri             (.substring (:uri request) 1)
        controller-name (find-prefix-match @*uri-list* uri)
        uri-parts       (if controller-name
                          (if (= controller-name uri) ; no params
                            ()
                            (.split (.substring uri (+ (.length controller-name) 1)) "/")))
        controller      (if controller-name (@*uri-mapping* controller-name))
        args            (if controller-name (map convert-kind uri-parts (controller :arg-types)))]
    (if controller
      (let [result (binding [*request*  request
                             *form*     (:form-params request)
                             *query*    (:query-params request)
                             *session*  (.getSession (:servlet-request request))]
                     (apply controller args))]
        (if (string? result)
          {:status 200
           :headers {}
           :body result}
          result)))))

(defroutes 
  webservice
  (ANY "/*"
       (or 
         (handler request)
         (serve-file (params :*)) 
         (page-not-found))))

(defservice webservice)

(ns adia.servlet
  (:use clojure.stacktrace)
  (:use compojure)
  (:use [clojure.contrib.sql :as sql])
  (:use adia.model)
  (:use adia.controller)
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
                                              (count (:arg-names controller)))
                                         controller-name
                                         (find-prefix-match (rest coll) uri))))
    :else (find-prefix-match (rest coll) uri)))

(defn handler [request]
  (let [uri             (.substring (:uri request) 1)
        controller-name (find-prefix-match @*uri-list* uri)
        uri-parts       (if controller-name
                          (if (= controller-name uri) ; no params
                            ()
                            (.split (.substring uri (+ (.length controller-name) 1)) "/")))
        controller      (if controller-name (@*uri-mapping* controller-name))
        args            (if controller-name (map convert-type uri-parts (:arg-types controller)))]
    (if controller
      (let [result (binding [*request*  request
                             *form*     (:form-params request)]
                     (with-conn (apply (:fn controller) args)))]
        (if (string? result)
          {:status 200
           :headers {}
           :body result}
          result)))))

(decorate handler (with-session :memory))

(defroutes 
  webservice
  (ANY "/*"
       (or 
         (try
           (handler request)
           (catch Throwable e
             (print-stack-trace e)
             (println)
             "Internal server error"))
         (serve-file (params :*)) 
         (page-not-found))))

(defservice webservice)

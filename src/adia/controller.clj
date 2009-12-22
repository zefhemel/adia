(ns adia.controller
  (:use compojure)
  (:use adia.util)
  (:require [adia.model :as model])
  (:require [somnium.congomongo :as mongo]))

(def *uri-mapping* (ref {}))
(def *uri-list* (ref []))

(def *request* nil)
(def *form* nil)
(def *session* nil)

(def *message* nil)

(defn convert-kind [val kind]
  (cond
    (= kind :string) val
    (= kind :int) (Integer/parseInt val)
    (fn? kind) (model/retrieve kind (mongo/object-id val))
    :else (throw (RuntimeException. (str "Could not convert to kind: " kind)))))

(defn input-string 
  ([attrs name] [:input (assoc attrs 
                               :name name
                               :value (*form* name)
                               :class "input-string")])
  ([name] (input-string {} name)))

(defn input-password 
  ([attrs name] [:input (assoc attrs 
                               :type "password"
                               :name name
                               :value (*form* name))])
  ([name] (input-password {} name)))

(defn input-text 
  ([attrs name] [:textarea (assoc attrs :name name
                                      (*form* name))])
  ([name] (input-text {} name)))

(defn args-to-uri [arg-types args uri]
  (if (empty? args)
    uri
    (recur (rest arg-types) (rest args) 
           (str uri "/"
                (condp = (first arg-types)
                  :string  (str (first args))
                  :int     (str (first args))
                  (:_id (first args)))))))

(defn form [[webfn & args] & body]
  (form-to [:post (args-to-uri (:arg-types webfn) args (:uri webfn))]
           body))

(defn navigate [[webfn & args] & body]
  [:a {:href (args-to-uri (:arg-types webfn) args (:uri webfn))} body])

(defn render [webfn & args]
  (if (= (first args) :flash)
    (binding [*message* (second args)]
      (apply (:fn webfn) (rest (rest args))))
    (apply (:fn webfn) args)))

(defn redirect [[webfn & args] & args]
  {:status 302
   :headers {"Location" (args-to-uri (:arg-types webfn) args (:uri webfn))}})

(defn set-session! [key value]
  (.setAttribute *session* (keyword->str key) value))

(defn get-session [key]
  (.getAttribute *session* (keyword->str key)))

(defmacro defwebfn
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
                    :uri       (str "/" ~uri)
                    :arg-names (quote ~(even-items args))
                    :arg-types ~(vec (odd-items args))
                    :doc       ~doc-str
                    :attrs     ~attrs
                    :fn        (fn [~@(even-items args)] ~body) })
        (dosync 
          (ref-set *uri-list*  (sort (fn [e1# e2#] (> (.length e1#) (.length e2#))) (conj @*uri-list* ~uri)))
          (commute *uri-mapping* assoc ~uri ~name)))))
  ([name doc-or-attrs args body] 
    (if (string? doc-or-attrs) ; doc
     `(defwebfn ~name ~doc-or-attrs {} ~args ~body)
     `(defwebfn ~name nil ~doc-or-attrs ~args ~body)))
  ([name args body] `(defwebfn ~name nil {} ~args ~body)))


(defmacro on-error [webfn & body]
  `(try
     ~@body
     (catch RuntimeException e#
       (render ~webfn :flash (.. e# getCause getMessage)))))

(ns adia.controller
  (:use compojure)
  (:use adia.util)
  (:require [adia.model :as model])
  (:require [somnium.congomongo :as mongo]))

(def *uri-mapping* (ref {}))
(def *uri-list* (ref []))

(def *request* nil)
(def *form* nil)
(def *query* nil)
(def *session* nil)
(def *message* nil)
(def *in-validated-form* nil)

(defn convert-kind [val kind]
  (cond
    (= kind :string) val
    (= kind :int) (Integer/parseInt val)
    (fn? kind) (model/retrieve kind (mongo/object-id val))
    :else (throw (RuntimeException. (str "Could not convert to kind: " kind)))))

(defn- validating-onblur [nam]
  (if *in-validated-form*
    (str "validateForm(this.form, '" (:uri *in-validated-form*) "', '" (name nam) "')")))

(defn- error-div [nam]
  [:div.error-message 
   {:id (str "validation-message-" (name nam))}])

(defn input-string 
  ([attrs nam] [:span
                [:input (assoc attrs 
                               :name nam
                               :value (*form* nam)
                               :class "input-string"
                               :onblur (validating-onblur nam)
                               :id (str "input-" (name nam)))]
                (error-div nam)])
  ([nam] (input-string {} nam)))

(defn input-password 
  ([attrs nam] [:span
                [:input (assoc attrs 
                               :type "password"
                               :name nam
                               :onblur (validating-onblur nam)
                               :class "input-password"
                               :value (*form* nam))]
                (error-div nam)])
  ([nam] (input-password {} nam)))

(defn input-text 
  ([attrs nam] [:span
                [:textarea (assoc attrs 
                                  :name nam
                                  :class "input-text"
                                  :onblur (validating-onblur nam)
                                  :id (str "input-" (name nam))
                                  (*form* nam))]
                (error-div nam)])
  ([nam] (input-text {} nam)))

(defn- args-to-uri [args uri]
  (if (empty? args)
    uri
    (recur (rest args) 
           (str uri "/"
                (let [arg (first args)]
                  (if (map? arg)
                    (:_id arg)
                    arg))))))

(defn call-to-uri [call]
  (if (vector? call)
    (let [[webfn & args] call]
      (if (map? webfn)
        (args-to-uri args (:uri webfn))
        (args-to-uri args webfn)))
    (if (map? call)
      (:uri call)
      call)))

(defn form [call attrs & body]
  (let [atts {:method :post :action (call-to-uri call)}]
    [:form (if (map? attrs)
             (merge atts attrs)
             atts)
     (if-not (map? attrs)
       attrs)
     body]))

(defmacro validated-form [validator call & body]
  `(binding [*in-validated-form* ~validator]
    (form ~call {:onsubmit (str "validateForm(this, '" (:uri ~validator) "', ''); return false")} ~@body)))

(defn navigate [call & body]
  [:a {:href (call-to-uri call)} body])

(defn render [webfn & args]
  (if (= (first args) :flash)
    (binding [*message* (second args)]
      (apply (:fn webfn) (rest (rest args))))
    (apply (:fn webfn) args)))

(defn redirect [call]
  {:status 302
   :headers {"Location" (call-to-uri call)}})

(defn set-session! [key value]
  (.setAttribute *session* (keyword->str key) value))

(defn get-session [key]
  (.getAttribute *session* (keyword->str key)))

(defmacro defwebfn [name args & body]
   (let [controller-parts (.split (str (ns-name *ns*)) "\\.")
         controller-name  (aget controller-parts (- (alength controller-parts) 1))
         action-name      (if (= name 'index)
                            ""
                            (str name))
         uri (if (= controller-name "index")
               action-name
               (str controller-name (if (empty? action-name)
                                      ""
                                      (str "/" action-name))))]
     `(do
        (def ~name {:name      (str (quote ~name))
                    :uri       (str "/" ~uri)
                    :arg-names (quote ~(even-items args))
                    :arg-types ~(vec (odd-items args))
                    :fn        (fn [~@(even-items args)] ~@body) })
        (dosync 
          (ref-set *uri-list*  (sort (fn [e1# e2#] (> (.length e1#) (.length e2#))) 
                                     (conj @*uri-list* ~uri)))
          (commute *uri-mapping* assoc ~uri ~name)))))

(defmacro on-error [webfn & body]
  `(try
     ~@body
     (catch RuntimeException e#
       (render ~webfn :flash (.. e# getCause getMessage)))))

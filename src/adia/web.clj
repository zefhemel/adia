(ns adia.web
  (:use compojure)
  (:use adia.util))

(def *webfns* (ref {})) ; unique id -> map
(def *uri-mapping* (ref {})) ; uri prefix -> unique id
(def *uri-list* (ref []))
(def *localhost* (ref nil))
(def *request* nil)
(def *form* nil)
(def *query* nil)
(def *session* nil)
(def *message* nil)
(def *in-validated-form* nil)

(defn- validating-onblur [nam]
  (if *in-validated-form*
    (str "validateForm(this.form, '" (*in-validated-form* :uri) "', '" (name nam) "')")))

(defn- error-div [nam]
  [:div.error-message 
   {:id (str "validation-message-" (name nam))}])

(defn integer 
  "Parse a string as integer, useful as parameter type"
  [s]
  (Integer/parseInt s))

(defn input-string 
  ([nam value] [:span
                [:input {:name nam
                         :value (if (*form* nam)
                                  (*form* nam)
                                  value)
                         :class "input-string"
                         :onblur (validating-onblur nam)
                         :id (str "input-" (name nam))}]
                (error-div nam)])
  ([nam] (input-string nam "")))

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
  ([nam value] [:span
                [:textarea {:name nam
                            :class "input-text"
                            :onblur (validating-onblur nam)
                            :id (str "input-" (name nam))}
                 (if (*form* nam)
                   (*form* nam)
                   value)]
                (error-div nam)])
  ([nam] (input-text nam "")))

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
      (if (fn? webfn)
        (args-to-uri args (webfn :uri))
        (args-to-uri args webfn)))
    (if (fn? call)
      (call :uri)
      call)))

(defn can-access? [call]
  (if (vector? call)
    (let [[webfn & args] call]
      (if (fn? webfn)
        (if (webfn :can-access?) ; does it have an AC function?
          (apply (webfn :can-access?) args)
          true)
        true))
    (if (fn? call)
      (if (call :can-access?) ; does it have an AC function?
        ((call :can-access?))
        true)
      true)))

(defn form [call attrs & body]
  (if (can-access? call)
    (let [atts {:method :post :action (call-to-uri call)}]
      [:form (if (map? attrs)
               (merge atts attrs)
               atts)
       (if-not (map? attrs)
         attrs)
       body])))

(defmacro validated-form [validator call & body]
  `(binding [*in-validated-form* ~validator]
    (form ~call {:onsubmit (str "validateForm(this, '" (~validator :uri) "', ''); return false")} ~@body)))

(defn navigate [call & body]
  (if (can-access? call)
    [:a {:href (call-to-uri call)} body]))

(defn redirect [call]
  {:status 302
   :headers {"Location" (call-to-uri call)}})

(defn set-session! [key value]
  (.setAttribute *session* (name key) value))

(defn get-session [key]
  (.getAttribute *session* (name key)))

(defmacro defwebfn [name args & body]
   (let [controller-parts (.split (str (ns-name *ns*)) "\\.")
         controller-name  (aget controller-parts (- (alength controller-parts) 1))
         unique-name      (keyword (gensym "webfn"))
         action-name      (if (= name 'index)
                            ""
                            (str name))
         uri (if (= controller-name "index")
               action-name
               (str controller-name (if (empty? action-name)
                                      ""
                                      (str "/" action-name))))]
     `(do
        (dosync
          (commute *webfns* assoc ~unique-name
                   {:name        (str (quote ~name))
                    :unique-name ~unique-name
                    :uri         (str "/" ~uri)
                    :arg-names   (quote ~(even-items args))
                    :arg-types   ~(vec (odd-items args))
                    :fn          (fn [~@(even-items args)] ~@body)
                    :fn-body     (quote ~@body)}))
        (defn ~name [& fn-args#]
          (if (and (= 1 (count fn-args#)) (keyword? (first fn-args#)))
            (if (= (first fn-args#) :meta)
              (@*webfns* ~unique-name)
              ((@*webfns* ~unique-name) (first fn-args#)))
            (apply (:fn (@*webfns* ~unique-name)) fn-args#)))
        (dosync 
          (ref-set *uri-list*  (sort (fn [e1# e2#] (> (.length e1#) (.length e2#))) 
                                     (conj @*uri-list* ~uri)))
          (commute *uri-mapping* assoc ~uri ~unique-name)))))

(defn assoc-webfn!
  "Change the function body of an already defined webfn. Use with care!"
  [webfn & args]
  (let [meta-map    (webfn :meta)
        unique-name (:unique-name meta-map)]
    (dosync
      (ref-set *webfns* (assoc @*webfns* unique-name 
                               (apply assoc meta-map args))))))

;:fn-body fn-body :fn (eval `(fn [~@(:arg-names meta-map)] ~@fn-body))))))))

(comment
  (defmacro on-error [webfn & body]
  `(try
  ~@body
  (catch RuntimeException e#
  (render ~webfn :flash (.. e# getCause getMessage))))))

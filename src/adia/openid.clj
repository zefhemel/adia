(ns adia.openid
  (:use adia.web))

(def *openid-manager* (ref nil))

(def *openid-callback-webfn* nil)

(add-watch *localhost* :set (fn [_ _ _ host] 
                              (dosync
                                (ref-set *openid-manager* (doto (org.expressme.openid.OpenIdManager.)
                                                            (.setReturnTo (str "http://" host "/openid/handle"))
                                                            (.setRealm (str "http://" host))
                                                            (.setTimeOut 10000))))))

(defn set-callback [fn]
  (def *openid-callback-webfn* fn))

(defwebfn login [service str]
  (let [endpoint (.lookupEndpoint @*openid-manager* service)
        association (.lookupAssociation @*openid-manager* endpoint)]
    (set-session! :openid-mac (.getRawMacKey association))
    (redirect (.getAuthenticationUrl @*openid-manager* endpoint association))))

(defwebfn handle []
  (let [authentication (.getAuthentication @*openid-manager* (:servlet-request *request*) (get-session :openid-mac))]
    (*openid-callback-webfn* {:uri        (.getIdentity  authentication)
                              :first-name (.getFirstname authentication)
                              :last-name  (.getLastname  authentication)
                              :email      (.getEmail     authentication)})))

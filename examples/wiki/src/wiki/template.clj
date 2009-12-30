(ns wiki.template
  (:use compojure)
  (:use adia.web)
  (:gen-class))

(defn main-layout
  [title & body]
  (html
    (doctype :html4)
    [:html
     [:head
      [:title (h title)]]
     [:body
      (if-let [username (get-session :username)]
        [:span "Hello, " username " " (navigate ["/logout"] "[ Logout ]")]
        [:span "Login with "
         (navigate ["/openid/login" "Google"] "Google")])
      [:hr]
      body
      [:hr]
      "&copy; Adia"]]))

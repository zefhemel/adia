(ns wiki.index
  (:use compojure)
  (:use adia.model)
  (:use adia.web)
  (:use adia.util)
  (:use adia.ac)
  (:use wiki.template)
  (:require [adia.openid :as openid])
  (:require [wiki.model :as model])
  (:gen-class))

(declare show handle-add edit handle-edit)

(defwebfn index []
  (main-layout
    "Wiki home"
    [:h1 "All wiki pages"]
    [:ul
     (for [p (query model/Page)]
      [:li (navigate [show p] (h (:title p)))])]
    (form [handle-add]
          [:h1 "Add a page"]
          [:div "Title: " (input-string :title)]
          [:div (input-text :text)]
          (submit-button "Add page"))))

(defwebfn handle-add []
  (let [p (databind (model/Page :author (get-session :username)) 
                    *form* [:title :text])]
    (persist! p)
    (redirect index)))

(defac handle-add (get-session :username))

(defwebfn show [p model/Page]
  (main-layout
    (:title p)
    [:h1 (h (:title p))]
    (markdown (:text p))
    [:em "Created by " (h (:author p)) " "]
    (navigate [edit p] "[ Edit ]")))

(defwebfn edit [p model/Page]
  (main-layout
    (str "Edit " (:title p))
    (form [handle-edit p]
          [:div "Title: " (input-string :title (:title p))]
          [:div (input-text :text (:text p))]
          (submit-button "Edit"))))

(defac edit (= (get-session :username) (:author p)))

(defwebfn handle-edit [p model/Page]
  (let [new-p (databind p *form* [:title :text])]
    (persist! new-p)
    (redirect [show new-p])))

(defn handle-login [m]
  (set-session! :username (str (:first-name m) " " (:last-name m)))
  (redirect index))

(defwebfn logout []
  (set-session! :username nil)
  (redirect index))

(openid/set-callback handle-login)

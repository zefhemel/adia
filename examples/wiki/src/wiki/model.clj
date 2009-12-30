(ns wiki.model
  (:use adia.model))

(set-db-config! :db "wiki")

(defent Page
  [:title    :string]
  [:author   :string]
  [:text     :text])

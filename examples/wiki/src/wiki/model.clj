(ns wiki.model
  (:use adia.model)
  (:require [adia.openid :as openid]))

(set-db-config! :db "wiki")

(defent Page
  [:title    :string]
  [:author   :string]
  [:text     :text])

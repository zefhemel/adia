(ns application
  (:require wiki.index)
  (:use compojure)
  (:use adia.servlet))

(run-server {:port 8080}
  "/*" (servlet webservice))

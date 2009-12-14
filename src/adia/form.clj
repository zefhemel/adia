(ns adia.form
  (:gen-class))


(defn field-row [label input]
  [:tr
   [:td label]
   [:td input]])

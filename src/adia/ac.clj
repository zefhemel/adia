(ns adia.ac
  (:use adia.web)
  (:use adia.util))

(defn wrap-ac [exp current-body]
  `(if ~exp
     (do ~current-body)
     "Access denied!"))

(defn wrap-fn [webfn & body]
  `(fn [~@(webfn :arg-names)] ~@body))

(defmacro defac [webfn exp]
  `(let [fn-body# (wrap-ac (quote ~exp) (~webfn :fn-body))]
     (assoc-webfn! ~webfn 
                   :fn-body fn-body#
                   :fn (eval (wrap-fn ~webfn fn-body#))
                   :can-access? (eval (wrap-fn ~webfn (quote ~exp))))))

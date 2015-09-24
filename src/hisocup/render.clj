(ns hisocup.render
  (:require [hisocup.core :refer [html]]))

(defmacro frender [args & body]
  `(with-meta
     (fn ~args
       (html ~@body))
     {:frender true}))

(defmacro defrender [name args & body]
  `(def ~name (frender ~args ~@body)))

(defn frender? [f]
  (= true (:frender (meta f))))

(def create-class identity)


(comment
  (let [r (Render. "e")]
    (apply r "r" "t"))
  )

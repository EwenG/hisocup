(ns hisocup.render
  (:refer-clojure :exclude [atom])
  #?(:clj (:require [hisocup.core :refer [html]]))
  #?(:cljs (:require [reagent.core :as reagent])))

(defn- cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

#?(:clj (defmacro if-cljs
          "Return then if we are generating cljs code and else for
  Clojure code.
  https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
          [then else]
          (if (cljs-env? &env) then else)))

#(:clj (defmacro frender [& fn-params]
         (let [wrap-body (fn [body] (cons 'hisocup.core/html body))
               [name params-body] (if (symbol? (first fn-params))
                                    [(first fn-params) (next fn-params)]
                                    [nil fn-params])
               [params body] (if (vector? (first params-body))
                               [(first params-body)
                                (wrap-body (next params-body))]
                               [nil nil])
               bodies (map (fn [params-body]
                             (if (list? params-body)
                               (list (first params-body)
                                     (wrap-body (rest params-body)))
                               params-body))
                           params-body)
               result-fn (cond (and name params)
                               `(fn ~name ~params ~body)
                               (and (not name) params)
                               `(fn ~params ~body)
                               (and name bodies)
                               `(fn ~name ~@bodies)
                               (and (not name) bodies)
                               `(fn ~@bodies))]
           `(if-cljs
             ~result-fn
             (with-meta ~result-fn {:frender true})))))

;;Taken from clojure.tools.macro
(defn name-with-attributes
  "To be used in macro definitions.
   Handles optional docstrings and attribute maps for a name to be defined
   in a list of macro arguments. If the first macro argument is a string,
   it is added as a docstring to name and removed from the macro argument
   list. If afterwards the first macro argument is a map, its entries are
   added to the name's metadata map and the map is removed from the
   macro argument list. The return value is a vector containing the name
   with its extended metadata map and the list of unprocessed macro
   arguments."
  [name macro-args]
  (let [[docstring macro-args] (if (string? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [nil macro-args])
        [attr macro-args]          (if (map? (first macro-args))
                                     [(first macro-args) (next macro-args)]
                                     [{} macro-args])
        attr                       (if docstring
                                     (assoc attr :doc docstring)
                                     attr)
        attr                       (if (meta name)
                                     (conj (meta name) attr)
                                     attr)]
    [(with-meta name attr) macro-args]))

#?(:clj (defmacro defrender [name & macro-args]
          (let [[name macro-args] (name-with-attributes name macro-args)]
            `(def ~name ~(cons 'hisocup.render/frender macro-args)))))

#?(:clj (def create-class identity)
   :cljs (def create-class reagent/create-class))

#?(:cljs (def render-component reagent/render-component))

#?(:clj (def atom clojure.core/atom)
   :cljs (def atom reagent/atom))

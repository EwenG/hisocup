(defproject hisocup "1.0.5"
  :description "A slightly modified version of hiccup which can be used to build isomorphic applications based on hiccup and reagent."
  :url "https://github.com/EwenG/hisocup"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [reagent "0.5.1"]]
  :profiles
  {:1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
   :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
   :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}})

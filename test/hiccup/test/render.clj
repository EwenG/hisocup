(ns hisocup.test.render
  (:require [clojure.test :refer :all]
            [hisocup.render :refer [frender defrender
                                    frender? create-class]]
            [hisocup.core :refer [html]]))

(deftest frender-type
  (testing "frender type checks"
    (is (= (fn? (frender [] [:div])) true))
    (is (= (frender? (frender [] [:div])) true))
    (is (= (frender? (fn [] [:div])) false))))

(deftest function-call-not-compiled
  (let [simple-fn (fn [x] [:div x])
        nested-fn (fn [x] (fn [x] [:div x]))
        class-map-render (fn [x y z]
                           (create-class
                            {:render (fn [x y z] [:div y])}))
        class-map-reagent-render (fn [x y z]
                                   (create-class
                                    {:reagent-render
                                     (fn [x y z]
                                       [:div y])}))]
    (is (= (html [:div [simple-fn 3]]) "<div><div>3</div></div>"))
    (is (= (html [:div [nested-fn 3]]) "<div><div>3</div></div>"))
    (is (= (html [:div [class-map-render 1 "e" 3]])
           "<div><div>e</div></div>"))
    (is (= (html [:div [class-map-reagent-render 1 "e" 3]])
           "<div><div>e</div></div>"))))

(deftest frender-call-not-compiled
  (let [simple-fn (frender [x] [:div x])
        nested-fn (fn [x] (frender [x] [:div x]))]
    (is (= (html [:div [simple-fn 3]]) "<div><div>3</div></div>"))
    (is (= (html [:div [nested-fn 3]]) "<div><div>3</div></div>"))))

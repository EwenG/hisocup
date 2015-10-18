(ns hisocup.vdom
  (:require [cljs.analyzer.api :as ana-api])
  (:use hisocup.util))

(comment
  (require-macros '[hisocup.vdom :refer [mm]])
  )

(defn analyze [env x]
  (let [ana-x (ana-api/analyze env x)
        form (:form ana-x)
        new-env (:env ana-x)]
    (cond
      (seq? form)
      (#(doall (map %1 %2))
       (partial analyze new-env) form)
      :else x)))

(defmacro mm [x]
  `(quote ~(analyze &env x)))

(def ^{:doc "Regular expression that parses a CSS-style id and class from an element name."
       :private true}
  re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(defn- unevaluated?
  "True if the expression has not been evaluated."
  [expr]
  (or (symbol? expr)
      (and (seq? expr)
           (not= (first expr) `quote))))

(defn- literal?
  "True if x is a literal value that can be rendered as-is."
  [x]
  (and (not (unevaluated? x))
       (or (not (or (vector? x) (map? x)))
           (every? literal? x))))

(defn- form-name
  "Get the name of the supplied form."
  [form]
  (if (and (seq? form) (symbol? (first form)))
    (name (first form))))

(defmulti compile-form
  "Pre-compile certain standard forms, where possible."
  {:private true}
  form-name)

#_(defmethod compile-form "for"
  [[_ bindings body]]
  `(apply str (for ~bindings ~(compile-html body))))

#_(defmethod compile-form "if"
  [[_ condition & body]]
  `(if ~condition ~@(for [x body] (compile-html x))))

(defmethod compile-form :default
  [expr]
  `(with-meta ~expr
     {:node nil :dynamic true}))

(defn- not-hint?
  "True if x is not hinted to be the supplied type."
  [x type]
  (if-let [hint (-> x meta :tag)]
    (not (isa? (eval hint) type))))

(defn- hint?
  "True if x is hinted to be the supplied type."
  [x type]
  (if-let [hint (-> x meta :tag)]
    (isa? (eval hint) type)))

(defn- not-implicit-map?
  "True if we can infer that x is not a map."
  [x]
  (or (= (form-name x) "for")
      (not (unevaluated? x))
      (not-hint? x java.util.Map)))

(defn- merge-attributes [{:keys [id class]} map-attrs]
  (->> map-attrs
       (merge (if id {:id id}))
       (merge-with #(if %1 (str %1 " " %2) %2) (if class {:class class}))))

(defn normalize-element
  "Ensure an element vector is of the form [tag-name attrs content]."
  [[tag & content]]
  (when (not (or (keyword? tag) (symbol? tag) (string? tag) (fn? tag)))
    (throw (IllegalArgumentException. (str tag " is not a valid element name."))))
  (if (fn? tag)
    [tag {} content]
    (let [[_ tag id class] (re-matches re-tag (as-str tag))
          tag-attrs        {:id id
                            :class (if class (.replace ^String class "." " "))}
          map-attrs        (first content)]
      (if (map? map-attrs)
        [tag (merge-attributes tag-attrs map-attrs) (next content)]
        [tag tag-attrs content]))))

(defn- element-compile-strategy
  "Returns the compilation strategy to use for a given element."
  [[tag attrs & content :as element]]
  (cond
    (every? literal? element)
    ::all-literal                    ; e.g. [:span "foo"]
    (and (literal? tag) (map? attrs))
    ::literal-tag-and-attributes     ; e.g. [:span {} x]
    (and (literal? tag) (not-implicit-map? attrs))
    ::literal-tag-and-no-attributes  ; e.g. [:span ^String x]
    (literal? tag)
    ::literal-tag                    ; e.g. [:span x]
    :else
    ::default))

(declare compile-seq)

(defmulti compile-element
  "Returns an unevaluated form that will render the supplied vector as a HTML
  element."
  {:private true}
  element-compile-strategy)

(defmethod compile-element ::all-literal
  [element]
  (let [[tag attrs content] (normalize-element (eval element))]
    `(StaticNode. ~tag ~attrs ~@(compile-seq content))))

(defmethod compile-element ::literal-tag-and-attributes
  [[tag attrs & content]]
  `(StaticNode. ~tag ~attrs ~@(compile-seq content)))

(defmethod compile-element ::literal-tag-and-no-attributes
  [[tag & content]]
  (compile-element (apply vector tag {} content)))

(defmethod compile-element ::literal-tag
  [[tag attrs & content]]
  `(DynamicNode. [~tag ~attrs] ~@(compile-seq content)))

(defmethod compile-element :default
  [element]
  (if (= (count element) 1)
    `(DynamicNode. [~(first element) nil])
    `(DynamicNode. [~(first element) ~(second element)
                 ~@(compile-seq (rest (rest element)))])))

(defn compile-seq
  "Pre-compile data structures into HTML where possible."
  [content]
  (doall (for [expr content]
           (cond
             (vector? expr) (compile-element expr)
             (literal? expr) `(StaticTextVNode. ~expr)
             (hint? expr String) `(StaticTextVNode. ~expr)
             (hint? expr Number) `(StaticTextVNode. ~expr)
             (seq? expr) (compile-form expr)
             :else `(DynamicVNode. ~expr)))))

(defn compile-html
  "Pre-compile data structures into HTML where possible."
  [content]
  (first (compile-seq [content])))

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

(defmacro defhtml [name & macro-args]
  `(def ~name (Vtree. ~(compile-html (last (rest macro-args)))
                      (quote ~(first macro-args)))))


(defn diff-trees [tree1 tree2]
  (let [result (volatile! [])
        point (volatile! nil)
        n (count tree1)
        m (count tree2)
        v (volatile! (vec (int-array (+ (* 2 (+ n m)) 3))))
        v-seq (volatile! [])
        offset (+ n m 1)]
    (loop [d 0]
      (loop [k (- d)]
        (let [down (or (= k (- d))
                       (and (not= k d)
                            (< (get @v (+ offset (dec k)))
                               (get @v (+ offset (inc k))))))
              k-prev (if down (inc k) (dec k))
              x-start (get @v (+ offset k-prev))
              y-start (- x-start k-prev)
              x-mid (if down x-start (inc x-start))
              y-mid (- x-mid k)
              [x-end y-end] (loop [x-end x-mid
                                   y-end y-mid]
                              (if (and (< x-end n)
                                       (< y-end m)
                                       (= (get tree1 x-end)
                                          (get tree2 y-end)))
                                (recur (inc x-end) (inc y-end))
                                [x-end y-end]))]
          (vswap! v assoc (+ offset k) x-end)
          (cond (and (>= x-end n) (>= y-end m))
                (vreset! point [x-end y-end])
                (> (+ 2 k) d)
                (vreset! point nil)
                :else
                (recur (+ k 2)))))
      (vswap! v-seq conj @v)
      (when (and (< d (+ n m)) (= nil @point))
        (recur (inc d))))
    (loop [d (dec (count @v-seq))
           [px py] @point]
      (let [v (get @v-seq d)
            k (- px py)
            x-end (get v (+ offset k))
            y-end (- x-end k)
            down (or (= k (- d))
                     (and (not= k d)
                          (< (get v (+ offset (dec k)))
                             (get v (+ offset (inc k))))))
            k-prev (if down (inc k) (dec k))
            x-start (get v (+ offset k-prev))
            y-start (- x-start k-prev)
            x-mid (if down x-start (inc x-start))
            y-mid (- x-mid k)
            diag-nb (- x-end x-mid)]
        (vswap! result conj
                [[x-start y-start] [x-mid y-mid] [ x-end y-end]])
        (if (or (> x-start 0)
                (> y-start 0))
          (recur (dec d) [x-start y-start])
          @result)))))


(comment

  (diff-trees [:a :b :c :a :b :b :a] [:c :b :a :b :a :c])
  (diff-trees [:a1 :b1 :c1] [:a2 :b3 :c1])
  (diff-trees [:a1 :b1 :c1] [:a1 :b1 :c1])

  (macroexpand-1 '(defhtml ee [e] [:div e]))

  (macroexpand-1 '(defhtml ee [] [:div]))

  (let [fragment (createDoocumentFragment)])

  (require '[clojure.walk :as walk])

  (walk/prewalk (fn [x] (prn x) x) [[1 [2] 3] [2 4]])

  (macroexpand-1 '(html [:div [:p]]))

  )

(defmacro ee [e]
  (let [bbb (meta e)]
    `(str ~bbb)))

(definline ee2 [r]
  `(str ~r))


; Add a mutable map to vtrees. Contains ID -> real dom node
; If branching -> store the conditional result, if different force the node(s) overriding.

(ns hisocup.compiler
  "Internal functions for compilation."
  (:use hisocup.util)
  (:import [clojure.lang IPersistentVector ISeq Named]
           [java.util.zip Adler32]
           [java.nio.charset StandardCharsets]))

;;Utils for Adler32 computations
(def ^:const MOD 65521)

(defn sign-bit [x]
  (bit-shift-right (bit-and 0x80000000 x) 31))

(defn to-adler-string [x]
  (if (= 1 (sign-bit x))
    (dec (- (bit-xor 0xFFFFFFFF (bit-and 0xFFFFFFFF x))))
    (bit-and 0xFFFFFFFF x)))

;;Data-reactid attributes generation
(def ^:dynamic *reactid* nil)

(defn format-reactid [[prev-reactid reactid]]
  (str prev-reactid "." (Integer/toString reactid 36)))

(defn reactid-down [reactid]
  (if-not reactid
    nil
    [(format-reactid reactid) 0]))

(defn reactid-next [[prev-reactid reactid]]
  (if-not reactid
    nil
    [prev-reactid (inc reactid)]))

(defn- xml-mode? []
  (#{:xml :xhtml} *html-mode*))

(defn- html-mode? []
  (#{:html :xhtml} *html-mode*))

(defn- end-tag [] ">")

(def ^:const supported-attributes #{"low" "role" "allowTransparency" "seamless" "noValidate" "selected" "lang" "loop" "min" "wmode" "width" "colSpan" "list" "media" "acceptCharset" "high" "coords" "scrolling" "defer" "height" "htmlFor" "for" "encType" "shape" "tabIndex" "formNoValidate" "muted" "className" "class" "scoped" "marginHeight" "download" "placeholder" "label" "method" "form" "radioGroup" "srcDoc" "max" "mediaGroup" "charSet" "hrefLang" "id" "dateTime" "frameBorder" "contextMenu" "formTarget" "src" "cellPadding" "async" "maxLength" "rowSpan" "href" "name" "useMap" "style" "accessKey" "value" "span" "formEncType" "preload" "optimum" "accept" "formAction" "srcSet" "start" "alt" "scope" "action" "content" "dir" "draggable" "autoComplete" "readOnly" "title" "required" "rel" "pattern" "type" "cols" "poster" "controls" "cellSpacing" "target" "hidden" "manifest" "autoPlay" "sandbox" "size" "formMethod" "rows" "marginWidth" "sizes" "classID" "open" "disabled" "data" "checked" "contentEditable" "spellCheck" "crossOrigin" "allowFullScreen" "httpEquiv" "autoFocus" "step" "multiple" "icon" "headers" :role :rel :formMethod :open :async :httpEquiv :min :sizes :rowSpan :frameBorder :noValidate :selected :tabIndex :dir :muted :seamless :placeholder :formTarget :disabled :alt :marginHeight :marginWidth :autoFocus :coords :method :content :formEncType :name :autoComplete :radioGroup :value :optimum :scoped :width :start :defer :cellSpacing :type :controls :manifest :src :icon :multiple :scope :sandbox :className :class :size :title :headers :loop :high :style :lang :rows :dateTime :allowFullScreen :cols :crossOrigin :scrolling :preload :colSpan :classID :poster :hrefLang :draggable :allowTransparency :list :readOnly :acceptCharset :formAction :hidden :max :label :id :wmode :checked :accessKey :shape :srcSet :mediaGroup :spellCheck :autoPlay :low :maxLength :contentEditable :action :htmlFor :for :useMap :encType :form :cellPadding :target :contextMenu :formNoValidate :download :step :charSet :srcDoc :media :href :required :height :pattern :accept :span :data "clipPath" "cx" "cy" "d" "dx" "dy" "fill" "fillOpacity" "fontFamily" "fontSize" "fx" "fy" "gradientTransform" "gradientUnits" "markerEnd" "markerMid" "markerStart" "offset" "opacity" "patternContentUnits" "patternUnits" "points" "preserveAspectRatio" "r" "rx" "ry" "spreadMethod" "stopColor" "stopOpacity" "stroke" "strokeDasharray" "strokeLinecap" "strokeOpacity" "strokeWidth" "textAnchor" "transform" "version" "viewBox" "x1" "x2" "x" "y1" "y2" "y" :clipPath :cx :cy :d :dx :dy :fill :fillOpacity :fontFamily :fontSize :fx :fy :gradientTransform :gradientUnits :markerEnd :markerMid :markerStart :offset :opacity :patternContentUnits :patternUnits :points :preserveAspectRatio :r :rx :ry :spreadMethod :stopColor :stopOpacity :stroke :strokeDasharray :strokeLinecap :strokeOpacity :strokeWidth :textAnchor :transform :version :viewBox :x1 :x2 :x :y1 :y2 :y})

(defn- xml-attribute [name value]
  (str " " name "=\"" (escape-html value) "\""))

(defn- render-attribute [[name value]]
  (let [name (.toLowerCase (as-str name))]
    (if (nil? value)
      ""
      (xml-attribute name value))))

(defn- render-attr-map [attrs]
  (let [attrs (select-keys attrs supported-attributes)
        reactid-attr (if *reactid*
                       [[:data-reactid (format-reactid *reactid*)]]
                       [])]
    (->> (into (vec attrs) reactid-attr)
         (map render-attribute)
         (apply str))))

(def ^{:doc "Regular expression that parses a CSS-style id and class from an element name."
       :private true}
  re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(def ^{:doc "A list of elements that must be rendered without a closing tag."
       :private true}
  void-tags
  #{"area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen"
    "link" "meta" "param" "source" "track" "wbr"})

(defn- container-tag?
  "Returns true if the tag has content or is not a void tag. In non-HTML modes,
  all contentless tags are assumed to be void tags."
  [tag content]
  (= nil (get void-tags tag)))

(defn before-content [tag]
  "Insert a \n character before the content of pre tags in order to match
react markup"
  (if (= "pre" tag)
    "\n" ""))

(defn frender? [f]
  (= true (:frender (meta f))))

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

(defprotocol HtmlRenderer
  (render-html [this]
    "Turn a Clojure data type into a string of HTML."))

(defn maybe-add-checksum [compiled-s]
  (let [index (.indexOf compiled-s (int \>))
        compiled-bytes (.getBytes compiled-s StandardCharsets/UTF_8)
        adler32 (doto (Adler32.) (.update compiled-bytes))
        checksum (.getValue adler32)
        checksum (to-adler-string checksum)]
    (if (= -1 index)
      compiled-s
      (str (subs compiled-s 0 index)
           " data-react-checksum=\""
           checksum
           "\""
           (subs compiled-s index)))))

(defn- render-element
  "Render an element vector as a HTML element."
  [element]
  (let [[tag attrs content] (normalize-element element)]
    (cond (frender? tag)
          (render-html
           (let [compiled-s
                 (binding [*reactid* (or *reactid* ["" 0])]
                   (apply tag content))
                 compiled-s (if-not *reactid*
                              (maybe-add-checksum compiled-s)
                              compiled-s)]
             (set! *reactid* (reactid-next *reactid*))
             compiled-s))
          (fn? tag)
          (let [next-fn-or-content (apply tag content)]
            (cond (fn? next-fn-or-content)
                  (recur (into [next-fn-or-content] content))
                  (and (map? next-fn-or-content)
                       (:render next-fn-or-content))
                  (recur (into [(:render next-fn-or-content)] content))
                  (and (map? next-fn-or-content)
                       (:reagent-render next-fn-or-content))
                  (recur (into [(:reagent-render next-fn-or-content)]
                               content))
                  :else
                  (let [compiled-s
                        (binding [*reactid* (or *reactid* ["" 0])]
                          (render-html next-fn-or-content))]
                    (set! *reactid* (reactid-next *reactid*))
                    compiled-s)))
          (container-tag? tag content)
          (let [compiled-s
                (str "<" tag (render-attr-map attrs) ">"
                     (before-content tag)
                     (binding [*reactid* (reactid-down *reactid*)]
                       (render-html content))
                     "</" tag ">")]
            (set! *reactid* (reactid-next *reactid*))
            compiled-s)
          :else
          (let [compiled-s
                (str "<" tag (render-attr-map attrs) (end-tag)
                     (before-content tag)
                     (binding [*reactid* (reactid-down *reactid*)]
                       (render-html content)))]
            (set! *reactid* (reactid-next *reactid*))
            compiled-s))))

(extend-protocol HtmlRenderer
  IPersistentVector
  (render-html [this]
    (render-element this))
  ISeq
  (render-html [this]
    (apply str (map render-html this)))
  Named
  (render-html [this]
    (name this))
  Object
  (render-html [this]
    (str this))
  nil
  (render-html [this]
    ""))

(defn- unevaluated?
  "True if the expression has not been evaluated."
  [expr]
  (or (symbol? expr)
      (and (seq? expr)
           (not= (first expr) `quote))))

(defn compile-attr-map
  "Returns an unevaluated form that will render the supplied map as HTML
  attributes."
  [attrs]
  `(#'render-attr-map ~attrs))

(defn- form-name
  "Get the name of the supplied form."
  [form]
  (if (and (seq? form) (symbol? (first form)))
    (name (first form))))

(declare compile-html)

(defmulti compile-form
  "Pre-compile certain standard forms, where possible."
  {:private true}
  form-name)

(defmethod compile-form "for"
  [[_ bindings body]]
  `(apply str (for ~bindings ~(compile-html body))))

(defmethod compile-form "if"
  [[_ condition & body]]
  `(if ~condition ~@(for [x body] (compile-html x))))

(defmethod compile-form :default
  [expr]
  `(#'render-html ~expr))

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

(defn- literal?
  "True if x is a literal value that can be rendered as-is."
  [x]
  (and (not (unevaluated? x))
       (or (not (or (vector? x) (map? x)))
           (every? literal? x))))

(defn- not-implicit-map?
  "True if we can infer that x is not a map."
  [x]
  (or (= (form-name x) "for")
      (not (unevaluated? x))
      (not-hint? x java.util.Map)))

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
    ::default))                      ; e.g. [x]

(declare compile-seq)

(defmulti compile-element
  "Returns an unevaluated form that will render the supplied vector as a HTML
  element."
  {:private true}
  element-compile-strategy)

(defn with-next-reactid [compiled-s]
  `(let [compiled-s# ~compiled-s]
     (set! *reactid* (reactid-next *reactid*))
     compiled-s#))

(defn with-down-reactid [compiled-seq]
  `(let [compiled-seq# ~compiled-seq]
     (set! *reactid* (reactid-next *reactid*))
     compiled-s#))

(defmethod compile-element ::all-literal
  [element]
  (let [[tag attrs content] (normalize-element (eval element))]
    (with-next-reactid
      (if (container-tag? tag content)
        `(str ~(str "<" tag) ~(compile-attr-map attrs) ">"
              ~(before-content tag)
              (binding [*reactid* (reactid-down *reactid*)]
                (str ~@(compile-seq content)))
              ~(str "</" tag ">"))
        `(str "<" ~tag ~(compile-attr-map attrs) ~(end-tag))))))

(defmethod compile-element ::literal-tag-and-attributes
  [[tag attrs & content]]
  (let [[tag attrs _] (normalize-element [tag attrs])]
    (with-next-reactid
      (if (container-tag? tag content)
        `(str ~(str "<" tag) ~(compile-attr-map attrs) ">"
              ~(before-content tag)
              (binding [*reactid* (reactid-down *reactid*)]
                (str ~@(compile-seq content)))
              ~(str "</" tag ">"))
        `(str "<" ~tag ~(compile-attr-map attrs) ~(end-tag))))))

(defmethod compile-element ::literal-tag-and-no-attributes
  [[tag & content]]
  (compile-element (apply vector tag {} content)))

(defmethod compile-element ::literal-tag
  [[tag attrs & content]]
  (let [[tag tag-attrs _] (normalize-element [tag])
        attrs-sym         (gensym "attrs")]
    (with-next-reactid
      `(let [~attrs-sym ~attrs]
         (if (map? ~attrs-sym)
           ~(if (container-tag? tag content)
              `(str ~(str "<" tag)
                    (#'render-attr-map (merge ~tag-attrs ~attrs-sym)) ">"
                    ~(before-content tag)
                    (binding [*reactid* (reactid-down *reactid*)]
                      (str ~@(compile-seq content)))
                    ~(str "</" tag ">"))
              `(str ~(str "<" tag)
                    (#'render-attr-map (merge ~tag-attrs ~attrs-sym))
                    ~(end-tag)))
           ~(if (container-tag? tag attrs)
              `(str ~(str "<" tag) (#'render-attr-map ~tag-attrs) ">"
                    ~(before-content tag)
                    (binding [*reactid* (reactid-down *reactid*)]
                      (str ~@(compile-seq (cons attrs-sym content))))
                    ~(str "</" tag ">"))
              `(str ~(str "<" tag) (#'render-attr-map ~tag-attrs)
                    ~(end-tag))))))))

(defmethod compile-element :default
  [element]
  `(cond (frender? ~(first element))
         (#'render-html
          (let [compiled#
                (binding [*reactid* (or *reactid* ["" 0])]
                  (apply ~(first element) ~(rest element)))
                compiled-checksum# (if-not *reactid*
                                     (maybe-add-checksum compiled#)
                                     compiled#)]
            (set! *reactid* (reactid-next *reactid*))
            compiled-checksum#))
         (fn? tag)
         (let [next-fn-or-content# (apply ~(first element) ~(rest element))]
           (cond (fn? next-fn-or-content#)
                 (recur (into [next-fn-or-content] content))
                 (and (map? next-fn-or-content)
                      (:render next-fn-or-content))
                 (recur (into [(:render next-fn-or-content)] content))
                 (and (map? next-fn-or-content)
                      (:reagent-render next-fn-or-content))
                 (recur (into [(:reagent-render next-fn-or-content)]
                              content))
                 :else
                 (let [compiled-s
                       (binding [*reactid* (or *reactid* ["" 0])]
                         (render-html next-fn-or-content))]
                   (set! *reactid* (reactid-next *reactid*))
                   compiled-s)))
         :else
         (#'render-element
          [~(first element)
           ~@(for [x (rest element)]
               (if (vector? x)
                 `(let [compiled# (binding [*reactid*
                                            (reactid-down *reactid*)]
                                    ~(compile-element x))]
                    (set! *reactid* (reactid-next *reactid*))
                    compiled#)
                 x))])))

(defn- compile-seq
  "Compile a sequence of data-structures into HTML."
  [content]
  (doall (for [expr content]
           (cond
             (vector? expr) (compile-element expr)
             (literal? expr) expr
             (hint? expr String) expr
             (hint? expr Number) expr
             (seq? expr) (compile-form expr)
             :else `(#'render-html ~expr)))))

(defn- collapse-strs
  "Collapse nested str expressions into one, where possible."
  [expr]
  (if (seq? expr)
    (cons
     (first expr)
     (mapcat
      #(if (and (seq? %) (symbol? (first %))
                (= (first %) (first expr) `str))
         (rest (collapse-strs %))
         (list (collapse-strs %)))
      (rest expr)))
    expr))

(defn compile-html
  "Pre-compile data structures into HTML where possible."
  [& content]
  (collapse-strs `(binding [*reactid* *reactid*]
                    (str ~@(compile-seq content)))))


(comment
  (require '[hisocup.core :refer [html]])
  (require '[hisocup.render :refer [defrender frender]])

   (let [x (frender [] [:div])]
    (html [x "e"]))

  (defrender ff [] [:div [:a] [:abbr] [:address] [:area] [:article] [:aside] [:audio] [:b]  [:base] [:bdi] [:bdo] [:big] [:blockquote] [:br] [:button] [:canvas] [:caption] [:cite] [:code] [:col] [:colgroup] [:data] [:datalist] [:dd] [:del] [:details] [:dfn] [:dialog] [:div] [:dl] [:dt] [:em] [:embed] [:fieldset] [:figcaption] [:figure] [:footer] [:form] [:h1] [:h2] [:h3] [:h4] [:h5] [:h6] [:header] [:hr] [:i] [:iframe] [:img] [:ins] [:kbd] [:keygen] [:label] [:legend] [:li] [:link] [:main] [:map] [:mark] [:menu] [:menuitem] [:meta] [:meter] [:nav] [:noscript] [:object] [:ol] [:optgroup] [:option] [:output] [:p] [:param] [:picture] [:pre] [:progress] [:q] [:rp] [:rt] [:ruby] [:s] [:samp] [:script] [:section] [:select] [:small] [:source] [:span] [:strong] [:style] [:sub] [:summary] [:sup] [:table] [:tbody] [:td] [:tfoot] [:th] [:thead] [:time] [:title] [:tr] [:track] [:u] [:ul] [:var] [:video] [:wbr] [:circle] [:clipPath] [:defs] [:ellipse] [:g] [:line] [:linearGradient] [:mask] [:path] [:pattern] [:polygon] [:polyline] [:radialGradient] [:rect] [:stop] [:svg] [:text] [:tspan]] #_[:div [:input] [:textarea] ])

(defrender ff [] [:div [:a "e"] [:abbr "e"] [:address "e"] [:area "e"] [:article "e"] [:aside "e"] [:audio "e"] [:b "e"] [:base "e"] [:bdi "e"] [:bdo "e"] [:big "e"] [:blockquote "e"] [:br "e"] [:button "e"] [:canvas "e"] [:caption "e"] [:cite "e"] [:code "e"] [:col "e"] [:colgroup "e"] [:data "e"] [:datalist "e"] [:dd "e"] [:del "e"] [:details "e"] [:dfn "e"] [:dialog "e"] [:div "e"] [:dl "e"] [:dt "e"] [:em "e"] [:embed "e"] [:fieldset "e"] [:figcaption "e"] [:figure "e"] [:footer "e"] [:form "e"] [:h1 "e"] [:h2 "e"] [:h3 "e"] [:h4 "e"] [:h5 "e"] [:h6 "e"] [:header "e"] [:hr "e"] [:i "e"] [:iframe "e"] [:img "e"] [:ins "e"] [:kbd "e"] [:keygen "e"] [:label "e"] [:legend "e"] [:li "e"] [:link "e"] [:main "e"] [:map "e"] [:mark "e"] [:menu "e"] [:menuitem "e"] [:meta "e"] [:meter "e"] [:nav "e"] [:noscript "e"] [:object "e"] [:ol "e"] [:optgroup "e"] [:option "e"] [:output "e"] [:p "e"] [:param "e"] [:picture "e"] [:pre "e"] [:progress "e"] [:q "e"] [:rp "e"] [:rt "e"] [:ruby "e"] [:s "e"] [:samp "e"] [:script "e"] [:section "e"] [:select "e"] [:small "e"] [:source "e"] [:span "e"] [:strong "e"] [:style "e"] [:sub "e"] [:summary "e"] [:sup "e"] [:table "e"] [:tbody "e"] [:td "e"] [:tfoot "e"] [:th "e"] [:thead "e"] [:time "e"] [:title "e"] [:tr "e"] [:track "e"] [:u "e"] [:ul "e"] [:var "e"] [:video "e"] [:wbr "e"] [:circle "e"] [:clipPath "e"] [:defs "e"] [:ellipse "e"] [:g "e"] [:line "e"] [:linearGradient "e"] [:mask "e"] [:path "e"] [:pattern "e"] [:polygon "e"] [:polyline "e"] [:radialGradient "e"] [:rect "e"] [:stop "e"] [:svg "e"] [:text "e"] [:tspan "e"]])

  (html [ff])

  (html [:img "e"])

  )

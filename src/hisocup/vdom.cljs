(ns hisocup.vdom
  "Internal functions for compilation."
  (:require-macros [hisocup.vdom :refer [html]]))

(defn compile-html
  "Pre-compile data structures into HTML where possible."
  [& content]
  nil)

(defprotocol Dom
  (make-dom [this args])
  (reconciliate! [this args]))

(defrecord Vtree [tree tree-args]
  Dom
  (make-dom [this args]
    (assert (= (count args) (count tree-args)))
    nil)
  (reconciliate! [this args]
    (assert (= (count args) (count tree-args)))))

(defprotocol VNode
  (make-dom-element [this]))

(deftype StaticVNode [tag attrs content]
  Dom
  (make-dom [this args]
    (goog.dom.createElement tag))
  (reconciliate! [this args]
    (assert false "Why trying to reconciliate a static node !?")))

(deftype StaticTextVNode [text]
  Dom
  (make-dom [this args]
    (goog.dom.createTextNode (str text)))
  (reconciliate! [this args]
    (assert false "Why trying to reconciliate a static node !?")))

(defrecord DynamicVNode [form content]
  Dom
  (make-dom [this args]
    (goog.dom.createElement tag))
  (reconciliate! [this args]
    (assert false "Why trying to reconciliate a static node !?")))

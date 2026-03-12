(ns gesso.components.input
  (:require [gesso.util :refer :all]))

(defn input
  "Single-line input.

  Short form:
    (input {:type \"email\" :attrs {:id \"email\" :name \"email\"}})

  Long form:
    (input {:type \"text\" :attrs {...}})

  Uses Basecoat's .input class."
  [& args]
  (let [[opts _children] (normalize-component-args args)
        {:keys [props class attrs]} (split-opts opts)
        type (or (:type props) "text")]
    (el :input
        {:class (class-names "input" class)
         :type type}
        attrs
        [])))

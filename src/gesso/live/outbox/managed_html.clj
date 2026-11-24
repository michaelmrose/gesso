(ns gesso.live.outbox.managed-html
  "Restricted Hiccup content admission for selected managed fragment responses.
   This is not a general HTML sanitizer or a browser effect/target validator."
  (:require [clojure.string :as str]
            [gesso.http :as http]
            [rum.server-render :as rum]))

(defn- reject! [kind path data]
  (throw (ex-info "Managed fragment HTML content not admitted."
                  (merge {:error/type ::invalid-content
                          :error/kind kind :render-path path}
                         data))))

(defn- name-like? [value]
  (or (string? value) (keyword? value) (symbol? value)))

(defn- safe-selector? [tag]
  ;; Rum emits the tag and shorthand id without escaping. Admit only simple
  ;; element names and identifier shorthand, plus its two fragment forms.
  (and (name-like? tag)
       (or (#{"<>" "*"} (name tag))
           (re-matches #"[A-Za-z][A-Za-z0-9:_-]*(?:[.#][A-Za-z0-9:_-]+)*" (name tag)))))

(def ^:private forbidden-tags
  #{"script" "iframe" "object" "embed" "base" "meta" "link"})

(def ^:private url-attrs
  #{"href" "src" "action" "formaction" "xlink:href" "poster" "background"})

(defn- executable-url? [value]
  (let [normalized (-> (rum/to-str value)
                       (str/replace #"[\x00-\x20]" "")
                       str/lower-case)]
    (or (str/starts-with? normalized "javascript:")
        (str/starts-with? normalized "vbscript:"))))

(defn- check-attrs! [attrs path]
  (doseq [[k v] attrs]
    (when-not (name-like? k)
      (reject! :invalid-attribute-name path {}))
    (let [attribute (str/lower-case (rum/normalize-attr-key k))]
      (when-not (re-matches #"[A-Za-z_:][A-Za-z0-9_.:-]*" attribute)
        (reject! :invalid-attribute-name path {:attribute attribute}))
      (cond
        (str/starts-with? attribute "on")
        (reject! :inline-event-handler path {:attribute attribute})

        (#{"dangerouslysetinnerhtml" "srcdoc" "src-doc"} attribute)
        (reject! :raw-html path {:attribute attribute})

        (and (url-attrs attribute) (executable-url? v))
        (reject! :executable-url path {:attribute attribute})

        (= "style" attribute)
        (when-not (or (nil? v)
                      (and (map? v)
                           (every? #(and (name-like? %)
                                         (re-matches #"[-A-Za-z_][A-Za-z0-9_-]*"
                                                     (rum/normalize-css-key %)))
                                   (keys v))))
          (reject! :invalid-style-key path {:attribute attribute}))))))

(defn- admitted-node [node path]
  (cond
    (vector? node)
    (if (= :rum/nothing (first node))
      [:rum/nothing]
      (let [tag (first node)]
        (when-not (safe-selector? tag)
          (reject! :invalid-element-selector path {}))
        (let [[physical-tag] (rum/parse-selector tag)
              attrs? (or (map? (second node)) (nil? (second node)))
              start (if attrs? (min 2 (count node)) 1)]
          (when (forbidden-tags (str/lower-case physical-tag))
            (reject! :forbidden-element path {:tag physical-tag}))
          (when (map? (second node)) (check-attrs! (second node) path))
          (with-meta
            (into (subvec node 0 start)
                  (map-indexed (fn [index child]
                                 (admitted-node child (conj path (+ start index))))
                               (subvec node start)))
            (meta node)))))

    (seq? node)
    ;; Realize child sequences once before checking/serialization. Preserve
    ;; sequence (not element-vector) semantics and framework metadata.
    (with-meta (apply list (map-indexed (fn [i child]
                                         (admitted-node child (conj path i))) node))
      (meta node))

    (or (nil? node) (string? node) (number? node) (boolean? node)
        (keyword? node) (symbol? node) (char? node) (uuid? node) (inst? node))
    node

    ;; Rum's HtmlRenderer protocol admits opaque raw markup objects; a
    ;; managed renderer must supply inspectable data rather than such objects.
    :else (reject! :unsupported-render-node path {})))

(defn html-response
  "Admit the selected fragment's actual render data, then serialize through
   the existing canonical HTTP boundary (including any installed preflight).
   Framework Hyperscript and HTMX attributes are not authorized by this check;
   response headers, OOB targets, CSP and physical browser admission are separate.
   Full-page shell asset scripts and unmanaged responses use other boundaries."
  [node]
  (http/html-response (admitted-node node [])))

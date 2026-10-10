(ns gesso.live.application-preflight.rendered-request
  "Physical request inspection for supplied Hiccup render trees.

   Internal application-preflight component. This namespace enumerates explicit
   HTMX and native form request coordinates without consulting assembled routes
   or deciding whether a request is a valid Choreo affordance. The caller must
   check attribute-conflicts and then compare request-coordinates with admitted
   semantic operations; neither function is an HTML validator or a guarantee
   that every possible browser request has been enumerated.

   No database, network, browser, or application-assembly effects occur here.
   :render-path values retain the original Hiccup-vector indexes so diagnostic
   locations remain stable when extracted from application-preflight."
  (:require
   [clojure.string :as str]
   [gesso.live.ui :as ui]))

(defn- issue
  [kind message data]
  (merge
   {:kind kind
    :message message}
   data))

(defn- nonblank-string?
  [value]
  (and
   (string? value)
   (not (str/blank? value))))

(def ^:private htmx-request-methods
  [[:hx-get :get]
   [:hx-post :post]
   [:hx-put :put]
   [:hx-patch :patch]
   [:hx-delete :delete]])

(def ^:private html-scanner-attribute-names
  "HTML attribute spellings that affect physical request coordinates. Rum
   serializes both keyword and string Hiccup keys; the preflight scanner must
   not recognize one while overlooking the other."
  #{"hx-get" "hx-post" "hx-put" "hx-patch" "hx-delete"
    "id" "method" "action" "form" "formaction" "formmethod" "type"})

(defn- html-scanner-attrs
  "Read scanner-relevant attributes from either Hiccup key representation.

   For a keyword/string duplicate, keep the keyword for deterministic scanning;
   rendered-scanner-attribute-conflicts independently rejects that ambiguous
   HTML before an application/surface report can be considered valid. Never
   allow the choice of Hiccup attr-key spelling to hide a physical request."
  [attrs]
  (reduce
   (fn [acc attribute]
     (let [attribute-keyword (keyword attribute)]
       (if (and (contains? attrs attribute)
                (not (contains? attrs attribute-keyword)))
         (assoc acc attribute-keyword (get attrs attribute))
         acc)))
   attrs
   html-scanner-attribute-names))

(defn- html-tag-name
  [node]
  (when (vector? node)
    (let [tag (first node)]
      (when (or (keyword? tag) (string? tag))
        (first (str/split (name tag) #"[.#]" 2))))))

(defn attribute-conflicts
  "Reject duplicate keyword/string spellings of request-relevant HTML attrs.

   Rum serializes both keys, creating duplicate physical attributes whose
   interpretation depends on HTML parsing. Fail closed even if both values are
   equal; a canonical Choreo affordance cannot acquire ambiguous request
   coordinates through a second, string-keyed attribute."
  [rendered surface]
  (letfn [(walk [value render-path]
            (lazy-seq
             (concat
              (when (and (vector? value)
                         (html-tag-name value)
                         (map? (second value)))
                (let [attrs (second value)]
                  (for [attribute (sort html-scanner-attribute-names)
                        :when (and (contains? attrs attribute)
                                   (contains? attrs (keyword attribute)))]
                    (issue
                     :rendered-ambiguous-html-attribute
                     "Rendered HTML has both string and keyword spellings of a request-relevant attribute. Rum emits duplicate attributes, so the effective physical request cannot be certified."
                     {:surface surface
                      :render-path render-path
                      :attribute (keyword attribute)}))))
              (when (sequential? value)
                (mapcat
                 (fn [[index child]]
                   (walk child (conj render-path index)))
                 (map-indexed vector value))))))]
    (vec (walk rendered []))))

(defn- html-method
  "HTML forms support GET and POST (or the non-submitting dialog method).
   Unknown/missing values use HTML's default GET rather than being promoted to
   an HTTP method supported only by HTMX."
  [value]
  (let [value' (when (or (keyword? value) (string? value))
                 (str/lower-case (name value)))]
    (case value'
      "post" :post
      "dialog" :dialog
      :get)))

(defn- form-identity
  "Recognize an explicit form ID, including ordinary Hiccup #id shorthand."
  [node attrs]
  (let [tag (first node)
        shorthand (when (or (keyword? tag) (string? tag))
                    (second (re-find #"#([^.#]+)" (name tag))))]
    (or (when (nonblank-string? (:id attrs)) (:id attrs))
        shorthand)))

(defn- rendered-form-contexts
  "Index forms by explicit ID for submit controls using HTML's form= attribute.
   Retain the first occurrence of duplicate IDs, matching HTML's form-owner
   resolution for that ID in the document order of the supplied Hiccup tree."
  [rendered]
  (letfn [(walk [value render-path]
            (lazy-seq
             (concat
              (when (= "form" (html-tag-name value))
                ;; A form written as [:form#owner ...] is valid Hiccup even
                ;; without an attribute map. Detached submitters must still
                ;; resolve its ID and HTML-default GET method.
                (let [attrs (html-scanner-attrs
                             (if (map? (second value)) (second value) {}))]
                  (when-let [id (form-identity value attrs)]
                    [[id {:method (html-method (:method attrs))
                          :path (:action attrs)
                          :render-path render-path}]])))
              (when (sequential? value)
                (mapcat
                 (fn [[index child]]
                   (walk child (conj render-path index)))
                 (map-indexed vector value))))))]
    (reduce
     (fn [forms [id form]]
       (if (contains? forms id) forms (assoc forms id form)))
     {}
     (walk rendered []))))

(defn- native-submitter?
  [node attrs]
  (let [tag (html-tag-name node)
        type' (when (or (keyword? (:type attrs))
                        (string? (:type attrs)))
                (str/lower-case (name (:type attrs))))]
    (case tag
      ;; HTML's button type defaults to Submit not only when omitted, but
      ;; also when its value is invalid (including the empty string).
      "button" (not (contains? #{"button" "reset"} type'))
      "input" (contains? #{"submit" "image"} type')
      false)))

(defn request-coordinates
  "Enumerate explicit physical request coordinates in rendered Hiccup.

   This includes all five named HTMX request methods, native POST forms with
   explicit actions, and submit controls carrying formaction/formmethod. A
   submitter's effective action/method comes from its form owner unless an
   override replaces it; form= may refer to a separate form in this tree.
   A button defaults to type=submit when omitted or invalid, while input
   requires submit/image. Hiccup forms may omit their attribute map. Keyword
   and string keys for request-relevant Hiccup attributes are both recognized;
   duplicate spellings must be rejected by the caller using
   attribute-conflicts; enumerating requests alone does not certify HTML.

   Forms without an explicit action and without a usable submitter formaction,
   JavaScript-initiated requests, and forms outside this supplied render tree
   are not enumerated. This scanner makes no HTML validity, deployment,
   authorization, or completeness claim about surfaces that were not supplied.

   Canonical Gesso Choreo post buttons carry framework-owned metadata;
   arbitrary Hiccup/HTMX does not acquire this metadata from its URL."
  [rendered]
  (let [forms-by-id (rendered-form-contexts rendered)]
    (letfn [(walk [value render-path enclosing-form]
              (lazy-seq
               (let [node? (some? (html-tag-name value))
                     attrs (when node?
                             (html-scanner-attrs
                              (if (map? (second value)) (second value) {})))
                     declared? (and node?
                                    (contains?
                                     (meta value)
                                     ui/choreo-affordance-metadata-key))
                     form? (and node? (= "form" (html-tag-name value)))
                     active-form (if form?
                                   {:method (html-method (:method attrs))
                                    :path (:action attrs)
                                    :render-path render-path}
                                   enclosing-form)
                     submitter? (and node?
                                     (native-submitter? value attrs)
                                     (or (contains? attrs :formaction)
                                         (contains? attrs :formmethod)))
                     submitter-form
                     (when submitter?
                       (if (contains? attrs :form)
                         (get forms-by-id (:form attrs))
                         enclosing-form))
                     submitter-method
                     (when submitter-form
                       (if (contains? attrs :formmethod)
                         (html-method (:formmethod attrs))
                         (:method submitter-form)))
                     submitter-path
                     (when submitter-form
                       (if (contains? attrs :formaction)
                         (:formaction attrs)
                         (:path submitter-form)))]
                 (concat
                  (when node?
                    (concat
                     (for [[attribute method] htmx-request-methods
                           :let [path (get attrs attribute)]
                           :when (nonblank-string? path)]
                       {:method method
                        :path path
                        :request-source attribute
                        :render-path render-path
                        :choreo-operation-declared? declared?})
                     (when (and form?
                                (= :post (:method active-form))
                                (nonblank-string? (:path active-form)))
                       [{:method :post
                         :path (:path active-form)
                         :request-source :native-form
                         :render-path render-path
                         :choreo-operation-declared? declared?}])
                     (when (and submitter?
                                (#{:get :post} submitter-method)
                                (nonblank-string? submitter-path))
                       [{:method submitter-method
                         :path submitter-path
                         :request-source :native-submitter
                         :render-path render-path
                         :form-render-path (:render-path submitter-form)
                         :submitter-overrides
                         (cond-> #{}
                           (contains? attrs :formaction) (conj :formaction)
                           (contains? attrs :formmethod) (conj :formmethod))
                         :choreo-operation-declared? declared?}])))
                  (when (sequential? value)
                    (mapcat
                     (fn [[index child]]
                       (walk child (conj render-path index) active-form))
                     (map-indexed vector value)))))))]
      (vec (walk rendered [] nil)))))

;; Both functions are intentionally narrow. The application preflight caller
;; remains responsible for canonical Choreo-affordance recognition, semantic
;; route matching, rendering gates, and reporting. Do not reimplement any of
;; those higher-layer checks here.

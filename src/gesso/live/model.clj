(ns gesso.live.model
  "Plain-data model layer for Gesso Live.

  This namespace validates and compiles app-owned live metadata:

    {:scopes    {...}
     :graph     {...}
     :fragments {...}}

  It is deliberately not a route/action/page framework.

  It does not generate routes. It does not own Ring handlers. It does not hide
  the HTTP response lifecycle. It compiles to the current Gesso Live runtime
  concepts: live rules, runtime scope maps, current fragment panels, and
  start-sse!.

  Important boundary:

    - :request-policy and :consistency are model metadata in this namespace.
      They are exposed through fragment descriptors and explain-live-app.
      They are not passed through to live/->fragment because current
      gesso.live.ui/->fragment does not define those as runtime keys.

    - render-fragment-response requires an explicit response renderer, either
      via opts {:response ...} or compiled app metadata {:response ...}. This
      namespace does not silently return Hiccup as an HTTP body.

    - fragment render functions receive one argument: the query result data map.
      If a view needs ctx, id, user/session info, or route data, the query
      function should put that information into the returned data map.

    - Vars are dereferenced at call time for REPL friendliness. compile-live-app
      validates that a Var contains a function at compile time, but later REPL
      redefinitions can still fail at runtime if the Var stops containing a
      compatible function."
  (:require
   [clojure.string :as str]
   [gesso.live.core :as live]
   [malli.core :as m]
   [malli.error :as me]))

;; -----------------------------------------------------------------------------
;; Generic helpers
;; -----------------------------------------------------------------------------

(defn present?
  [x]
  (cond
    (string? x) (not (str/blank? x))
    (coll? x)   (seq x)
    :else       (some? x)))

(defn callable?
  "True for functions and Vars that currently contain functions.

  We intentionally do not use ifn? here. Keywords, maps, sets, and vectors are
  invokable in Clojure, but a live fragment :query or :render should be an
  actual function-like value."
  [x]
  (or (fn? x)
      (and (var? x)
           (fn? @x))))

(defn call
  "Invoke a function or a Var containing a function."
  [f & args]
  (apply (if (var? f) @f f) args))

(defn validation-error
  ([kind path message]
   (validation-error kind path message {}))
  ([kind path message data]
   {:kind kind
    :path (vec path)
    :message message
    :data data}))

(defn throw-validation-errors!
  [label errors]
  (when (seq errors)
    (throw
     (ex-info
      (str label " failed validation.")
      {:error/type :gesso.live/validation-failed
       :errors (vec errors)}))))

;; -----------------------------------------------------------------------------
;; Malli schemas
;; -----------------------------------------------------------------------------

(def callable-schema
  [:fn
   {:error/message "must be a function or a Var containing a function"}
   callable?])

(def scope-descriptor-schema
  [:and
   [:map
    [:name {:optional true} keyword?]
    [:topic keyword?]
    [:id-key keyword?]
    [:label {:optional true} string?]
    [:description {:optional true} string?]
    [:public? {:optional true} boolean?]
    [:authorized? {:optional true} callable-schema]]
   [:fn
    {:error/message "scope must have :authorized? unless :public? is true"}
    (fn [{:keys [public? authorized?]}]
      (or public? authorized?))]])

(def graph-target-schema
  ;; :id-key is intentionally optional at the structural layer.
  ;;
  ;; Known scope targets inherit :id-key during normalization. Unknown scope
  ;; targets should be reported by semantic validation as :unknown-scope rather
  ;; than getting masked by a Malli missing-key error.
  [:map
   [:scope keyword?]
   [:id-key {:optional true} keyword?]
   [:optional? {:optional true} boolean?]
   [:when {:optional true} callable-schema]
   [:label {:optional true} string?]])

(def fragment-descriptor-schema
  [:map
   [:name {:optional true} keyword?]
   [:scope keyword?]
   [:id-fn callable-schema]
   [:query callable-schema]
   [:render callable-schema]
   [:swap {:optional true} keyword?]
   [:consistency {:optional true} [:or keyword? map?]]
   [:request-policy {:optional true} [:or keyword? map?]]])

(def live-app-schema
  [:map
   [:scopes [:map-of keyword? scope-descriptor-schema]]
   [:graph [:map-of keyword? [:vector graph-target-schema]]]
   [:fragments [:map-of keyword? fragment-descriptor-schema]]
   [:response {:optional true} callable-schema]])

(defn- malli-error-path
  [problem]
  ;; Malli's :in is the data path. :path is the schema path. For user-facing
  ;; boot errors, the data path is usually what we want.
  (vec (or (:in problem)
           (:path problem)
           [])))

(defn- malli-error-message
  [problem]
  (or (get-in problem [:properties :error/message])
      "Live app metadata does not match schema."))

(defn malli-errors
  "Return normalized error maps for Malli structural validation problems.

  The raw Malli problem and the humanized tree are preserved under :data, but
  each individual problem also gets a normal :path and :message."
  [schema value]
  (when-let [explanation (m/explain schema value)]
    (let [humanized (me/humanize explanation)]
      (mapv
       (fn [problem]
         (validation-error
          :malli/schema
          (malli-error-path problem)
          (malli-error-message problem)
          {:schema-path (:path problem)
           :value (:value problem)
           :humanized humanized
           :problem problem}))
       (:errors explanation)))))

;; -----------------------------------------------------------------------------
;; Normalization
;; -----------------------------------------------------------------------------

(defn default-label
  [k]
  (-> k name (str/replace #"-" " ")))

(defn default-fragment-id-fn
  [fragment-name]
  (fn [id]
    (str (name fragment-name) "-" id)))

(defn normalize-scope-entry
  [scope-name scope]
  (if-not (map? scope)
    scope
    (cond-> (assoc scope :name scope-name)
      (nil? (:label scope))
      (assoc :label (default-label scope-name)))))

(defn normalize-scopes
  [scopes]
  (if-not (map? scopes)
    scopes
    (into {}
          (map (fn [[scope-name scope]]
                 [scope-name (normalize-scope-entry scope-name scope)]))
          scopes)))

(defn normalize-target
  "Normalize one graph target.

  Supported target forms:

    :store-queue
    [:store-queue :store/id]
    {:scope :store-queue}
    {:scope :store-queue :id-key :store/id}

  If :id-key is omitted and the scope is declared, it is inherited from the
  declared scope. If the target is malformed, leave it in a shape Malli will
  reject. If the target references an unknown scope, leave :id-key absent so the
  semantic validation pass can report :unknown-scope clearly.

  Important: only inherit :id-key when the declared scope actually has a keyword
  id-key. Do not assoc :id-key nil, because that masks the semantic
  :missing-id-key error."
  [scopes target]
  (let [target'
        (cond
          (keyword? target)
          {:scope target}

          (and (vector? target)
               (= 2 (count target)))
          {:scope (first target)
           :id-key (second target)}

          (map? target)
          target

          :else
          target)]
    (if-not (map? target')
      target'
      (let [scope-desc (get scopes (:scope target'))]
        (cond-> target'
          (and scope-desc
               (nil? (:id-key target'))
               (keyword? (:id-key scope-desc)))
          (assoc :id-key (:id-key scope-desc)))))))

(defn normalize-graph
  [scopes graph]
  (if-not (map? graph)
    graph
    (into {}
          (map
           (fn [[event targets]]
             [event
              (if (vector? targets)
                (mapv #(normalize-target scopes %) targets)
                targets)]))
          graph)))

(defn normalize-fragment-entry
  [fragment-name fragment]
  (if-not (map? fragment)
    fragment
    (cond-> (assoc fragment :name fragment-name)
      (nil? (:id-fn fragment))
      (assoc :id-fn (default-fragment-id-fn fragment-name))

      (nil? (:swap fragment))
      (assoc :swap :outerHTML)

      (nil? (:request-policy fragment))
      (assoc :request-policy :default-safe-live-fragment))))

(defn normalize-fragments
  [fragments]
  (if-not (map? fragments)
    fragments
    (into {}
          (map (fn [[fragment-name fragment]]
                 [fragment-name (normalize-fragment-entry fragment-name fragment)]))
          fragments)))

(defn normalize-live-app
  [app]
  (let [scopes'    (normalize-scopes (:scopes app))
        graph'     (normalize-graph (if (map? scopes') scopes' {}) (:graph app))
        fragments' (normalize-fragments (:fragments app))]
    (cond-> app
      (contains? app :scopes)
      (assoc :scopes scopes')

      (contains? app :graph)
      (assoc :graph graph')

      (contains? app :fragments)
      (assoc :fragments fragments'))))

;; -----------------------------------------------------------------------------
;; Semantic validation
;; -----------------------------------------------------------------------------

(defn duplicate-topic-errors
  [scopes]
  (when (map? scopes)
    (let [topic->scopes
          (reduce-kv
           (fn [acc scope-name scope]
             (if-let [topic (:topic scope)]
               (update acc topic (fnil conj []) scope-name)
               acc))
           {}
           scopes)]
      (for [[topic names] topic->scopes
            :when (< 1 (count names))]
        (validation-error
         :duplicate-topic
         [:scopes]
         "Multiple scopes declare the same :topic. Use a single scope name or make the topic split explicit."
         {:topic topic
          :scopes (vec names)})))))

(defn graph-unknown-scope-errors
  [{:keys [scopes graph]}]
  (when (and (map? scopes) (map? graph))
    (let [known (set (keys scopes))]
      (for [[event targets] graph
            :when (vector? targets)
            [idx target] (map-indexed vector targets)
            :when (map? target)
            :let [scope-name (:scope target)]
            :when (and scope-name (not (contains? known scope-name)))]
        (validation-error
         :unknown-scope
         [:graph event idx :scope]
         "Graph target references a scope that is not declared."
         {:scope scope-name
          :known-scopes (sort known)})))))

(defn fragment-unknown-scope-errors
  [{:keys [scopes fragments]}]
  (when (and (map? scopes) (map? fragments))
    (let [known (set (keys scopes))]
      (for [[fragment-name fragment] fragments
            :when (map? fragment)
            :let [scope-name (:scope fragment)]
            :when (and scope-name (not (contains? known scope-name)))]
        (validation-error
         :unknown-scope
         [:fragments fragment-name :scope]
         "Fragment references a scope that is not declared."
         {:scope scope-name
          :known-scopes (sort known)})))))

(defn graph-missing-id-key-errors
  [{:keys [scopes graph]}]
  ;; After normalization, known scopes should have inherited :id-key. If a known
  ;; target still lacks a keyword id-key, something is inconsistent in the app
  ;; metadata.
  (when (and (map? scopes) (map? graph))
    (let [known (set (keys scopes))]
      (for [[event targets] graph
            :when (vector? targets)
            [idx target] (map-indexed vector targets)
            :when (map? target)
            :let [scope-name (:scope target)]
            :when (and (contains? known scope-name)
                       (not (keyword? (:id-key target))))]
        (validation-error
         :missing-id-key
         [:graph event idx :id-key]
         "Graph target is missing :id-key and could not inherit it from the declared scope."
         {:scope scope-name
          :target target})))))

(defn semantic-errors
  [app]
  (vec
   (concat
    (duplicate-topic-errors (:scopes app))
    (graph-unknown-scope-errors app)
    (fragment-unknown-scope-errors app)
    (graph-missing-id-key-errors app))))

(defn validate-normalized-live-app
  "Return validation errors for an already-normalized live app map."
  [app]
  (vec
   (concat
    (malli-errors live-app-schema app)
    (semantic-errors app))))

(defn validate-live-app
  "Return validation errors for app-owned live metadata.

  This performs normalization before validation so shorthand graph targets and
  fragment defaults are accepted."
  [app]
  (validate-normalized-live-app (normalize-live-app app)))

;; -----------------------------------------------------------------------------
;; Scope construction and graph expansion
;; -----------------------------------------------------------------------------

(defn event-topic
  [change]
  (or (:topic change)
      (:event/type change)
      (:change/topic change)))

(defn scope-descriptor
  [compiled scope-name]
  (or (get-in compiled [:scopes scope-name])
      (throw
       (ex-info
        "Unknown live scope."
        {:scope scope-name
         :known-scopes (sort (keys (:scopes compiled)))}))))

(defn scope
  "Construct a runtime scope map from a compiled app, scope name, and id.

  The resulting map is compatible with the current Gesso Live runtime."
  [compiled scope-name id]
  (let [{:keys [topic label]} (scope-descriptor compiled scope-name)]
    {:topic topic
     :id id
     :gesso.live/scope scope-name
     :gesso.live/scope-label label}))

(defn scope-key
  [scope]
  [(:topic scope) (:id scope)])

(defn target-active?
  [ctx change target]
  (if-let [pred (:when target)]
    (boolean (call pred ctx change))
    true))

(defn scope-from-change
  [compiled target change]
  (let [scope-name (:scope target)
        id-key     (:id-key target)
        optional?  (:optional? target)
        id         (get change id-key)]
    (cond
      (present? id)
      (scope compiled scope-name id)

      optional?
      nil

      :else
      (throw
       (ex-info
        "Cannot expand change to live scope; missing id-key."
        {:target target
         :id-key id-key
         :change change})))))

(defn dedupe-scopes
  "Return scopes deduped by [:topic :id], preserving first occurrence order."
  [scopes]
  (loop [remaining scopes
         seen #{}
         out []]
    (if-let [scope (first remaining)]
      (let [k (scope-key scope)]
        (if (contains? seen k)
          (recur (rest remaining) seen out)
          (recur (rest remaining) (conj seen k) (conj out scope))))
      out)))

(defn expand-change
  "Expand one primary app change into concrete runtime scopes."
  ([compiled change]
   (expand-change compiled nil change))
  ([compiled ctx change]
   (let [topic (event-topic change)
         targets (get-in compiled [:graph topic] ::missing)]
     (when (= ::missing targets)
       (throw
        (ex-info
         "No invalidation graph rule for change topic."
         {:topic topic
          :known-events (sort (keys (:graph compiled)))})))
     (->> targets
          (filter #(target-active? ctx change %))
          (map #(scope-from-change compiled % change))
          (remove nil?)
          dedupe-scopes))))

(defn expand-changes
  "Expand many primary app changes into concrete runtime scopes.

  This is pure expansion/introspection. It does not submit to the dispatcher.
  Core runtime helpers remain responsible for async dispatch."
  ([compiled changes]
   (expand-changes compiled nil changes))
  ([compiled ctx changes]
   (dedupe-scopes
    (mapcat #(expand-change compiled ctx %) changes))))

(defn compile-live-rules
  [compiled]
  (mapv
   (fn [[topic _targets]]
     {:when-topic topic
      :expand
      (fn [ctx change]
        (expand-change compiled ctx change))})
   (:graph compiled)))

(defn compile-live-app
  "Normalize, validate, and compile live metadata.

  Returns the normalized app map with:

    :compiled?  true
    :live-rules current Gesso Live rule vector

  Throws ex-info with :gesso.live/validation-failed if validation fails."
  [app]
  (let [app'   (normalize-live-app app)
        errors (validate-normalized-live-app app')]
    (throw-validation-errors! "Gesso Live app model" errors)
    (let [compiled (assoc app' :compiled? true)]
      (assoc compiled
             :live-rules (compile-live-rules compiled)))))

(defn live-rules
  [compiled]
  (:live-rules compiled))

;; -----------------------------------------------------------------------------
;; Authorization
;; -----------------------------------------------------------------------------

(defn authorized-for-scope?
  [compiled ctx scope-name id]
  (let [{:keys [public? authorized?]} (scope-descriptor compiled scope-name)]
    (or public?
        (boolean (call authorized? ctx id)))))

(defn require-scope-authorized!
  [compiled ctx scope-name id]
  (when-not (authorized-for-scope? compiled ctx scope-name id)
    (throw
     (ex-info
      "Not authorized for live scope."
      {:error/type :gesso.live/not-authorized
       :scope scope-name
       :id id})))
  true)

;; -----------------------------------------------------------------------------
;; Fragment helpers
;; -----------------------------------------------------------------------------

(defn fragment-descriptor
  [compiled fragment-name]
  (or (get-in compiled [:fragments fragment-name])
      (throw
       (ex-info
        "Unknown live fragment."
        {:fragment fragment-name
         :known-fragments (sort (keys (:fragments compiled)))}))))

(defn fragment-scope-name
  [compiled fragment-name]
  (:scope (fragment-descriptor compiled fragment-name)))

(defn fragment-dom-id
  [compiled fragment-name id]
  (call (:id-fn (fragment-descriptor compiled fragment-name)) id))

(defn fragment-scope-instance
  [compiled fragment-name id]
  (scope compiled (fragment-scope-name compiled fragment-name) id))

(defn query-fragment
  [compiled ctx fragment-name id]
  (call (:query (fragment-descriptor compiled fragment-name)) ctx id))

(defn render-fragment-node
  "Run a fragment query and pass the resulting data map to the render function.

  Render functions intentionally receive only data. If a renderer needs the
  component id or selected request/session data, the query function should include
  those values in the returned data map."
  [compiled ctx fragment-name id]
  (let [fragment (fragment-descriptor compiled fragment-name)
        data     (call (:query fragment) ctx id)]
    (call (:render fragment) data)))

(defn response-fn
  [compiled opts]
  (or (:response opts)
      (:response compiled)
      (throw
       (ex-info
        "gesso.live.model/render-fragment-response requires a response renderer."
        {:error/type :gesso.live.model/missing-response-renderer
         :hint "Pass {:response gesso.core/html-response} or include :response in compile-live-app data."}))))

(defn render-fragment-response
  "Render a named fragment and return a Ring response.

  Options:

    :response    node -> Ring response. Required unless compiled app metadata
                 contains :response.
    :authorize?  Defaults to true."
  ([compiled ctx fragment-name id]
   (render-fragment-response compiled ctx fragment-name id {}))
  ([compiled ctx fragment-name id {:keys [authorize?] :or {authorize? true} :as opts}]
   (when authorize?
     (require-scope-authorized!
      compiled
      ctx
      (fragment-scope-name compiled fragment-name)
      id))
   ((response-fn compiled opts)
    (render-fragment-node compiled ctx fragment-name id))))

(defn fragment->runtime-fragment
  "Build the current Gesso Live UI fragment descriptor from a model fragment.

  The app must pass explicit URLs. This function does not own routing.

  Note: :request-policy and :consistency are intentionally not passed to
  live/->fragment in this first version because current gesso.live.ui/->fragment
  does not define those as runtime keys. They remain model metadata available via
  fragment-descriptor and explain-live-app."
  [compiled fragment-name id {:keys [fragment-url stream-url]}]
  (when-not (present? fragment-url)
    (throw
     (ex-info
      "fragment-url is required to build a live fragment panel."
      {:fragment fragment-name
       :id id})))
  (when-not (present? stream-url)
    (throw
     (ex-info
      "stream-url is required to build a live fragment panel."
      {:fragment fragment-name
       :id id})))
  (let [fragment (fragment-descriptor compiled fragment-name)]
    (live/->fragment
     {:id (fragment-dom-id compiled fragment-name id)
      :src fragment-url
      :stream-url stream-url
      :subscription (fragment-scope-instance compiled fragment-name id)
      :swap (:swap fragment :outerHTML)})))

(defn fragment-panel
  "Render a client-side live fragment panel using the existing Gesso Live UI
  helper. Routes remain explicit in the app."
  [compiled fragment-name id opts]
  (live/fragment-panel
   (fragment->runtime-fragment compiled fragment-name id opts)))

(defn start-fragment-stream
  "Start an SSE stream for a named fragment.

  This is only a convenience wrapper around live/start-sse!. The app still owns
  the route."
  ([compiled ctx system fragment-name id]
   (start-fragment-stream compiled ctx system fragment-name id {}))
  ([compiled ctx system fragment-name id opts]
   (require-scope-authorized!
    compiled
    ctx
    (fragment-scope-name compiled fragment-name)
    id)
   (:response
    (live/start-sse!
     system
     (fragment-scope-instance compiled fragment-name id)
     opts))))

;; -----------------------------------------------------------------------------
;; Introspection
;; -----------------------------------------------------------------------------

(defn explain-live-app
  [compiled]
  {:compiled? (:compiled? compiled)

   :scopes
   (into {}
         (map (fn [[k scope]]
                [k (select-keys scope
                                [:name
                                 :topic
                                 :id-key
                                 :label
                                 :description
                                 :public?])]))
         (:scopes compiled))

   :graph
   (:graph compiled)

   :fragments
   (into {}
         (map (fn [[k fragment]]
                [k (select-keys fragment
                                [:name
                                 :scope
                                 :swap
                                 :consistency
                                 :request-policy])]))
         (:fragments compiled))

   :events
   (sort (keys (:graph compiled)))

   :live-rules
   (mapv :when-topic (:live-rules compiled))})

(comment
  ;; Minimal valid app:
  ;;
  ;; (def app
  ;;   {:response gesso.core/html-response
  ;;    :scopes
  ;;    {:store-queue
  ;;     {:topic :humanhelp/store-queue
  ;;      :id-key :store/id
  ;;      :authorized? (fn [_ctx _store-id] true)}}
  ;;
  ;;    :graph
  ;;    {:task/assigned [:store-queue]}
  ;;
  ;;    :fragments
  ;;    {:store-queue
  ;;     {:scope :store-queue
  ;;      :query (fn [_ctx store-id]
  ;;               {:fragment/id (str "store-queue-" store-id)
  ;;                :store/id store-id})
  ;;      :render (fn [{:keys [fragment/id store/id]}]
  ;;                [:div {:id id}
  ;;                 "Store " id])}}})
  ;;
  ;; (def compiled (compile-live-app app))
  ;;
  ;; (expand-change compiled
  ;;                {:topic :task/assigned
  ;;                 :store/id "store-42"})
  ;; => [{:topic :humanhelp/store-queue
  ;;      :id "store-42"
  ;;      :gesso.live/scope :store-queue
  ;;      :gesso.live/scope-label "store queue"}]
  ;;
  ;; Typo protection:
  ;;
  ;; (compile-live-app
  ;;  (assoc app :graph {:task/assigned [{:scope :store-qeue}]}))
  ;;
  ;; => throws ex-info with :error/type :gesso.live/validation-failed
  ;;    and an :unknown-scope error at [:graph :task/assigned 0 :scope].
  )

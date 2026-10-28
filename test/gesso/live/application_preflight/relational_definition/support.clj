(ns gesso.live.application-preflight.relational-definition.support
  "Shared pure and controlled test constructions for relational definition regression suites.

   Test-only API; all helpers are inherited verbatim except private visibility.
   Mutable authority handles and browser artifacts remain per-test."
  (:require
   [clojure.test :refer [is]]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.operation-family-correspondence :as correspondence]
   [gesso.live.application-preflight.relational-rule-coverage :as relations]
   [gesso.live.application-preflight.relational-definition :as sut]
   [gesso.model.command :as command]))

(def version-keys
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def counter-key [:counters :one])
(def initial-state
  {counter-key {:xt/id :one
                :revision 0
                :created-at :t0
                :updated-at :t0
                :n 0
                :parent nil
                :active? true}})

(def parent-rule
  {:id :counter/parent-active
   :source-kind :counters
   :source-where {}
   :reference-attr :parent
   :allow-unset? true
   :target-kind :counters
   :target-where {:active? true}})

(defn family
  ([id] (family id false))
  ([id invalid-parent?]
   {:id id
    :principals [:helper/a]
    :arguments [{:delta 1}]
    :enabled? (fn [_ _ _] true)
    :prepare (fn [state _ _]
               (let [before (get state counter-key)
                     after (cond-> (-> before
                                       (update :revision inc)
                                       (update :n inc)
                                       (assoc :updated-at :t1))
                             invalid-parent? (assoc :parent :missing))]
                 {:commands [(command/update-command
                              :counters :increment before after version-keys)]
                  :guards []
                  :emit false}))}))

(defn relational-model
  ([] (relational-model false))
  ([invalid-parent?]
   {:initial-state initial-state
    :families [(family :request/claim invalid-parent?)
               (family :request/reassign)
               (family :request/cancel)]
    :declarations [parent-rule]
    :options {:max-depth 2 :max-transitions 50}}))

(defn definition
  [application-options]
  {:application-options application-options
   :relational-model (relational-model)})

(defn error-data [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      (ex-data error))))

(defn error-kind [f]
  (:error/kind (error-data f)))

(def unexercised-parent-rule
  (assoc parent-rule
         :id :counter/parent-at-ninety-nine
         :source-where {:n 99}))

(defn relational-audit
  [handle]
  (:semantic-audit (sut/explain handle)))

(defn finite-exploration [handle]
  (get-in (sut/explain handle)
          [:semantic-audit :relational-assurance :finite-exploration]))

(defn reachable-model-facts [handle]
  (get-in (relational-audit handle)
          [:business-rule-catalog :reachable-state-rule-coverage]))

(defn source-at
  [id n]
  (assoc parent-rule :id id :source-where {:n n}))

(defn modeled-operation-exercise
  [handle]
  (get-in (relational-audit handle)
          [:relational-assurance :modeled-operation-exercise]))

(def expected-operation-count-keys
  #{:enablement-checks :disabled-checks :enabled-attempts
    :committed-attempts :rejected-attempts})

(defn operation-attempt-audit
  [handle]
  (get-in (relational-audit handle) [:relational-assurance]))

(defn outcome-audit [handle]
  (get-in (sut/explain handle) [:semantic-audit :relational-assurance]))

(defn stale-guard-definition
  "Use a valid but unsatisfied guard against a separate, existing document.
   This exercises transaction rejection, not a malformed command or an
   undeclared relational invariant."
  [base]
  (let [aux-key [:counters :aux]
        aux-doc (assoc (get initial-state counter-key) :xt/id :aux)
        stale (assoc aux-doc :revision 99 :updated-at :stale)
        guard (command/guard :counters stale version-keys)]
    (-> base
        (assoc-in [:relational-model :options :max-depth] 1)
        (assoc-in [:relational-model :initial-state aux-key] aux-doc)
        (update-in [:relational-model :families 0 :prepare]
                   (fn [prepare]
                     (fn [& args]
                       (update (apply prepare args) :guards conj guard)))))))

(defn modeled-transition-tree [handle]
  (get-in (relational-audit handle)
          [:relational-assurance :modeled-transition-tree]))

(defn with-stubbed-transition-comparison
  [make-declaration f]
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [physical-comparison (atom nil)
            comparator-calls (atom 0)
            comparator-evidence (atom [])]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (fn [evidence]
                          (swap! comparator-calls inc)
                          (swap! comparator-evidence conj evidence)
                          @physical-comparison))]
          (let [handle (sut/construct! (make-declaration options))
                tree (modeled-transition-tree handle)]
            (f {:handle handle
                :tree tree
                :physical-comparison physical-comparison
                :comparator-calls comparator-calls
                :comparator-evidence comparator-evidence})))))))

(defn comparison-for-edge
  [edge]
  {:status :matched
   :reason :exact-observed-domain-plan-match
   :operation (:registered-operation-id edge)
   :family-instance-id (:model-instance-id edge)
   :physical-status (:status edge)})

(defn prefix-context-for!
  [handle tree comparison-result edge]
  (reset! comparison-result (comparison-for-edge edge))
  (:modeled-transition-context (sut/compare! handle (Object.))))

(def matching-root-observation
  {:status :matches-captured-pretransaction-state
   :relation :exact-clojure-state-map-equality
   :scope :declared-model-root-vs-controlled-dependency-snapshot})

(defn root-step-for!
  [handle physical-comparison edge comparison]
  (reset! physical-comparison (merge (comparison-for-edge edge)
                                     {:declared-root-state-comparison
                                      matching-root-observation}
                                     comparison))
  (get-in (sut/compare! handle (Object.))
          [:modeled-transition-context :root-step-correspondence]))

(def v872-matched-poststate
  {:status :matches-captured-posttransaction-dependencies
   :scope :one-controlled-serial-root-step-vs-captured-dependencies
   :relation :exact-clojure-domain-document-map-equality
   :poststate-data-exposed? false
   :pinned-xtdb-snapshot :not-established
   :production-trace-refinement :not-established})

(defn v872-root-poststate-for!
  [handle physical-comparison edge comparison]
  (:captured-posttransaction-dependency-comparison
   (root-step-for! handle physical-comparison edge comparison)))

(def v874-established-dimensions
  #{:same-admitted-model-choice
    :definitive-matching-outcome
    :modeled-root-vs-observed-pretransaction-dependencies
    :submitted-domain-plan-agreement
    :modeled-root-step-vs-observed-posttransaction-dependencies})

(defn v874-local-for!
  [handle physical-comparison edge overrides]
  (:local-step-observation
   (root-step-for! handle physical-comparison edge
                   (merge {:declared-root-poststate-comparison
                           v872-matched-poststate}
                          overrides))))

(defn with-sequence-comparator
  [make-declaration f]
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [responses (atom {})
            calls (atom [])]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (fn [evidence]
                          (swap! calls conj evidence)
                          (get @responses evidence)))]
          (let [handle (sut/construct! (make-declaration options))]
            (f {:handle handle
                :tree (modeled-transition-tree handle)
                :responses responses
                :calls calls
                :artifact-path artifact-path})))))))

(defn set-sequence-response! [responses evidence edge]
  (swap! responses assoc evidence (comparison-for-edge edge)))

(defn sequence-root-edge [tree]
  (first (filter #(= (:root-node-id tree) (:parent-node-id %))
                 (:edges tree))))

(defn sequence-child-edge [tree parent]
  (first (filter #(= (:node-id parent) (:parent-node-id %))
                 (:edges tree))))

(defn with-v880-adjacency
  ([f] (with-v880-adjacency definition f))
  ([make-definition f]
   (with-sequence-comparator
     make-definition
     (fn [{:keys [handle tree responses calls artifact-path]}]
       (let [first-edge (sequence-root-edge tree)
             second-edge (sequence-child-edge tree first-edge)
             a (Object.)
             b (Object.)
             c (Object.)
             pair-calls (atom [])
             pair-response (atom {:status :captured-domain-documents-match
                                  :dependency-key-set-comparison :same-captured-keys})]
         (set-sequence-response! responses a first-edge)
         (set-sequence-response! responses b second-edge)
         (set-sequence-response! responses c first-edge)
         (with-redefs [correspondence/compare-observation-pair!
                       (fn [assembly earlier later]
                         (swap! pair-calls conj {:assembly assembly
                                                 :earlier earlier :later later})
                         @pair-response)]
           (f {:handle handle :tree tree :first-edge first-edge
               :second-edge second-edge :a a :b b :c c
               :responses responses :calls calls :pair-calls pair-calls
               :pair-response pair-response :artifact-path artifact-path})))))))

(defn with-v882-root-anchor
  ([f] (with-v882-root-anchor definition f))
  ([make-definition f]
   (with-v880-adjacency
     make-definition
     (fn [{:keys [responses a first-edge] :as environment}]
       (swap! responses update a merge
              {:declared-root-state-comparison matching-root-observation
               :declared-root-poststate-comparison v872-matched-poststate})
       (is (= (:model-instance-id first-edge)
              (:family-instance-id (get @responses a))))
       (f environment)))))

(defn v882-anchor [report]
  (:initial-root-step-and-captured-adjacency report))

(def v886-matching-captured-local-step
  {:status :matches-captured-posttransaction-dependencies
   :scope :one-controlled-serial-step-from-captured-pretransaction-dependencies
   :relation :exact-clojure-domain-document-map-equality
   :input-source :controlled-observation-pretransaction-dependencies
   :declared-model-root-equality-required? false
   :poststate-data-exposed? false
   :additional-model-callbacks? false
   :additional-xtdb-reads? false
   :pinned-xtdb-snapshot :not-established
   :complete-dependency-coverage :not-established
   :equality-with-reachable-non-root-model-state :not-established
   :authenticated-execution-history :not-established
   :production-trace-refinement :not-established})

(defn v886-local-joint [report]
  (:captured-local-steps-and-adjacency report))

(defn v886-set-local! [responses evidence local]
  (swap! responses assoc-in [evidence :captured-local-poststate-comparison]
         local))

(defn v892-root-and-committed-claim-child
  [handle]
  (let [tree (modeled-transition-tree handle)
        root (:root-node-id tree)
        child (first (filter (fn [edge]
                               (and (= root (:parent-node-id edge))
                                    (= :request/claim
                                       (:registered-operation-id edge))
                                    (= :committed (:status edge))))
                             (:edges tree)))]
    (when-not child
      (throw (ex-info "Fixture has no admitted committed claim root edge"
                      {:error/kind :invalid-v892-test-fixture})))
    {:root root :child-id (:node-id child) :edge child}))


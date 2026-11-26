(ns gesso.assurance.reference-mapped-evidence
  "Source-relative v4 witness correspondence; not an installed admission
   or a physical transaction attestation."
  (:require [gesso.assurance.proof.mapped-prestate-lowering :as lowering]
            [gesso.assurance.proof.typed-value-semantics :as values]
            [gesso.assurance.reference-mapped-writes :as writes]
            [gesso.assurance.reference-mapped-input :as input]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.reference-mapped-decoder :as decoder]
            [gesso.assurance.reference-mapped-publication :as publication]
            [gesso.assurance.reference-mapped-residual :as residual]
            [gesso.assurance.reference-provider :as provider]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.assurance.subject :as subject]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.inductive-plan :as induction]))

(def profile :selected-mapped-evidence-v2)
(def ^:private guard-profile :gesso.model.mapped-frame/xtdb-2.2.0-beta1-v5)

(defn- reject! [reason]
  (throw (ex-info "Unsupported mapped evidence expectation." {::unsupported true :reason reason})))

(defn- digest [id interface]
  (:fingerprint (subject/describe-subject {:kind :rule :id id :interface interface})))

(defn- fingerprint? [v]
  (and (string? v) (boolean (re-matches #"sha256:[0-9a-f]{64}" v))))

(defn- encode-value [v]
  (case (values/value-kind! v)
    :nil ["nil"]
    :boolean ["boolean" (str v)]
    :integer ["integer" (str v)]
    :string ["string" v]
    :keyword (if (namespace v) ["qualified-keyword" (namespace v) (name v)]
                              ["keyword" (name v)])))

(defn- valuation [m]
  (mapv (fn [k] (into [(namespace k) (name k)] (encode-value (get m k))))
        (sort-by (juxt namespace name) (keys m))))

(defn- prefix-value [v]
  (cond
    (vector? v) [:vector (mapv prefix-value v)]
    (uuid? v) [:uuid (str v)]
    (integer? v) [:integer v]
    (boolean? v) [:boolean v]
    (keyword? v) [:keyword v]
    (string? v) [:string v]
    :else (reject! :unsupported-prefix-value)))

(defn derive-record!
  "Derive the complete unbound v4 record using the original construction,
   exact independently checked prefix and the forward typed transition.
   Catalog/induction fingerprints and input selectors are supplied premises;
   selector evaluation and installed admission identity are not checked.
   The shared subject encoder and mapping constructor remain dependencies."
  [original targets docs journal prefix spec
   {:keys [catalog-fingerprint induction-fingerprint selectors] :as context}]
  (when-not (and (map? context) (nil? (meta context))
                 (= #{:catalog-fingerprint :induction-fingerprint :selectors} (set (keys context)))
                 (fingerprint? catalog-fingerprint) (fingerprint? induction-fingerprint)
                 (map? selectors) (nil? (meta selectors)))
    (reject! :invalid-evidence-context))
  (let [checked (lowering/check-prefix! original targets docs journal prefix)
        mapping (plan/check-target-mapping! original targets)
        {:keys [before inputs after]} (writes/derive-transition! original targets docs journal spec)]
    (when-not (and (= 5 (:version checked)) (= :checked-exact-prefix (:status checked)))
      (reject! :unsupported-prefix-profile))
    {:xt/id (resolution/record-id (:binding spec)) :profile-version 4
     :guard-profile guard-profile
     :guard-prefix-fingerprint (digest :gesso.model.mapped-frame/guard-prefix
                                       {:profile guard-profile :prefix (prefix-value prefix)})
     :catalog-fingerprint catalog-fingerprint :induction-fingerprint induction-fingerprint
     :mapping-fingerprint (:fingerprint mapping)
     :transition-fingerprint (:transition-fingerprint mapping)
     :alternative (get-in original [:subject :id])
     :input-derivation-fingerprint (digest :gesso.model.inductive-plan/mapped-input-derivation
                                          {:selectors selectors})
     :prestate-witness (valuation before) :input-witness (valuation inputs)
     :witness-fingerprint (digest :gesso.model.inductive-plan/mapped-transition-witness
                                  {:before before :inputs inputs :after after})
     :poststate-fingerprint (digest :gesso.model.inductive-plan/mapped-poststate {:state after})}))

(defn check-record!
  "Check an unbound record, without reading candidate values into the
   derivation. Bound envelope records require separate envelope validation."
  [original targets docs journal prefix spec context record]
  (let [base {:profile profile :authority :none :certificate :not-issued
              :provider-refinement :not-established}]
    (try
      (if (= (derive-record! original targets docs journal prefix spec context) record)
        (assoc base :status :matched :scope :source-relative-unbound-record)
        (assoc base :status :mismatch :reason :transition-evidence-record-mismatch))
      (catch clojure.lang.ExceptionInfo _
        (assoc base :status :unsupported :reason :invalid-transition-evidence-source)))))

(def retained-profile :selected-mapped-retained-evidence-v1)

(defn compare-local-processed-retained-mapped-evidence!
  "Compose the selected evidence-bearing mapped envelope: leading evidence
   absence, exact mapped prefix, command-ID absence, complete derived puts,
   and final bound v4 record. Recompute the shared envelope digest and check
   both identities at the same retained predecessor. No publication, other
   guards, authenticated provenance or selected commitment is established."
  [node tables original targets docs journal expectation prefix-count spec context]
  (let [tag #(assoc % :mapped-retained-evidence-profile retained-profile)
        fail #(tag {:status %1 :reason %2 :authority :none :certificate :not-issued
                    :provider-refinement :not-established})
        checked (try
                  (let [ops (:ops expectation) id (resolution/record-id (:binding spec))]
                    (if-not (and (vector? ops) (integer? prefix-count)
                                 (<= 1 prefix-count (- (count ops) 4))
                                 (= ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" id]
                                    (first ops))
                                 (contains? #{["ASSERT 0 = (SELECT COUNT(*) FROM gesso_commands WHERE _id = ?)" id]
                                              ["ASSERT ? = (SELECT COUNT(*) FROM gesso_commands WHERE _id = ?)" 0 id]}
                                            (nth ops (inc prefix-count)))
                                 (vector? (peek ops)) (= 3 (count (peek ops)))
                                 (= [:put-docs :gesso_transition_evidence] (subvec (peek ops) 0 2))
                                 (map? (peek (peek ops))))
                      {:status :unsupported :reason :invalid-mapped-evidence-envelope}
                      (let [prefix (subvec ops 1 (inc prefix-count))
                            witness (peek (peek ops))
                            expected (derive-record! original targets docs journal prefix spec context)]
                        (cond
                          (not= expected (apply dissoc witness envelope/binding-keys))
                          {:status :mismatch :reason :transition-evidence-record-mismatch}
                          (not= (writes/put-row-frequencies! (writes/derive-puts! original targets docs journal spec))
                                (writes/put-row-frequencies! (subvec ops (+ 2 prefix-count) (dec (count ops)))))
                          {:status :mismatch :reason :selected-retained-write-suffix-mismatch}
                          (not (envelope/bound-transition-envelope? ops))
                          {:status :mismatch :reason :transition-evidence-envelope-binding-mismatch}
                          :else {:status :matched :id id}))))
                  (catch clojure.lang.ExceptionInfo _
                    {:status :unsupported :reason :invalid-mapped-evidence-source-or-envelope}))]
    (if (= :matched (:status checked))
      (let [r (tag (provider/compare-local-processed-retained-mapped-evidence-absence!
                    node tables original targets docs journal expectation prefix-count (:id checked)))]
        (if (= :matched (:status r))
          (assoc r :mapped-write-suffix :matched-source-derived-puts
                   :command-id-absence-guard :matched :evidence-id-absence-guard :matched
                   :transition-evidence :matched-source-relative-record
                   :transition-envelope-binding :matched)
          r))
      (fail (:status checked) (:reason checked)))))

(def publication-retained-profile :selected-mapped-retained-evidence-v2)

(defn compare-local-processed-retained-mapped-publication!
  "Compose an explicitly declared publication fragment with the selected
   evidence envelope. Require canonical publication guards after the mapped
   prefix, command absence next, complete mapped puts, the exact outbox put
   fragment and final bound witness. No other residual assertions admitted.
   All three identity domains share the same independently replayed prestate.
   Source/declaration provenance and selected commitment remain unqualified."
  [node tables original targets docs journal expectation prefix-count spec context source-id declarations]
  (let [tag #(assoc % :mapped-retained-evidence-profile publication-retained-profile)
        fail #(tag {:status %1 :reason %2 :authority :none :certificate :not-issued
                    :provider-refinement :not-established})
        checked (try
                  (let [{:keys [intents guard-ops put-ops]}
                        (publication/derive-publication! original targets docs journal spec source-id declarations)
                        ops (:ops expectation) id (resolution/record-id (:binding spec))
                        guard-end (+ 1 (if (integer? prefix-count) prefix-count 0) (count guard-ops))
                        put-end (- (count ops) 1 (count put-ops))]
                    (if-not (and (vector? ops) (integer? prefix-count) (<= 1 prefix-count 256)
                                 (< (inc guard-end) put-end)
                                 (= ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" id]
                                    (first ops))
                                 (contains? #{["ASSERT 0 = (SELECT COUNT(*) FROM gesso_commands WHERE _id = ?)" id]
                                              ["ASSERT ? = (SELECT COUNT(*) FROM gesso_commands WHERE _id = ?)" 0 id]}
                                            (nth ops guard-end))
                                 (vector? (peek ops)) (= 3 (count (peek ops)))
                                 (= [:put-docs :gesso_transition_evidence] (subvec (peek ops) 0 2))
                                 (map? (peek (peek ops))))
                      {:status :unsupported :reason :invalid-mapped-publication-envelope}
                      (let [prefix (subvec ops 1 (inc prefix-count))
                            witness (peek (peek ops))
                            expected (derive-record! original targets docs journal prefix spec context)
                            mapped (writes/derive-puts! original targets docs journal spec)
                            fragment {:guard-ops (subvec ops (inc prefix-count) guard-end)
                                      :put-ops (subvec ops put-end (dec (count ops)))}]
                        (cond
                          (some #(#{:gesso_live_outbox :gesso_transition_evidence} (second %)) mapped)
                          {:status :unsupported :reason :reserved-mapped-publication-table}
                          (not= :matched (:status (publication/check-publication!
                                                  original targets docs journal spec source-id declarations fragment)))
                          {:status :mismatch :reason :publication-fragment-mismatch}
                          (not= expected (apply dissoc witness envelope/binding-keys))
                          {:status :mismatch :reason :transition-evidence-record-mismatch}
                          (not= (writes/put-row-frequencies! mapped)
                                (writes/put-row-frequencies! (subvec ops (inc guard-end) put-end)))
                          {:status :mismatch :reason :selected-retained-write-suffix-mismatch}
                          (not (envelope/bound-transition-envelope? ops))
                          {:status :mismatch :reason :transition-evidence-envelope-binding-mismatch}
                          :else {:status :matched :id id :publication-ids (mapv :xt/id intents)}))))
                  (catch clojure.lang.ExceptionInfo _
                    {:status :unsupported :reason :invalid-mapped-publication-source-or-envelope}))]
    (if (= :matched (:status checked))
      (let [r (tag (provider/compare-local-processed-retained-mapped-publication-absence!
                    node tables original targets docs journal expectation prefix-count (:id checked) (:publication-ids checked)))]
        (if (= :matched (:status r))
          (assoc r :mapped-write-suffix :matched-source-derived-puts
                   :command-id-absence-guard :matched :evidence-id-absence-guard :matched
                   :transition-evidence :matched-source-relative-record
                   :transition-envelope-binding :matched
                   :publication-fragment :matched-source-relative-publication)
          r))
      (fail (:status checked) (:reason checked)))))

(def residual-retained-profile :selected-mapped-retained-evidence-v3)

(defn compare-local-processed-retained-mapped-residual!
  "Check the selected production-shaped envelope with residual assertions
   before publication guards. Derive the complete residual fragment, including
   its own command-ID guard (when declared), rather than adding another guard.
   Explicit nil declares an empty residual fragment, not missing provenance.
   Exact selected order is stricter than production's guard multiset check.
   All historical absence checks and replay still use the same retained sample.
   No installed admission provenance or selected commitment is established."
  [node tables original targets docs journal expectation prefix-count spec context source-id declarations residual-declarations]
  (let [tag #(assoc % :mapped-retained-evidence-profile residual-retained-profile)
        fail #(tag {:status %1 :reason %2 :authority :none :certificate :not-issued
                    :provider-refinement :not-established})
        checked (try
                  (let [{:keys [intents guard-ops put-ops]}
                        (publication/derive-publication! original targets docs journal spec source-id declarations)
                        residual-ops (residual/derive-assertions! original targets docs journal (:binding spec) residual-declarations)
                        ops (:ops expectation) id (resolution/record-id (:binding spec))
                        residual-end (+ 1 (if (integer? prefix-count) prefix-count 0) (count residual-ops))
                        guard-end (+ residual-end (count guard-ops))
                        put-end (- (count ops) 1 (count put-ops))]
                    (if-not (and (vector? ops) (integer? prefix-count) (<= 1 prefix-count 256)
                                 (<= (count residual-ops) 256) (< guard-end put-end)
                                 (= ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" id]
                                    (first ops))
                                 (vector? (peek ops)) (= 3 (count (peek ops)))
                                 (= [:put-docs :gesso_transition_evidence] (subvec (peek ops) 0 2))
                                 (map? (peek (peek ops))))
                      {:status :unsupported :reason :invalid-mapped-residual-envelope}
                      (let [prefix (subvec ops 1 (inc prefix-count))
                            witness (peek (peek ops))
                            expected (derive-record! original targets docs journal prefix spec context)
                            mapped (writes/derive-puts! original targets docs journal spec)
                            fragment {:guard-ops (subvec ops residual-end guard-end)
                                      :put-ops (subvec ops put-end (dec (count ops)))}]
                        (cond
                          (some #(#{:gesso_live_outbox :gesso_transition_evidence} (second %)) mapped)
                          {:status :unsupported :reason :reserved-mapped-publication-table}
                          (not= :matched (:status (residual/check-assertions!
                                                  original targets docs journal (:binding spec) residual-declarations
                                                  (subvec ops (inc prefix-count) residual-end))))
                          {:status :mismatch :reason :residual-assertion-fragment-mismatch}
                          (not= :matched (:status (publication/check-publication!
                                                  original targets docs journal spec source-id declarations fragment)))
                          {:status :mismatch :reason :publication-fragment-mismatch}
                          (not= expected (apply dissoc witness envelope/binding-keys))
                          {:status :mismatch :reason :transition-evidence-record-mismatch}
                          (not= (writes/put-row-frequencies! mapped)
                                (writes/put-row-frequencies! (subvec ops guard-end put-end)))
                          {:status :mismatch :reason :selected-retained-write-suffix-mismatch}
                          (not (envelope/bound-transition-envelope? ops))
                          {:status :mismatch :reason :transition-evidence-envelope-binding-mismatch}
                          :else {:status :matched :id id :publication-ids (mapv :xt/id intents)
                                 :residual-ops residual-ops}))))
                  (catch clojure.lang.ExceptionInfo _
                    {:status :unsupported :reason :invalid-mapped-residual-source-or-envelope}))]
    (if (= :matched (:status checked))
      (let [r (tag (provider/compare-local-processed-retained-mapped-publication-absence!
                    node tables original targets docs journal expectation prefix-count (:id checked)
                    (:publication-ids checked) (:residual-ops checked)))]
        (if (= :matched (:status r))
          (assoc r :mapped-write-suffix :matched-source-derived-puts
                   :evidence-id-absence-guard :matched
                   :command-id-absence-guard (if (some #{["ASSERT ? = (SELECT COUNT(*) FROM gesso_commands WHERE _id = ?)" 0 (:id checked)]}
                                                       (:residual-ops checked)) :matched :not-declared)
                   :transition-evidence :matched-source-relative-record
                   :transition-envelope-binding :matched
                   :publication-fragment :matched-source-relative-publication
                   :residual-fragment :matched-source-relative-residual)
          r))
      (fail (:status checked) (:reason checked)))))

(def executor-retained-profile :selected-mapped-retained-evidence-v4)

(defn- check-executor-envelope
  "Pure source-relative validation shared by single-step and recovery diagnostics."
  [original targets docs journal expectation prefix-count spec context source-id declarations residual-declarations]
  (try
    (let [{:keys [intents guard-ops put-ops]}
          (publication/derive-publication! original targets docs journal spec source-id declarations)
          residual-ops (residual/derive-assertions! original targets docs journal (:binding spec) residual-declarations)
          ops (:ops expectation) id (resolution/record-id (:binding spec))
          publication-end (+ 1 (if (integer? prefix-count) prefix-count 0) (count guard-ops))
          guard-end (+ publication-end (count residual-ops))
          put-end (- (count ops) 1 (count put-ops))]
      (if-not (and (vector? ops) (nil? (meta ops)) (integer? prefix-count) (<= 1 prefix-count 256)
                   (<= (count residual-ops) 256) (< guard-end put-end)
                   (= ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" id]
                      (first ops))
                   (vector? (peek ops)) (= 3 (count (peek ops)))
                   (= [:put-docs :gesso_transition_evidence] (subvec (peek ops) 0 2))
                   (map? (peek (peek ops))))
        {:status :unsupported :reason :invalid-mapped-executor-envelope}
        (let [prefix (subvec ops 1 (inc prefix-count))
              witness (peek (peek ops))
              expected (derive-record! original targets docs journal prefix spec context)
              mapped (writes/derive-puts! original targets docs journal spec)
              actual-residuals (subvec ops publication-end guard-end)
              ;; Match multiplicities before reordering solely for
              ;; the standalone canonical operand validator.
              canonical-residuals (when (= (frequencies residual-ops) (frequencies actual-residuals))
                                    (let [remaining (volatile! (group-by identity actual-residuals))]
                                      (mapv (fn [op]
                                              (let [candidate (first (get @remaining op))]
                                                (vswap! remaining update op subvec 1)
                                                candidate)) residual-ops)))
              fragment {:guard-ops (subvec ops (inc prefix-count) publication-end)
                        :put-ops (subvec ops put-end (dec (count ops)))}]
          (cond
            (some #(#{:gesso_live_outbox :gesso_transition_evidence} (second %)) mapped)
            {:status :unsupported :reason :reserved-mapped-publication-table}
            (not (and (some? canonical-residuals)
                      (= :matched (:status (residual/check-assertions!
                                            original targets docs journal (:binding spec) residual-declarations canonical-residuals)))))
            {:status :mismatch :reason :residual-assertion-fragment-mismatch}
            (not= :matched (:status (publication/check-publication!
                                    original targets docs journal spec source-id declarations fragment)))
            {:status :mismatch :reason :publication-fragment-mismatch}
            (not= expected (apply dissoc witness envelope/binding-keys))
            {:status :mismatch :reason :transition-evidence-record-mismatch}
            (not= (writes/put-row-frequencies! mapped)
                  (writes/put-row-frequencies! (subvec ops guard-end put-end)))
            {:status :mismatch :reason :selected-retained-write-suffix-mismatch}
            (not (envelope/bound-transition-envelope? ops))
            {:status :mismatch :reason :transition-evidence-envelope-binding-mismatch}
            :else {:status :matched :id id :publication-ids (mapv :xt/id intents)
                   :residual-ops actual-residuals}))))
    (catch clojure.lang.ExceptionInfo _
      {:status :unsupported :reason :invalid-mapped-executor-source-or-envelope})))

(defn- compare-executor*
  "Check the selected executor ordering: evidence guard, mapped prefix,
   canonical publication guards, residual guard multiset, ordinary puts,
   publication batch and final witness. The complete captured vector,
   including actual residual order, is bound to stable retained history.
   Declarations and outcome cases are premises, not authenticated issuance."
  [node tables original targets docs journal expectation prefix-count spec context source-id declarations residual-declarations mode]
  (let [abort? (= :abort mode)
        commit? (contains? #{:commit :abstract} mode)
        tag #(assoc % :mapped-retained-evidence-profile (case mode
                                                         :abort :selected-mapped-retained-executor-abort-v1
                                                         :commit :selected-mapped-retained-executor-commit-v1
                                                         :abstract :selected-mapped-retained-executor-abstract-v1
                                                         executor-retained-profile))
        fail #(tag {:status %1 :reason %2 :authority :none :certificate :not-issued
                    :provider-refinement :not-established})
        checked (check-executor-envelope original targets docs journal expectation prefix-count spec context source-id declarations residual-declarations)]
    (if (= :matched (:status checked))
      (let [r (tag (if abort?
                     (provider/compare-local-processed-retained-mapped-abort!
                      node tables original targets docs journal expectation prefix-count (:id checked))
                     (if (= :abstract mode)
                       (provider/compare-local-processed-retained-mapped-publication-absence!
                        node tables original targets docs journal expectation prefix-count (:id checked)
                        (:publication-ids checked) (:residual-ops checked) :publication-first true
                        (select-keys spec [:inputs :outcome-key :outcome-cases]))
                       (provider/compare-local-processed-retained-mapped-publication-absence!
                        node tables original targets docs journal expectation prefix-count (:id checked)
                        (:publication-ids checked) (:residual-ops checked) :publication-first commit?))))]
        (if (= :matched (:status r))
          (if abort?
            (assoc r :candidate-envelope :matched-source-relative-attempt
                   :candidate-transition-evidence :matched-uncommitted-record
                   :transition-envelope-binding :matched)
            (assoc r :mapped-write-suffix :matched-source-derived-puts
                   :evidence-id-absence-guard :matched
                   :command-id-absence-guard (if (some #{["ASSERT ? = (SELECT COUNT(*) FROM gesso_commands WHERE _id = ?)" 0 (:id checked)]}
                                                       (:residual-ops checked)) :matched :not-declared)
                   :transition-evidence :matched-source-relative-record
                   :transition-envelope-binding :matched
                   :publication-fragment :matched-source-relative-publication
                   :residual-fragment :matched-source-relative-residual))
          r))
      (fail (:status checked) (:reason checked)))))

(defn compare-local-processed-retained-mapped-executor!
  "Compare the complete selected executor envelope with its historical prestate."
  [node tables original targets docs journal expectation prefix-count spec context source-id declarations residual-declarations]
  (compare-executor* node tables original targets docs journal expectation prefix-count spec context source-id declarations residual-declarations :candidate))

(defn compare-local-processed-retained-mapped-executor-abort!
  "Match a complete source-derived candidate whose mapped prefix failed at
   a stale retained predecessor. The candidate witness is not a committed
   transition; only physical abort and rollback are qualified."
  [node tables original targets docs journal expectation prefix-count spec context source-id declarations residual-declarations]
  (compare-executor* node tables original targets docs journal expectation prefix-count spec context source-id declarations residual-declarations :abort))

(defn compare-local-processed-retained-mapped-executor-commit!
  "Match the full source-derived executor envelope and independently qualify
   successful physical commitment in the same stable retained history.
   Does not authenticate supplied premises or discharge abstract refinement."
  [node tables original targets docs journal expectation prefix-count spec context source-id declarations residual-declarations]
  (compare-executor* node tables original targets docs journal expectation prefix-count spec context source-id declarations residual-declarations :commit))

(defn compare-local-processed-retained-mapped-executor-abstract!
  "Additionally compare the independently replayed abstract pre/poststate
   and original guarded relation inside the same retained-history bracket.
   Uses the same supplied inputs and outcome interpretation as the envelope.
   No authenticated provenance or universal refinement is established."
  [node tables original targets docs journal expectation prefix-count spec context source-id declarations residual-declarations]
  (compare-executor* node tables original targets docs journal expectation prefix-count spec context source-id declarations residual-declarations :abstract))

(def executor-recovery-profile :selected-mapped-retained-executor-recovery-v1)

(def executor-interleaved-recovery-profile :selected-mapped-retained-executor-worker-interleaved-recovery-v1)

(defn- compare-executor-recovery*
  [node tables steps interleaved?]
  (let [roles [:winner :abort :recovery]
        required #{:original :targets :docs :journal :expectation :prefix-count
                   :spec :context :source-id :declarations :residual-declarations}
        tag #(assoc % :executor-recovery-profile (if interleaved? executor-interleaved-recovery-profile executor-recovery-profile))
        fail #(tag {:status %1 :reason %2 :authority :none :certificate :not-issued
                    :provider-refinement :not-established})]
    (if-not (and (vector? steps) (nil? (meta steps)) (= 3 (count steps))
                 (every? #(and (map? %) (nil? (meta %)) (= required (set (keys %)))) steps)
                 (apply = (map :source-id steps))
                 (apply = (map #(select-keys (:context %) [:catalog-fingerprint :induction-fingerprint]) steps))
                 (apply = (map :declarations steps))
                 (apply = (map :residual-declarations steps)))
      (fail :unsupported :invalid-executor-recovery-source)
      (let [checked (mapv (fn [{:keys [original targets docs journal expectation prefix-count
                                      spec context source-id declarations residual-declarations]}]
                            (check-executor-envelope original targets docs journal expectation prefix-count
                                                     spec context source-id declarations residual-declarations)) steps)]
        (if-let [failure (first (remove #(= :matched (:status %)) checked))]
          (fail (:status failure) (:reason failure))
          (let [expected (zipmap roles (mapv (fn [{:keys [spec]}]
                                               (resolution/command-record (:binding spec) (:outcome spec) (:now spec))) steps))
                mapped (mapv (fn [step report role]
                               (cond-> (assoc (select-keys step [:original :targets :docs :journal :expectation :prefix-count])
                                              :command-id (:id report))
                                 (not= :abort role)
                                 (assoc :publication-ids (:publication-ids report)
                                        :residual-ops (:residual-ops report)
                                        :abstract-spec (select-keys (:spec step) [:inputs :outcome-key :outcome-cases]))))
                             steps checked roles)
                result (tag (if interleaved?
                              (provider/compare-local-processed-retained-mapped-worker-interleaved-recovery!
                               node tables mapped expected (:source-id (first steps)))
                              (provider/compare-local-processed-retained-mapped-recovery! node tables mapped expected)))]
            (cond
              (not= :matched (:status result)) result
              (not (and (= (if interleaved? provider/mapped-interleaved-recovery-profile provider/mapped-recovery-profile)
                           (:mapped-recovery-profile result))
                        (= (mapv #(get-in % [:expectation :tx-id]) steps) (:selected-tx-ids result))
                        (= [:committed :aborted :committed] (:selected-commitments result))
                        (= :matched-retained-journal-trajectory (:recovery-linkage result))
                        (or (not interleaved?)
                            (and (vector? (:intervening-maintenance result))
                                 (integer? (:replayed-transaction-count result))))))
              (fail :unsupported :invalid-executor-recovery-provider-comparison)
              :else (assoc result :executor-envelopes [:matched-source-relative-record
                                                       :matched-uncommitted-record
                                                       :matched-source-relative-record]))))))))

(defn compare-local-processed-retained-mapped-executor-recovery!
  "Derive all three complete envelopes before checking their shared retained
   provider bracket. The journal records, publication IDs, residual operands
   and abstract inputs come from the same source-relative checks. The source
   catalog, selectors, inputs and outcome interpretation remain premises."
  [node tables steps]
  (compare-executor-recovery* node tables steps false))

(defn compare-local-processed-retained-mapped-executor-worker-interleaved-recovery!
  "Derive all three complete candidate envelopes and compare the same full
   retained history, including independently checked intervening worker
   transactions. Use the common candidate publication source for worker
   qualification; do not accept a separate worker source premise. Catalog,
   inputs, outcomes and actual execution provenance remain conditional."
  [node tables steps]
  (compare-executor-recovery* node tables steps true))

(def executor-input-recovery-profile :selected-mapped-retained-executor-input-recovery-v1)

(defn compare-local-processed-retained-mapped-executor-input-recovery!
  "Check the candidate input derivations against one supplied original mapped
   catalog, then qualify the same steps in the complete recovery bracket.
   The aborted step uses its prepared candidate state, not its actual stale
   predecessor. Source issuance and outcome interpretation remain premises."
  [node tables source steps]
  (let [tag #(assoc % :executor-input-recovery-profile executor-input-recovery-profile
                      :source-provenance :not-established :outcome-provenance :not-established)
        fail #(tag {:status %1 :reason %2 :authority :none :certificate :not-issued
                    :provider-refinement :not-established})
        required #{:original :targets :docs :journal :expectation :prefix-count
                   :spec :context :source-id :declarations :residual-declarations}]
    (if-not (and (map? source) (nil? (meta source))
                 (vector? steps) (nil? (meta steps)) (= 3 (count steps))
                 (every? #(and (map? %) (nil? (meta %)) (= required (set (keys %)))) steps))
      (fail :unsupported :invalid-executor-input-recovery-source)
      (let [reports (mapv (fn [{:keys [original targets docs journal expectation spec context]}]
                            (input/compare-selected! source original targets docs journal expectation spec context)) steps)]
        (if-let [failure (first (remove #(= :matched (:status %)) reports))]
          (fail (:status failure) (:reason failure))
          (if-not (every? #(and (= input/profile (:profile %))
                                (= :matched-source-relative-selectors (:input-derivation %))
                                (= :matched (:candidate-command-record %))) reports)
            (fail :unsupported :invalid-executor-input-comparison)
            (let [result (tag (compare-local-processed-retained-mapped-executor-recovery! node tables steps))]
              (cond
                (not= :matched (:status result)) result
                (not= executor-recovery-profile (:executor-recovery-profile result))
                (fail :unsupported :invalid-executor-recovery-comparison)
                :else (assoc result :input-derivations (mapv :input-derivation reports))))))))))

(def executor-outcome-recovery-profile :selected-mapped-retained-executor-outcome-recovery-v1)

(defn compare-local-processed-retained-mapped-executor-outcome-recovery!
  "Require source-relative outcome cases and selected alternative linkage for
   all three candidates, then qualify the same input and recovery trajectory.
   Concrete outcome declarations and installed decoder issuance remain premises."
  [node tables source outcome-declarations steps]
  (let [tag #(assoc % :executor-outcome-recovery-profile executor-outcome-recovery-profile
                      :source-provenance :not-established :outcome-provenance :not-established)
        fail #(tag {:status %1 :reason %2 :authority :none :certificate :not-issued
                    :provider-refinement :not-established})
        required #{:original :targets :docs :journal :expectation :prefix-count
                   :spec :context :source-id :declarations :residual-declarations}]
    (if-not (and (map? source) (nil? (meta source))
                 (vector? steps) (nil? (meta steps)) (= 3 (count steps))
                 (every? #(and (map? %) (nil? (meta %)) (= required (set (keys %)))) steps))
      (fail :unsupported :invalid-executor-outcome-recovery-source)
      (let [reports (mapv (fn [{:keys [original targets expectation spec]}]
                            (outcome/compare-selected! source outcome-declarations original targets expectation spec)) steps)]
        (if-let [failure (first (remove #(= :matched (:status %)) reports))]
          (fail (:status failure) (:reason failure))
          (if-not (every? #(and (= outcome/profile (:profile %))
                                (= :matched-declared-journal-assignment (:outcome-derivation %))
                                (= :matched (:candidate-command-record %))) reports)
            (fail :unsupported :invalid-executor-outcome-comparison)
            (let [result (tag (compare-local-processed-retained-mapped-executor-input-recovery! node tables source steps))]
              (cond
                (not= :matched (:status result)) result
                (not= executor-input-recovery-profile (:executor-input-recovery-profile result))
                (fail :unsupported :invalid-executor-input-recovery-comparison)
                :else (assoc result :outcome-derivations (mapv :outcome-derivation reports))))))))))

(def issued-executor-recovery-profile :selected-issued-mapped-executor-recovery-v1)

(defn- issued-step-matches? [description step]
  (let [id (get-in step [:spec :alternative])]
    (and (contains? (:operation-mappings description) id)
         (= (:context step)
            (assoc (select-keys description [:catalog-fingerprint :induction-fingerprint])
                   :selectors (get-in description [:input-derivations id])))
         (= (:targets step) (:target-map description))
         (= (get-in step [:spec :effects]) (get-in description [:document-effects id]))
         (= (:declarations step) (:publication-derivations description))
         (= (:residual-declarations step) (:residual-assertion-derivations description)))))

(defn compare-local-processed-retained-issued-executor-recovery!
  "Bind the selected retained recovery to the exact local issued mapped
   admission and registered closed decoder. Require its checked declaration
   context for every step. Source-id, authenticated command origin, actual
   submission use of this admission and installed host refinement remain
   premises. No new transaction authority or certificate is issued."
  [node tables admission source outcome-declarations steps]
  (let [tag #(assoc % :issued-executor-recovery-profile issued-executor-recovery-profile
                      :source-provenance :not-established :outcome-provenance :not-established)
        fail #(tag {:status %1 :reason %2 :authority :none :certificate :not-issued
                    :provider-refinement :not-established})
        required #{:original :targets :docs :journal :expectation :prefix-count
                   :spec :context :source-id :declarations :residual-declarations}]
    (try
      (if-not (and (vector? steps) (nil? (meta steps)) (= 3 (count steps))
                   (every? #(and (map? %) (nil? (meta %)) (= required (set (keys %)))) steps))
        (fail :unsupported :invalid-issued-executor-recovery-source)
        (let [description (induction/describe-mapped-admission admission)
              fingerprints (select-keys description [:catalog-fingerprint :induction-fingerprint])]
          (if-not (and (map? (:publication-derivations description))
                       (map? (:residual-assertion-derivations description))
                       (every? #(issued-step-matches? description %) steps))
            (fail :mismatch :issued-executor-recovery-declaration-mismatch)
            (let [binding (decoder/compare-issued-closed! admission source outcome-declarations)]
              (cond
                (not= :matched (:status binding)) (fail (:status binding) (:reason binding))
                (not (and (= decoder/closed-profile (:profile binding))
                          (= :matched-issued-closed-construction (:closed-decoder-binding binding))
                          (= fingerprints (select-keys binding (keys fingerprints)))))
                (fail :unsupported :invalid-issued-recovery-decoder-binding)
                :else
                (let [result (tag (compare-local-processed-retained-mapped-executor-outcome-recovery!
                                   node tables source outcome-declarations steps))]
                  (cond
                    (not= :matched (:status result)) result
                    (not= executor-outcome-recovery-profile (:executor-outcome-recovery-profile result))
                    (fail :unsupported :invalid-issued-recovery-comparison)
                    :else (assoc result :issued-admission-binding :matched-issued-declarations
                                        :closed-decoder-binding (:closed-decoder-binding binding)
                                        :issued-catalog-fingerprint (:catalog-fingerprint fingerprints)
                                        :issued-induction-fingerprint (:induction-fingerprint fingerprints)))))))))
      (catch clojure.lang.ExceptionInfo e
        (fail :unsupported (or (:reason (ex-data e)) :invalid-issued-executor-recovery-source))))))

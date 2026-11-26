(ns gesso.assurance.reference-mapped-evidence
  "Source-relative v4 witness correspondence; not an installed admission
   or a physical transaction attestation."
  (:require [gesso.assurance.proof.mapped-prestate-lowering :as lowering]
            [gesso.assurance.proof.typed-value-semantics :as values]
            [gesso.assurance.reference-mapped-writes :as writes]
            [gesso.assurance.reference-mapped-publication :as publication]
            [gesso.assurance.reference-provider :as provider]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.assurance.subject :as subject]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]))

(def profile :selected-mapped-evidence-v1)
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
                 (map? selectors) (nil? (meta selectors))
                 (= (get-in original [:subject :id]) (get-in spec [:binding :operation])))
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

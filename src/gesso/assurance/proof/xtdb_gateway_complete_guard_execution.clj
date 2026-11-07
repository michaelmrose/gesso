(ns gesso.assurance.proof.xtdb-gateway-complete-guard-execution
  "v1537: Opt-in top-level compare-and-set strengthening for ONE Gesso
   whole-document UPDATE before an existing restricted gateway is issued.

   The ordinary Gesso command guards only its declared version keys. That
   permits a concurrent writer to change an unguarded field which a later
   whole-document PUT silently restores. For a strictly bounded subset,
   strengthen the original command's canonical expected-version check to
   include ALL non-nil top-level fields in its caller-provided :model/before
   document, and add the canonical document-field-count ASSERT. The existing
   gateway then admits and owns EXACTLY the strengthened transaction vector.

   This does not authenticate the supplied prestate, provide the transaction's
   internal read basis, prevent ABA writes, protect nested attributes or
   other documents, or establish a universal XTDB isolation/serialization
   theorem. SQL ASSERT behavior remains a backend premise. No certificate,
   physical causality proof, writer closure, or principal authority follows.
   Reports do not contain identifiers, values, SQL text or transaction keys."
  (:require [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gateway]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]))

(def guarded-type
  :gesso.assurance.proof.xtdb-gateway-complete-guard-execution/one-document)
(def guarded-version 1)

(def ^:private original-update? command/update?)
(def ^:private original-count tx/assert-document-field-count)
(def ^:private original-prepare tx/prepare)
(def ^:private original-gateway gateway/prepare-one-shot!)

(defn- reject! [reason]
  (throw (ex-info "Unsupported XTDB complete-guard gateway construction."
                  {:xtdb-gateway-complete-guard-execution/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- plain-vector? [x]
  (and (instance? clojure.lang.APersistentVector x)
       (nil? (meta x))))

(defn- scalar? [x]
  ;; Intentionally much narrower than XTDB's full value language. A nil
  ;; (including a nil field in :before) is not SQL-equality-comparable here.
  (or (string? x) (boolean? x)
      (and (integer? x) (not (instance? clojure.lang.BigInt x)))
      (instance? java.math.BigDecimal x)
      (instance? java.util.UUID x)
      (instance? java.time.Instant x)))

(defn- check-seams! []
  (when-not (and (identical? original-update? (var-get #'command/update?))
                 (identical? original-count
                             (var-get #'tx/assert-document-field-count))
                 (identical? original-prepare (var-get #'tx/prepare))
                 (identical? original-gateway
                             (var-get #'gateway/prepare-one-shot!)))
    (reject! :substituted-preparation-seam)))

(defn- strengthened-source! [source]
  (when-not (and (plain-map? source)
                 (= #{:plan :protected-documents} (set (keys source))))
    (reject! :invalid-gateway-source))
  (let [{:keys [plan protected-documents]} source]
    ;; Refuse a broader plan instead of silently dropping its guards,
    ;; assertions, changes, entry, callbacks, or transaction options.
    (when-not (and (plain-map? plan)
                   (= #{:commands :emit} (set (keys plan)))
                   (= false (:emit plan))
                   (plain-vector? (:commands plan))
                   (= 1 (count (:commands plan)))
                   (plain-vector? protected-documents)
                   (= 1 (count protected-documents)))
      (reject! :unsupported-plan-scope))
    (let [c (first (:commands plan))
          entry (first protected-documents)
          before (:model/before c)
          after (:model/after c)
          original-checks (get-in c [:model/expected :model/checks])
          target [(:model/entity-type c) (:model/id c)]]
      (when-not (and (original-update? c)
                     (plain-map? c) (plain-map? before) (plain-map? after)
                     (<= 2 (count before) 64)
                     (= (set (keys before)) (set (keys after)))
                     (= (:xt/id before) (:xt/id after) (:model/id c))
                     (every? keyword? (keys before))
                     (every? scalar? (vals before))
                     (every? scalar? (vals after))
                     (plain-map? entry)
                     (= #{:target :fields} (set (keys entry)))
                     (= target (:target entry))
                     (= :all (:fields entry)))
        (reject! :unsupported-single-document-update))
      (let [all-fields (disj (set (keys before)) :xt/id)
            checked-fields (set (map first original-checks))]
        (when-not (and (seq original-checks)
                       (every? (fn [[field value]]
                                 (and (contains? all-fields field)
                                      (= (get before field) value)))
                               original-checks))
          (reject! :version-check-not-bound-to-before))
        (let [additional-fields (sort-by str
                                         (remove checked-fields all-fields))
              stronger-checks (into (vec original-checks)
                                   (map (fn [field]
                                          [field (get before field)]))
                                   additional-fields)
              stronger (assoc-in c [:model/expected :model/checks]
                                 stronger-checks)
              count-assert (original-count (:model/entity-type c)
                                           (:model/id c) (count before))
              enriched {:plan {:commands [stronger]
                               :assertions [count-assert]
                               :emit false}
                        :protected-documents protected-documents}
              prepared (try (original-prepare nil (:plan enriched))
                            (catch Exception _
                              (reject! :strengthened-preparation-unsupported)))
              tx-ops (:tx-ops prepared)]
          (when-not (and (plain-vector? tx-ops)
                         (= 3 (count tx-ops))
                         (every? (fn [op]
                                   (and (plain-vector? op)
                                        (string? (first op))
                                        (re-find #"(?i)^ASSERT\b" (first op))))
                                 (take 2 tx-ops))
                         (= :put-docs (first (last tx-ops)))
                         (= (:model/after c) (last (last tx-ops))))
            (reject! :noncanonical-strengthened-vector))
          {:source enriched
           :field-count (count all-fields)
           :added-check-count (count additional-fields)})))))

(defn prepare-complete-guard-one-shot!
  "Prepare a strictly guarded ONE-document update using the existing
   v1525+ restricted one-shot gateway. Input is exactly the original ordinary
   one-command Gesso plan plus its :all protected-document declaration.

   Returns {:type :version :gateway :submit! :summary!}. The :gateway member
   is the original restricted gateway descriptor and supports its exact-return,
   attempt and ledger readback APIs. The submission closure sends only the
   source-rederived strengthened vector. Its result is unchanged.

   Every observed non-id top-level field must match its caller-supplied
   :model/before value at the transaction's ASSERT stage, and the canonical
   count ASSERT detects added or removed fields, subject to the tested XTDB
   beta1 semantics and accepted SQL scalar types. These are transaction
   PRECONDITIONS, not independent attestation of an internal prestate basis.
   There is no application admission, authenticated writer identity, or proof
   that unrelated writes cannot occur before or after the transaction."
  [node source]
  (check-seams!)
  (let [{:keys [source field-count added-check-count]}
        (strengthened-source! source)
        gate (original-gateway node source)
        summary! (fn []
                   (merge {:type guarded-type
                           :version guarded-version
                           :scope :one-update-complete-top-level-cas
                           :compared-top-level-field-count field-count
                           :added-snapshot-comparison-count added-check-count
                           :field-count-assertion :canonical-prewrite
                           :all-source-observed-fields-guarded? true
                           :source-prestate-authenticity :caller-supplied-unattested
                           :transaction-internal-read-basis :not-authenticated
                           :backend-assert-semantics :trusted-bounded-premise
                           :nested-and-external-fields :not-established
                           :aba-history-detection :not-established
                           :physical-poststate-causality :not-established
                           :complete-deployment-writer-coverage :not-established
                           :principal-authorization :not-established
                           :universal-physical-refinement :not-established
                           :proof-status :guarded-submission-only
                           :certificate :not-issued
                           :admission :not-admitted
                           :authority :none}
                          (select-keys ((:summary! gate))
                                       [:status :owned-execute-tx-call-entries])))]
    {:type guarded-type
     :version guarded-version
     :gateway gate
     :submit! (:submit! gate)
     :summary! summary!}))

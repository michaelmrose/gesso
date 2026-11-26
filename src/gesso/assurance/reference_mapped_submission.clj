(ns gesso.assurance.reference-mapped-submission
  "Compare local observed framed handoffs and adapter calls to selected retained steps."
  (:require [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.live.consistency.xtdb :as live]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.inductive-plan :as induction])
  (:import [xtdb.api TransactionKey]))

(def profile :selected-mapped-submission-observation-v1)

(defn- reject! [reason]
  (throw (ex-info "Unsupported mapped submission observation." {:reason reason})))

(defn- receipt-matches? [expected result]
  (and (instance? TransactionKey result)
       (= (:tx-id expected) (.getTxId ^TransactionKey result))
       (= (:system-time expected) (.getSystemTime ^TransactionKey result))))

(defn compare-selected!
  "Compare three supplied policy-claim observations and one adapter capture
   to the same three selected recovery steps. Reconstruct the checked
   handoff witness through the issued admission. Match each exact operation
   vector and default options; match returned public keys for the two commits.
   The middle exception has no public transaction identity. Its association
   relies on the observed ordering and retained payload, not a receipt.
   Inputs are local observations, not authenticated attestations. No write
   or replay is performed by this diagnostic. Do not log raw observations."
  [admission steps claims capture]
  (let [base {:profile profile :authority :none :certificate :not-issued
              :provider-refinement :not-established
              :submission-provenance :not-established
              :aborted-receipt-identity :not-established}
        fail #(assoc base :status %1 :reason %2)]
    (try
      (let [events (:events capture)]
        (if-not (and (vector? steps) (= 3 (count steps)) (every? map? steps)
                     (vector? claims) (= 3 (count claims)) (every? map? claims)
                     (= live/submission-capture-type (:type capture))
                     (= live/submission-capture-version (:version capture))
                     (= :instrumented-xtdb-adapter-call-site (:observation-origin capture))
                     (false? (:actual-xtdb-api-authenticated? capture))
                     (= :returned (:outcome capture))
                     (vector? events) (= 6 (count events)) (every? map? events))
          (fail :unsupported :invalid-mapped-submission-observations)
          (let [pairs (mapv vec (partition 2 events))
                call-ids (mapv (comp :call-id first) pairs)]
            (when-not (and (every? uuid? call-ids) (= 3 (count (set call-ids))))
              (reject! :invalid-mapped-submission-call-identities))
            (doseq [[i step {:keys [submission checked]} [started finished]]
                    (map vector (range 3) steps claims pairs)]
              (let [{:keys [expectation spec source-id]} step
                    ops (:ops expectation)
                    id (resolution/record-id (:binding spec))
                    actual-witness (when (and (vector? ops) (seq ops)
                                              (vector? (peek ops)) (= 3 (count (peek ops)))
                                              (= [:put-docs :gesso_transition_evidence] (subvec (peek ops) 0 2))
                                              (map? (peek (peek ops))))
                                     (apply dissoc (peek (peek ops)) envelope/binding-keys))]
                (when-not (and (= {:api :execute-tx :tx-ops ops :tx-options (live/tx-opts nil)} submission)
                               (= submission (select-keys started [:api :tx-ops :tx-options]))
                               (= :invoked (:phase started)) (= :execute-tx (:api finished))
                               (= (:call-id started) (:call-id finished)))
                  (reject! :mapped-submission-envelope-mismatch))
                (when-not (and (= {:alternative (:alternative spec) :command-id id} (:identity checked))
                               (= source-id (get-in checked [:candidate :publication-source-id]))
                               (some? actual-witness)
                               (= actual-witness (induction/mapped-transition-evidence! admission checked)))
                  (reject! :mapped-submission-handoff-mismatch))
                (if (= 1 i)
                  (when-not (and (= :threw (:phase finished)) (string? (:exception-class finished))
                                 (not (contains? finished :tx-result)))
                    (reject! :mapped-submission-abort-observation-mismatch))
                  (when-not (and (= :returned (:phase finished))
                                 (receipt-matches? expectation (:tx-result finished)))
                    (reject! :mapped-submission-receipt-mismatch)))))
            (assoc base :status :matched
                   :framed-handoffs :matched-observed-checked-candidates
                   :adapter-payloads :matched-selected-retained-operations
                   :returned-transaction-keys :matched-selected-commit-identities
                   :aborted-call :observed-throw-without-transaction-key))))
      (catch clojure.lang.ExceptionInfo e
        (fail :mismatch (or (:reason (ex-data e)) :invalid-mapped-submission-handoff))))))

(def observed-profile :selected-issued-mapped-submission-observation-v1)

(defn compare-observed!
  "Compare only a locally issued, completed framed observation. The handle
   binds its actual consumed handoffs to the policy-scoped adapter capture.
   Historical comparison after policy release is supported; no authority,
   external authentication or aborted receipt identity is established."
  [admission steps handle]
  (if-let [{:keys [claims capture]} (publication/read-framed-submission-observation! admission handle)]
    (let [result (assoc (compare-selected! admission steps claims capture) :observed-profile observed-profile)]
      (cond-> result
        (= :matched (:status result))
        (assoc :observation-origin :matched-local-issued-policy-window)))
    {:status :unsupported :reason :unissued-framed-submission-observation
     :observed-profile observed-profile :authority :none :certificate :not-issued
     :provider-refinement :not-established :submission-provenance :not-established
     :aborted-receipt-identity :not-established}))

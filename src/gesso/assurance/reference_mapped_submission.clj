(ns gesso.assurance.reference-mapped-submission
  "Compare local observed framed handoffs and adapter calls to selected retained steps."
  (:require [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.choreo.identity :as identity]
            [gesso.live.consistency.xtdb :as live]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.live.outbox.http-route :as route]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.inductive-plan :as induction])
  (:import [xtdb.api TransactionKey]))

(def profile :selected-mapped-submission-observation-v4)

(defn- reject! [reason]
  (throw (ex-info "Unsupported mapped submission observation." {:reason reason})))

(defn- receipt-matches? [expected result]
  (and (instance? TransactionKey result)
       (= (:tx-id expected) (.getTxId ^TransactionKey result))
       (= (:system-time expected) (.getSystemTime ^TransactionKey result))))

(def ^:private origin-keys
  (into resolution/binding-keys [:execution-id :origin :authentication]))

(defn- route-matches? [origin]
  (if-not (contains? origin :http-route)
    true
    (let [observed (:http-route origin)
          segments (when (string? (:path observed)) (route/route-segments (:path observed)))
          captures (when (and segments (string? (:uri observed)))
                     (let [candidate (assoc observed :transport-segments segments)]
                       (when (route/matching-route [candidate] (:uri observed))
                         (route/captured-arguments candidate (:uri observed)))))]
      (and (map? observed) (not (record? observed)) (nil? (meta observed))
           (= #{:route-id :path :method :operation :required-transport :uri :path-arguments
                :request-gate :evidence} (disj (set (keys observed)) :authority-binding))
           (or (not (contains? observed :authority-binding))
               (route/authority-binding? (:authority-binding observed)))
           (keyword? (:route-id observed))
           (= :post (:method observed)) (= :htmx (:required-transport observed))
           (= (:operation origin) (:operation observed))
           (= :locally-accepted (:request-gate observed))
           (= :matched-canonical-htmx-handoff (:evidence observed))
           (some? captures) (= captures (:path-arguments observed))
           (every? (fn [[k v]] (= v (get (:arguments origin) k))) captures)))))

(defn- compare-command-origins! [steps claims]
  (let [selected
        (mapv (fn [step claim]
                (when (contains? claim :command-origin)
                  (let [origin (:command-origin claim)
                        binding (get-in step [:spec :binding])]
                    (when-not (and (map? origin) (not (record? origin)) (nil? (meta origin))
                                   (= origin-keys (disj (set (keys origin)) :http-route))
                                   (route-matches? origin)
                                   (= :registered-server-invocation (:origin origin))
                                   (= :conditional-on-trusted-principal-resolver (:authentication origin))
                                   (identity/execution-id? (:execution-id origin))
                                   (= binding (select-keys origin resolution/binding-keys)))
                      (reject! :mapped-command-origin-mismatch))
                    ;; The execution ID is observed, not a retained XTDB field.
                    ;; In particular, the abort association is not a receipt.
                    (assoc (select-keys origin [:principal :command-id :execution-id :operation :origin :authentication :http-route])
                           :selected-tx-id (get-in step [:expectation :tx-id])))))
              steps claims)
        n (count (filter some? selected))
        [_ aborted recovered] selected]
    {:command-origin-correspondence (case n
                                      0 :not-observed
                                      3 :matched-supplied-command-bindings
                                      :partial-supplied-command-bindings)
     :selected-command-origins selected
     :abort-recovery-execution (if (and aborted recovered)
                                 (if (= (:execution-id aborted) (:execution-id recovered))
                                   :same-observed-execution :distinct-observed-executions)
                                 :not-observed)
     :command-authentication :not-established}))

(defn compare-selected!
  "Compare three supplied policy-claim observations and one adapter capture
   to the same three selected recovery steps. Reconstruct the checked
   handoff witness through the issued admission. Match each exact operation
   vector and default options; match returned public keys for the two commits.
   The middle invocation throws instead of returning a transaction key.
   The legacy :aborted-call disposition describes the missing returned key,
   not absence of provider exception metadata. This comparison does not yet
   consume the separate :xtdb-anomalies diagnostic; its abort association
   relies on observed ordering and retained payload, not an identified key.
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
            (merge base (compare-command-origins! steps claims)
                   {:status :matched
                    :framed-handoffs :matched-observed-checked-candidates
                    :adapter-payloads :matched-selected-retained-operations
                    :returned-transaction-keys :matched-selected-commit-identities
                    :aborted-call :observed-throw-without-transaction-key}))))
      (catch clojure.lang.ExceptionInfo e
        (fail :mismatch (or (:reason (ex-data e)) :invalid-mapped-submission-handoff))))))

(def observed-profile :selected-issued-mapped-submission-observation-v4)

(def ^:private original-observation-reader publication/read-framed-submission-observation!)
(def ^:private original-selected-comparison compare-selected!)

(defn compare-observed!
  "Compare only a locally issued, completed framed observation. The handle
   binds its actual consumed handoffs to the policy-scoped adapter capture.
   Historical comparison after policy release is supported; no authority,
   external authentication or aborted receipt identity is established."
  [admission steps handle]
  (if-let [{:keys [claims capture]} (publication/read-framed-submission-observation! admission handle)]
    (let [result (assoc (compare-selected! admission steps claims capture) :observed-profile observed-profile)]
      (if (and (= :matched (:status result))
               (not= :not-observed (:command-origin-correspondence result))
               (not (and (identical? original-observation-reader publication/read-framed-submission-observation!)
                         (identical? original-selected-comparison compare-selected!))))
        {:status :unsupported :reason :command-origin-comparison-boundary-replaced
         :observed-profile observed-profile :authority :none :certificate :not-issued
         :provider-refinement :not-established :submission-provenance :not-established
         :aborted-receipt-identity :not-established}
        (cond-> result
          (= :matched (:status result))
          (-> (assoc :observation-origin :matched-local-issued-policy-window)
              (update :command-origin-correspondence
                      #(case %
                         :matched-supplied-command-bindings :matched-issued-command-bindings
                         :partial-supplied-command-bindings :partial-issued-command-bindings
                         %))))))
    {:status :unsupported :reason :unissued-framed-submission-observation
     :observed-profile observed-profile :authority :none :certificate :not-issued
     :provider-refinement :not-established :submission-provenance :not-established
     :aborted-receipt-identity :not-established}))

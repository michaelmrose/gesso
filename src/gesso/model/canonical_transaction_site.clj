(ns gesso.model.canonical-transaction-site
  "v1768: An opt-in, process-local diagnostic for the ACTUAL canonical
   resolved-operation invocation site of gesso.model.tx/transact!, after
   incorporation and the source-owned pre-submit checks have finished.

   This is a cooperatively instrumented JVM call site, not a trusted witness
   of code provenance, the physical XTDB API/receipt, business authorization,
   or a complete-CAS typed gateway. Application callbacks, including custom
   transaction adapters, remain outside this observation's proof authority.
   In particular, do NOT use a canonical transaction-site observation as
   evidence of typed gateway adoption. No retry is authorized by a report."
)

(def report-type
  :gesso.model.canonical-transaction-site/process-local-canonical-site)
(def report-version 1)
(def ^:dynamic ^:private *site-capture* nil)

(defn- reject! [reason]
  (throw (ex-info "Invalid canonical transaction site capture."
                  {:canonical-transaction-site/rejection true
                   :reason reason})))

(defn record-site!
  "Internal instrument only: resolved-operation calls this at its canonical
   tx/transact! invocation site. It cannot be independently authenticated
   against hostile Clojure code using runtime instrumentation/reflection.
   Never expose transaction arguments or results through this hook."
  [operation token phase physical-status]
  (when-let [{:keys [thread events]} *site-capture*]
    ;; The diagnostic must never affect authoritative transaction semantics.
    (try
      (swap! events conj {:operation operation :token token :phase phase
                          :status physical-status
                          :on-original-thread?
                          (identical? thread (Thread/currentThread))})
      (catch Throwable _ nil))))

(defn observe-canonical-transaction-site!
  "Run invoke! once in a synchronous, dynamically scoped observation.
   Only exactly one matching entered/completed pair from the original
   resolved-operation transaction site qualifies; it must be on the same
   thread, with the same opaque attempt token and selected operation.

   A :committed status is the existing Gesso model transaction
   ACKNOWLEDGMENT classification, not an authenticated XTDB receipt or
   verified durable state. The separate :execution may contain sensitive
   return values and Throwables and MUST NOT be published as assurance.
   Missing, multiple, cross-thread, rejected and uncertain observations
   never qualify as acknowledged canonical submissions."
  [operation invoke!]
  (when-not (and (keyword? operation) (namespace operation) (fn? invoke!))
    (reject! :invalid-observation-request))
  (when *site-capture*
    (reject! :nested-canonical-site-observation))
  (let [events (atom [])
        capture {:thread (Thread/currentThread) :events events}
        execution (binding [*site-capture* capture]
                    (try {:outcome :returned :result (invoke!)}
                         (catch Throwable e {:outcome :threw :error e})))
        pair @events
        matched? (and (= 2 (count pair))
                      (let [[start finish] pair]
                        (and (= :entered (:phase start))
                             (= :completed (:phase finish))
                             (= operation (:operation start)
                                (:operation finish))
                             (identical? (:token start) (:token finish))
                             (true? (:on-original-thread? start))
                             (true? (:on-original-thread? finish)))))
        definitive? (and matched? (= :returned (:outcome execution)))
        acknowledged? (and definitive?
                           (= :committed (:status (second pair))))
        status (cond
                 acknowledged? :one-canonical-model-tx-acknowledged
                 (empty? pair) :no-canonical-model-tx-site
                 (not matched?) :ambiguous-or-mismatched-canonical-site
                 (= :rejected (:status (second pair)))
                 :canonical-model-tx-rejected
                 :else :canonical-model-tx-unconfirmed)]
    {:execution execution
     :report {:type report-type :version report-version
              :operation-id operation :status status
              :scope :single-process-local-canonical-resolved-operation-tx-site
              :model-tx-callsite (if matched? :observed :not-established)
              :model-tx-acknowledgment
              (if acknowledged? :committed-ack-observed :not-established)
              :typed-complete-cas-gateway-used :not-established
              :gateway-to-canonical-tx-causality :not-established
              :xtdb-receipt-authenticity :not-established
              :deployed-http-adoption :not-established
              :business-policy-authenticity :not-established
              :certificate :not-issued :admission :not-admitted
              :authority :none}}))

(ns gesso.assurance.reference-xtdb-provider-contract
  "Versioned OPEN provider requirements for the closed mapped Claim fragment.
   Neither an XTDB implementation proof nor a provider trust declaration."
  (:require [clojure.set :as set]
            [gesso.assurance.assumption :as assumption]
            [gesso.assurance.reference-clm-r2-condition-inventory :as inventory]))

(def profile :open-xtdb-provider-contract-v1)
(def reviewed-provider
  ;; Reviewed local dependency artifacts; not executing-code attestation.
  {:version "2.2.0-beta1"
   :artifacts {"com.xtdb/xtdb-api" "0d9a36af73d52041fdf182d9d1da6bf5369cc954b58000c8c77f63cbc58775b9"
               "com.xtdb/xtdb-core" "e81b66a71af40ebf921cfcacfc52906ca9550734adfd1d34cf7870e5d1a24804"}
   :scope :embedded-single-node-mapped-claim-fragment})
(def ^:private original-inventory inventory/derive!)
(def ^:private original-describe assumption/describe-assumption)

;; These are REQUIRED semantic properties, not claims about XTDB behavior.
;; Exact prefix, envelope and target identity remain Gesso obligations.
(def ^:private clauses
  {::decoded-assertions
   {:property "For the exact admitted SQL vectors and bound values, ASSERT succeeds exactly when its specified predicate is true, including row and slot counts, visible field counts, scalar type distinctions, nested journal structs and vectors. Identifier and value encoding/decoding preserve the specified meanings; false, unknown or invalid predicates cannot yield a successful assertion."
    :fragment #{:count :length-nest-one :scalar-equality-str-array :nested-journal-paths :struct-length :empty-struct :vector-index :residual-assertions}
    :dependencies #{}}
   ::ordered-transaction-basis
   {:property "Each supported execute-tx has one ordered transaction position. All operations run in vector order against its transaction-internal working state, starting from the actual predecessor at the transaction's valid-time coordinate; earlier writes are visible to later operations, and no other transaction interleaves within it."
    :fragment #{:execute-tx-empty-options :ordered-assert-and-put :actual-predecessor}
    :dependencies #{}}
   ::put-replacement
   {:property "Each admitted put-docs replaces exactly the named document over its prescribed valid interval (default start at transaction system time, unbounded end), with faithful decoded document values, identity and temporal overlay. No other document is mutated except provider transaction metadata."
    :fragment #{:ordinary-put-docs :default-valid-interval :decoded-document-replacement}
    :dependencies #{::ordered-transaction-basis}}
   ::commit-abort
   {:property "A definitive committed result publishes the entire ordered working-state effect atomically; assertion failure or definitive abort publishes none of the attempted document effects, including an executed prefix of puts. Assertions have no application-document write effect. Ambiguous transport outcomes are not definitive aborts."
    :fragment #{:definitive-commit :definitive-abort :assertions :rollback-after-put-prefix}
    :dependencies #{::decoded-assertions ::ordered-transaction-basis ::put-replacement}}
   ::worker-clock
   {:property "CURRENT_TIMESTAMP in the admitted worker assertions denotes the transaction clock, with exact comparison and five-minute interval arithmetic. _system_from identifies the actual visible row version at that basis. The clock and valid-time interpretation agree with the transaction's recorded system time."
    :fragment #{:lease-acquisition :lease-reclaim :active-fence :dispatch-acknowledgment :system-from :transaction-clock}
    :dependencies #{::decoded-assertions ::ordered-transaction-basis}}
   ::pinned-read
   {:property "For the supported pinned snapshot-token queries and explicit system/valid-time coordinates, all rows and decoded values correspond to the same indexed cut and requested coordinates. SHOW SNAPSHOT_TOKEN faithfully identifies that basis; repeated query results do not silently use a newer snapshot or default valid time."
    :fragment #{:snapshot-token :current-time :explicit-system-time :explicit-valid-time :complete-mapped-row-readback}
    :dependencies #{::put-replacement}}
   ::retained-history
   {:property "The selected retained log decoder and xt.txs observations faithfully identify every transaction in the requested complete prefix, with exact order, IDs, envelope operations, system times and definitive commitment. Missing, pruned or unsupported input is not evidence of completeness."
    :fragment #{:retained-source-log :xt-txs :transaction-order :definitive-outcome :processed-frontier}
    :dependencies #{::commit-abort}}
   ::valid-time-boundaries
   {:property "Supported all-time rows faithfully describe system and valid intervals, including open-ended intervals, with start-inclusive/end-exclusive valid-time membership. Complete retained segments expose every protected boundary between selected diagonal coordinates, including transient changes with equal endpoints."
    :fragment #{:all-time-rectangles :valid-time-segments :diagonal-interval-boundaries}
    :dependencies #{::put-replacement ::pinned-read ::retained-history}}})

;; Relevance only: each condition also needs non-provider evidence.
(def ^:private links
  {:successful-guard-establishes-exact-prepared-image #{::decoded-assertions ::ordered-transaction-basis}
   :commit-applies-exact-source-derived-put-image #{::put-replacement ::commit-abort}
   :abort-preserves-complete-mapped-image #{::commit-abort}
   :qualified-maintenance-preserves-complete-mapped-image #{::put-replacement ::commit-abort ::worker-clock}
   :maintenance-exact-footprint-and-provider-effect-correspondence #{::decoded-assertions ::ordered-transaction-basis ::put-replacement ::commit-abort ::worker-clock}
   :faithful-pinned-provider-readbacks #{::pinned-read}
   :faithful-authoritative-record-and-historical-evidence-readback #{::pinned-read ::retained-history}
   :faithful-single-cut-record-and-state-projection #{::pinned-read}
   :exact-actual-endpoint-and-attributed-observation-boundaries #{::ordered-transaction-basis ::pinned-read ::retained-history}
   :protected-image-constant-across-time-advance #{::valid-time-boundaries}
   :faithful-complete-clock-and-valid-time-boundaries #{::worker-clock ::retained-history ::valid-time-boundaries}})

(defn- reject! [reason]
  (throw (ex-info "Unqualified XTDB provider contract" {:reason reason})))

(defn- check-links! [required rows mapping]
  (let [ids (set (keys rows))]
    (when-not (and (= required (set (keys mapping)))
                   (= ids (into #{} cat (vals mapping)))
                   (every? #(and (set? %) (seq %) (every? ids %)) (vals mapping))
                   (every? #(and (string? (:property %)) (seq (:property %))
                                 (set? (:fragment %)) (seq (:fragment %))
                                 (set? (:dependencies %)) (every? ids (:dependencies %))) (vals rows)))
      (reject! :provider-contract-inventory-mismatch))
    ;; A dependency cycle cannot be a well-founded premise inventory.
    (loop [remaining ids completed #{}]
      (when (seq remaining)
        (let [ready (into #{} (filter #(set/subset? (get-in rows [% :dependencies]) completed)) remaining)]
          (when (empty? ready) (reject! :cyclic-provider-contract-dependencies))
          (recur (set/difference remaining ready) (into completed ready)))))))

(defn- boundaries! []
  (when-not (and (identical? original-inventory inventory/derive!)
                 (identical? original-describe assumption/describe-assumption))
    (reject! :provider-contract-boundary-replaced)))

(defn derive!
  "Re-derive the issued CLM-R2 inventory and bind the exact provider/read-time
   condition subset to a versioned structural contract. The supplied provider
   descriptor selects the reviewed target, NOT the actually executing code."
  [admission source declarations fingerprint cases provider]
  (let [base {:profile profile :provider-correspondence :not-established
              :loaded-provider-identity :not-established :writer-coverage :not-established
              :universal-refinement :not-established :proof-discharge :not-performed
              :premise-acceptance :not-performed :authority :none :certificate :not-issued}]
    (try
      (boundaries!)
      (when-not (= reviewed-provider provider) (reject! :unsupported-provider-target))
      (let [i (original-inventory admission source declarations fingerprint cases)
            required (set/union (get-in i [:groups :provider-effects :conditions])
                                (get-in i [:groups :read-and-time :conditions]))]
        (when-not (and (= :complete-open-condition-review (:status i))
                       (= fingerprint (:catalog-fingerprint i))
                       (= :open (:original-obligation-status i))
                       (= :not-performed (:proof-discharge i) (:premise-acceptance i))
                       (set/subset? required (:remaining-conditions i)))
          (reject! :unqualified-provider-condition-inventory))
        (check-links! required clauses links)
        (let [scope {:profile profile :provider reviewed-provider :catalog-fingerprint fingerprint
                     :induction-fingerprint (:induction-fingerprint i)}
              premises (into {} (map (fn [[id {:keys [property fragment dependencies]}]]
                                       [id (original-describe
                                            {:id id :subject {:kind :implementation :id ::provider :interface scope}
                                             :scope {:kind :implementation :id ::provider}
                                             :proposition [id property] :parameters {:fragment fragment}
                                             :semantic-context scope :dependencies dependencies
                                             :origin :implementation-correspondence
                                             :implementation-boundary ::xtdb-provider})])) clauses)]
          (boundaries!)
          (merge base {:status :open-provider-contract :scope scope :premises premises
                       :condition-links links :condition-link-semantics :related-not-entailing
                       :linked-conditions required :remaining-conditions (:remaining-conditions i)
                       :non-provider-requirements #{:exact-source-derived-envelope :complete-writer-mediation
                                                    :qualified-deployment-isolation :faithful-projection-and-attribution
                                                    :actual-loaded-artifact-correspondence :complete-temporal-observation}
                       :original-inventory i})))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-provider-contract))))))

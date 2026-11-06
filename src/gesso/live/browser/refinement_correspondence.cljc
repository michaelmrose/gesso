(ns gesso.live.browser.refinement-correspondence
  "Witness-relative reconciliation of real browser observations with an independently
   replayed concrete Choreo realization schedule.

   The browser trace comes from adapter transitions (v1453). The reference
   trace comes from gesso.choreo.correspondence/check-weak-witness, which
   independently executes the global semantics and the projected realization.
   Callers must supply explicit increasing realization indices for *every*
   browser receive candidate. This prevents omission, reuse, reordering, or
   substitution of occurrences. Unmatched global observations are permitted:
   a role-local browser need not observe other roles' operations.

   This is an exact SHAPE/ORDER reconciliation for one supplied model schedule,
   not an attestation that the browser's actual network traffic or imported
   machine came from that model execution. Physical message identity, browser
   host provenance, choreography-to-browser instance linkage, terminal outcome,
   and XTDB commit correspondence remain open obligations. Reports are copyable
   diagnostic data, never proof credentials. Portable CLJC."
  (:require [gesso.choreo.correspondence :as correspondence]
            [gesso.choreo.semantics :as semantics]
            [gesso.live.browser.refinement :as refinement]))

(def correspondence-version 1)
(def report-type :gesso.live.browser.refinement-correspondence/report)

(def ^:private unverified-premises
  #{:actual-browser-event-origin
    :actual-transport-send-and-delivery-identity
    :browser-projection-to-global-execution-identity
    :physical-host-execution-correspondence
    :authoritative-operation-and-xtdb-commit-correspondence
    :all-schedules-trace-refinement-not-established})

(defn- reject! [kind data]
  (throw (ex-info "Browser refinement witness correspondence rejected."
                  (assoc data
                         :error/type :gesso.live.browser.refinement-correspondence/error
                         :error/kind kind))))

(defn- require-alignment! [alignment candidates realized]
  (when-not (and (vector? alignment)
                 (= (count alignment) (count candidates)))
    (reject! :incomplete-candidate-alignment
             {:candidate-count (count candidates)
              :alignment-count (when (vector? alignment) (count alignment))}))
  ;; Validate the *whole* positional mapping before comparing observations.
  ;; This makes missing/reused/reordered positions fail deterministically even
  ;; when an earlier mismatched value would otherwise mask the ordering error.
  (doseq [[candidate-index index] (map-indexed vector alignment)]
    (let [previous-index (if (zero? candidate-index)
                           -1 (nth alignment (dec candidate-index)))]
      (when-not (and (integer? index)
                     (<= 0 index)
                     (integer? previous-index)
                     (< previous-index index)
                     (< index (count realized)))
        (reject! :invalid-or-reordered-alignment
                 {:candidate-index candidate-index
                  :realization-index index
                  :previous-realization-index previous-index
                  :realization-count (count realized)}))))
  (mapv (fn [candidate-index index]
          (let [candidate (nth candidates candidate-index)
                semantic (:distributed-observation candidate)
                witnessed (nth realized index)]
            (when-not (and (= :role-local-receive (:origin candidate))
                           (= :candidate-only (:mapping candidate))
                           (semantics/distributed-observation? semantic)
                           (= :communication (:kind semantic))
                           (semantics/distributed-observation? witnessed)
                           (= semantic witnessed))
              (reject! :candidate-witness-mismatch
                       {:candidate-index candidate-index
                        :realization-index index
                        :candidate-observation semantic
                        :witness-observation witnessed}))
            {:candidate-index candidate-index
             :realization-index index
             :execution-id (:execution-id candidate)
             :generation (:generation candidate)
             :distributed-observation semantic
             :correspondence :model-witness-shape-and-order-only}))
        (range (count candidates)) alignment))

(defn check-correspondence
  "Replay browser events and a separate explicit Choreo witness, then match all
   browser receive candidates to increasing positions in the independently
   checked realization trace.

   `alignment` is a vector of realization-trace indices in browser candidate
   order. An empty alignment is valid only if there are no candidates. The
   optional options map is forwarded, without reinterpretation, to Gesso's
   closed-envelope weak correspondence checker. An invalid model witness
   rejects before any candidate can match. Model replay is not physical proof."
  ([initial-state browser-events choreography witness alignment]
   (check-correspondence initial-state browser-events choreography witness
                         alignment nil))
  ([initial-state browser-events choreography witness alignment options]
   (let [browser (refinement/trace-projected initial-state browser-events)
         checker (correspondence/check-weak-witness
                  choreography witness options)]
     (when-not (and (correspondence/result? checker)
                    (= correspondence/weak-correspondence-property
                       (:property checker))
                    (correspondence/valid? checker)
                    (true? (get-in checker [:observable-trace-replay :valid?])))
       (reject! :invalid-independent-witness
                {:counterexample (:counterexample checker)
                 :property (:property checker)}))
     (let [realized (:realization-trace checker)
           candidates (:distributed-candidates browser)]
       (when-not (and (vector? realized)
                      (every? semantics/distributed-observation? realized))
         (reject! :invalid-independent-realization-trace
                  {:realization-trace realized}))
       (let [matches (require-alignment! alignment candidates realized)]
         {:gesso.live.browser.refinement-correspondence/type report-type
          :gesso.live.browser.refinement-correspondence/version correspondence-version
          :status :conditionally-matched-model-witness
          :browser-candidate-count (count candidates)
          :browser-event-count (count browser-events)
          :witness-step-count (count witness)
          :realization-trace realized
          :realization-trace-replay (:observable-trace-replay checker)
          :correspondence-property (:property checker)
          :alignment alignment
          :matches matches
          :browser-missing-witnesses (:missing-witnesses browser)
          :unverified-premises unverified-premises
          :missing-physical-witnesses (into unverified-premises
                                            (:missing-witnesses browser))
          :global-trace-admitted? false
          :browser-host-refinement-established? false
          :proof-authority :none})))))

(defn recheck-correspondence
  "Exactly rederive a copyable report from the original browser inputs and
   independent Choreo witness. Equality is diagnostic and grants no authority."
  ([initial-state browser-events choreography witness alignment proposed]
   (recheck-correspondence initial-state browser-events choreography witness
                           alignment nil proposed))
  ([initial-state browser-events choreography witness alignment options proposed]
   (let [actual (check-correspondence initial-state browser-events choreography
                                      witness alignment options)]
     (when-not (= actual proposed)
       (reject! :correspondence-report-mismatch
                {:actual-candidate-count (:browser-candidate-count actual)
                 :proposed-candidate-count (:browser-candidate-count proposed)}))
     actual)))

(ns gesso.assurance.proof.xtdb-gateway-typed-transition-seam
  "v1755: Opt-in, one-document, pre-submission join from Gesso's original
   ABSTRACT typed guarded-assignment interpreter to the EXISTING real XTDB
   certified-plan, acquired-before, complete-top-level-CAS one-shot gateway.

   The caller supplies a transition declaration and input snapshot. Require
   the original and independently implemented abstract interpreters to agree
   that the COMPLETE, explicitly bijective before/after document projections in the
   candidate UPDATE satisfy the
   typed guard, simultaneous assignments and full declared frame. Require the
   selected route operation and canonical model command operation to match
   the declaration's source subject. These checks run BEFORE the physical
   gateway is prepared and AGAIN on the wrapper's sole submit attempt.

   The underlying existing gateway independently checks the current certified
   route target, acquires a matching external XTDB before, and submits a
   complete top-level CAS. It is the ONLY physical writer used here. The
   external prestate read is not transaction-internal; the transaction CAS
   rejects changed top-level fields but does not exclude ABA or raw writers.

   This is conditional physical gate COMPOSITION, NOT source authenticity,
   complete proof-premise coverage, business authorization, global writer
   completeness, a commit-causality theorem or formal-to-production transfer.
   In particular, the v1754 source-read invariant predicates have NOT been
   bound to this caller-supplied transition. A forged declaration can describe
   a different business policy. No certificate or application admission is
   issued. A submitted result is not automatically a committed result."
  (:require [gesso.assurance.proof.transition :as transition]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate :as physical]
            [gesso.model.command :as command])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type :gesso.assurance.proof.xtdb-gateway-typed-transition-seam/one-shot)
(def report-version 1)

(def ^:private original-describe transition/describe-transition!)
(def ^:private original-observe transition/observe-transition!)
(def ^:private independent-observe independent/observe-transition!)
(def ^:private original-physical physical/prepare-plan-bound-acquired-one-shot!)
(def ^:private original-operation command/operation)

(defn- reject! [reason]
  ;; Do not include documents, inputs, target IDs, plans, tokens or exception causes.
  (throw (ex-info "Unsupported typed transition / guarded XTDB join."
                  {:xtdb-gateway-typed-transition-seam/rejection true
                   :reason reason})))

(defn- seams! []
  ;; Reject accidental JVM var substitution, not hostile bytecode/redefinitions.
  (when-not (and (identical? original-describe (var-get #'transition/describe-transition!))
                 (identical? original-observe (var-get #'transition/observe-transition!))
                 (identical? independent-observe
                             (var-get #'independent/observe-transition!))
                 (identical? original-physical
                             (var-get #'physical/prepare-plan-bound-acquired-one-shot!))
                 (identical? original-operation (var-get #'command/operation)))
    (reject! :substituted-source-or-physical-interpreter)))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))))

(defn- source-relation!
  [selected-operation candidate-source declaration inputs field-map]
  (seams!)
  (when-not (and (keyword? selected-operation) (namespace selected-operation)
                 (plain-map? declaration)
                 (plain-map? inputs)
                 (plain-map? field-map)
                 (= selected-operation (get-in declaration [:subject :id]))
                 (plain-map? candidate-source)
                 (plain-map? (:plan candidate-source))
                 (= 1 (count (get-in candidate-source [:plan :commands])))
                 (false? (get-in candidate-source [:plan :emit])))
    (reject! :operation-or-candidate-source-mismatch))
  (let [cmd (first (get-in candidate-source [:plan :commands]))
        before (:model/before cmd)
        after (:model/after cmd)]
    (when-not (and (plain-map? before) (plain-map? after)
                   (keyword? (:model/operation cmd))
                   (= (:model/operation cmd)
                      (get-in declaration [:subject :interface :operation])))
      (reject! :canonical-command-or-source-interface-mismatch))
    (let [state-ids (set (keys (:state-types declaration)))
          physical-ids (set (keys before))
          mapped (vals field-map)
          _ (when-not (and (= state-ids (set (keys field-map)))
                           (= (count mapped) (count (set mapped)))
                           (= physical-ids (set mapped))
                           (= physical-ids (set (keys after)))
                           (every? keyword? mapped))
              (reject! :incomplete-or-nonbijective-physical-field-map))
          abstract-before (into {} (map (fn [[field physical-id]]
                                          [field (get before physical-id)])) field-map)
          abstract-after (into {} (map (fn [[field physical-id]]
                                         [field (get after physical-id)])) field-map)
          description (try (original-describe declaration)
                           (catch Exception _
                             (reject! :unsupported-abstract-declaration)))
          observed (try (original-observe declaration abstract-before inputs abstract-after)
                        (catch Exception _
                          (reject! :invalid-abstract-transition-values)))
          independent (try (independent-observe declaration abstract-before inputs abstract-after)
                           (catch Exception _
                             (reject! :invalid-independent-transition-values)))]
      (when-not (and (= transition/description-type (:type description))
                     (= transition/observation-type (:type observed))
                     (= independent/report-type (:type independent))
                     (= (:fingerprint description) (:transition-fingerprint observed))
                     (true? (:matches? observed))
                     (true? (:matches? independent))
                     (= (mapv #(get observed %) [:guard-holds? :assignments-hold? :frame-holds?])
                        (mapv #(get independent %) [:guard-holds? :assignments-hold? :frame-holds?]))
                     (= :none (:authority description) (:authority observed)
                        (:authority independent))
                     (= :not-admitted (:admission description) (:admission observed)
                        (:admission independent)))
        (reject! :original-and-independent-abstract-relation-not-established))
      (seams!)
      true)))

(defn prepare-typed-transition-one-shot!
  "Join the original abstract guarded transition to an actual plan-bound,
   externally acquired complete-CAS one-shot XTDB gateway. `declaration` and
   `inputs` are caller supplied and NOT authenticated as source-owned business
   meaning; nor are theorem read-premise inventories checked. Source before
   and after MUST be the full physical documents. `field-map` bijectively
   maps every qualified abstract state key to one physical document key;
   no unmodeled physical keys or aliases are permitted.

   Returns only {:type :version :submit! :summary!}, exposing no raw writer.
   A first submission attempt consumes the wrapper permit before rechecking
   current abstract/physical seams. Any intervening physical write is refused
   by the existing transaction-time full-top-level CAS. Neither a copied
   abstract report nor a preceding preflight can submit a transaction."
  [node contract-source resolver-issuance selected-operation request
   candidate-source declaration inputs field-map]
  (source-relation! selected-operation candidate-source declaration inputs field-map)
  (let [prepared (try (original-physical node contract-source resolver-issuance
                                        selected-operation request candidate-source)
                      (catch Exception _
                        (reject! :physical-gateway-preparation-rejected)))
        summary (try ((:summary! prepared))
                     (catch Exception _ (reject! :invalid-physical-gateway-summary)))]
    (when-not (and (= physical/report-type (:type prepared) (:type summary))
                   (= physical/report-version (:version prepared) (:version summary))
                   (= :prepared (:status summary))
                   (= :canonical-complete-top-level-cas (:transaction-preconditions summary))
                   (= :same-node-pinned-read-matched-at-preparation
                      (:external-before summary))
                   (fn? (:submit! prepared)) (fn? (:summary! prepared)))
      (reject! :unexpected-physical-gateway-contract))
    (source-relation! selected-operation candidate-source declaration inputs field-map)
    (let [attempted (AtomicBoolean. false)
          submit! (fn []
                    (when-not (.compareAndSet attempted false true)
                      (reject! :one-shot-already-attempted))
                    (source-relation! selected-operation candidate-source
                                      declaration inputs field-map)
                    ;; The underlying prepared gateway rechecks current route
                    ;; correspondence and consumes its own one-use permit.
                    ;; Its XTDB transaction, not this preflight, guards against
                    ;; physical changes after external prestate acquisition.
                    ((:submit! prepared)))
          summary! (fn []
                     {:type report-type
                      :version report-version
                      :status (if (.get attempted) :wrapper-permit-consumed :prepared)
                      :original-and-independent-abstract-relations :checked-per-attempt
                      :prestate-source :candidate-complete-bijective-document
                      :external-prestate :same-node-pinned-read-matched-at-preparation
                      :transaction-time-guard :existing-complete-top-level-cas
                      :source-declaration-provenance :caller-supplied-not-authenticated
                      :source-read-theorem-premises :not-established
                      :principal-authorization :not-established
                      :whole-application-writer-coverage :not-established
                      :aba-history-detection :not-established
                      :commit-causality :not-established
                      :physical-proof-refinement :not-established
                      :submission-capability :one-use
                      :proof-status :conditional-abstract-and-physical-gateway-join
                      :certificate :not-issued :admission :not-admitted
                      :transaction-authority :one-shot-submission-capability})]
      (seams!)
      {:type report-type :version report-version
       :submit! submit! :summary! summary!})))

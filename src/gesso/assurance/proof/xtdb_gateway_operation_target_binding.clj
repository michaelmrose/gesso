(ns gesso.assurance.proof.xtdb-gateway-operation-target-binding
  "v1574: fail-closed PRE-SUBMISSION comparison of one selected Gesso
   operation, an independently supplied expected physical document, and the
   command actually handed to the guarded XTDB source builder.

   The checked route's operation ID and the expected command operation/target
   MUST come through separate arguments from the candidate source. This is a
   local shape-and-equality check, NOT authentication of the expected target:
   that premise must be discharged by the calling application construction.
   A source builder cannot substitute an unrelated document unnoticed WHEN
   the expectation is independently correct. A matching document still says
   nothing about its complete business-rule semantics or transaction causality.

   No database calls, writers, callbacks, certificates, or admission are issued.
   No document ID, row, SQL parameter, plan, or exception cause is returned."
  (:require [gesso.model.command :as command])
  (:import (java.util UUID)))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-operation-target-binding/local-match)
(def report-version 1)

(def ^:private original-update? command/update?)
(def ^:private original-target command/target)
(def ^:private original-operation command/operation)

(defn- reject! [reason]
  (throw
   (ex-info "Unsupported XTDB operation/physical-target binding."
            {:xtdb-gateway-operation-target-binding/rejection true
             :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- plain-vector? [x]
  (and (instance? clojure.lang.APersistentVector x)
       (nil? (meta x))))

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- table? [x]
  ;; Match the supported finite, direct gateway's ordinary XTDB table subset.
  ;; SQL fragments, schema-qualified names, and dynamic table expressions are
  ;; intentionally excluded here, not silently normalized.
  (and (keyword? x)
       (nil? (namespace x))
       (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (name x))))

(defn- physical-id? [x]
  (or (and (string? x) (not (empty? x)) (<= (count x) 512))
      (instance? UUID x)
      (and (integer? x)
           (not (instance? clojure.lang.BigInt x))
           (not (instance? java.math.BigInteger x)))))

(defn- physical-target? [target]
  (and (plain-vector? target)
       (= 2 (count target))
       (table? (nth target 0))
       (physical-id? (nth target 1))))

(defn- pinned! []
  ;; Ordinary JVM Var substitution guard; NOT bytecode integrity or a sandbox.
  (when-not (and (identical? original-update?
                             (var-get #'command/update?))
                 (identical? original-target
                             (var-get #'command/target))
                 (identical? original-operation
                             (var-get #'command/operation)))
    (reject! :substituted-command-checker)))

(defn require-declared-operation-target!
  "Check a locally selected route's expected document against the *actual*
   one-command, one-protected-document XTDB gateway source, without executing.

   Args:
     selected-operation-id : checked route's qualified operation keyword
     expectation           : {:operation-id <same qualified keyword>
                              :command-operation <exact model command keyword>
                              :target [unqualified-xtdb-table immutable-id]}
     candidate-source      : {:plan {:commands [canonical UPDATE] :emit false}
                              :protected-documents
                              [{:target [table id] :fields :all}]}

   The expectation is intentionally NOT extracted from candidate-source. A
   caller must derive it independently from its admitted operation + request
   target. This method cannot authenticate that provenance and MUST NOT be
   represented as a proof of route-to-business-rule equivalence. It rejects
   target, operation and protection disagreement BEFORE writer issuance.

   Returns a *redacted* diagnostic. This is not a reusable proof witness, and
   not authority to submit. The source must still pass the existing complete
   guard and physical-prestate gateway checks."
  [selected-operation-id expectation candidate-source]
  (pinned!)
  (when-not (qualified-id? selected-operation-id)
    (reject! :invalid-selected-operation-id))
  (when-not (and (plain-map? expectation)
                 (= #{:operation-id :command-operation :target}
                    (set (keys expectation)))
                 (qualified-id? (:operation-id expectation))
                 (keyword? (:command-operation expectation))
                 (physical-target? (:target expectation)))
    (reject! :invalid-independent-physical-expectation))
  (when-not (= selected-operation-id (:operation-id expectation))
    (reject! :selected-operation-disagrees-with-expectation))
  (when-not (and (plain-map? candidate-source)
                 (= #{:plan :protected-documents}
                    (set (keys candidate-source)))
                 (plain-map? (:plan candidate-source))
                 (= #{:commands :emit} (set (keys (:plan candidate-source))))
                 (false? (get-in candidate-source [:plan :emit]))
                 (plain-vector? (get-in candidate-source [:plan :commands]))
                 (= 1 (count (get-in candidate-source [:plan :commands])))
                 (plain-vector? (:protected-documents candidate-source))
                 (= 1 (count (:protected-documents candidate-source))))
    (reject! :unsupported-physical-source-shape))
  (let [cmd (first (get-in candidate-source [:plan :commands]))
        protection (first (:protected-documents candidate-source))]
    (when-not (and (plain-map? cmd)
                   (original-update? cmd))
      (reject! :not-one-canonical-update-command))
    (when-not (= (:command-operation expectation)
                 (original-operation cmd))
      (reject! :command-operation-disagrees-with-expectation))
    (when-not (= (:target expectation) (original-target cmd))
      (reject! :command-target-disagrees-with-expectation))
    (when-not (and (plain-map? protection)
                   (= #{:target :fields} (set (keys protection)))
                   (= :all (:fields protection))
                   (= (:target expectation) (:target protection)))
      (reject! :protected-document-disagrees-with-expectation)))
  (pinned!)
  {:type report-type
   :version report-version
   :status :conditional-declared-command-target-match
   :selected-route-operation-checked? true
   :canonical-command-operation-checked? true
   :declared-command-and-protected-document-match-expected-target? true
   :source-submitted? false
   :expected-target-provenance :caller-supplied-not-authenticated
   :abstract-operation-to-command-semantics :not-established
   :complete-deployed-writer-coverage :not-established
   :physical-transaction-causality :not-established
   :proof-status :local-structural-comparison-only
   :open-obligations
   #{:independent-request-to-expected-target-provenance
     :certified-operation-to-model-command-semantics
     :actual-issued-writer-must-use-this-validated-candidate
     :transaction-commit-causality-and-physical-refinement
     :unmediated-or-external-writers-not-excluded}
   :certificate :not-issued
   :admission :not-admitted
   :transaction-authority :none
   :authority :none})

(ns gesso.live.application-preflight.relational-rule-coverage
  "Admit source-interpreted relational business rules alongside the operation
   families of a CURRENT Gesso ApplicationAssembly.

   Application code supplies ordinary Clojure DATA describing relationships,
   a finite modeled initial state, and state-dependent operation families.
   Gesso itself interprets the relations (gesso.model.relational-invariant),
   checks EXACT coverage of the current registered operation IDs, and explores
   the finite reachable model. No arbitrary caller-authored :holds? callback
   is accepted at this boundary. This is a diagnostic admission API, not an
   automatic discovery of all business rules or a startup certificate.

   check-registered-relations returns the finite verdict and an inspectable
   description of the declared rules. build-comparator uses the SAME type of
   rule construction to admit a registry-bound executable comparison. The
   comparator captures the supplied declarations and operation callbacks at
   construction time; at use time it accepts only genuine executor-bound
   diagnostic evidence (through operation-family-correspondence).

   In particular, this does NOT establish completeness of declared business
   policies, modeled documents, principals or arguments; correspondence of
   the authored family callbacks to all production behavior; or unbounded
   invariant preservation. Initial-state vacuity is reported, not silently
   misrepresented as proof that a relation has been exercised. A family may
   create source documents in later states even when none exist initially.

   Neither API executes a database transaction or invokes a real registered
   authoritative executor. Both perform bounded Clojure-model exploration;
   the returned comparator can subsequently inspect one already issued,
   process-local execution-evidence handle without initiating execution."
  (:require [gesso.live.application-preflight.operation-family-coverage
             :as coverage]
            [gesso.live.application-preflight.operation-family-correspondence
             :as correspondence]
            [gesso.model.relational-invariant :as relational]
            [gesso.model.tx-model :as reference]))

(def assurance-scope
  {:checker :gesso.live.application-preflight.relational-rule-coverage/registered-finite-v1
   :checked #{:source-interpreted-relational-rule-admission
              :exact-current-registered-operation-id-coverage
              :full-declared-principal-argument-enumeration
              :bounded-state-dependent-relational-invariant-checking
              :explicit-initial-state-rule-vacuity-reporting
              :executor-bound-single-attempt-comparison-when-requested}
   :assumed #{:complete-and-truthful-declared-business-relationships
              :truthful-pure-total-operation-family-callbacks
              :representative-complete-modeled-state-and-finite-domains
              :correct-current-application-assembly
              :serial-atomic-commit-reference-semantics
              :controlled-process-local-execution-observation}
   :not-established #{:source-derived-completeness-of-business-relationships
                      :source-derived-completeness-of-policy-dependencies
                      :completeness-of-real-principals-or-arguments
                      :business-authorization-or-actual-principal-authenticity
                      :truth-of-developer-chosen-business-relations
                      :unbounded-invariant-preservation
                      :correspondence-of-all-family-callbacks-to-production
                      :real-xtdb-constraint-enforcement
                      :verified-browser-projection
                      :whole-application-certification}})

(defn- describe-initial-rules
  "Return local, *non-certifying* coverage facts. Counting initially matching
   sources avoids treating vacuity as evidence that a rule was exercised.
   Missing source-selector attributes are violations reported by inspect, not
   instances to omit from the modeled invariant."
  [initial-state declarations]
  (reference/require-state initial-state)
  (mapv (fn [declaration]
          (let [inspection (relational/inspect initial-state declaration)]
            {:rule-id (:id declaration)
             :source-kind (:source-kind declaration)
             :target-kind (:target-kind declaration)
             :initially-matching-source-count (:inspected-source-count inspection)
             :initially-exercised? (pos? (:inspected-source-count inspection))
             :initially-holds? (:holds? inspection)}))
        declarations))

(defn check-registered-relations
  "Check declared relational rules against the exact operation-ID inventory
   obtained from a freshly rechecked ApplicationAssembly.

   (check-registered-relations assembly state families declarations options)

   `declarations` is a nonempty vector of the closed, source-interpreted
   relation maps accepted by relational/compile-rules. No caller-authored
   arbitrary invariant predicate is passed through. The existing operation
   family builder callbacks remain trusted, and up to six instances are
   explored within the caller's strict depth and transition budgets.

   Returns the existing registry+family finite verdict, augmented with the
   canonical declarations, initial-state rule coverage, and explicit scopes.
   A :counterexample is a finite-model violation, not an exception. Malformed
   declarations, registry mismatches, stale assemblies and budget exhaustion
   fail closed. Passing does NOT certify actual application business rules."
  [assembly initial-state families declarations options]
  (let [rules (relational/compile-rules declarations)
        verdict (coverage/check-registered-families
                 assembly initial-state families rules options)
        root-facts (describe-initial-rules initial-state declarations)]
    (assoc verdict
           :relational-declarations declarations
           :relational-coverage
           {:status :checked-declared-relations-in-finite-model
            :rule-ids (mapv :id declarations)
            :initial-state-rule-coverage root-facts
            :initially-unexercised-rule-ids
            (mapv :rule-id (remove :initially-exercised? root-facts))
            :declaration-completeness :not-established
            :registered-executor-correspondence :not-established}
           :relational-scope relational/assurance-scope
           :registered-relational-scope assurance-scope)))

(defn build-comparator
  "Admit declared relational invariants with the EXACT registered operation
   families, then return a one-argument comparison function over a genuine
   opaque execution-evidence handle.

   The admission must pass bounded relational-invariant exploration; a
   counterexample prevents comparator construction. v828 then freezes the
   admitted family callbacks and verifies the currently registered executor
   identity and the observed submitted plan whenever called.

   Unlike check-registered-relations, this returns an executable diagnostic
   comparison capability rather than a report. The function exposes no
   arbitrary :holds? callback slot and does not accept replacement business
   declarations at comparison time. A matching observed attempt still does
   not prove completeness or correctness of the application's domain policy."
  [assembly initial-state families declarations options]
  (let [rules (relational/compile-rules declarations)
        compare! (correspondence/build-comparator
                  assembly initial-state families rules options)
        rule-ids (mapv :id declarations)]
    (fn [handle]
      (assoc (compare! handle)
             :relational-rule-ids rule-ids
             :relational-admission :holds-in-explored-model
             :relational-scope relational/assurance-scope
             :registered-relational-scope assurance-scope))))

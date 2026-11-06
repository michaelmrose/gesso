(ns gesso.assurance.proof.application-writer-coverage-mutation-test
  "v1447: adversarial checks for the v1446 DECLARED application to observed
   writer-ID join. A matching set of identifiers is not evidence of matching
   handler semantics or complete discovery of physical XTDB writers.

   Most mutation cases intentionally exercise the pure, PRIVATE source-report
   reconciliation function using synthetic snapshots. This is not the trusted
   production source-acquisition path. Separate assertions exercise public
   rejection and fresh-report comparison boundaries; the existing v1367
   integration suite owns actual issued-executor and host acquisition."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.application-writer-coverage :as coverage]))

(def ^:private claim :request/claim)
(def ^:private unclaim :request/unclaim)
(def ^:private desired #{claim unclaim})
(def ^:private unknown :request/unknown)
(def ^:private direct :request/direct-xtdb-write)

(def ^:private declared
  {:catalog-id :request/board
   :catalog-fingerprint "sha256:declared"
   :induction-fingerprint "sha256:induction"
   :ledger-fingerprint "sha256:ledger"
   :operation-ids desired
   :original-open-obligation-ids-by-operation
   {claim #{:claim-guard-physical-correspondence}
    unclaim #{:unclaim-write-physical-correspondence}}
   :open-meta-obligations #{:physical-write-path-coverage}
   :open-assumptions #{:untrusted-external-state}
   :installed-writer-coverage :not-established
   :certificate :not-issued :admission :not-admitted :authority :none})

(def ^:private observed
  {:induction-catalog-id :request/induction
   :application-catalog-fingerprint "sha256:issued"
   :independent-operation-ids desired
   :issued-application-operation-ids desired
   :observed-semantic-route-ids desired
   :observed-entry-point-ids desired
   :observed-unclassified-entry-point-ids #{}
   :open-obligations #{:host-enumerator-completeness}
   :conditional-id-join? true})

;; This function is deliberately private in production. These inputs represent
;; independently obtained source and host summaries ONLY for mutation testing;
;; no fabricated report is ever passed to the public acquisition boundary.
(def ^:private assemble
  (var-get (ns-resolve 'gesso.assurance.proof.application-writer-coverage
                       'assemble-snapshot-report!)))

(defn- rejected [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- no-physical-authority! [r]
  (doseq [[k expected]
          [[:physical-writer-coverage :not-established]
           [:abstract-to-physical-correspondence :not-established]
           [:declaration-semantic-equivalence :not-established]
           [:host-enumerator-exhaustiveness :not-established]
           [:deployment-correspondence :not-established]
           [:universal-kernel-soundness :not-established]
           [:assumption-discharge :not-performed]
           [:proof-status :conditional-snapshot-id-join-only]
           [:certificate :not-issued]
           [:admission :not-admitted]
           [:transaction-authority :none]
           [:authority :none]]]
    (is (= expected (get r k))
        (str "Unexpectedly upgraded assurance for " k))))

(deftest matching-three-way-ids-retain-the-original-open-obligations
  (let [r (assemble declared observed)]
    (is (= coverage/coverage-type (:type r)))
    (is (= coverage/coverage-version (:version r)))
    (is (= :conditional-observed-operation-id-join (:status r)))
    (is (true? (:exact-observed-operation-id-join? r)))
    (is (= desired (:declared-operation-ids r)))
    (is (= desired (:independent-operation-ids r)))
    (is (= desired (:issued-operation-ids r)))
    (is (= desired (set (map :operation-id (:operation-rows r)))))
    (is (= #{:claim-guard-physical-correspondence}
           (get-in r [:original-open-obligation-ids-by-operation claim])))
    (is (set/subset? #{:physical-write-path-coverage
                       :host-enumerator-completeness
                       :all-direct-and-nonroute-xtdb-writers-enumerated
                       :all-physical-transitions-refine-the-abstract-machine}
                     (:open-obligations r)))
    (is (= #{:untrusted-external-state} (:open-assumptions r)))
    (no-physical-authority! r)))

(deftest missing-and-foreign-issued-operations-are-never-silent
  (doseq [ids [#{} #{claim} #{unknown} #{claim unknown} (conj desired unknown)]]
    (let [r (assemble declared (assoc observed :issued-application-operation-ids ids))
          d (:declared-vs-issued r)]
      (is (= :open-operation-catalog-mismatch (:status r)))
      (is (false? (:exact-observed-operation-id-join? r)))
      (is (= (set/difference desired ids) (:missing-from-right d)))
      (is (= (set/difference ids desired) (:unmodeled-in-right d)))
      (is (false? (:exact? d)))
      (is (contains? (:open-obligations r)
                     :declared-independent-and-issued-operation-id-mismatch))
      (is (= (set/union desired ids) (set (map :operation-id (:operation-rows r)))))
      (no-physical-authority! r))))

(deftest missing-and-foreign-independent-operations-remain-distinct
  (doseq [ids [#{} #{claim} #{unknown} #{claim unknown} (conj desired unknown)]]
    (let [r (assemble declared (assoc observed :independent-operation-ids ids))
          d (:declared-vs-independent r)]
      (is (= :open-operation-catalog-mismatch (:status r)))
      (is (= (set/difference desired ids) (:missing-from-right d)))
      (is (= (set/difference ids desired) (:unmodeled-in-right d)))
      (is (true? (get-in r [:declared-vs-issued :exact?])))
      (no-physical-authority! r))))

(deftest unclassified-observed-writers-are-preserved-not-ignored
  (doseq [extras [#{direct} #{unknown direct}]]
    (let [r (assemble declared
                      (-> observed
                          (update :observed-entry-point-ids set/union extras)
                          (assoc :observed-unclassified-entry-point-ids extras)))]
      (is (= :open-unclassified-observed-entry-points (:status r)))
      (is (true? (:exact-observed-operation-id-join? r)))
      (is (= extras (:observed-unclassified-entry-point-ids r)))
      (is (= :contains-unclassified-observed-entries (:host-snapshot-status r)))
      (is (contains? (:open-obligations r)
                     :observed-entry-points-outside-declared-application))
      (no-physical-authority! r))))

(deftest both-catalog-mismatch-and-unclassified-writer-remain-visible
  (let [extra #{direct}
        r (assemble declared
                    (-> observed
                        (assoc :issued-application-operation-ids #{claim})
                        (update :observed-entry-point-ids set/union extra)
                        (assoc :observed-unclassified-entry-point-ids extra)))]
    (is (= :open-operation-catalog-mismatch (:status r)))
    (is (= #{unclaim} (get-in r [:declared-vs-issued :missing-from-right])))
    (is (= extra (:observed-unclassified-entry-point-ids r)))
    (is (every? (:open-obligations r)
                [:declared-independent-and-issued-operation-id-mismatch
                 :observed-entry-points-outside-declared-application]))
    (no-physical-authority! r)))

(deftest same-ids-cannot-prove-substituted-handler-semantics
  ;; A malicious handler can retain its operation ID while changing its
  ;; implementation. Since this bridge receives IDs, not a semantic proof of
  ;; that handler, it MUST leave equivalence open even on a clean snapshot.
  (let [r (assemble declared observed)]
    (is (= :conditional-observed-operation-id-join (:status r)))
    (doseq [row (:operation-rows r)]
      (is (= :not-established (:abstract-source-equivalence row)))
      (is (= :not-established (:installed-handler-semantic-equivalence row)))
      (is (= :not-established (:physical-transition-refinement row))))
    (no-physical-authority! r)))

(deftest unobserved-direct-writer-cannot-be-declared-impossible
  ;; An omitted physical writer produces exactly the same finite observed
  ;; snapshot: no source-only algorithm can infer that it doesn't exist.
  (let [r (assemble declared observed)]
    (is (empty? (:observed-unclassified-entry-point-ids r)))
    (is (= :no-unclassified-entries-observed (:host-snapshot-status r)))
    (is (true? (:exact-observed-operation-id-join? r)))
    (is (contains? (:open-obligations r)
                   :host-observed-entry-point-enumerator-exhaustiveness))
    (is (contains? (:open-obligations r)
                   :all-direct-and-nonroute-xtdb-writers-enumerated))
    (no-physical-authority! r)))

(deftest unresolved-upstream-status-must-not-be-promoted
  (let [r (assemble declared (assoc observed :conditional-id-join? false))]
    (is (= :open-independent-source-premises (:status r)))
    (is (true? (:exact-observed-operation-id-join? r)))
    (no-physical-authority! r)))

(deftest malformed-original-application-summary-rejects
  (let [bad [(assoc declared :certificate :issued)
             (assoc declared :admission :admitted)
             (assoc declared :authority :application-certified)
             (assoc declared :installed-writer-coverage :proved)
             (update declared :original-open-obligation-ids-by-operation
                     dissoc claim)]]
    (doseq [candidate bad]
      (is (= :unexpected-application-closure-contract
             (:reason (rejected #(assemble candidate observed))))))))

(deftest report-copy-and-forged-claims-rejected-by-recheck-boundary
  ;; Isolate exact-report comparison from expensive trusted source derivation.
  ;; The public source path is exercised separately by its input boundary and
  ;; the permanent upstream application/writer tests.
  (let [original (assemble declared observed)
        copy (edn/read-string (pr-str original))
        candidates [(assoc copy :certificate :issued)
                    (assoc copy :physical-writer-coverage :proved)
                    (assoc copy :host-enumerator-exhaustiveness :established)
                    (assoc copy :exact-observed-operation-id-join? false)
                    (assoc copy :observed-entry-point-ids #{})
                    (update copy :open-obligations disj
                            :host-observed-entry-point-enumerator-exhaustiveness)
                    (with-meta copy {:trusted true})]]
    (is (= original copy))
    (with-redefs [coverage/describe-application-writer-coverage!
                  (fn [& _] original)]
      (is (= original (coverage/require-current-application-writer-coverage!
                       copy nil nil nil nil)))
      (doseq [[i candidate] (map-indexed vector candidates)]
        (testing (str "Forged report " i)
          (is (= :stale-or-substituted-coverage-report
                 (:reason (rejected
                           #(coverage/require-current-application-writer-coverage!
                             candidate nil nil nil nil))))))))))

(deftest malformed-writer-source-rejected-before-any-expensive-closure
  (doseq [source [nil {} {:induction-catalog {}}
                  {:application-induction-source {}}
                  {:induction-catalog {}
                   :application-induction-source {}
                   :certificate :issued}
                  (with-meta {:induction-catalog {}
                              :application-induction-source {}}
                    {:certified true})]]
    (is (= :invalid-original-writer-source
           (:reason (rejected
                     #(coverage/describe-application-writer-coverage!
                       nil nil nil source)))))))

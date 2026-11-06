(ns gesso.assurance.proof.application-assurance-invalidation-test
  "v1449: adversarial source invalidation and premise propagation for the
   NONAUTHORITATIVE v1448 application report. Synthetic snapshots exercise
   the pure cross-source join; the public recheck tests deliberately replace
   source-acquisition functions so stale reports cannot pass by identity alone.
   These tests do not establish host enumeration exhaustiveness or physical
   writer coverage."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.application-assurance-report :as sut]
            [gesso.assurance.proof.application-dependency :as app]
            [gesso.assurance.proof.operation-interface-composition :as comp]
            [gesso.assurance.proof.operation-interface-provenance :as prov]
            [gesso.assurance.proof.application-writer-coverage :as writer]))

(def catalog-id :request/catalog)
(def catalog-fingerprint "sha256:source")
(def induction-fingerprint "sha256:induction")
(def ledger-fingerprint "sha256:ledger")
(def ids #{:request/claim :request/release})
(def invs #{:request/ownership})
(def original {:request/claim #{:guard :write}
               :request/release #{:guard}})
(def open-meta #{:physical-write-path-coverage :implementation-correspondence})
(def assumptions #{:unverified-business-policy})
(def details
  (into {}
        (for [id ids]
          [id {:operation-id id
               :source-fingerprint (str "source:" (name id))
               :transition-fingerprint (str "transition:" (name id))
               :original-open-obligation-ids (get original id)
               :physical-writer-coverage :not-established
               :implementation-correspondence :not-established
               :certificate :not-issued :admission :not-admitted :authority :none}])))
(def inv-results
  {:request/ownership
   {:initial-status :checked-conditional-inference
    :preservation-by-operation
    {:request/claim :checked-conditional-inference
     :request/release :checked-conditional-inference}
    :induction-status :checked-conditional-induction
    :open-reason nil}})
(def declared
  {:type app/closure-type :version app/closure-version
   :catalog-id catalog-id :catalog-fingerprint catalog-fingerprint
   :induction-fingerprint induction-fingerprint
   :ledger-fingerprint ledger-fingerprint
   :operation-ids ids :invariant-ids invs
   :node-count 13 :edge-count 19
   :original-open-obligation-ids-by-operation original
   :open-meta-obligations open-meta :open-assumptions assumptions
   :assumption-discharge :not-performed
   :installed-writer-coverage :not-established
   :implementation-correspondence :not-established
   :certificate :not-issued :admission :not-admitted :authority :none})
(def composed
  {:type comp/composition-type :version comp/composition-version
   :catalog-id catalog-id :catalog-fingerprint catalog-fingerprint
   :induction-fingerprint induction-fingerprint
   :fingerprint "sha256:composition" :interface-fingerprint "sha256:interface"
   :operation-ids ids :invariant-ids invs
   :operation-contracts details :invariant-results inv-results
   :original-open-obligation-ids-by-operation original
   :open-meta-obligations open-meta :open-assumptions assumptions
   :assumption-discharge :not-performed
   :physical-writer-coverage :not-established
   :implementation-correspondence :not-established
   :concurrency-refinement :not-established
   :certificate :not-issued :admission :not-admitted :authority :none})
(def manifest
  {:type prov/provenance-type :version prov/provenance-version
   :fingerprint "sha256:manifest"
   :lineage
   {:catalog-id catalog-id :catalog-fingerprint catalog-fingerprint
    :induction-fingerprint induction-fingerprint
    :induction-ledger-fingerprint ledger-fingerprint
    :composition-fingerprint "sha256:composition"
    :operation-interface-fingerprint "sha256:interface"
    :operations (into {} (for [[id contract] details]
                           [id (select-keys contract
                                            [:source-fingerprint :transition-fingerprint
                                             :original-open-obligation-ids])]))
    :invariants {:request/ownership {:induction-status :checked-conditional-induction}}
    :graph {:node-count 13 :edge-count 19}
    :original-open-obligation-ids-by-operation original
    :open-meta-obligations open-meta :open-assumptions assumptions}
   :assumption-discharge :not-performed
   :installed-writer-coverage :not-established
   :implementation-correspondence :not-established
   :certificate :not-issued :admission :not-admitted :authority :none})
(def coverage
  {:type writer/coverage-type :version writer/coverage-version
   :catalog-id catalog-id :catalog-fingerprint catalog-fingerprint
   :induction-fingerprint induction-fingerprint
   :application-dependency-ledger-fingerprint ledger-fingerprint
   :writer-application-catalog-fingerprint "sha256:writer-source"
   :declared-operation-ids ids :independent-operation-ids ids
   :issued-operation-ids ids :exact-observed-operation-id-join? true
   :status :conditional-observed-operation-id-join
   :observed-entry-point-ids ids
   :observed-unclassified-entry-point-ids #{}
   :declared-vs-independent {:exact? true}
   :declared-vs-issued {:exact? true}
   :independent-vs-issued {:exact? true}
   :operation-rows [{:operation-id :request/claim}
                    {:operation-id :request/release}]
   :original-open-obligation-ids-by-operation original
   :open-assumptions assumptions
   :open-obligations #{:host-enumerator-exhaustiveness}
   :assumption-discharge :not-performed
   :physical-writer-coverage :not-established
   :abstract-to-physical-correspondence :not-established
   :deployment-correspondence :not-established
   :universal-kernel-soundness :not-established
   :certificate :not-issued :admission :not-admitted
   :transaction-authority :none :authority :none})

(defn- summarize
  ([] (summarize [declared composed manifest coverage]))
  ([[d c p w]]
   ((var-get (ns-resolve
              'gesso.assurance.proof.application-assurance-report
              'checked-summary!)) d c p w)))

(defn- error [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejected? [f]
  (= ::sut/invalid-application-assurance-report
     (:error/type (error f))))

(defn- changed-source [kind]
  (case kind
    :catalog-fingerprint
    [(assoc declared :catalog-fingerprint "sha256:source-v2")
     (assoc composed :catalog-fingerprint "sha256:source-v2")
     (assoc-in manifest [:lineage :catalog-fingerprint] "sha256:source-v2")
     (assoc coverage :catalog-fingerprint "sha256:source-v2")]

    :operation-transition
    [declared
     (-> composed
         (assoc-in [:operation-contracts :request/claim :source-fingerprint]
                   "source:claim-v2")
         (assoc-in [:operation-contracts :request/claim :transition-fingerprint]
                   "transition:claim-v2"))
     (-> manifest
         (assoc-in [:lineage :operations :request/claim :source-fingerprint]
                   "source:claim-v2")
         (assoc-in [:lineage :operations :request/claim :transition-fingerprint]
                   "transition:claim-v2"))
     coverage]

    :invariant-result
    [declared
     (-> composed
         (assoc-in [:invariant-results :request/ownership :induction-status] :open)
         (assoc-in [:invariant-results :request/ownership :preservation-by-operation :request/release]
                   :open))
     (assoc-in manifest [:lineage :invariants :request/ownership :induction-status]
               :open)
     coverage]

    :writer-snapshot
    [declared composed manifest
     (-> coverage
         (assoc :writer-application-catalog-fingerprint "sha256:host-v2")
         (assoc :status :open-unclassified-observed-entry-points
                :observed-unclassified-entry-point-ids #{:request/unclassified})
         (update :open-obligations conj :unclassified-host-writer))]

    :assumptions
    [(update declared :open-assumptions conj :new-opaque-policy)
     (update composed :open-assumptions conj :new-opaque-policy)
     (-> manifest
         (update-in [:lineage :open-assumptions] conj :new-opaque-policy))
     (update coverage :open-assumptions conj :new-opaque-policy)]

    :original-premise
    (let [premises #{:guard :write :membership}
          updated (assoc original :request/claim premises)]
      [(assoc declared :original-open-obligation-ids-by-operation updated)
       (-> composed
           (assoc :original-open-obligation-ids-by-operation updated)
           (assoc-in [:operation-contracts :request/claim :original-open-obligation-ids]
                     premises))
       (-> manifest
           (assoc-in [:lineage :original-open-obligation-ids-by-operation] updated)
           (assoc-in [:lineage :operations :request/claim :original-open-obligation-ids]
                     premises))
       (assoc coverage :original-open-obligation-ids-by-operation updated)])

    :narrow-catalog
    (let [one #{:request/claim}
          source "sha256:narrower-source"
          operations (select-keys details one)
          premises (select-keys original one)]
      [(-> declared
           (assoc :operation-ids one :catalog-fingerprint source
                  :original-open-obligation-ids-by-operation premises))
       (-> composed
           (assoc :operation-ids one :catalog-fingerprint source
                  :operation-contracts operations
                  :original-open-obligation-ids-by-operation premises)
           (assoc-in [:invariant-results :request/ownership :preservation-by-operation]
                     {:request/claim :checked-conditional-inference}))
       (-> manifest
           (assoc-in [:lineage :catalog-fingerprint] source)
           (assoc-in [:lineage :operations]
                     (select-keys (get-in manifest [:lineage :operations]) one))
           (assoc-in [:lineage :original-open-obligation-ids-by-operation]
                     premises))
       (-> coverage
           (assoc :catalog-fingerprint source
                  :declared-operation-ids one
                  :independent-operation-ids one
                  :issued-operation-ids one
                  :observed-entry-point-ids one
                  :operation-rows [{:operation-id :request/claim}]
                  :original-open-obligation-ids-by-operation premises))])))

(defn- recheck-from!
  "Simulate source acquisition returning freshly built, internally coherent
   snapshots; the checker itself still performs its exact report comparison."
  [prior snapshots]
  (let [[d c p w] snapshots
        acquisition (atom [])
        result (with-redefs [app/describe-application-dependencies
                             (fn [_] (swap! acquisition conj :dependencies) d)
                             comp/compose-operation-interfaces!
                             (fn [& _] (swap! acquisition conj :composition) c)
                             prov/describe-composition-provenance!
                             (fn [& _] (swap! acquisition conj :provenance) p)
                             writer/describe-application-writer-coverage!
                             (fn [& _] (swap! acquisition conj :writers) w)]
                 (error #(sut/require-current-application-assurance!
                          prior ::catalog ::graph ::closure ::writers)))]
    [result @acquisition]))

(deftest unchanged-source-remains-recheckable-but-not-authoritative
  (let [report (summarize)
        [failure calls] (recheck-from! (read-string (pr-str report))
                                        [declared composed manifest coverage])]
    (is (nil? failure))
    (is (= [:dependencies :composition :provenance :writers] calls))
    (is (= :conditional-declared-application-assurance (:status report)))
    (is (= :declared-abstract-machine-and-supplied-host-observation
           (:scope report)))
    (is (= :not-performed (:assumption-discharge report)))
    (is (= :not-issued (:certificate report)))
    (is (= :none (:transaction-authority report)))
    (is (= :none (:authority report)))))

(deftest source-relative-updates-invalidate-copied-reports
  (let [old (summarize)]
    (doseq [change [:catalog-fingerprint :operation-transition
                    :invariant-result :writer-snapshot
                    :assumptions :original-premise :narrow-catalog]]
      (testing (name change)
        (let [snapshots (changed-source change)
              current (summarize snapshots)
              [failure calls] (recheck-from! old snapshots)]
          (is (not= old current))
          (is (not= (:fingerprint old) (:fingerprint current)))
          (is (= :stale-or-substituted-application-assurance-report
                 (:reason failure)))
          (is (= (:fingerprint current) (:expected failure)))
          (is (= (:fingerprint old) (:actual failure)))
          (is (= [:dependencies :composition :provenance :writers] calls))
          (is (= :none (:authority current)))
          (is (= :not-issued (:certificate current))))))))

(deftest incomplete-but-coherent-catalog-does-not-inherit-old-assurance
  (let [fresh (summarize (changed-source :narrow-catalog))
        old (summarize)]
    (is (= #{:request/claim} (:operation-ids fresh)))
    (is (= #{:request/claim} (set (keys (:operation-contracts fresh)))))
    (is (= #{:request/claim}
           (set (keys (get-in fresh [:invariant-results :request/ownership
                                    :preservation-by-operation])))))
    (is (not= (:fingerprint fresh) (:fingerprint old)))
    (is (= :not-established (:physical-writer-coverage fresh)))
    (is (= :not-issued (:certificate fresh)))))

(deftest original-premises-and-host-obligations-never-disappear
  (doseq [kind [:original-premise :assumptions :writer-snapshot]]
    (let [r (summarize (changed-source kind))]
      (is (= :not-performed (:assumption-discharge r)))
      (is (= :not-established (:abstract-to-physical-correspondence r)))
      (is (= :not-issued (:certificate r)))
      (case kind
        :original-premise
        (do (is (contains? (:open-obligations r) :membership))
            (is (= #{:guard :write :membership}
                   (get-in r [:original-open-obligation-ids-by-operation
                              :request/claim]))))
        :assumptions
        (is (contains? (:open-assumptions r) :new-opaque-policy))
        :writer-snapshot
        (do (is (contains? (:open-obligations r) :unclassified-host-writer))
            (is (contains? (:open-obligations r) :observed-writer-coverage-open))
            (is (= #{:request/unclassified}
                   (get-in r [:observed-writer-accounting
                              :observed-unclassified-entry-point-ids]))))))))

(deftest concurrent-invariant-and-writer-gaps-remain-visible
  (let [[d c p _] (changed-source :invariant-result)
        [_ _ _ w] (changed-source :writer-snapshot)
        report (summarize [d c p w])]
    (is (= :open-observed-writer-coverage (:status report))
        "The one primary status does not suppress the other open obligation")
    (is (= #{:request/ownership} (:open-invariant-ids report)))
    (is (contains? (:open-obligations report)
                   :declared-abstract-invariant-induction-open))
    (is (contains? (:open-obligations report) :observed-writer-coverage-open))
    (is (contains? (:open-obligations report) :unclassified-host-writer))
    (is (= :not-issued (:certificate report)))
    (is (= :none (:authority report)))))

(deftest cross-source-mutations-reject-rather-than-reconcile-away
  (let [cases [
               [(assoc declared :catalog-fingerprint "forged") composed manifest coverage]
               [declared (update composed :operation-contracts dissoc :request/release)
                manifest coverage]
               [declared composed
                (update-in manifest [:lineage :operations] dissoc :request/claim)
                coverage]
               [declared composed manifest
                (assoc coverage :declared-operation-ids #{:request/claim})]
               [declared composed manifest
                (assoc coverage :exact-observed-operation-id-join? false)]
               [declared composed
                (update-in manifest [:lineage :original-open-obligation-ids-by-operation]
                           dissoc :request/claim) coverage]
               [declared (assoc-in composed [:operation-contracts :request/claim
                                             :original-open-obligation-ids] #{})
                manifest coverage]
               [declared composed
                (assoc-in manifest [:lineage :invariants :request/ownership
                                    :induction-status] :open) coverage]
               [declared composed manifest (assoc coverage :certificate :issued)]
               [declared composed manifest (assoc coverage :physical-writer-coverage
                                                    :established)]]]
    (doseq [snapshots cases]
      (is (rejected? #(summarize snapshots)) (pr-str snapshots)))))

(deftest forged-prior-reports-never-become-credentials
  (let [old (summarize)
        snapshots [declared composed manifest coverage]]
    (doseq [edited [(assoc old :certificate :issued)
                    (assoc old :authority :full)
                    (assoc old :transaction-authority :full)
                    (assoc old :open-obligations #{})
                    (assoc old :open-assumptions #{})
                    (with-meta old {:signed :by-caller})]]
      (let [[failure calls] (recheck-from! edited snapshots)]
        (is (= :stale-or-substituted-application-assurance-report
               (:reason failure)))
        (is (= [:dependencies :composition :provenance :writers] calls))))))

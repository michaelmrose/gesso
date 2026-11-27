(ns gesso.assurance.reference-recovery-inventory-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.assumption :as assumption]
            [gesso.assurance.reference-mapped-observed-recovery :as observed-recovery]
            [gesso.assurance.reference-recovery-inventory :as inventory]))

(deftest obligation-inventory-refuses-inconsistent-premise-graphs
  (let [inventory inventory/describe-inventory
        describe (fn [id deps]
                   (assumption/describe-assumption
                    {:id id :subject {:kind :runtime :id :fixture/history :interface {:fixture true}}
                     :scope {:kind :runtime :id :fixture/history} :proposition [:fixture/required]
                     :parameters {} :semantic-context {} :dependencies deps
                     :origin :implementation-correspondence :implementation-boundary :fixture/boundary}))
        storage-obligation (describe :fixture/storage #{})
        storage {:obligations [storage-obligation] :trusted-assumptions #{(:premise-fingerprint storage-obligation)}
                 :premise-acceptance :explicit-declaration-only :deployment-acceptance {:authentication :not-established}}
        family (fn [ds] {:obligations ds :open-premise-fingerprints (set (map :premise-fingerprint ds))
                         :premise-acceptance :not-performed})
        a (describe :fixture/a #{})
        b (describe :fixture/b #{:fixture/a})
        abstract {:declared-operation-ids #{:fixture/op} :fingerprint "fixture"
                  :classification {:original-obligations {:status :all-original-obligations-remain-open
                                                          :by-operation {:fixture/op #{:fixture/opaque}}}
                                   :certificate {:open-premises #{:fixture/induction}}
                                   :open {:assumptions #{:fixture/meta} :assumption-classes {:meta #{:fixture/meta}}
                                          :certificate-premises #{:fixture/induction}}}}
        empty-family (family [])
        build #(inventory abstract storage % empty-family empty-family)
        reason (fn [f] (try (f) :unexpected-success (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
        r (build (family [b a]))]
    (is (= 3 (count (:premises-by-fingerprint r))))
    (is (= #{(:premise-fingerprint a) (:premise-fingerprint b)} (:unaccepted-premise-fingerprints r)))
    (is (= #{(:premise-fingerprint a)} (get-in r [:premises-by-fingerprint (:premise-fingerprint b) :dependency-fingerprints])))
    (is (= :duplicate-premise-identity (reason #(build (family [a a])))))
    (is (= :duplicate-premise-identity (reason #(build (family [storage-obligation])))))
    (is (= :missing-or-cross-family-premise-dependency (reason #(build (family [b])))))
    (is (= :missing-or-cross-family-premise-dependency
           (reason #(build (family [(describe :fixture/a #{:fixture/storage})])))))
    (is (= :cyclic-or-missing-premise-dependency
           (reason #(build (family [(describe :fixture/a #{:fixture/b}) b])))))
    (is (= :invalid-premise-family (reason #(build (assoc (family [a]) :premise-acceptance :explicit-declaration-only)))))
    (is (= :invalid-premise-family (reason #(build (assoc (family [a]) :open-premise-fingerprints #{})))))
    (is (= :invalid-premise-family (reason #(build (family [(assoc a :discharge-state :discharged)])))))
    (is (= :invalid-abstract-obligation-inventory
           (reason #(inventory (assoc-in abstract [:classification :open :assumptions] #{})
                               storage (family [a]) empty-family empty-family))))))

(deftest physical-refinement-premises-bind-selected-history
  (let [describe #'observed-recovery/physical-refinement-premises
        selected [{:tx-id 3 :intent-id (random-uuid) :maintenance :lease-acquisition}]
        entry {:scope {:publication-source-id "claim" :historical-cut {:payload-fingerprint "fixture"}}
               :command-observation-ref "original-cut"
               :judgment-subject {:catalog-id :fixture/claim :selected-operations [:accept :accept :refuse]}
               :original-recovery {:manifest-tx-id 0 :initialization-tx-id 1
                                   :selected-tx-ids [2 4 5] :selected-commitments [:committed :aborted :committed]
                                   :intervening-maintenance selected
                                   :provider-refinement :not-established
                                   :aborted-receipt-identity :not-established}}
        r (describe entry)
        fingerprints (:open-premise-fingerprints r)
        obligations (:obligations r)
        ids (mapv #(get-in % [:assumption :id]) obligations)]
    (is (= 11 (count fingerprints)))
    (is (= r (describe (assoc-in entry [:scope :retained-acquisition-ref] "other-target"))))
    (is (= (set (drop-last 3 ids))
           (get-in (nth obligations (- (count obligations) 3)) [:assumption :dependencies])))
    (is (= #{:reference-physical/submission-attribution :reference-physical/abort-atomicity}
           (get-in (first (filter #(= :reference-physical/aborted-attempt-attribution
                                     (get-in % [:assumption :id])) obligations)) [:assumption :dependencies])))
    (is (= :not-established (:provider-refinement r) (:complete-writer-coverage r) (:universal-refinement r)))
    (is (every? #(= :open (:discharge-state %)) obligations))
    (doseq [changed [(assoc entry :command-observation-ref "other-cut")
                     (assoc-in entry [:scope :publication-source-id] "other-source")
                     (assoc-in entry [:scope :historical-cut :payload-fingerprint] "other-content")
                     (assoc-in entry [:judgment-subject :catalog-id] :fixture/other)
                     (assoc-in entry [:original-recovery :manifest-tx-id] 1)
                     (assoc-in entry [:original-recovery :selected-tx-ids] [2 4 6])
                     (assoc-in entry [:original-recovery :selected-commitments] [:committed :committed :committed])
                     (assoc-in entry [:original-recovery :intervening-maintenance 0 :intent-id] (random-uuid))
                     (assoc-in entry [:original-recovery :worker-observation-provenance] :matched-local-issued-host-capture)]]
      (let [other (describe changed)]
        (is (not-any? fingerprints (:open-premise-fingerprints other)))
        (is (= :not-performed (:premise-acceptance other)))
        (is (= :not-established (:universal-refinement other)))))
    (let [adjacent (describe (update entry :original-recovery dissoc :intervening-maintenance))
          no-abort (describe (assoc-in entry [:original-recovery :selected-commitments] [:committed :committed :committed]))]
      (is (= 10 (count (:obligations adjacent))))
      (is (= 9 (count (:obligations no-abort))))
      (is (empty? (get-in no-abort [:scope :selected-aborted-tx-ids])))
      (is (= :not-established (:complete-writer-coverage adjacent) (:complete-writer-coverage no-abort))))))

(deftest finite-transfer-binds-only-original-open-premises
  (let [subject {:catalog-fingerprint "catalog" :abstract-induction-fingerprint "abstract"}
        cut {:payload-fingerprint "history"}
        physical (#'observed-recovery/physical-refinement-premises
                  {:scope {:publication-source-id "claim" :historical-cut cut}
                   :command-observation-ref "original-cut" :judgment-subject subject
                   :original-recovery {:manifest-tx-id 0 :initialization-tx-id 1
                                       :selected-tx-ids [2 3 4]
                                       :selected-commitments [:committed :aborted :committed]}})
        abstract {:fingerprint "abstract" :declared-invariant-ids #{:fixture/invariant}
                  :declared-operation-ids #{:fixture/op}
                  :classification {:original-obligations {:status :all-original-obligations-remain-open
                                                          :by-operation {:fixture/op #{:fixture/opaque}}}
                                   :certificate {:open-premises #{:fixture/induction}}
                                   :open {:assumptions #{:fixture/meta} :assumption-classes {:meta #{:fixture/meta}}
                                          :certificate-premises #{:fixture/induction}}}}
        empty-family {:obligations [] :open-premise-fingerprints #{} :premise-acceptance :not-performed}
        storage {:obligations [] :trusted-assumptions #{} :premise-acceptance :explicit-declaration-only}
        composed (inventory/describe-inventory abstract storage empty-family empty-family physical)
        coordinates [{:tx-id 1 :side :initial}]
        checked {:status :checked :recovery-cut-ref "original-cut" :historical-cut cut
                 :judgment-subject subject :catalog-fingerprint "catalog" :retained-payload-fingerprint "history"
                 :selected-tx-ids [2 3 4] :invariant-ids #{:fixture/invariant}
                 :physical-readbacks {:status :matched :checked-coordinates coordinates}
                 :conditional-physical-invariant
                 {:status :conditional :conclusion :declared-invariants-hold-on-selected-physical-projections
                  :conditional-on #{:faithful-pinned-provider-readbacks :faithful-authoritative-state-projection
                                    :faithful-invariant-expression-semantics}
                  :checked-coordinates coordinates :invariant-ids #{:fixture/invariant}}}
        bind #(inventory/describe-transfer-binding %1 %2 %3 abstract)
        good (bind checked physical composed)
        snapshot (get-in good [:condition-premise-fingerprints :faithful-pinned-provider-readbacks])]
    (is (= :bound (:status good)) (pr-str good))
    (is (= 5 (count (:required-open-premise-fingerprints good))))
    (is (= :not-established (:sufficiency good)))
    (is (= :not-performed (:proof-discharge good)))
    (doseq [[c p i] [[(assoc checked :recovery-cut-ref "other-cut") physical composed]
                      [(assoc checked :historical-cut {:payload-fingerprint "other-history"}) physical composed]
                      [(assoc checked :catalog-fingerprint "other-catalog") physical composed]
                      [(assoc checked :retained-payload-fingerprint "other-payload") physical composed]
                      [(assoc checked :selected-tx-ids [2 3 5]) physical composed]
                      [(assoc-in checked [:conditional-physical-invariant :status] :not-established) physical composed]
                      [(assoc-in checked [:physical-readbacks :status] :mismatch) physical composed]
                      [(assoc-in checked [:conditional-physical-invariant :conditional-on] #{}) physical composed]
                      [(assoc-in checked [:conditional-physical-invariant :checked-coordinates] []) physical composed]
                      [(assoc-in checked [:conditional-physical-invariant :invariant-ids] #{:fixture/other}) physical composed]
                      [checked (update physical :obligations pop) composed]
                      [checked physical (update composed :premises-by-fingerprint dissoc snapshot)]
                      [checked physical (assoc-in composed [:premises-by-fingerprint snapshot :discharge-state] :discharged)]
                      [checked physical (assoc-in composed [:premises-by-fingerprint snapshot :acceptance] :explicit-declaration-only)]
                      [checked physical (assoc-in composed [:premises-by-fingerprint snapshot :dependency-fingerprints] #{})]
                      [checked physical (update composed :dependency-order #(filterv (partial not= snapshot) %))]
                      [checked physical (assoc-in composed [:abstract :report-fingerprint] "foreign")]]]
      (let [failure (bind c p i)]
        (is (= :not-established (:status failure)) (pr-str failure))
        (is (not (contains? failure :required-open-premise-fingerprints)))
        (is (not (contains? failure :condition-premise-fingerprints)))))))

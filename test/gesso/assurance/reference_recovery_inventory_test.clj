(ns gesso.assurance.reference-recovery-inventory-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.assumption :as assumption]
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

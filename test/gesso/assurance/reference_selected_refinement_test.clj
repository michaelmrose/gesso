(ns gesso.assurance.reference-selected-refinement-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-selected-refinement :as selected]
            [gesso.assurance.reference-writer-classification :as classification]
            [gesso.assurance.reference-source-transition-argument :as commit]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.proof.operation-set :as catalog]
            [gesso.live.outbox.claim-semantic-fixture :as claim])
  (:import (java.time Instant)))

(defn- fixture []
  (let [original (claim/catalog)
        source {:inductive-source {:catalog original} :target-map claim/target-map}
        declarations (claim/outcome-declarations)
        fp (:fingerprint (catalog/check-operation-set! original))
        cases (:cases (outcome/derive-cases! source declarations))
        classified (assoc (classification/derive! source declarations fp cases)
                          :status :conditional-current-installed-writer-classification)
        id (claim/alternative-id "helper-1" :accept)
        identity (get-in classified [:alternative-identities id])
        cut {:transaction-ids [1 3 7 9 11] :payload-fingerprint "example"}
        partition [{:tx-id 7 :class :committed-mapped :alternative identity}
                   {:tx-id 9 :class :committed-worker :policy-branch :lease-acquisition}
                   {:tx-id 11 :class :aborted-attempt :alternative identity}]
        at #(.plusSeconds Instant/EPOCH %)
        spans (mapv (fn [a b schema] {:from-tx-id a :to-tx-id b :from (at a) :to (at b)
                                      :schema schema :intervening-protected-boundaries :none})
                    [3 7 9] [7 9 11] [:source-bound-committed-transition :maintenance-stutter :aborted-stutter])
        intervals {:profile :conditional-selected-writer-temporal-coverage-v1
                   :status :conditional-selected-diagonal-coverage :proof-discharge :not-performed
                   :protected-tables (:protected-tables classified) :intervals spans
                   :remaining-conditions #{:example-clock-condition}}
        points (into [{:tx-id 3 :side :initial :system-time (at 3) :valid-time (at 3)}]
                     (mapcat (fn [{:keys [to-tx-id from to]}]
                               [{:tx-id to-tx-id :side :prestate :system-time from :valid-time to}
                                {:tx-id to-tx-id :side :poststate :system-time to :valid-time to}])) spans)
        observed {:profile :selected-retained-mapped-transition-refinement-v1
                  :status :conditional-selected-transition-correspondence
                  :row-correspondence :exact-complete-mapped-tables
                  :envelope-binding :exact-retained-expectation-with-qualified-issued-envelope-premise
                  :field-coordinates claim/target-map :checked-coordinates points :proof-discharge :not-performed
                  :conditions #{:example-provider-condition}
                  :steps [{:tx-id 7 :alternative-id id :relation :committed-abstract-transition :checked-row-effect :exact-source-derived-puts}
                          {:tx-id 9 :relation :maintenance-stutter :checked-row-effect :exact-mapped-row-stutter}
                          {:tx-id 11 :alternative-id id :relation :aborted-stutter :checked-row-effect :exact-row-stutter}]}
        checked {:status :checked :catalog-fingerprint fp :historical-cut cut :retained-payload-fingerprint "example"
                 :checked-boundaries (mapv #(hash-map :tx-id %) [3 7 9 11])
                 :physical-readbacks {:status :matched :checked-coordinates points :transition-correspondence observed}}]
    [source declarations fp cases classified cut partition intervals checked]))

(defn- run [args]
  (try (apply selected/compose! args)
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest compose-selected-physical-schemas
  (let [args (fixture) report (run args)]
    (is (= :conditional-selected-physical-refinement (:status report)) (pr-str report))
    (is (= [:committed-abstract-transition :maintenance-stutter :aborted-stutter] (mapv :relation (:steps report))))
    (is (= 4 (count (:declared-replay-schemas report))))
    (is (= :not-established (:replay-observation-coverage report) (:universal-refinement report) (:writer-coverage report)))
    (is (= :not-performed (:proof-discharge report)))
    (is (every? (:remaining-conditions report) #{:example-clock-condition :example-provider-condition
                                               :complete-admitted-writer-and-temporal-boundary-coverage}))
    (doseq [bad [(assoc-in args [8 :retained-payload-fingerprint] "foreign")
                 (assoc-in args [8 :historical-cut :payload-fingerprint] "foreign")
                 (assoc-in args [8 :physical-readbacks :status] :unsupported)
                 (update-in args [8 :physical-readbacks :checked-coordinates] pop)
                 (update-in args [7 :intervals] pop)
                 (assoc-in args [7 :intervals 1 :from-tx-id] 3)
                 (assoc-in args [7 :intervals 1 :from] Instant/EPOCH)
                 (assoc-in args [4 :alternative-identities] {})]]
      (is (= :refinement-composition-evidence-mismatch (run bad))))
    (doseq [bad [(assoc-in args [6 0 :alternative :mapping-fingerprint] "foreign")
                 (assoc-in args [6 1 :policy-branch] :initializer)
                 (assoc-in args [7 :intervals 1 :schema] :aborted-stutter)
                 (assoc-in args [7 :intervals 0 :intervening-protected-boundaries] :present)
                 (assoc-in args [8 :physical-readbacks :transition-correspondence :steps 0 :checked-row-effect] :exact-row-stutter)]]
      (is (= :refinement-composition-step-mismatch (run bad))))
    (with-redefs [commit/derive! (constantly {})]
      (is (= :refinement-composition-boundary-replaced (run args))))))

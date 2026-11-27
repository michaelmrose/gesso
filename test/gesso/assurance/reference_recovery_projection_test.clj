(ns gesso.assurance.reference-recovery-projection-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-recovery-projection :as projection]
            [gesso.assurance.reference-mapped-poststate :as poststate]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.proof.operation-set :as catalog]
            [gesso.model.certified-plan :as plan]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(defn- record [id principal operation arguments outcome]
  (resolution/command-record {:command-id (identity/command-id id)
                              :principal (identity/principal principal)
                              :operation operation :arguments arguments} outcome "fixture-time"))

(deftest genuine-claim-coordinate-decoding
  (let [source {:inductive-source {:catalog (claim/catalog)} :target-map claim/target-map}
        cases (:cases (outcome/derive-cases! source (claim/outcome-declarations)))
        rows {:requests {"r1" {:xt/id "r1" :status :claimed :revision 1}}
              :request_assignments {"primary-r1" {:xt/id "primary-r1" :helper "helper-1"}}
              :memberships {"helper-1" {:xt/id "helper-1" :active true}
                            "helper-2" {:xt/id "helper-2" :active false}}}
        accepted (record "a" "helper-1" :request/claim {:request-id "r1"}
                         {:outcome :request/claimed :helper "helper-1"})
        refused (record "b" "helper-2" :request/claim {:request-id "r1"}
                        {:outcome :request/not-claimable :reason :already-claimed})
        run #(projection/project! claim/target-map % cases)]
    (doseq [visible [rows (assoc rows :gesso_commands {"a" accepted})
                     (assoc rows :gesso_commands {"a" accepted "b" refused})
                     (dissoc rows :request_assignments)]]
      (is (= (poststate/project-visible! claim/target-map visible {:outcome-cases cases})
             (run visible))))
    (is (false? (:membership/two (run rows))))
    (is (nil? (:assignment/helper (run (dissoc rows :request_assignments)))))
    (is (nil? (:journal/one (run rows))))
    (doseq [bad [(assoc rows :gesso_commands {"wrong" accepted})
                 (assoc rows :gesso_commands {"a" (assoc accepted :gesso.command/status :rejected)})
                 (assoc rows :gesso_commands {"a" accepted "c" (record "c" "helper-1" :request/claim
                                                               {:request-id "r1"} {:outcome :request/claimed :helper "helper-1"})})
                 (assoc rows :gesso_commands {"a" (assoc-in accepted [:gesso.command/binding :arguments] {:request-id "other"})})
                 (assoc rows :gesso_commands {"a" (assoc accepted :gesso.command/outcome {:outcome :unknown})})
                 (assoc-in rows [:request_assignments "primary-r1"] nil)
                 (update-in rows [:memberships "helper-2"] dissoc :active)
                 (assoc-in rows [:requests "r1" :xt/id] "foreign")
                 (dissoc rows :requests)]]
      (is (thrown? clojure.lang.ExceptionInfo (run bad))))))

(deftest coordinate-shape-and-boundary-rejection
  (let [coords {:fixture/value [:document :fixture_rows "row" :some_value]}
        rows {:fixture_rows {"row" {:xt/id "row" :some-value nil}}}]
    (is (= {:fixture/value nil} (projection/project! coords rows {})))
    (doseq [coord [[:other :fixture_rows "row" :some_value]
                   [:document :fixture_rows "row" :some-value]
                   [:document :gesso_commands "row" :some_value]
                   [:journal-slot "helper" :unqualified]]]
      (is (thrown? clojure.lang.ExceptionInfo (projection/project! {:fixture/value coord} rows {}))))
    (with-redefs [resolution/require-record identity]
      (is (not (projection/boundaries-current?)))
      (is (thrown? clojure.lang.ExceptionInfo (projection/project! coords rows {}))))))

(deftest original-catalog-mapping-and-outcome-qualification
  (let [original (claim/catalog)
        source {:inductive-source {:catalog original} :target-map claim/target-map}
        declarations (claim/outcome-declarations)
        fp (:fingerprint (catalog/check-operation-set! original))
        cases (:cases (outcome/derive-cases! source declarations))
        run #(projection/check-catalog! %1 %2 fp %3)
        report (run source declarations cases)
        alternatives (:operation-mappings report)]
    (is (= :matched (:status report)) (pr-str report))
    (is (= claim/target-map (:field-coordinates report)))
    (is (= cases (:closed-outcome-cases report)))
    (is (= 8 (count alternatives)))
    (is (= 4 (count (filter :journal-write (vals alternatives)))))
    (is (= (set (keys declarations)) (set (vals (:case-alternatives report)))))
    (doseq [{:keys [id construction]} (:operations original)]
      (let [a (alternatives id)]
        (is (= (:writes construction) (set (keys (:write-coordinates a)))))
        (is (= (:frame construction) (set (keys (:frame-coordinates a)))))))
    (doseq [bad [(update source :target-map dissoc :request/status)
                 (assoc-in source [:target-map :request/revision] (:request/status claim/target-map))
                 (assoc-in source [:target-map :membership/two] [:nullable-document :memberships "helper-2" :active])
                 (assoc-in source [:target-map :request/status] [:journal-slot "helper-1" :request/claim])]]
      (is (= :not-established (:status (run bad declarations cases)))))
    (is (= :not-established (:status (run source (dissoc declarations (first (keys declarations))) cases))))
    (is (= :not-established (:status (run source declarations (assoc cases (first (keys cases)) :foreign)))))
    (is (= :projection-catalog-mismatch (:reason (projection/check-catalog! source declarations "foreign" cases))))
    (doseq [v [#'catalog/check-operation-set! #'plan/check-target-mapping! #'outcome/derive-cases!]]
      (with-redefs-fn {v (constantly {})}
        #(is (= :projection-boundary-replaced (:reason (run source declarations cases))))))))

(deftest bind-original-projection-and-open-snapshot-dependency
  (let [original (claim/catalog)
        source {:inductive-source {:catalog original} :target-map claim/target-map}
        declarations (claim/outcome-declarations)
        fp (:fingerprint (catalog/check-operation-set! original))
        cases (:cases (outcome/derive-cases! source declarations))
        report (projection/check-catalog! source declarations fp cases)
        ids (:invariant-ids report)
        id (claim/alternative-id "helper-1" :accept)
        subject {:catalog-id (:id original) :catalog-fingerprint fp :abstract-induction-fingerprint "induction"
                 :selected-operations [{:alternative-id id :mapping-fingerprint (get-in report [:operation-mappings id :mapping-fingerprint])}]}
        points [{:tx-id 1 :side :initial}]
        physical {:status :matched :checked-coordinates points
                  :projection-correspondence {:profile projection/profile :status :matched :proof-discharge :not-performed
                                              :universal-projection :not-established :checked-coordinates points
                                              :field-coordinates claim/target-map :closed-outcome-cases cases}}
        checked {:status :checked :catalog-fingerprint fp :judgment-subject subject :recovery-cut-ref "cut"
                 :invariant-ids ids :physical-readbacks physical :catalog-projection-correspondence report
                 :conditional-physical-invariant {:checked-coordinates points}}
        transfer {:status :bound :recovery-cut-ref "cut" :abstract-report-fingerprint "induction" :invariant-ids ids
                  :proof-discharge :not-performed :required-open-premise-fingerprints #{"projection" "snapshot"}
                  :condition-premise-fingerprints {:faithful-authoritative-state-projection "projection"
                                                   :faithful-pinned-provider-readbacks "snapshot"}}
        open {:family :physical :acceptance :not-performed :discharge-state :open}
        inventory {:abstract {:report-fingerprint "induction"} :unaccepted-premise-fingerprints #{"projection" "snapshot"}
                   :premises-by-fingerprint {"projection" (assoc open :id :reference-physical/authoritative-projection :dependency-fingerprints #{"snapshot"})
                                             "snapshot" (assoc open :id :reference-physical/snapshot-continuity)}}
        abstract {:catalog-id (:id original) :identity {:catalog-fingerprint fp} :fingerprint "induction" :declared-invariant-ids ids}
        bind #(projection/bind-premise %1 %2 %3 abstract)
        result (bind checked transfer inventory)]
    (is (= :conditional-premise-support (:status result)) (pr-str result))
    (is (= "projection" (:premise-fingerprint result)))
    (is (= "snapshot" (:snapshot-premise-fingerprint result)))
    (is (= :open (:premise-state result)))
    (is (= :not-performed (:proof-discharge result) (:premise-acceptance result)))
    (doseq [[c t i] [[(assoc checked :recovery-cut-ref "foreign") transfer inventory]
                      [(assoc-in checked [:judgment-subject :selected-operations 0 :mapping-fingerprint] "foreign") transfer inventory]
                      [(assoc-in checked [:catalog-projection-correspondence :catalog-fingerprint] "foreign") transfer inventory]
                      [(assoc-in checked [:physical-readbacks :projection-correspondence :field-coordinates] {}) transfer inventory]
                      [(assoc-in checked [:physical-readbacks :projection-correspondence :closed-outcome-cases] {}) transfer inventory]
                      [(assoc-in checked [:physical-readbacks :projection-correspondence :checked-coordinates] []) transfer inventory]
                      [checked (update transfer :required-open-premise-fingerprints disj "snapshot") inventory]
                      [checked transfer (assoc-in inventory [:premises-by-fingerprint "projection" :dependency-fingerprints] #{})]
                      [checked transfer (assoc-in inventory [:premises-by-fingerprint "projection" :discharge-state] :discharged)]
                      [checked transfer (assoc-in inventory [:premises-by-fingerprint "snapshot" :acceptance] :explicit-declaration-only)]]]
      (let [bad (bind c t i)]
        (is (= :not-established (:status bad)))
        (is (not (contains? bad :premise-fingerprint)))))))

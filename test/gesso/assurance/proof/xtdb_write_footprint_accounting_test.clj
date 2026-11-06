(ns gesso.assurance.proof.xtdb-write-footprint-accounting-test
  "v1518: adversarial regressions for v1517's source-relative XTDB footprint.

   A supplied, readmitted transaction vector is not authenticated physical
   execution. These tests deliberately attack the whole-vector accounting,
   whole-document replacement semantics, finite protection, source binding,
   seam integrity, and the explicit *absence* of a certification claim.
   Tests never need to start an XTDB node or submit a mutation."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.xtdb-write-footprint-accounting :as footprint]
            [gesso.assurance.proof.protected-write-domain :as domain]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]))

(def ^:private source-keys
  #{:plan :protected-documents :candidate-tx-ops})

(defn- document [id]
  (physical/initial-document id))

(defn- updated [before]
  (-> before
      (assoc :status "reserved"
             :claims 1
             :revision (inc (:revision before))
             :updated-at (str "v1518-revision-" (inc (:revision before))))))

(defn- update-command [before]
  (command/update-command physical/table :slot/reserve before
                          (updated before) physical/version-keys))

(defn- update-plan
  ([ids] (update-plan ids false))
  ([ids count-guard?]
   (let [before (mapv document ids)]
     {:commands (mapv update-command before)
      :assertions (if count-guard?
                    (mapv (fn [doc]
                            (tx/assert-document-field-count
                             physical/table (:xt/id doc) (count doc)))
                          before)
                    [])
      :emit false})))

(defn- source
  ([id] (source [id] {}))
  ([ids {:keys [count-guard? frame-only] :or {count-guard? true
                                             frame-only []}}]
   (let [plan (update-plan ids count-guard?)]
     {:plan plan
      :protected-documents
      (vec (concat (map (fn [id]
                          {:target [physical/table id] :fields :all})
                        ids)
                   (map (fn [id]
                          {:target [physical/table id]
                           :fields #{:audit-tag}})
                        frame-only)))
      :candidate-tx-ops (:tx-ops (tx/prepare nil plan))})))

(defn- rejected [thunk]
  (try
    (thunk)
    :not-rejected
    (catch clojure.lang.ExceptionInfo e
      (if (:xtdb-write-footprint-accounting/rejection (ex-data e))
        (:reason (ex-data e))
        (throw e)))))

(defn- expect-rejection [reason thunk]
  (is (= reason (rejected thunk))))

(defn- inspection [src]
  (footprint/inspect-footprint! src))

(deftest admitted-single-write-accounted-for-entire-vector
  (let [src (source "write-1")
        r (inspection src)]
    (is (= footprint/report-type (:type r)))
    (is (= footprint/report-version (:version r)))
    (is (= :source-readmitted-whole-transaction-vector-and-finite-protection
           (:checking r)))
    (is (= 3 (:candidate-operation-count r)))
    (is (= 2 (:prewrite-assertion-count r)))
    (is (= 1 (:whole-document-replacement-count r)))
    (is (= 1 (:declared-command-count r)))
    (is (= 1 (:update-command-count r)))
    (is (= 0 (:create-command-count r)))
    (is (= [:canonical-sql-assertion :canonical-sql-assertion
            :whole-document-replacement]
           (:operation-classes r)))
    (is (true? (:all-candidate-operations-accounted-for? r)))
    (is (true? (:all-declared-write-targets-covered-in-finite-domain? r)))
    (is (true? (:all-declared-writes-whole-document-replacements? r)))
    (is (= r (footprint/require-current-footprint! src r)))
    (is (= r (footprint/redacted-summary src r)))))

(deftest assertion-count-and-field-delta-never-narrow-write-footprint
  (let [s1 (source ["delta-a"] {:count-guard? false})
        s2 (source "delta-a")
        r1 (inspection s1)
        r2 (inspection s2)]
    (is (= 1 (:whole-document-replacement-count r1)))
    (is (= 1 (:whole-document-replacement-count r2)))
    (is (= 2 (:candidate-operation-count r1)))
    (is (= 3 (:candidate-operation-count r2)))
    (is (= 1 (:prewrite-assertion-count r1)))
    (is (= 2 (:prewrite-assertion-count r2)))
    (is (< (:syntactic-changed-field-count r1)
           (count (document "delta-a"))))
    (is (= :not-established (:physical-frame-preservation r1)))
    (is (= :not-established (:physical-frame-preservation r2)))))

(deftest multi-document-and-frame-only-are-distinct
  (let [src (source ["write-A" "write-B"]
                    {:frame-only ["protected-frame"]})
        r (inspection src)]
    (is (= 2 (:whole-document-replacement-count r)))
    (is (= 2 (:declared-command-count r)))
    (is (= 2 (:update-command-count r)))
    (is (= 1 (:declared-frame-only-target-count r)))
    (is (= 4 (:prewrite-assertion-count r)))
    (is (= 6 (:candidate-operation-count r)))
    (is (= 2 (count (filter #{:whole-document-replacement}
                            (:operation-classes r)))))
    (is (= :not-established (:physical-frame-preservation r)))
    (is (not (re-find #"write-A|write-B|protected-frame" (pr-str r))))))

(deftest create-command-is-a-whole-document-replacement
  (let [created (document "newly-created")
        plan {:commands [(command/create physical/table created
                                         physical/version-keys)]
              :emit false}
        src {:plan plan
             :protected-documents
             [{:target [physical/table "newly-created"] :fields :all}]
             :candidate-tx-ops (:tx-ops (tx/prepare nil plan))}
        r (inspection src)]
    (is (= 1 (:create-command-count r)))
    (is (= 0 (:update-command-count r)))
    (is (= 1 (:whole-document-replacement-count r)))
    (is (= 1 (:prewrite-assertion-count r)))
    (is (= :not-established (:physical-write-footprint-completeness r)))))

(deftest candidate-write-extra-or-unsupported-forms-rejected
  (let [src (source "untrusted-extra")
        ops (:candidate-tx-ops src)
        extra [[:delete-docs physical/table "untrusted-extra"]
               [:erase-docs physical/table "untrusted-extra"]
               [:patch-docs physical/table {:xt/id "untrusted-extra"}]
               ["INSERT INTO other_table ..."]
               ["UPDATE other_table SET owner='x'"]
               [:put-docs :other_table {:xt/id "surprise"}]]]
    (doseq [op extra]
      (expect-rejection :changed-missing-extra-or-reordered-operation
                        #(inspection (assoc src :candidate-tx-ops
                                            (conj ops op)))))
    (expect-rejection :changed-missing-extra-or-reordered-operation
                      #(inspection (assoc src :candidate-tx-ops
                                          (into [(last ops)] ops))))))

(deftest candidate-missing-or-reordered-statements-rejected
  (let [src (source "missing-statement")
        ops (:candidate-tx-ops src)]
    (doseq [bad [(pop ops)
                 (subvec ops 1)
                 (subvec ops 0 1)
                 (vec (reverse ops))
                 [(last ops) (first ops) (second ops)]
                 (conj ops (last ops))]]
      (expect-rejection :changed-missing-extra-or-reordered-operation
                        #(inspection (assoc src :candidate-tx-ops bad))))))

(deftest assertion-sql-and-parameters-are-exact
  (let [src (source "assertion-tamper")
        ops (:candidate-tx-ops src)
        a (first ops)]
    (doseq [bad [(assoc a 0 "ASSERT true")
                 (assoc a 0 (str (first a) " /* same meaning? */"))
                 (assoc a 1 "different-id")
                 (conj a "extra-parameter")]]
      (expect-rejection :changed-missing-extra-or-reordered-operation
                        #(inspection (assoc src :candidate-tx-ops
                                            (assoc ops 0 bad)))))))

(deftest candidate-write-target-and-payload-are-exact
  (let [src (source "write-tamper")
        ops (:candidate-tx-ops src)
        index (dec (count ops))
        write (last ops)]
    (doseq [bad [(assoc write 1 :other_table)
                 (assoc-in write [2 :xt/id] "different-target")
                 (assoc-in write [2 :claims] 9)
                 (assoc-in write [2 :created-at] "changed-framed-value")
                 (update write 2 dissoc :audit-tag)
                 [:delete-docs physical/table "write-tamper"]]]
      (expect-rejection :changed-missing-extra-or-reordered-operation
                        #(inspection (assoc src :candidate-tx-ops
                                            (assoc ops index bad)))))))

(deftest even-syntactically-identical-writes-require-full-protection
  (let [src (source "partial-coverage")]
    (doseq [fields [#{:xt/id :status :claims :revision :created-at
                      :updated-at :audit-tag}
                    #{:status}]]
      (expect-rejection
       :incomplete-whole-document-protection
       #(inspection (assoc-in src [:protected-documents 0 :fields] fields))))
    (expect-rejection
     :incomplete-whole-document-protection
     #(inspection (assoc src :protected-documents
                         [{:target [physical/table "another"]
                           :fields :all}])))))

(deftest empty-field-protection-is-an-invalid-domain
  (let [src (source "empty-protection")]
    (expect-rejection :unsupported-declared-domain
                      #(inspection (assoc-in src [:protected-documents 0 :fields] #{})))))

(deftest duplicate-target-or-frame-overlap-is-not-legal
  (let [src (source "one-target")
        entry (first (:protected-documents src))]
    (expect-rejection :unsupported-declared-domain
                      #(inspection (update src :protected-documents conj entry)))
    (expect-rejection :unsupported-declared-domain
                      #(inspection (update src :protected-documents conj
                                           {:target [physical/table "one-target"]
                                            :fields #{:status}})))))

(deftest unsupported-source-plan-does-not-pass-by-matching-candidate
  (let [src (source "unsupported-plan")]
    (doseq [bad-assertions [[{:assert true}]
                            [{:sql "ASSERT true"}]]]
      (expect-rejection
       :unsupported-declared-domain
       #(inspection (assoc-in src [:plan :assertions] bad-assertions))))
    (expect-rejection
     :unsupported-declared-domain
     #(inspection (assoc-in src [:plan :emit] true)))))

(deftest altered-plan-without-relowering-candidate-is-not-current
  (let [src (source "altered-plan")
        original (get-in src [:plan :commands 0 :model/after])
        edited (assoc original :audit-tag "changed")]
    (expect-rejection
     :changed-missing-extra-or-reordered-operation
     #(inspection (assoc-in src [:plan :commands 0 :model/after] edited)))))

(deftest exact-top-level-schema-and-finite-bounds
  (let [src (source "exact-keys")
        ops (:candidate-tx-ops src)]
    (is (= source-keys (set (keys src))))
    (doseq [bad [(assoc src :certificate :issued)
                 (dissoc src :plan)
                 (dissoc src :candidate-tx-ops)
                 (assoc src :candidate-tx-ops [])
                 (assoc src :candidate-tx-ops (list (first ops)))
                 (assoc src :candidate-tx-ops (vec (repeat 257 (first ops))))
                 (assoc src :protected-documents [])]]
      (is (not= :not-rejected (rejected #(inspection bad)))))))

(deftest metadata-bearing-input-and-candidate-vectors-rejected
  (let [src (source "metadata")]
    (expect-rejection :invalid-source-shape
                      #(inspection (with-meta src {:elevated true})))
    (expect-rejection :invalid-source-shape
                      #(inspection (update src :candidate-tx-ops
                                           with-meta {:authenticated true})))
    (expect-rejection :invalid-transaction-operation
                      #(inspection (update-in src [:candidate-tx-ops 0]
                                              with-meta {:authenticated true})))))

(deftest source-recheck-binding-detects-equal-count-substitutions
  (let [s1 (source ["binder-A"] {:frame-only ["frame-A"]})
        s2 (source ["binder-A"] {:frame-only ["frame-B"]})
        r1 (inspection s1)
        r2 (inspection s2)]
    (is (= (:candidate-operation-count r1) (:candidate-operation-count r2)))
    (is (= (:declared-command-count r1) (:declared-command-count r2)))
    (is (= (:declared-frame-only-target-count r1)
           (:declared-frame-only-target-count r2)))
    (is (not= (:process-local-source-binding r1)
              (:process-local-source-binding r2)))
    (expect-rejection :stale-or-forged-footprint-report
                      #(footprint/require-current-footprint! s1 r2))
    (expect-rejection :stale-or-forged-footprint-report
                      #(footprint/redacted-summary s2 r1))))

(deftest forged-positive-claims-never-survive-recheck
  (let [src (source "forged-claim")
        report (inspection src)]
    (doseq [[k forged] [[:certificate :issued]
                        [:admission :admitted]
                        [:authority :physical]
                        [:actual-xtdb-execution :committed]
                        [:physical-frame-preservation :proved]
                        [:complete-physical-writer-coverage :proved]
                        [:universal-physical-refinement :proved]
                        [:candidate-submission-authenticated? true]
                        [:process-local-source-binding "invented"]]]
      (expect-rejection
       :stale-or-forged-footprint-report
       #(footprint/require-current-footprint!
         src (assoc report k forged))))))

(deftest declared-accounting-never-promotes-physical-certification
  (let [src (source "uncertified")
        r (inspection src)]
    (is (= :bounded-declared-transaction-vector-only (:proof-status r)))
    (is (= :not-observed (:actual-xtdb-execution r)))
    (is (false? (:source-binding-authenticates-execution? r)))
    (is (false? (:candidate-submission-authenticated? r)))
    (is (= :not-established (:complete-physical-writer-coverage r)))
    (is (= :not-established (:physical-write-footprint-completeness r)))
    (is (= :not-authenticated (:transaction-internal-prestate r)))
    (is (= :not-established (:physical-frame-preservation r)))
    (is (= :not-established (:universal-physical-refinement r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-admitted (:admission r)))
    (is (= :none (:authority r)))))

(deftest no-transaction-submission-from-analysis-or-recheck
  (let [src (source "no-submit")
        submissions (atom 0)]
    (with-redefs [tx/transact! (fn [& _]
                                 (swap! submissions inc)
                                 (throw (ex-info "Unexpected mutation" {})))]
      (let [r (inspection src)]
        (footprint/require-current-footprint! src r)
        (footprint/redacted-summary src r)
        (is (= 0 @submissions))))))

(deftest replaced-upstream-functions-rejected-instead-of-trusted
  (let [src (source "substituted-seam")]
    (with-redefs [tx/prepare (fn [& _] {:tx-ops (:candidate-tx-ops src)})]
      (expect-rejection :substituted-upstream-checker #(inspection src)))
    (with-redefs [command/target (fn [& _] [physical/table "fake"])]
      (expect-rejection :substituted-upstream-checker #(inspection src)))
    (with-redefs [domain/derive-protected-write-domain!
                  (fn [& _] {:declared-replacement-domain-covered? true})]
      (expect-rejection :substituted-upstream-checker #(inspection src)))))

(deftest errors-and-diagnostics-do-not-expose-sensitive-document-data
  (let [secret-id "SECRET_TARGET_ID_1518"
        src (source secret-id)
        report (inspection src)
        bad (assoc-in src [:candidate-tx-ops 0 0] "ASSERT true")
        error (try (inspection bad) nil
                   (catch clojure.lang.ExceptionInfo e e))]
    (is (instance? clojure.lang.ExceptionInfo error))
    (is (not (re-find #"SECRET_TARGET_ID_1518|v1494-created|preserved"
                      (str (.getMessage error) (pr-str (ex-data error))))))
    (is (not (re-find #"SECRET_TARGET_ID_1518|v1494-created|preserved"
                      (pr-str report))))
    (is (= report (footprint/redacted-summary src report)))))

(ns gesso.live.outbox.claim-semantic-fixture-test
  "Same-source bounded preservation and restricted conditional induction for
   the installed Claim witness. Not physical writer closure or certification."
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.outbox.claim-semantic-fixture :as claim]
            [gesso.assurance.proof.induction :as induction]
            [gesso.model.certified-plan :as plan]
            [gesso.model.mapped-guard :as guard]
            [gesso.model.tx :as model-tx]
            [gesso.model.inductive-plan :as mapped]
            [gesso.model.command-resolution :as resolution]
            [gesso.choreo.identity :as identity]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.assurance.observed-correspondence :as correspondence]
            [gesso.assurance.proof.guarded-substitution :as guarded]
            [gesso.assurance.proof.unified-preservation :as unified]
            [gesso.assurance.proof.finite-model-oracle :as oracle]))

(defn- eq [field value] [:= [:read field] [:literal value]])
(defn- conjunction [fields]
  (into [:and] (map (fn [[field value]] (eq field value))) fields))

(def ^:private domain
  {:request/status [:open :claimed] :request/revision [0 1]
   :assignment/helper [nil "helper-1" "helper-2"]
   :journal/one [nil :accepted :rejected] :journal/two [nil :accepted :rejected]
   :membership/one [false true] :membership/two [false true]})

(defn- finite-source [c]
  {:catalog c :state-domains domain
   :input-domains (zipmap (:declared-operation-ids c) (repeat {}))
   :max-cases 4000})

(defn- alter-source [c id f]
  (update c :operations #(mapv (fn [entry] (if (= id (:id entry)) (update entry :construction f) entry)) %)))

(deftest bounded-claim-ownership-and-journal-preservation
  (let [c (claim/catalog)
        report (oracle/check-finite-model! (finite-source c))]
    (is (= 8 (count (:declared-operation-ids c))))
    (is (= :bounded-no-counterexample (:status report)) (pr-str report))
    (is (:exhaustive? report))
    (is (= 432 (get-in report [:universe :states])))
    (is (= 3456 (get-in report [:examined :transition-cases-examined])))
    (is (= 4 (get-in report [:examined :initial-states-satisfying])))
    (is (false? (get-in report [:universe :all-type-domains-complete?])))
    (is (= :none (:authority report)))
    (is (= :not-established (:universal-induction report)))
    (is (= :not-established (:physical-correspondence report)))
    ;; Every original journal-writing alternative requires an empty own slot;
    ;; other alternatives frame it. This is a source shape check, not a theorem.
    (doseq [[helper [slot _]] claim/helpers alternative claim/alternatives]
      (let [source (claim/source helper alternative)]
        (if (#{:accept :refuse} alternative)
          (do (is (= #{slot} (set (filter #{:journal/one :journal/two} (:writes source)))))
              (is (= [:literal (if (= :accept alternative) :accepted :rejected)]
                     (get-in source [:replacements slot])))
              (is (= :and (first (get-in source [:guard :expression]))))
              (is (some #{(eq slot nil)} (rest (get-in source [:guard :expression])))))
          (is (every? (:frame source) [:journal/one :journal/two])))))))

(deftest relational-counterexamples-are-not-hidden-by-passing-domain-invariants
  (let [c (claim/catalog)
        accept (claim/alternative-id "helper-1" :accept)
        refuse (claim/alternative-id "helper-1" :refuse)
        mutations [(alter-source c accept #(assoc-in % [:replacements :assignment/helper] [:literal "helper-2"]))
                   (alter-source c accept #(assoc-in % [:replacements :journal/one] [:literal nil]))
                   (alter-source c refuse #(-> % (update :reads disj :journal/one)
                                               (update-in [:guard :readers] disj :journal/one)
                                               (update-in [:guard :expression] (fn [expr] (vec (remove #{(eq :journal/one nil)} expr))))))]]
    (doseq [mutant mutations]
      (let [r (oracle/check-finite-model! (finite-source mutant))]
        (is (= :counterexample (:status r)) (pr-str r))
        (is (= :step (get-in r [:counterexample :kind])))
        (is (= :claim/ownership-and-journal (get-in r [:counterexample :invariant-id])))))))

(deftest complete-conditional-induction-does-not-promote-to-certification
  (let [r (induction/check-induction! (claim/catalog))]
    (is (= #{:claim/nonnegative-revision :claim/ownership-and-journal} (:conditionally-checked-inductive-invariant-ids r)) (pr-str (:induction-outcomes r)))
    (is (empty? (:unsupported-inductive-invariant-ids r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-established (:physical-write-path-coverage r)))
    (is (= :none (:authority r)))))

(defn- guarded-report [c id]
  (guarded/check-guarded-preservation!
   {:construction (:construction (first (filter #(= id (:id %)) (:operations c))))
    :initial-condition (:initial-condition c) :invariants (:invariants c)}))

(deftest guarded-substitution-checks-every-claim-alternative
  (let [c (claim/catalog)]
    (doseq [id (:declared-operation-ids c)]
      (let [r (guarded-report c id)]
        (is (= #{[:preservation :claim/ownership-and-journal]}
               (:conditionally-checked-preservation-ids r)) (pr-str r))
        (is (= :not-issued (:certificate r)))
        (is (= :none (:authority r)))))
    (let [accept (claim/alternative-id "helper-1" :accept)
          refuse (claim/alternative-id "helper-1" :refuse)]
      (doseq [[id mutant]
              [[accept (alter-source c accept #(assoc-in % [:replacements :assignment/helper] [:literal "helper-2"]))]
               [accept (alter-source c accept #(assoc-in % [:replacements :journal/one] [:literal nil]))]
               [refuse (alter-source c refuse #(-> % (update :reads disj :journal/one)
                                                   (update-in [:guard :readers] disj :journal/one)
                                                   (update-in [:guard :expression] (fn [expr] (vec (remove #{(eq :journal/one nil)} expr))))))]]]
        (let [r (guarded-report mutant id)]
          (is (empty? (:conditionally-checked-preservation-ids r)))
          (is (= :post-branch-not-established
                 (:reason (first (filter #(= :claim/ownership-and-journal (:invariant %)) (:preservation-outcomes r)))))))))))

(deftest guarded-substitution-leaves-unsupported-cases-open
  (let [c (claim/catalog)
        accept (claim/alternative-id "helper-1" :accept)
        replay (claim/alternative-id "helper-1" :replay-accepted)
        ownership-id :claim/ownership-and-journal
        no-proof? (fn [report]
                    (= :open (:status (first (filter #(= ownership-id (:invariant %))
                                                     (:preservation-outcomes report))))))]
    (is (no-proof? (guarded-report (alter-source c accept #(assoc-in % [:replacements :request/status]
                                                                    [:read :request/status])) accept)))
    (is (no-proof? (guarded-report (alter-source c replay #(assoc % :guard {:readers #{} :expression [:literal false]}
                                                                   :reads #{})) replay)))
    (is (no-proof? (guarded-report (assoc-in c [:invariants ownership-id]
                                            {:readers #{:request/status}
                                             :expression [:not (eq :request/status :claimed)]}) replay)))
    (is (no-proof? (guarded-report (assoc-in c [:invariants ownership-id]
                                            {:readers #{:request/revision}
                                             :expression (into [:or] (map #(eq :request/revision %) (range 129)))}) replay)))))

(deftest unified-checker-selects-whole-guarded-claim-inferences
  (let [c (claim/catalog)
        id :claim/ownership-and-journal
        obligation-id [:preservation id]
        source (fn [catalog operation-id]
                 {:construction (:construction (first (filter #(= operation-id (:id %)) (:operations catalog))))
                  :initial-condition (:initial-condition catalog) :invariants (:invariants catalog)})]
    (doseq [op (:declared-operation-ids c)]
      (let [s (source c op)
            r (unified/check-unified-preservation! s)
            outcome (first (filter #(= id (:invariant %)) (:preservation-outcomes r)))]
        (is (contains? (:conditionally-checked-preservation-ids r) obligation-id))
        (is (some #{:guarded} (:supporting-families outcome)))
        (is (contains? (:open-meta-obligations r) :guarded-literal-substitution-rule-soundness))
        (is (= :none (:authority r)))))
    (let [accept (claim/alternative-id "helper-1" :accept)
          mutant (alter-source c accept #(assoc-in % [:replacements :assignment/helper] [:literal "helper-2"]))
          r (unified/check-unified-preservation! (source mutant accept))
          induction (induction/check-induction! c)
          row (first (filter #(= id (:invariant %)) (:induction-outcomes induction)))]
      (is (contains? (:unsupported-preservation-ids r) obligation-id))
      (is (= (:declared-operation-ids c) (:preserving-operation-ids row)))
      (is (= :checked-conditional-induction (:status row)))
      (is (= :required-conjunct-disjunction-introduction (:initial-rule row)))
      (is (= :not-issued (:certificate induction))))))

(deftest claim-induction-refuses-bad-initial-state-or-transition
  (let [c (claim/catalog)
        id :claim/ownership-and-journal
        bad-init (assoc-in c [:initial-condition :expression]
                           [:and (conjunction (assoc claim/open-state :assignment/helper "helper-1"))
                            (:expression claim/revision)])
        accept (claim/alternative-id "helper-1" :accept)
        bad-step (alter-source c accept #(assoc-in % [:replacements :assignment/helper] [:literal "helper-2"]))
        row (fn [r] (first (filter #(= id (:invariant %)) (:induction-outcomes r))))
        init-result (row (induction/check-induction! bad-init))
        step-result (row (induction/check-induction! bad-step))]
    (is (= :initial-consequence-open (:reason init-result)))
    (is (= (:declared-operation-ids c) (:preserving-operation-ids init-result)))
    (is (= :checked-conditional-inference (:initial-status step-result)))
    (is (= :some-preservation-open (:reason step-result)))
    (is (= #{accept} (:unsupported-operation-ids step-result)))))

(deftest declared-observation-report-rechecks-exact-sources-and-divergence
  (let [before {:request/status :open :request/revision 0 :assignment/helper nil
                :journal/one nil :journal/two nil :membership/one true :membership/two true}
        after (assoc before :request/status :claimed :request/revision 1
                     :assignment/helper "helper-1" :journal/one :accepted)
        step {:construction (claim/source "helper-1" :accept) :before before :inputs {} :after after}
        id (claim/alternative-id "helper-1" :accept)
        source {:catalog (claim/catalog) :observations [step (assoc-in step [:after :membership/two] false)]}
        r (correspondence/check-declared-observations! source)
        reason (fn [s] (try (correspondence/check-declared-observations! s) nil
                            (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= #{id} (:observed-operation-ids r)))
    (is (= #{id} (:divergent-operation-ids r)))
    (is (= 7 (count (:unobserved-operation-ids r))))
    (is (= [true false] (mapv #(get-in % [:checks :matches?]) (:observations r))))
    (is (false? (:all-supplied-observations-match? r)))
    (is (= :caller-supplied-unauthenticated (:physical-observation-provenance r)))
    (is (= :not-issued (:certificate r)))
    (is (= :none (:transaction-authority r)))
    (is (= :observation-not-bound-to-declared-construction
           (reason (assoc source :observations [(assoc-in step [:construction :replacements :assignment/helper]
                                                          [:literal "helper-2"])]))))
    (is (= :invalid-source-shape (reason (assoc source :certificate :issued))))
    (is (= :invalid-observation-selection (reason (assoc source :observations []))))))

(deftest bound-declared-observations-require-live-same-catalog-handles
  (let [bound (claim/bound-inductive-source!)
        before (assoc claim/open-state :membership/one true :membership/two true)
        after (assoc before :request/status :claimed :request/revision 1
                     :assignment/helper "helper-1" :journal/one :accepted)
        step {:construction (claim/source "helper-1" :accept) :before before :inputs {} :after after}
        source {:inductive-source bound :observations [step]}
        check correspondence/check-bound-declared-observations!
        rejected? (fn [s] (try (check s) false (catch clojure.lang.ExceptionInfo _ true)))
        result (check source)]
    (is (:all-supplied-observations-match? result))
    (is (= :source-bound-conditional-abstract-safety (get-in result [:bound-inductive-report :verdict])))
    (is (= :not-issued (:certificate result)))
    (is (= :none (:authority result)))
    (is (= :not-established (:installed-executor-correspondence result)))
    (is (= 7 (count (:unobserved-operation-ids result))))
    (is (rejected? (assoc source :inductive-source {:catalog (claim/catalog)
                                                    :certificate-handle nil :dependency-handle nil :current-graph nil})))
    (is (rejected? (assoc-in source [:inductive-source :certificate-handle] {})))
    (is (rejected? (assoc-in source [:inductive-source :catalog :id] :fixture/other-epoch)))
    (is (rejected? (assoc-in source [:observations 0 :construction :replacements :assignment/helper]
                            [:literal "helper-2"])))
    (is (false? (:all-supplied-observations-match?
                 (check (assoc-in source [:observations 0 :after :membership/two] false)))))))

(deftest multi-record-target-mapping-checks-original-catalog
  (let [bound (claim/bound-inductive-source!)
        source {:inductive-source bound :target-map claim/target-map}
        result (mapped/check-inductive-target-mapping! source)
        accept (claim/alternative-id "helper-1" :accept)
        refuse (claim/alternative-id "helper-1" :refuse)
        replay (claim/alternative-id "helper-1" :replay-accepted)
        rows (:operation-mappings result)
        reason (fn [m] (try (plan/check-target-mapping! (claim/source "helper-1" :accept) m) nil
                           (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= (:declared-operation-ids (:catalog bound)) (set (keys rows))))
    (is (= #{[:document :requests "r1"] [:document :request_assignments "primary-r1"]
             [:journal-slot "helper-1" :request/claim]} (:write-targets (get rows accept))))
    (is (= #{[:journal-slot "helper-1" :request/claim]} (:write-targets (get rows refuse))))
    (is (empty? (:write-targets (get rows replay))))
    (is (= (select-keys claim/target-map #{:membership/one :membership/two :journal/two})
           (:frame-coordinates (get rows accept))))
    (is (= (get rows accept) (plan/check-target-mapping! (claim/source "helper-1" :accept) claim/target-map)))
    (is (= :not-issued (:executor result)))
    (is (= :none (:authority result)))
    (is (= :wrong-keys (reason (dissoc claim/target-map :journal/two))))
    (is (= :aliased-target-coordinate (reason (assoc claim/target-map :journal/two (:journal/one claim/target-map)))))
    (is (= :aliased-target-coordinate (reason (assoc claim/target-map :request/revision (:request/status claim/target-map)))))
    (is (= :invalid-document-coordinate (reason (assoc claim/target-map :request/status [:document :requests "r1" :xt/id]))))
    (is (= :invalid-document-coordinate (reason (assoc claim/target-map :request/status [:document :requests "r1" :status-name]))))
    (is (= :invalid-journal-slot-coordinate (reason (assoc claim/target-map :request/revision [:journal-slot "helper-1" :request/claim]))))
    (is (= :conflicting-document-presence
           (reason (assoc claim/target-map :assignment/helper [:nullable-document :requests "r1" :helper]))))
    (is (not= (:fingerprint (get rows accept))
              (:fingerprint (plan/check-target-mapping! (claim/source "helper-1" :accept)
                              (assoc claim/target-map :membership/two [:document :memberships "helper-3" :active])))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (mapped/check-inductive-target-mapping! (assoc-in source [:inductive-source :certificate-handle] {}))))))

(deftest issued-mapped-admission-requires-original-live-source
  (let [bound (claim/bound-inductive-source!)
        source (claim/mapped-admission-source bound)
        issue mapped/build-inductive-mapped-admission!
        current mapped/require-current-mapped-admission!
        admission (issue source)
        info (mapped/describe-mapped-admission admission)
        alternative (claim/alternative-id "helper-1" :accept)
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= :issued-local-preparation-registry (:admission info)))
    (is (= :not-issued (:executor info) (:certificate info)))
    (is (= :none (:authority info) (:transaction-authority info)))
    (is (= info (current admission source)))
    (is (false? (admission :pending? alternative "c1")))
    (is (= :exact-envelope-with-trusted-footprint-checker (:transaction-admission info)))
    (is (= :unsupported-mapped-submission
           (reason #(admission :admit! {:api :submit-tx :tx-ops [] :tx-options {}}))))
    (is (= :unsupported-mapped-submission
           (reason #(admission :admit! {:api :execute-tx :tx-ops [] :tx-options {:database :other}}))))
    (is (= :unsupported-mapped-transaction
           (reason #(admission :admit! {:api :execute-tx :tx-ops [[:put-docs :requests {}]] :tx-options {}}))))
    (is (= :unsupported-mapped-transaction
           (reason #(admission :admit! {:api :execute-tx :tx-ops [["ASSERT TRUE; DELETE FROM requests"] [:put-docs :requests {}]] :tx-options {}}))))
    (is (= :unknown-mapped-admission-action (reason #(admission :submit!))))
    (is (= :unregistered-mapped-admission (reason #(current info source))))
    (is (= :unregistered-mapped-admission (reason #(current (fn [& _] info) source))))
    (is (= :changed-mapped-admission-registration
           (reason #(current admission (assoc source :decode-outcome! (constantly :accepted))))))
    (is (= :changed-mapped-admission-registration
           (reason #(current admission (assoc source :transaction-footprint! (constantly {}))))))
    (is (= :changed-mapped-admission-registration
           (reason #(current admission (assoc source :auxiliary-tables #{})))))
    (is (= :invalid-auxiliary-tables
           (reason #(issue (assoc source :auxiliary-tables #{:requests})))))
    (is (= :invalid-auxiliary-tables
           (reason #(issue (assoc source :auxiliary-tables #{:gesso_commands})))))
    (is (= :changed-mapped-admission-registration
           (reason #(current admission (assoc-in source [:target-map :membership/two]
                                                [:document :memberships "helper-3" :active])))))
    (is (= :changed-mapped-admission-registration
           (reason #(current admission (assoc-in source [:inductive-source :certificate-handle] {})))))
    (is (= :changed-mapped-admission-registration
           (reason #(current admission (assoc-in source [:inductive-source :dependency-handle] {})))))
    (is (thrown? clojure.lang.ExceptionInfo (issue (assoc-in source [:inductive-source :certificate-handle] {}))))
    (is (thrown? clojure.lang.ExceptionInfo (issue (assoc-in source [:inductive-source :catalog :id] :fixture/other))))
    (is (= :invalid-mapped-admission-checker-or-decoder
           (with-redefs [mapped/check-inductive-target-mapping! (constantly info)]
             (reason #(issue source)))))))

(deftest mapped-publication-expectations-derive-from-admitted-document-selectors
  (let [source (claim/mapped-admission-source (claim/bound-inductive-source!))
        accept (claim/alternative-id "helper-1" :accept)
        refuse (claim/alternative-id "helper-1" :refuse)
        declaration {:purpose :gesso.live/refresh
                     :contributions [{:target [:document :requests "r1"]
                                      :topic :request :field :request/location-id}
                                     {:target [:document :request_assignments "primary-r1"]
                                      :topic :request-assignment :field :request/location-id}]}
        declarations (into {} (for [helper (keys claim/helpers) alternative claim/alternatives]
                                [(claim/alternative-id helper alternative)
                                 (when (= :accept alternative) declaration)]))
        source* (assoc source :publication-derivations declarations)
        issue mapped/build-inductive-mapped-admission!
        admission (issue source*)
        record (resolution/command-record {:principal (identity/principal "helper-1")
                                            :command-id (identity/command-id "c1")
                                            :operation :request/claim :arguments {:request-id "r1"}}
                                           {:outcome :request/claimed :helper "helper-1"} "t1")
        before {:requests [{:xt/id "r1" :request/location-id "l1"}]
                :request_assignments []}
        after {:requests [{:xt/id "r1" :request/location-id "l2"}]
               :request_assignments [{:xt/id "primary-r1" :request/location-id "l2"}]}
        derive #(mapped/reconstruct-mapped-publication! admission % "source" record before %2)
        expectation (fn [changes]
                      {:status :derived
                       :intents (:intents (outbox/prepare! {:intent-ids (mapv #(outbox/derived-intent-id "source" :request/claim "helper-1" "c1" %) (range (count changes)))
                                                           :source-id "source" :operation :request/claim :purpose :gesso.live/refresh :command-id "c1"} changes))})
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= declarations (:publication-derivations (mapped/describe-mapped-admission admission))))
    (is (= (expectation [{:topic :request :id "l1"} {:topic :request :id "l2"}
                         {:topic :request-assignment :id "l2"}]) (derive accept after)))
    (is (= (expectation [{:topic :request :id "l1"} {:topic :request-assignment :id "l2"}])
           (derive accept (assoc-in after [:requests 0 :request/location-id] "l1"))))
    (is (= {:status :derived :intents []} (derive refuse before)))
    (is (= {:status :not-declared}
           (mapped/reconstruct-mapped-publication! (issue source) accept "source" record before after)))
    (is (= :invalid-publication-document-subject
           (reason #(derive accept (assoc-in after [:requests 0 :request/location-id] "")))))
    (is (= :invalid-publication-document-rows
           (reason #(derive accept (assoc after :request_assignments [])))))
    (is (= :publication-command-slot-mismatch
           (reason #(derive (claim/alternative-id "helper-2" :accept) after))))
    (is (= :changed-mapped-admission-registration
           (reason #(mapped/require-current-mapped-admission! admission (assoc-in source* [:publication-derivations accept :purpose] :example/other)))))
    (doseq [bad [(dissoc declarations accept)
                 (assoc-in declarations [accept :contributions 0 :target] [:document :memberships "helper-1"])
                 (update-in declarations [accept :contributions] pop)
                 (update-in declarations [accept :contributions] conj (first (:contributions declaration)))
                 (assoc-in declarations [accept :purpose] :unqualified)]]
      (is (thrown? clojure.lang.ExceptionInfo (issue (assoc source* :publication-derivations bad)))))))

(deftest declared-publication-is-checked-at-production-frame-boundary
  (let [accept (claim/alternative-id "helper-1" :accept)
        source (assoc (claim/publication-mapped-admission-source (claim/bound-inductive-source!))
                      ;; Isolate this check from the fixture footprint's own
                      ;; restrictions, which already reject some bad intents.
                      :transaction-footprint! (constantly {:alternative accept :command-id "c1"}))
        admission (mapped/build-inductive-mapped-admission! source)
        no-publication (mapped/build-inductive-mapped-admission!
                        (assoc-in source [:publication-derivations accept] nil))
        binding {:principal (identity/principal "helper-1") :command-id (identity/command-id "c1")
                 :operation :request/claim :arguments {:request-id "r1"}}
        record (resolution/command-record binding {:outcome :request/claimed :helper "helper-1"} "t1")
        before {:xt/id "r1" :status :open :revision 0 :created-at "t0" :updated-at "t0" :request/location-id "l1"}
        member {:active true :revision 0 :created-at "t0" :updated-at "t0"}
        docs {:requests [before] :request_assignments []
              :memberships [(assoc member :xt/id "helper-1") (assoc member :xt/id "helper-2")]}
        snapshot {:document-rows docs :records {}
                  :state (claim/snapshot! (fn [[sql]] (get (assoc docs :gesso_commands []) (keyword (last (clojure.string/split sql #" "))))))}
        writes [[:put-docs :requests (assoc before :status :claimed :revision 1 :updated-at "t1")]
                [:put-docs :request_assignments {:xt/id "primary-r1" :request-id "r1" :helper "helper-1"
                                                 :revision 0 :created-at "t1" :updated-at "t1" :request/location-id "l1"}]
                [:put-docs :gesso_commands record]]
        pub (outbox/prepare! {:intent-ids (mapv #(outbox/derived-intent-id "source" :request/claim "helper-1" "c1" %) [0 1])
                              :source-id "source" :operation :request/claim :purpose :gesso.live/refresh :command-id "c1"}
                             [{:topic :request :id "l1"} {:topic :request-assignment :id "l1"}])
        ops (into (into (:guard-ops pub) writes) [(:tx-op pub)])
        ctx {publication/preparation-key (with-meta (constantly nil) {::publication/source-id "source"})}
        frame! (fn [a ctx ops]
                 (a :prepare! accept binding snapshot {})
                 ;; Same function used before either executor's physical try.
                 (#'publication/frame-resolved-plan! ctx {:ops ops :record record} a))
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
        marker #(get-in % [:frame-handoff :checked :candidate :publication-expectation])]
    ;; The registry callback is private to the issuer; the legacy admission
    ;; action cannot be used to extract the captured complete document rows.
    (admission :prepare! accept binding snapshot {})
    (let [base {:api :execute-tx :tx-options {} :tx-ops ops}
          framed (admission :frame-submission! base)
          called (atom false)]
      (is (= :invalid-framed-admission-arguments
             (reason #(admission :admit-framed! base framed (fn [& _] (reset! called true))))))
      (is (false? @called))
      (is (admission :pending? accept "c1")))
    (admission :discard! accept "c1")
    (is (= :matched (marker (frame! admission ctx ops))))
    (is (not (admission :pending? accept "c1")))
    (doseq [bad [(pop ops)
                 (update-in ops [(dec (count ops)) 2] assoc :gesso.live.outbox/purpose :example/wrong)
                 (update-in ops [(dec (count ops)) 2] assoc :gesso.live.outbox/subject-id "other")
                 (update ops (dec (count ops)) conj (first (:intents pub)))
                 (assoc-in ops [(dec (count ops)) 2 :xt/id] (:xt/id (second (:intents pub))))]]
      (is (= :candidate-publication-expectation-mismatch (reason #(frame! admission ctx bad))))
      (is (not (admission :pending? accept "c1"))))
    (doseq [bad [(subvec ops 1) (into [(first ops)] ops) (assoc-in ops [0 1] (random-uuid))]]
      (is (= :candidate-publication-guard-mismatch (reason #(frame! admission ctx bad))))
      (is (not (admission :pending? accept "c1"))))
    (is (= :invalid-publication-source (reason #(frame! admission {} ops))))
    (is (not (admission :pending? accept "c1")))
    (is (= :candidate-publication-expectation-mismatch (reason #(frame! no-publication ctx ops))))
    (is (= :matched (marker (frame! no-publication ctx (into [["ASSERT TRUE"]] writes)))))
    (is (not (no-publication :pending? accept "c1")))))

(deftest mapped-residual-assertions-derive-from-admitted-prestate
  (let [source (claim/publication-mapped-admission-source (claim/bound-inductive-source!))
        accept (claim/alternative-id "helper-1" :accept)
        refuse (claim/alternative-id "helper-1" :refuse)
        declarations (claim/residual-assertion-derivations)
        source* (assoc source :residual-assertion-derivations declarations)
        issue mapped/build-inductive-mapped-admission!
        admission (issue source*)
        binding {:principal (identity/principal "helper-1") :command-id (identity/command-id "c1")
                 :operation :request/claim :arguments {:request-id "r1"}}
        record (resolution/command-record binding {:outcome :request/claimed :helper "helper-1"} "t1")
        docs {:requests [{:xt/id "r1" :status :open :revision 0 :updated-at "t0" :request/location-id "l1"}]
              :memberships [{:xt/id "helper-1" :active true :revision 0 :updated-at "t0"}]
              :request_assignments []}
        derive #(mapped/reconstruct-mapped-residual-assertions! admission % record %2)
        expected (fn [alternative layout subject]
                   {:status :derived
                    :assertions (cond-> (guard/lower-assertions! (claim/source "helper-1" alternative)
                                                               claim/target-map binding layout)
                                  subject (conj (model-tx/format-assertion
                                                 (model-tx/assert-one :requests [:and [:= :xt/id [:lift "r1"]]
                                                                                      [:= :request/location-id [:lift subject]]]))))})
        layout {[:requests "r1"] [[:state :request/revision] [:extra :updated-at "t0"] [:state :request/status]]
                [:memberships "helper-1"] [[:extra :revision 0] [:extra :updated-at "t0"] [:state :membership/one]]}
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= declarations (:residual-assertion-derivations (mapped/describe-mapped-admission admission))))
    (is (= (expected :accept layout "l1") (derive accept docs)))
    (is (= (expected :accept (assoc-in layout [[:memberships "helper-1"] 0 2] 9) "l2")
           (derive accept (-> docs (assoc-in [:memberships 0 :revision] 9)
                              (assoc-in [:requests 0 :request/location-id] "l2")))))
    (is (= (expected :refuse {[:requests "r1"] [[:state :request/revision] [:extra :updated-at "t1"]
                                                  [:state :request/status]]} nil)
           (derive refuse (assoc-in docs [:requests 0 :updated-at] "t1"))))
    (is (= {:status :not-declared}
           (mapped/reconstruct-mapped-residual-assertions! (issue source) accept record docs)))
    (is (= {:status :derived :assertions []}
           (mapped/reconstruct-mapped-residual-assertions!
            (issue (assoc-in source* [:residual-assertion-derivations accept] nil)) accept record docs)))
    (is (= :invalid-residual-prestate-value
           (reason #(derive accept (update-in docs [:requests 0] dissoc :updated-at)))))
    (is (= :invalid-residual-prestate-rows (reason #(derive accept (assoc docs :memberships [])))))
    (is (= :invalid-residual-prestate-rows
           (reason #(derive accept (update docs :requests conj (first (:requests docs)))))))
    (is (= :residual-command-slot-mismatch (reason #(derive (claim/alternative-id "helper-2" :accept) docs))))
    (is (= :changed-mapped-admission-registration
           (reason #(mapped/require-current-mapped-admission! admission (assoc-in source* [:residual-assertion-derivations accept] nil)))))
    (doseq [bad [(dissoc declarations refuse)
                 (assoc-in declarations [accept :subject-checks] [[:memberships "helper-2" :active]])
                 (assoc-in declarations [accept :subject-checks] [[:requests "r1" :xt/id]])
                 (assoc-in declarations [accept :document-layout [:requests "r1"] 1] [:literal :updated-at])
                 (update-in declarations [accept :document-layout [:requests "r1"]] pop)
                 (assoc-in declarations [accept :document-layout [:requests "r1"] 1] [:prestate :revision])]]
      (is (thrown? clojure.lang.ExceptionInfo (issue (assoc source* :residual-assertion-derivations bad)))))))

(ns gesso.assurance.xtdb-inductive-claim-mutation-test
  "v1306: adversarial physical-write mutations after the v1290 induction-bound
   Claim executor has admitted the canonical candidate. Exercise genuine
   embedded XTDB commits and check their observed before/after documents
   against the exact v1233 abstract transition via v1298/v1300/v1302.

   The mutations are TEST-ONLY replacements of the final tx/transact! call;
   production admission, issued handler, abstract semantics and XTDB itself
   are not changed. A committed command is NOT evidence of a conforming
   domain write. Aborted writes and incomplete physical observations cannot
   produce positive correspondence reports.

   This pilot proves neither ownership uniqueness nor universal refinement.
   Its abstract invariant is restricted request-status membership. The
   observation reports remain copyable and not independently authenticated."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.observed-correspondence :as joined]
            [gesso.assurance.xtdb-inductive-claim-integration-test]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.inductive-plan :as induction-plan]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def ^:private operation :example/claim)
(def ^:dynamic *pilot* nil)

(defn- pilot-function [sym]
  (let [v (ns-resolve 'gesso.assurance.xtdb-inductive-claim-integration-test sym)]
    (when-not (and v (fn? @v))
      (throw (ex-info "Missing v1304 pilot helper" {:symbol sym})))
    @v))

(defn- physical-row [node table id]
  (first (xt/q node [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))

(defn- emit-claim! [edit]
  ;; `run-claim!` seeds the canonical current document, invokes the exact
  ;; issued v1290 executor and captures the v1298 witness. The outer dynamic
  ;; replacement intercepts ONLY the final transaction submission, after
  ;; the original candidate was checked by the v1251 certified-plan gate.
  (let [real-submit tx/transact!
        intercepted (atom [])
        run! (pilot-function 'run-claim!)
        sample (with-redefs [tx/transact!
                             (fn [ctx plan]
                               (let [modified (edit plan)]
                                 (swap! intercepted conj
                                        {:declared plan :submitted modified})
                                 (real-submit ctx modified)))]
                 (run! (:node *pilot*) (:package *pilot*) {}))]
    (assoc sample :submission-events @intercepted)))

(defn- edit-domain [f]
  (fn [plan]
    (update plan :commands
            (fn [commands]
              (mapv (fn [command]
                      (if (= :claim (:model/operation command))
                        (f command)
                        command))
                    commands)))))

(defn- rewrite-poststate [f]
  (edit-domain #(update % :model/after f)))

(defn- no-mutation [plan] plan)

(defn- drop-domain [plan]
  (update plan :commands
          (fn [commands]
            (filterv #(= :create (:model/operation %)) commands))))

(defn- check! [sample]
  (joined/check-observed-correspondence!
   {:application-source (:application-source *pilot*)
    :transition-source ((pilot-function 'transition-source)
                        (:package *pilot*) sample)}))

(defn- caught [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- preserved-boundaries! [r]
  (is (= :none (:authority r)))
  (is (= :none (:transaction-authority r)))
  (is (= :caller-supplied-unauthenticated
         (:physical-observation-provenance r)))
  (is (= :not-established (:authenticated-physical-correspondence r)))
  (is (= :not-established (:universal-xtdb-refinement r)))
  (is (= :not-established (:application-invariant-physical-preservation r)))
  (is (= :not-established (:physical-write-path-completeness r)))
  (is (= :all-original-obligations-remain-open
         (:original-obligation-status r)))
  (is (contains? (:open-obligations r)
                 :physical-observation-origin-is-unauthenticated)))

(defn- committed-mutation! [mutation]
  (let [sample (emit-claim! mutation)
        report (check! sample)]
    (is (= 1 (count (:submission-events sample)))
        "One physically submitted plan from one issued executor invocation")
    (is (= :incorporated (get-in sample [:result :status])))
    (is (= :committed (get-in sample [:physical-outcome :physical/status])))
    (is (= [:prepared :physical-outcome]
           (mapv :phase (:events sample))))
    (is (= (:binding sample)
           (:gesso.command/binding (:observed-record sample))))
    (is (some? (:witness sample)))
    (preserved-boundaries! report)
    {:sample sample :report report}))

(defn- assert-divergent! [mutation checks]
  (let [{:keys [sample report]} (committed-mutation! mutation)]
    (is (false? (:diagnostic-conforms? report))
        (str "Physical mutation must not pass " (pr-str (:observed-checks report))))
    (is (= :diverges-from-declared-step (:diagnostic-correspondence report)))
    (doseq [check checks]
      (is (false? (get-in report [:observed-checks check]))
          (str "Expected failed diagnostic check " check)))
    (is (= (:observed-request sample)
           (get-in sample [:witness :observed-after (:target sample)])))
    report))

(use-fixtures
  :once
  (fn [run]
    (with-open [node (xtn/start-node {})]
      (let [package ((pilot-function 'source-package) node)]
        (fixture/with-closed-application
          {:operations {operation (fixture/trusted-operation
                                   operation {:published-change-topics #{}})}}
          (fn [{:keys [assembly]}]
            (binding [*pilot* {:node node :package package
                               :application-source
                               ((pilot-function 'source-for-application)
                                assembly package)}]
              (run))))))))

(deftest unmodified-physical-submit-corresponds-to-declared-step
  (let [{:keys [sample report]} (committed-mutation! no-mutation)]
    (is (true? (:diagnostic-conforms? report)))
    (is (= :corresponds-to-declared-step (:diagnostic-correspondence report)))
    (is (= :claimed (get-in sample [:observed-request :request/status])))
    (is (= 3 (get-in sample [:observed-request :request/version])))
    (is (= 12 (get-in sample [:observed-request :request/score])))))

(deftest command-only-commit-is-not-an-implemented-claim
  (let [{:keys [sample report]} (committed-mutation! drop-domain)]
    (is (= (:before sample) (:observed-request sample)))
    (is (false? (:diagnostic-conforms? report)))
    (is (false? (get-in report [:observed-checks :abstract-assignments])))
    (is (false? (get-in report [:observed-checks :model-poststate])))))

(deftest committed-incorrect-status-is-detected
  (assert-divergent!
   (rewrite-poststate #(assoc % :request/status :open))
   [:abstract-assignments :model-poststate]))

(deftest committed-incorrect-version-is-detected
  (assert-divergent!
   (rewrite-poststate #(assoc % :request/version 4))
   [:abstract-assignments :model-poststate]))

(deftest committed-framed-score-mutation-is-detected
  (assert-divergent!
   (rewrite-poststate #(assoc % :request/score 13))
   [:abstract-frame :model-poststate]))

(deftest committed-optional-field-presence-mutation-is-detected
  (assert-divergent!
   (rewrite-poststate #(assoc % :request/note "unexpected after claim"))
   [:abstract-frame :model-poststate]))

(deftest committed-incorrect-status-type-is-not-a-proof
  (assert-divergent!
   (rewrite-poststate #(assoc % :request/status 123))
   [:abstract-assignments]))

(deftest committed-missing-status-is-not-a-proof
  (assert-divergent!
   (rewrite-poststate #(dissoc % :request/status))
   [:complete-physical-domain-projection]))

(deftest committed-undeclared-physical-field-is-not-a-proof
  (assert-divergent!
   (rewrite-poststate #(assoc % :request/unmodeled true))
   [:complete-physical-domain-projection]))

(deftest mutated-physical-version-assertion-aborts-without-a-witness
  ;; A post-admission change to the physical CAS expected version must fail
  ;; at XTDB. There is no committed command record and no physical witness
  ;; to compare; "unresolved" is not a durable business rejection.
  (let [mutation (edit-domain
                  #(assoc-in % [:model/expected :model/checks]
                             [[:request/version 999]]))
        sample (emit-claim! mutation)]
    (is (= 1 (count (:submission-events sample))))
    (is (= :unresolved (get-in sample [:result :status])))
    (is (= :rejected (get-in sample [:physical-outcome :physical/status])))
    (is (= :xtdb-explicit-assertion-failure
           (get-in sample [:physical-outcome :physical/source])))
    (is (nil? (:witness sample)))
    (is (nil? (:observed-record sample)))
    (is (= (:before sample) (:observed-request sample)))
    (is (some? (caught #(check! sample)))
        "Missing a committed witness must not yield correspondence")))

(deftest committed-command-cannot-upgrade-abstract-induction
  (let [{:keys [sample report]} (committed-mutation! no-mutation)
        source (get-in *pilot* [:package :induction-source])
        executor (get-in *pilot* [:package :executor])
        plan-report (induction-plan/require-current-inductive-plan!
                     executor source)]
    (is (= :conditionally-checked-in-declared-abstract-machine
           (:inductive-invariant plan-report)))
    (is (= :not-established (:physical-correspondence plan-report)))
    (is (= :none (:authority plan-report)))
    (is (= :not-established (:production-certification report)))
    (is (= (:xt/id (:observed-record sample))
           (:xt/id (physical-row (:node *pilot*) :gesso_commands
                                 (resolution/record-id (:binding sample))))))))

(deftest copyable-physical-witness-cannot-authenticate-itself
  (let [{:keys [sample]} (committed-mutation! no-mutation)
        forged (update sample :witness assoc :physical-tx-id 900000000)
        r (check! forged)]
    (is (true? (:diagnostic-conforms? r)))
    (is (= :caller-supplied-unauthenticated
           (:physical-observation-provenance r)))
    (is (= :not-established (:authenticated-physical-correspondence r)))
    (is (= :none (:authority r)))))

(deftest forged-authority-in-physical-witness-is-rejected
  (let [sample (emit-claim! no-mutation)
        forged (assoc-in sample [:witness :authority] :certified)
        error (caught #(check! forged))]
    (is (= :gesso.model.transition-conformance/invalid-transition-conformance
           (:error/type error)))
    (is (= :invalid-observation-claims (:reason error)))))

(deftest changed-abstract-transition-does-not-certify-physical-write
  (let [sample (emit-claim! no-mutation)
        source ((pilot-function 'transition-source) (:package *pilot*) sample)
        forged (assoc-in source [:construction :guard :expression]
                         [:literal true])
        error (caught #(joined/check-observed-correspondence!
                        {:application-source (:application-source *pilot*)
                         :transition-source forged}))]
    (is (= :gesso.assurance.observed-correspondence/invalid-observed-correspondence
           (:error/type error)))
    (is (= :observation-not-bound-to-issued-plan (:reason error)))))

(deftest malformed-physical-witness-cannot-become-matching-evidence
  (let [sample (emit-claim! no-mutation)
        forged (update-in sample [:witness :observed-after] dissoc (:target sample))
        error (caught #(check! forged))]
    (is (= :gesso.model.transition-conformance/invalid-transition-conformance
           (:error/type error)))
    (is (= :inconsistent-observed-footprint-or-command (:reason error)))))

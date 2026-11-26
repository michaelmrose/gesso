(ns gesso.assurance.reference-mapped-decoder-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-decoder :as decoder]
            [gesso.assurance.reference-mapped-outcome-test :as fixture]
            [gesso.choreo.identity :as identity]
            [gesso.live.outbox.claim-semantic-fixture :as claim]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.inductive-plan :as induction]))

(defn- row-key [row]
  (let [{:keys [principal operation arguments]} (:gesso.command/binding row)]
    [(identity/raw-value principal) operation arguments (:gesso.command/outcome row)]))

(defn- closed-decode! [row]
  (or (get fixture/cases (row-key row)) (throw (ex-info "Outside finite cases" {}))))

(def outside (resolution/command-record (assoc-in (:binding fixture/spec) [:arguments :request-id] "r2")
                                        {:status :ok} "t1"))

(deftest samples-are-bounded-and-issued-source-relative
  (let [source (assoc fixture/source :decode-outcome! closed-decode!)
        calls (atom 0)
        admission (fn [& _] (throw (ex-info "Not an execution" {})))
        checked (fn [a s]
                  (swap! calls inc)
                  (if (and (identical? a admission) (identical? (:decode-outcome! s) (:decode-outcome! source)))
                    {:catalog-fingerprint "test-catalog" :induction-fingerprint "test-induction"}
                    (throw (ex-info "Wrong admission" {:reason :changed-mapped-admission-registration}))))]
    (with-redefs [induction/require-current-mapped-admission! checked]
      (let [r (decoder/compare-issued! admission source fixture/declarations [outside])]
        (is (= :matched (:status r)) (pr-str r))
        (is (= :matched-finite-samples (:decoder-observation r)))
        (is (= 2 (:declared-case-count r)))
        (is (= 4 (:positive-match-count r) (:positive-sample-count r)))
        (is (= 1 (:outside-throw-count r)))
        (is (= 0 (:outside-return-count r)))
        (is (= :not-established (:universal-decoder-correspondence r)))
        (is (= :not-established (:outcome-provenance r)))
        (is (= :none (:authority r)))
        (is (= :not-issued (:certificate r))))
      (is (= :changed-mapped-admission-registration (:reason (decoder/compare-issued! admission (assoc source :decode-outcome! (constantly :claimed)) fixture/declarations []))))
      (is (= :changed-mapped-admission-registration (:reason (decoder/compare-issued! nil source fixture/declarations []))))
      (let [n @calls]
        (is (= :unsupported (:status (decoder/compare-issued! admission source fixture/declarations [{}]))))
        (is (= :invalid-outside-decoder-samples (:reason (decoder/compare-issued! admission source fixture/declarations [(nth (first (:ops (fixture/candidate fixture/spec))) 2)]))))
        (is (= n @calls))))))

(deftest samples-detect-incorrect-values-and-outside-returns
  ;; Stub only issuance rechecking; exercise real finite derivation and callback invocation.
  (with-redefs [induction/require-current-mapped-admission! (constantly {})]
    (doseq [[f reason] [[(constantly :claimed) :declared-decoder-sample-mismatch]
                        [(fn [r] (get fixture/cases (row-key r) :claimed)) :outside-decoder-sample-returned]
                        [(fn [r] (get fixture/cases (row-key r))) :outside-decoder-sample-returned]
                        [(fn [r] (or (get fixture/cases (row-key r)) (throw (IllegalStateException. "failure")))) :outside-decoder-sample-unexpected-exception]
                        [(fn [r] (if (= "sample-time-1" (:gesso.command/created-at r)) :wrong (closed-decode! r))) :declared-decoder-sample-mismatch]]]
      (let [r (decoder/compare-issued! nil (assoc fixture/source :decode-outcome! f) fixture/declarations [outside])]
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :decoder-observation)))
        (is (= :not-established (:universal-decoder-correspondence r)))))))

(deftest actual-issued-claim-decoder-matches-closed-domain
  (let [source (claim/physical-mapped-admission-source (claim/bound-inductive-source!))
        admission (induction/build-inductive-mapped-admission! source)
        accepted (fn [helper] {:outcome :request/claimed :helper helper})
        refused {:outcome :request/not-claimable :reason :already-claimed}
        declarations (into {} (for [helper (keys claim/helpers) branch [:accept :refuse]]
                                [(claim/alternative-id helper branch)
                                 {:arguments {:request-id "r1"}
                                  :outcome (if (= :accept branch) (accepted helper) refused)}]))
        record (fn [id principal operation arguments outcome]
                 (resolution/command-record {:command-id (identity/command-id id)
                                             :principal (identity/principal principal)
                                             :operation operation :arguments arguments} outcome "t1"))
        probes [(record "foreign-op" "helper-1" :request/other {:request-id "r1"} (accepted "helper-1"))
                (record "foreign-principal" "helper-3" :request/claim {:request-id "r1"} refused)
                (record "foreign-request" "helper-1" :request/claim {:request-id "r2"} refused)
                (record "foreign-outcome" "helper-1" :request/claim {:request-id "r1"} {:outcome :other})
                (record "wrong-helper" "helper-1" :request/claim {:request-id "r1"} (accepted "helper-2"))]
        result (decoder/compare-issued! admission source declarations probes)
        closed (decoder/compare-issued-closed! admission source declarations)]
    (is (= declarations (claim/outcome-declarations)))
    (is (= :matched (:status closed)) (pr-str closed))
    (is (= :matched-issued-closed-construction (:closed-decoder-binding closed)))
    (is (= 4 (:declared-case-count closed)))
    (is (= (:catalog-fingerprint result) (:catalog-fingerprint closed)))
    (is (= (:induction-fingerprint result) (:induction-fingerprint closed)))
    (is (= :not-established (:outcome-provenance closed)))
    (is (= :matched (:status result)) (pr-str result))
    (is (= :matched-finite-samples (:decoder-observation result)))
    (is (= 4 (:declared-case-count result)))
    (is (= 8 (:positive-match-count result) (:positive-sample-count result)))
    (is (= 5 (:outside-sample-count result) (:outside-throw-count result)))
    (is (= 0 (:outside-return-count result)))
    (is (= 0 (:outside-unexpected-exception-count result)))
    (is (thrown? clojure.lang.ExceptionInfo
                 ((:decode-outcome! source) (assoc (record "valid" "helper-1" :request/claim {:request-id "r1"} (accepted "helper-1"))
                                                   :xt/id "wrong-record-id"))))
    (is (= :not-established (:universal-decoder-correspondence result)))
    (is (= :not-established (:outcome-provenance result)))))

(deftest closed-construction-binding-is-not-sampling
  (let [decode! (decoder/build-closed! fixture/source fixture/declarations)
        source (assoc fixture/source :decode-outcome! decode!)
        admission (fn [& _] nil)
        calls (atom 0)
        other (decoder/build-closed! source fixture/declarations)
        record (nth (first (:ops (fixture/candidate fixture/spec))) 2)]
    (is (= :claimed (decode! record)))
    (is (thrown? clojure.lang.ExceptionInfo (decode! outside)))
    (with-redefs [induction/require-current-mapped-admission!
                  (fn [a s]
                    (swap! calls inc)
                    (if (and (identical? admission a) (identical? decode! (:decode-outcome! s)))
                      {:catalog-fingerprint "test-catalog" :induction-fingerprint "test-induction"}
                      (throw (ex-info "Changed registration" {:reason :changed-mapped-admission-registration}))))]
      (let [r (decoder/compare-issued-closed! admission source fixture/declarations)]
        (is (= :matched (:status r)) (pr-str r))
        (is (= decoder/closed-profile (:profile r)))
        (is (= :matched-issued-closed-construction (:closed-decoder-binding r)))
        (is (= "test-catalog" (:catalog-fingerprint r)))
        (is (= 2 (:declared-case-count r)))
        (is (= :not-established (:outcome-provenance r)))
        (is (= :not-established (:universal-decoder-correspondence r)))
        (is (= :none (:authority r)))
        (is (= :not-issued (:certificate r))))
      (doseq [[s d reason] [[(assoc source :decode-outcome! (fn [r] (decode! r))) fixture/declarations :unregistered-closed-decoder]
                             [(assoc source :target-map {}) fixture/declarations :changed-closed-decoder-construction]
                             [(assoc-in source [:inductive-source :catalog :id] :other/catalog) fixture/declarations :changed-closed-decoder-construction]
                             [source (assoc-in fixture/declarations [:example/claim :arguments] {:request-id "r2"}) :changed-closed-decoder-construction]]]
        (let [r (decoder/compare-issued-closed! admission s d)]
          (is (= reason (:reason r)) (pr-str r))
          (is (not (contains? r :closed-decoder-binding)))))
      (is (= 1 @calls))
      (is (= :changed-mapped-admission-registration
             (:reason (decoder/compare-issued-closed! admission (assoc source :decode-outcome! other) fixture/declarations))))
      (is (= :changed-mapped-admission-registration
             (:reason (decoder/compare-issued-closed! nil source fixture/declarations))))
      (is (= 3 @calls)))))

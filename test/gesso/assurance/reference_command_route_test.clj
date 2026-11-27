(ns gesso.assurance.reference-command-route-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-command-authority :as authority]
            [gesso.assurance.reference-mapped-observed-recovery :as recovery]
            [gesso.assurance.reference-mapped-submission :as submission]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.live.outbox.command-authority-owner :as owner]))

(def ^:private route
  {:route-id :request/claim :path "/operations/request/:request-id/claim"
   :method :post :operation :request/claim :required-transport :htmx
   :uri "/operations/request/r1/claim" :path-arguments {:request-id "r1"}
   :request-gate :locally-accepted :evidence :matched-canonical-htmx-handoff})

(defn- origin [id execution]
  {:principal (identity/principal "helper") :command-id (identity/command-id id)
   :execution-id (identity/execution-id execution) :operation :request/claim
   :arguments {:request-id "r1"} :origin :registered-server-invocation
   :authentication :conditional-on-trusted-principal-resolver :http-route route})

(deftest selected-route-observation-preserves-checked-bindings
  (let [origins [(origin "one" "e1") (origin "two" "e2") (origin "two" "e2")]
        steps (mapv (fn [tx o] {:spec {:binding (select-keys o resolution/binding-keys)}
                                :expectation {:tx-id tx}}) [1 2 3] origins)
        claims (mapv #(hash-map :command-origin %) origins)
        compare! #'submission/compare-command-origins!
        report (compare! steps claims)]
    (is (= :matched-supplied-command-bindings (:command-origin-correspondence report)))
    (is (= [route route route] (mapv :http-route (:selected-command-origins report))))
    (is (= :same-observed-execution (:abort-recovery-execution report)))
    (is (= :not-established (:command-authentication report)))
    (is (nil? (get-in (compare! steps (update-in claims [0 :command-origin] dissoc :http-route))
                      [:selected-command-origins 0 :http-route])))
    (doseq [bad [nil (dissoc route :route-id) (assoc route :extra true)
                 (with-meta route {:unexpected true})
                 (assoc route :route-id "claim") (assoc route :method :get)
                 (assoc route :operation :request/cancel) (assoc route :required-transport :sse)
                 (assoc route :path "/wrong/:request-id")
                 (assoc route :uri "/operations/request/r2/claim")
                 (assoc route :uri "/operations/request/r1/claim?x=1")
                 (assoc route :path-arguments {:request-id "r2"})
                 (assoc route :request-gate :authenticated) (assoc route :evidence :forged)]]
      (is (= :mapped-command-origin-mismatch
             (try (compare! steps (assoc-in claims [0 :command-origin :http-route] bad)) nil
                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
    (let [describe #'recovery/command-authentication-premises
          entry {:scope {:publication-source-id "claim" :historical-cut {:payload-fingerprint "fixture"}}
                 :command-observation-ref "original-cut"
                 :judgment-subject {:catalog-id :fixture/claim :selected-operations [:accept :accept :refuse]}
                 :original-recovery (merge report {:selected-tx-ids [1 2 3]
                                                   :selected-commitments [:committed :aborted :committed]})}
          premise (describe entry)
          path [:original-recovery :selected-command-origins 0 :http-route]]
      (is (= route (get-in premise (into [:scope] (drop 1 path)))))
      (doseq [other [(update-in entry (pop path) dissoc :http-route)
                     (assoc-in entry (conj path :route-id) :another/claim)
                     (assoc-in entry (conj path :request-gate) :other)]]
        (let [changed (describe other)]
          (is (not-any? (:open-premise-fingerprints premise) (:open-premise-fingerprints changed)))
          (is (= :not-established (:command-authentication changed)))
          (is (= :not-performed (:premise-acceptance changed))))))))

(deftest command-authority-contract-is-exact-and-unaccepted
  (let [origins [(origin "one" "e1") (origin "two" "e2") (origin "two" "e2")]
        steps (mapv (fn [tx o] {:spec {:binding (select-keys o resolution/binding-keys)}
                                :expectation {:tx-id tx}}) [1 2 3] origins)
        report (assoc (#'submission/compare-command-origins! steps (mapv #(hash-map :command-origin %) origins))
                      :command-origin-correspondence :matched-issued-command-bindings)
        entry {:scope {:publication-source-id "claim" :historical-cut {:payload-fingerprint "fixture"}}
               :command-observation-ref "original-cut"
               :judgment-subject {:catalog-id :fixture/claim :selected-operations [:accept :accept :refuse]}
               :original-recovery (merge report {:selected-tx-ids [1 2 3]
                                                :selected-commitments [:committed :aborted :committed]})}
        command (#'recovery/command-authentication-premises entry)
        policy {:deployment-id :fixture/host :request-gate-policy :fixture/csrf
                :principal-session-policy :fixture/session}
        contract (authority/describe! command policy)
        premises (mapv :assumption (:obligations contract))
        selected (authority/select! command policy (vec (reverse premises)))
        problem (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= :open-command-authority-contract (:status contract)))
    (is (= :selected-open-command-authority-contract (:status selected)))
    (is (= (:obligations contract) (:obligations selected)))
    (is (= (:scope command) (get-in contract [:scope :command-subject])))
    (is (= (:open-premise-fingerprints command) (get-in contract [:scope :original-command-premises])))
    (is (not-any? (:open-premise-fingerprints command) (:open-premise-fingerprints contract)))
    (is (= [:reference-command-authority/request-gate :reference-command-authority/principal-session]
           (mapv :id premises)))
    (is (every? #(= :environment (:origin %)) premises))
    (is (every? #(= :open (:discharge-state %)) (:obligations selected)))
    (is (= :not-established (:configured-authority selected) (:command-authentication selected)))
    (is (= :not-performed (:premise-acceptance selected) (:proof-discharge selected)))
    (is (= :none (:authority selected)))
    (is (not (contains? selected :trusted-assumptions)))
    (is (not (contains? selected :subject-provenance)))
    (doseq [bad [nil [] [(first premises) (first premises)] (pop premises)
                 (:obligations contract) (mapv :assumption (:obligations command))]]
      (is (= :incorrect-command-authority-premise-selection
             (problem #(authority/select! command policy bad)))))
    (doseq [changed [(assoc policy :deployment-id :fixture/other)
                     (assoc policy :request-gate-policy :fixture/other)
                     (assoc policy :principal-session-policy :fixture/other)]]
      (is (= :subject-mismatch (problem #(authority/select! command changed premises)))))
    (is (= :premise-mismatch (problem #(authority/select! command policy (assoc-in premises [0 :proposition] [:fixture/other])))))
    (is (= :invalid-command-authority-policy (problem #(authority/describe! command (assoc policy :extra true)))))
    (doseq [changed [(assoc-in entry [:original-recovery :command-origin-correspondence] :matched-supplied-command-bindings)
                     (assoc-in entry [:original-recovery :command-origin-correspondence] :partial-issued-command-bindings)
                     (update-in entry [:original-recovery :selected-command-origins 0] dissoc :http-route)]]
      (is (= :incomplete-issued-http-command-subject
             (problem #(authority/describe! (#'recovery/command-authentication-premises changed) policy)))))
    (let [other (#'recovery/command-authentication-premises (assoc entry :command-observation-ref "different-cut"))]
      (is (= :subject-mismatch (problem #(authority/select! other policy premises)))))
    (is (= :unissued-conditional-recovery
           (:reason (recovery/describe-command-authority-contract! (Object.) (Object.) policy))))
    (is (= :unissued-conditional-recovery
           (:reason (recovery/select-command-authority-contract! (Object.) (Object.) policy premises))))
    (with-redefs [authority/describe! (constantly contract)]
      (is (= :command-authority-boundary-replaced
             (:reason (recovery/describe-command-authority-contract! (Object.) (Object.) policy)))))))

(deftest installed-policy-must-match-every-selected-origin
  (let [policy {:deployment-id :fixture/host :request-gate-policy :fixture/gate
                :principal-session-policy :fixture/session}
        binding {:installation-ref (str (random-uuid)) :policy policy}
        origins (mapv #(assoc-in % [:http-route :authority-binding] binding)
                      [(origin "one" "e1") (origin "two" "e2") (origin "two" "e2")])
        steps (mapv (fn [tx o] {:spec {:binding (select-keys o resolution/binding-keys)}
                                :expectation {:tx-id tx}}) [1 2 3] origins)
        claims (mapv #(hash-map :command-origin %) origins)
        compare! #'submission/compare-command-origins!
        describe (fn [claims]
                   (#'recovery/command-authentication-premises
                    {:scope {:publication-source-id "claim" :historical-cut {:payload-fingerprint "fixture"}}
                     :command-observation-ref "original-cut"
                     :judgment-subject {:catalog-id :fixture/claim :selected-operations [:accept :accept :refuse]}
                     :original-recovery (assoc (compare! steps claims)
                                              :command-origin-correspondence :matched-issued-command-bindings
                                              :selected-tx-ids [1 2 3]
                                              :selected-commitments [:committed :aborted :committed])}))
        problem (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
        contract (authority/describe! (describe claims) policy)
        qualified (#'owner/match-contract! binding (assoc contract :subject-provenance :matched-original-issued-cut))
        declaration {:acceptance :trust-open-premises
                     :premise-ids #{:reference-command-authority/request-gate :reference-command-authority/principal-session}}
        selected (assoc (authority/select! (describe claims) policy (mapv :assumption (:obligations contract)))
                        :subject-provenance :matched-original-issued-cut)
        adopted (#'owner/adopt-selected! qualified declaration selected)]
    (is (= :conditional-configured-command-authority (:status adopted)))
    (is (= :configured-local-host (:deployment-adoption adopted)))
    (is (= :explicit-declaration-only (:premise-acceptance adopted)))
    (is (= (:obligations qualified) (:obligations adopted)))
    (is (= (:open-premise-fingerprints qualified) (:trusted-assumptions adopted)))
    (is (= (:scope qualified) (:scope adopted)))
    (is (= :not-established (:command-authentication adopted)))
    (is (= :not-performed (:proof-discharge adopted)))
    (is (= :none (:authority adopted)))
    (is (= :not-issued (:certificate adopted)))
    (doseq [bad [nil {} (assoc declaration :acceptance :prove-premises)
                 (update declaration :premise-ids disj :reference-command-authority/request-gate)
                 (update declaration :premise-ids conj :reference-command/principal-resolution)]]
      (is (= :command-authority-selection-mismatch
             (problem #(#'owner/adopt-selected! qualified bad selected)))))
    (doseq [bad [contract (assoc selected :subject-provenance :supplied)
                 (assoc selected :open-premise-fingerprints #{})
                 (assoc selected :proof-discharge :performed)]]
      (is (= :command-authority-selection-mismatch
             (problem #(#'owner/adopt-selected! qualified declaration bad)))))
    (is (= :matched-current-local-installation (:configured-authority qualified)))
    (is (= :not-performed (:premise-acceptance qualified) (:proof-discharge qualified)))
    (is (= :not-established (:command-authentication qualified)))
    (is (= binding (:installation-binding qualified)))
    (is (= :command-authority-installation-mismatch
           (problem #(authority/describe! (describe claims) (assoc policy :deployment-id :other/host)))))
    (doseq [changed [(update-in claims [1 :command-origin :http-route] dissoc :authority-binding)
                     (assoc-in claims [1 :command-origin :http-route :authority-binding :installation-ref] (str (random-uuid)))]]
      (is (= :command-authority-installation-mismatch
             (problem #(authority/describe! (describe changed) policy)))))
    (doseq [bad [nil {} (assoc binding :installation-ref "fake") (assoc binding :policy {})]]
      (is (= :mapped-command-origin-mismatch
             (problem #(describe (assoc-in claims [0 :command-origin :http-route :authority-binding] bad))))))
    (is (= :command-authority-original-installation-mismatch
           (problem #(#'owner/match-contract! (assoc binding :installation-ref (str (random-uuid)))
                      (assoc contract :subject-provenance :matched-original-issued-cut)))))
    (is (= :command-authority-original-installation-mismatch
           (problem #(#'owner/match-contract! binding contract))))))

(ns gesso.assurance.example.certified-claim-http-principal-test
  "v1787: Real network principal acquisition and exact owner grant must be
   checked before the v1785 boot's ONE catalog-bound typed XTDB write."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.example.certified-claim-http-boot :as boot]
            [gesso.assurance.proof.checked-biff-request-principal :as gate]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test
             :as previous]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation-test]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.time Duration]))

(def ^:private original-test 'gesso.assurance.example.certified-claim-http-boot-test)
(def ^:private earlier-ns
  'gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test)
(def ^:private model-ns 'gesso.model.resolved-operation-test)
(defn- fixture-helper [n sym]
  (var-get (or (ns-resolve n sym)
               (throw (ex-info "Fixture unavailable" {:ns n :sym sym})))))
(defn- old [sym] (fixture-helper original-test sym))
(defn- model [sym] (fixture-helper model-ns sym))
(def ^:dynamic *client* nil)
(use-fixtures :once
  (fn [f]
    ((model 'with-real-xtdb)
     (fn []
       (fixture/with-closed-application
        {:operations {:request/claim
                      (fixture/trusted-operation
                       :request/claim {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (binding [previous/*assembly* assembly
                    *client* (-> (HttpClient/newBuilder)
                                 (.connectTimeout (Duration/ofSeconds 5))
                                 (.build))]
            (f))))))))

(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
(defn- with-source
  ([] (with-source nil nil))
  ([acquire override-policy]
   (let [env ((old 'environment))
         session-id (str (random-uuid))
         known (atom {session-id :user/helper-a "other" :user/outsider})
         calls (atom 0)
         acquire! (or acquire
                      (fn [request]
                        (swap! calls inc)
                        (if-let [person (get @known
                                             (get-in request [:headers "cookie"]))]
                          {:status :authenticated :principal-id person}
                          {:status :anonymous})))
         policy (or override-policy
                    {:policy-id :request/claim-authority
                     :principal-owners {:user/helper-a "helper-a"}
                     :grants {:user/helper-a #{:request/claim}}
                     :acquire-principal! acquire!})]
     (assoc env :calls calls :sessions known :session-id session-id
            :source (assoc (:source env) :principal-policy policy)))))
(defn- send! [env cookie & [extra-headers]]
  (let [port (:port @(:server-state env))
        builder (-> (HttpRequest/newBuilder
                     (URI/create (str "http://127.0.0.1:" port
                                      "/operations/request/claim")))
                    (.timeout (Duration/ofSeconds 6)))
        _ (when cookie (.header builder "cookie" cookie))
        _ (doseq [[name value] extra-headers]
            (.header builder name value))
        request (.POST builder (HttpRequest$BodyPublishers/noBody))
        response (.send ^HttpClient *client* (.build request)
                        (HttpResponse$BodyHandlers/ofString))]
    (.statusCode response)))
(defn- seed! [& items] (apply (model 'seed!) items))
(defn- read-request [id] ((model 'read-request) id))
(defn- read-command [id] ((model 'read-command) id))
(defn- cleanup! [env issued]
  (when issued ((:stop! issued)))
  ((:biff.core/stop (:adapter env)) {}))

(deftest acquired-session-principal-matches-claim-owner-at-actual-http-handler
  (let [{:keys [source session-id calls data env] :as e} (with-source)
        {:keys [request binding]} data
        issued (boot/issue-claim-http-boot! source)]
    (seed! [:requests request])
    (try
      (is (= :verified-configured-start (:status ((:boot! issued)))))
      (is (= :trusted-host-result-must-match-granted-typed-owner
             (:principal-authorization-check ((:summary! issued)))))
      (is (= :not-established
             (:host-principal-acquisition-authenticity ((:summary! issued)))))
      ;; Challenge is read-only and never acquires a user.
      (is (zero? @calls))
      (is (= 401 (send! e nil [["x-user" "user/helper-a"]])))
      (is (= 401 (send! e "forged-cookie" [["x-user" "user/helper-a"]])))
      (is (= 403 (send! e "other")))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (is (zero? @(get-in env [:environment :constructs])))
      ;; Authorized session has been resolved by a trusted host callback,
      ;; not by a user-supplied identity header.
      (is (= 200 (send! e session-id [["x-user" "user/outsider"]])))
      (is (= ((model 'next-version) request {:owner "helper-a"})
             (read-request (:xt/id request))))
      (is (= (resolution/command-record
              binding {:status :claimed :owner "helper-a"} "t1")
             (read-command (resolution/record-id binding))))
      (is (= 1 @(get-in env [:environment :constructs])))
      (is (= 4 @calls))
      ;; The authoritative transaction is not replayed after acknowledgment.
      (is (= 500 (send! e session-id)))
      (is (= 1 @(get-in env [:environment :constructs])))
      (finally (cleanup! e issued)))))

(deftest anonymous-request-never-consumes-catalog-typed-trial
  (let [{:keys [source session-id data env] :as e} (with-source)
        {:keys [request binding]} data
        issued (boot/issue-claim-http-boot! source)]
    (seed! [:requests request])
    (try
      ((:boot! issued))
      (is (= 401 (send! e nil)))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (is (zero? @(get-in env [:environment :constructs])))
      (is (= 200 (send! e session-id)))
      (is (= 1 @(get-in env [:environment :constructs])))
      (finally (cleanup! e issued)))))

(deftest principal-owner-mismatch-rejected-before-http-start
  (let [{:keys [source server-state]} (with-source)
        bad (assoc-in source [:principal-policy :principal-owners :user/helper-a]
                      "not-the-declared-owner")]
    (is (= :invalid-or-unbound-principal-owner-grant
           (reason #(boot/issue-claim-http-boot! bad))))
    (is (nil? @server-state))))

(deftest ungranted-claim-is-not-accepted-as-an-authorization-declaration
  (let [{:keys [source server-state]} (with-source)
        bad (assoc-in source [:principal-policy :grants :user/helper-a]
                      #{:request/cancel})]
    (is (= :invalid-or-unbound-principal-owner-grant
           (reason #(boot/issue-claim-http-boot! bad))))
    (is (nil? @server-state))))

(deftest malformed-acquisition-result-fails-closed-before-original-handler
  (let [calls (atom 0)
        acquire (fn [_] (swap! calls inc)
                  {:status :authenticated :principal-id :user/helper-a
                   :certificate :forged})
        {:keys [source data] :as e} (with-source acquire nil)
        {:keys [request binding]} data
        issued (boot/issue-claim-http-boot! source)]
    (seed! [:requests request])
    (try
      ((:boot! issued))
      (is (= 500 (send! e "anything")))
      (is (= 1 @calls))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (finally (cleanup! e issued)))))

(deftest throwing-principal-callback-fails-closed-before-original-handler
  (let [acquire (fn [_] (throw (ex-info "authenticator unavailable" {})))
        {:keys [source data env] :as e} (with-source acquire nil)
        {:keys [request binding]} data
        issued (boot/issue-claim-http-boot! source)]
    (seed! [:requests request])
    (try
      ((:boot! issued))
      (is (= 500 (send! e "anything")))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (is (zero? @(get-in env [:environment :constructs])))
      (finally (cleanup! e issued)))))

(deftest stale-membership-guard-rejects-after-valid-principal-acquisition
  (let [{:keys [source session-id data] :as e} (with-source)
        ;; original certified executor is generated from the immutable
        ;; fixture; override the source using the earlier membership fixture.
        guarded ((old 'environment) {:guard-membership? true} :honest)
        source (assoc (:source guarded) :principal-policy (:principal-policy source))
        {:keys [request membership binding]} (:data guarded)
        issued (boot/issue-claim-http-boot! source)]
    (seed! [:requests request] [:memberships membership])
    (xt/execute-tx ((model 'database))
                   (:tx-ops (tx/prepare {}
                                        {:commands [((model 'revoke-command) membership)]
                                         :emit false})))
    (try
      ((:boot! issued))
      (is (= 500 (send! guarded session-id)))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (finally (cleanup! guarded issued)))))

(deftest lost-acknowledgment-after-granted-principal-remains-uncertain
  (let [{:keys [source session-id data] :as e} (with-source)
        {:keys [request binding]} data
        issued (boot/issue-claim-http-boot! source)
        original tx/transact!
        tx-count (atom 0)]
    (seed! [:requests request])
    (try
      ((:boot! issued))
      (with-redefs [tx/transact! (fn [ctx plan]
                                  (swap! tx-count inc)
                                  (original ctx plan)
                                  (throw (ex-info "unknown ack" {})))]
        (is (= 500 (send! e session-id))))
      (is (= 1 @tx-count))
      (is (some? (read-command (resolution/record-id binding))))
      (is (= 500 (send! e session-id)))
      (is (= 1 @tx-count))
      (finally (cleanup! e issued)))))

(deftest forged-acquisition-policy-shape-is-rejected-before-start
  (let [{:keys [source server-state]} (with-source)]
    (is (= :invalid-or-unbound-principal-owner-grant
           (reason #(boot/issue-claim-http-boot!
                     (assoc-in source [:principal-policy :grants]
                               {:user/helper-a #{:request/claim}
                                :user/outsider #{:request/claim}})))))
    (is (nil? @server-state))))

(deftest original-unauthenticated-boot-remains-explicitly-unproven
  (let [source (:source ((old 'environment)))
        issued (boot/issue-claim-http-boot! source)]
    (is (= :not-established
           (:principal-authorization-check ((:summary! issued)))))
    (is (= :not-established
           (:principal-authentication ((:summary! issued)))))))

(deftest request-intent-gate-uses-same-ring-request-and-cannot-grant
  (let [{:keys [source session-id data env] :as e} (with-source)
        {:keys [request binding]} data
        original-acquire (get-in source [:principal-policy :acquire-principal!])
        acquired (atom nil)
        intent-calls (atom [])
        source (-> source
                   (assoc-in [:principal-policy :acquire-principal!]
                             (fn [ring-request]
                               (reset! acquired ring-request)
                               (original-acquire ring-request)))
                   (assoc-in [:principal-policy :verify-request-intent!]
                             (fn [ring-request person operation]
                               (swap! intent-calls conj
                                      {:same-request? (identical? @acquired ring-request)
                                       :principal person :operation operation})
                               (= "valid-request-token"
                                  (get-in ring-request [:headers "x-request-token"])))))
        issued (boot/issue-claim-http-boot! source)]
    (seed! [:requests request])
    (try
      ((:boot! issued))
      ;; An authenticated and granted principal still cannot submit without
      ;; the host-supplied request-integrity decision on this SAME request.
      (is (= 403 (send! e session-id)))
      (is (= 403 (send! e session-id [["x-request-token" "forged"]])))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (is (zero? @(get-in env [:environment :constructs])))
      (is (= 2 (count @intent-calls)))
      (is (every? #(= {:same-request? true
                       :principal :user/helper-a
                       :operation :request/claim} %)
                  @intent-calls))
      (is (= 200 (send! e session-id
                        [["x-request-token" "valid-request-token"]])))
      (is (= 3 (count @intent-calls)))
      (is (= ((model 'next-version) request {:owner "helper-a"})
             (read-request (:xt/id request))))
      (is (some? (read-command (resolution/record-id binding))))
      (is (= 1 @(get-in env [:environment :constructs])))
      (finally (cleanup! e issued)))))

(deftest malformed-request-intent-or-throwing-callback-fails-closed
  (doseq [bad-intent [(fn [_ _ _] {:status :ok})
                      (fn [_ _ _] nil)
                      (fn [_ _ _] (throw (ex-info "intent unavailable" {})))]]
    (let [{:keys [source session-id data env] :as e} (with-source)
          {:keys [request binding]} data
          source (assoc-in source [:principal-policy :verify-request-intent!]
                           bad-intent)
          issued (boot/issue-claim-http-boot! source)]
      (seed! [:requests request])
      (try
        ((:boot! issued))
        (is (= 500 (send! e session-id)))
        (is (= request (read-request (:xt/id request))))
        (is (nil? (read-command (resolution/record-id binding))))
        (is (zero? @(get-in env [:environment :constructs])))
        (finally (cleanup! e issued))))))

(deftest malformed-intent-policy-is-rejected-before-http-start
  (let [{:keys [source server-state]} (with-source)]
    (doseq [bad [nil true {:untrusted true}]]
      (is (= :invalid-or-unbound-principal-owner-grant
             (reason #(boot/issue-claim-http-boot!
                       (assoc-in source [:principal-policy :verify-request-intent!]
                                 bad))))))
    (is (nil? @server-state))))

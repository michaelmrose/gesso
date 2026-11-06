(ns gesso.live.browser.refinement-exploration-parity-test
  "Permanent adversarial and cross-host corpus for choice-relative browser
   state-space exploration. This suite invokes the actual adapter and the
   refinement explorer; neither synthetic transitions nor caller-supplied
   success summaries substitute for source execution.

   Corpus results contain only stable vectors, keywords, booleans, integers,
   and explicitly sorted sets. This enables an exact JVM/Node comparison.
   Bounded success must never imply exhaustive host-event enumeration, global
   choreography refinement, physical delivery, or XTDB commit correctness."
  (:require [gesso.choreo.core :as choreo]
            [gesso.choreo.machine :as machine]
            [gesso.choreo.project :as project]
            [gesso.live.browser.adapter :as adapter]
            [gesso.live.browser.refinement-exploration :as explorer]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :as t :refer-macros [deftest is testing]])))

(def corpus-prefix "GESSO_BROWSER_EXPLORATION_PARITY ")
(def corpus-version 1)

(defn- scenario []
  (choreo/->choreography
   {:initial :one
    :states {:one (choreo/communicate :server :browser :notice/one
                                       :two {:via :http})
             :two (choreo/communicate :server :browser :notice/two
                                       :work {:via :http})
             :work (choreo/local :browser :browser/work :done)
             :done (choreo/return :done)}}))

(defn- start-event []
  {:event :execution/start :execution-id :run
   :execution (machine/start (project/project (scenario) :browser))})

(defn- receive-event [state id event]
  {:event :machine/message :execution-id :run
   :generation (adapter/execution-generation state :run)
   :message-id id
   :envelope (machine/message :server :browser event {} {:via :http})})

(defn- event-choices [{:keys [state events]}]
  (case (count events)
    0 [(start-event) {:event :live/invalidated :fragment-id :panel}]
    1 (if (get-in state [:executions :run])
        [(receive-event state :physical-one :notice/one)
         (receive-event state :physical-wrong :notice/not-waited)
         {:event :unknown/nonexistent}]
        [])
    2 (if (get-in state [:executions :run])
        [(receive-event state :physical-two :notice/two)
         (receive-event state :physical-one :notice/one)]
        [])
    []))

(defn- alternate-physical-choices [context]
  (mapv (fn [event]
          (if (= :physical-one (:message-id event))
            (assoc event :message-id :physical-one-alternate)
            event))
        (event-choices context)))

(defn- explore
  ([] (explore {}))
  ([opts]
   (explorer/explore (adapter/initial-state) event-choices opts)))

(defn- attempted [f]
  (try (f) [:accepted]
       (catch #?(:clj clojure.lang.ExceptionInfo
                 :cljs cljs.core.ExceptionInfo) e
         [:rejected (:error/kind (ex-data e))])))

(defn- kind [f]
  (second (attempted f)))

(defn- sorted-tags [x]
  (vec (sort-by str x)))

(defn- event-signature [events]
  (mapv (fn [event]
          [(:event event) (:message-id event)
           (get-in event [:envelope :event]) (:fragment-id event)])
        events))

(defn- prefix-signature [prefix]
  [(event-signature (:events prefix))
   (:candidate-count prefix)
   (sorted-tags (:missing-witnesses prefix))])

(defn- report-signature [report]
  [(:status report)
   (:visited-prefix-count report)
   (:attempted-edge-count report)
   (:accepted-edge-count report)
   (:distributed-candidate-count report)
   (mapv prefix-signature (:prefixes report))
   (mapv (fn [edge] [(event-signature (:events edge))
                      (:error/kind edge)])
         (:rejected-edges report))
   (mapv (fn [bad] [(:kind bad) (:depth bad)
                     (event-signature (:events bad))
                     (:candidate-count bad)
                     (sorted-tags (:missing-witnesses bad))])
         (:counterexamples report))
   (mapv (fn [frontier] [(prefix-signature frontier) (:reason frontier)])
         (:frontier report))
   (sorted-tags (:missing-witnesses report))
   (:choice-relative-depth-complete? report)
   (:global-event-enumeration-established? report)
   (:global-trace-admitted? report)
   (:proof-authority report)
   (sorted-tags (:limitations report))])

(defn- forbid-second-notice [{:keys [refinement]}]
  (not-any? #(= :notice/two
                (get-in % [:distributed-observation :event]))
            (:distributed-candidates refinement)))

(defn- targeted-mutations [report]
  [[:false-global-trace (assoc report :global-trace-admitted? true)]
   [:forged-proof-authority (assoc report :proof-authority :certified)]
   [:false-event-enumeration (assoc report :global-event-enumeration-established? true)]
   [:altered-bounds (assoc-in report [:bounds :max-depth] 4)]
   [:dropped-rejection (update report :rejected-edges pop)]
   [:dropped-prefix (update report :prefixes pop)]
   [:forged-candidate-count (update report :distributed-candidate-count inc)]
   [:dropped-witnesses (assoc report :missing-witnesses #{})]
   [:forged-status (assoc report :status :globally-verified)]
   [:forged-limitations (assoc report :limitations #{})]
   [:false-completeness (assoc report :choice-relative-depth-complete? false)]])

(defn portable-corpus
  "Deterministic source-executed exploration signatures for JVM/Node parity.
   The event generator and invariant predicate remain caller-supplied and are
   not themselves proven exhaustive or correct."
  []
  (let [base (explore)
        bad (explore {:transition-predicate forbid-second-notice})
        root (adapter/initial-state)
        depth-zero (explore {:max-depth 0})
        depth-one (explore {:max-depth 1})
        no-choices (explorer/explore root (constantly []) {:max-depth 3})
        rejected-event (explorer/explore root
                                         (constantly [{:event :future/unknown}])
                                         {:max-depth 1})]
    [[:full-bound (report-signature base)]
     [:counterexample (report-signature bad)]
     [:depth-zero (report-signature depth-zero)]
     [:depth-one (report-signature depth-one)]
     [:empty-generator (report-signature no-choices)]
     [:adapter-rejection (report-signature rejected-event)]
     [:physical-delivery-identity
      (let [alternate (explorer/explore root alternate-physical-choices {})]
        [(= (:distributed-candidate-count base)
            (:distributed-candidate-count alternate))
         (not= (report-signature base) (report-signature alternate))
         (attempted #(explorer/recheck-exploration
                      root alternate-physical-choices {} base))])]
     [:strict-replay (report-signature
                      (explorer/recheck-exploration root event-choices {} base))]
     [:replay-mutants
      (mapv (fn [[id forged]]
              [id (attempted #(explorer/recheck-exploration
                              root event-choices {} forged))])
            (targeted-mutations base))]
     [:duplicate-choices
      (attempted #(explorer/explore root
                                  (constantly [(start-event) (start-event)])
                                  {}))]
     [:nonvector-choices
      (attempted #(explorer/explore root (constantly (list (start-event))) {}))]
     [:fanout-budget (attempted #(explore {:max-choices 1}))]
     [:node-budget (attempted #(explore {:max-nodes 2}))]
     [:edge-budget (attempted #(explore {:max-edges 1}))]
     [:invalid-bound (attempted #(explore {:max-depth -1}))]
     [:invalid-predicate
      (attempted #(explore {:transition-predicate (constantly :yes)}))]
     [:forged-adapter-rejection
      (attempted #(explore {:transition-predicate
                           (fn [_] (throw (ex-info "caller error"
                                                   {:error/type :gesso.live.browser.adapter/error
                                                    :error/kind :forged-adapter-error})))}))]
     [:generator-exception
      (attempted #(explorer/explore root
                                   (fn [_] (throw (ex-info "generator error"
                                                             {:error/kind :generator-error})))
                                   {}))]]))

(def ^:private expected-case-ids
  [:full-bound :counterexample :depth-zero :depth-one :empty-generator
   :adapter-rejection :physical-delivery-identity :strict-replay :replay-mutants :duplicate-choices
   :nonvector-choices :fanout-budget :node-budget :edge-budget
   :invalid-bound :invalid-predicate :forged-adapter-rejection
   :generator-exception])

(deftest explores-complete-choice-relative-tree-and-stuttering-history
  (let [r (explore)
        prefixes (:prefixes r)]
    (is (= :bounded-no-counterexamples (:status r)))
    (is (= 9 (:visited-prefix-count r)))
    (is (= 9 (:attempted-edge-count r)))
    (is (= 8 (:accepted-edge-count r)))
    (is (= 3 (:distributed-candidate-count r)))
    (is (= 1 (count (:rejected-edges r))))
    (is (= :unknown-event (get-in r [:rejected-edges 0 :error/kind])))
    (is (= 1 (count (filter #(empty? (:events %)) prefixes))))
    (is (= 2 (count (filter #(= 1 (count (:events %))) prefixes))))
    (is (= 2 (count (filter #(= 2 (count (:events %))) prefixes))))
    (is (= 4 (count (filter #(= 3 (count (:events %))) prefixes))))
    (is (every? #(#{:depth-bound :no-choices} (:reason %))
                (:frontier r)))))

(deftest physical-delivery-identities-are-distinct-unmerged-prefixes
  (let [root (adapter/initial-state)
        original (explore)
        alternate (explorer/explore root alternate-physical-choices {})]
    (is (= (:visited-prefix-count original)
           (:visited-prefix-count alternate)))
    (is (= (:distributed-candidate-count original)
           (:distributed-candidate-count alternate)))
    (is (not= (:prefixes original) (:prefixes alternate)))
    (is (not= (report-signature original) (report-signature alternate)))
    (is (= :exploration-report-mismatch
           (kind #(explorer/recheck-exploration
                   root alternate-physical-choices {} original))))
    (is (= :exploration-report-mismatch
           (kind #(explorer/recheck-exploration
                   root event-choices {} alternate))))
    (is (false? (:global-trace-admitted? alternate)))))

(deftest depth-bounds-and-empty-generator-do-not-create-global-proof
  (let [root (adapter/initial-state)
        count-choose (atom 0)
        zero (explorer/explore root
                               (fn [_] (swap! count-choose inc) [(start-event)])
                               {:max-depth 0})
        empty-generator (explorer/explore root (constantly []) {:max-depth 2})]
    (is (zero? @count-choose))
    (is (= 1 (:visited-prefix-count zero)))
    (is (= :depth-bound (get-in zero [:frontier 0 :reason])))
    (is (= 0 (:attempted-edge-count zero)))
    (is (= :no-choices (get-in empty-generator [:frontier 0 :reason])))
    (is (= 1 (:visited-prefix-count empty-generator)))
    (doseq [r [zero empty-generator (explore)]]
      (is (false? (:global-event-enumeration-established? r)))
      (is (false? (:global-trace-admitted? r)))
      (is (= :none (:proof-authority r)))
      (is (contains? (:limitations r) :not-physical-transport-correspondence)))))

(deftest reproducible-counterexample-retains-entire-failing-prefix
  (let [opts {:transition-predicate forbid-second-notice}
        r (explore opts)
        bad (first (:counterexamples r))]
    (is (= :counterexamples-found (:status r)))
    (is (= 1 (count (:counterexamples r))))
    (is (= :caller-transition-predicate-failed (:kind bad)))
    (is (= 3 (:depth bad)))
    (is (= [:execution/start :machine/message :machine/message]
           (mapv :event (:events bad))))
    (is (= 2 (:candidate-count bad)))
    (is (= r (explore opts)))
    (is (= r (explorer/recheck-exploration
              (adapter/initial-state) event-choices opts r)))
    (is (= :exploration-report-mismatch
           (kind #(explorer/recheck-exploration
                   (adapter/initial-state) event-choices opts
                   (assoc r :counterexamples [])))))))

(deftest invalid-adapter-events-are-distinct-from-checker-failures
  (let [r (explorer/explore (adapter/initial-state)
                            (constantly [{:event :future/unknown}])
                            {:max-depth 1})]
    (is (= 0 (:accepted-edge-count r)))
    (is (= 1 (:attempted-edge-count r)))
    (is (= :unknown-event (get-in r [:rejected-edges 0 :error/kind])))
    (is (empty? (:counterexamples r)))
    (is (= :forged-adapter-error
           (kind #(explore {:transition-predicate
                            (fn [_] (throw (ex-info "fake adapter error"
                                                    {:error/type :gesso.live.browser.adapter/error
                                                     :error/kind :forged-adapter-error})))}))))
    (is (= :generator-error
           (kind #(explorer/explore (adapter/initial-state)
                                    (fn [_] (throw (ex-info "generator broken"
                                                           {:error/kind :generator-error})))
                                    {}))))))

(deftest exhaustion-bounds-and-malformed-generators-fail-closed
  (let [root (adapter/initial-state)]
    (doseq [[wanted f] [[:duplicate-event-choices
                         #(explorer/explore root (constantly [(start-event) (start-event)]) {})]
                        [:nonvector-event-choices
                         #(explorer/explore root (constantly (list (start-event))) {})]
                        [:choice-fanout-limit-exceeded #(explore {:max-choices 1})]
                        [:node-limit-exceeded #(explore {:max-nodes 2})]
                        [:edge-limit-exceeded #(explore {:max-edges 1})]
                        [:invalid-exploration-bound #(explore {:max-depth -1})]
                        [:invalid-exploration-bound #(explore {:max-nodes 0})]
                        [:invalid-exploration-bound #(explore {:max-edges -1})]
                        [:unknown-option-keys #(explore {:certified? true})]
                        [:invalid-transition-predicate #(explore {:transition-predicate true})]
                        [:nonboolean-transition-predicate #(explore {:transition-predicate (constantly :yes)})]
                        [:invalid-event-generator #(explorer/explore root [] {})]]]
      (is (= wanted (kind f)) (str "expected rejection " wanted)))))

(deftest report-recheck-rejects-all-mutations-without-granting-authority
  (let [root (adapter/initial-state)
        base (explore)]
    (is (= base (explorer/recheck-exploration root event-choices base)))
    (doseq [[id forged] (targeted-mutations base)]
      (is (= :exploration-report-mismatch
             (kind #(explorer/recheck-exploration root event-choices {} forged)))
          (str "must reject mutation " id)))
    (is (= :exploration-report-mismatch
           (kind #(explorer/recheck-exploration
                   root event-choices {:max-depth 1} base))))
    (is (= :none (:proof-authority base)))
    (is (false? (:global-trace-admitted? base)))))

(deftest stable-corpus-covers-all-adversarial-boundaries
  (let [cases (portable-corpus)
        by-id (into {} cases)]
    (is (= expected-case-ids (mapv first cases)))
    (is (= (count cases) (count (distinct (map first cases)))))
    (is (= cases (portable-corpus)))
    (is (= :bounded-no-counterexamples
           (get-in by-id [:full-bound 0])))
    (is (= :counterexamples-found
           (get-in by-id [:counterexample 0])))
    (is (= [:rejected :duplicate-event-choices]
           (get by-id :duplicate-choices)))
    (is (= [:rejected :nonvector-event-choices]
           (get by-id :nonvector-choices)))
    (is (= [:rejected :node-limit-exceeded]
           (get by-id :node-budget)))
    (is (= [:rejected :edge-limit-exceeded]
           (get by-id :edge-budget)))
    (is (= [true true [:rejected :exploration-report-mismatch]]
           (get by-id :physical-delivery-identity)))
    (is (= [:rejected :forged-adapter-error]
           (get by-id :forged-adapter-rejection)))
    (is (= [:rejected :generator-error]
           (get by-id :generator-exception)))
    (is (every? #(= [:rejected :exploration-report-mismatch] (second %))
                (get by-id :replay-mutants)))
    (is (= (count (targeted-mutations (explore)))
           (count (get by-id :replay-mutants))))))

(defn -main [& _]
  (println corpus-prefix (pr-str (portable-corpus)))
  #?(:clj (clojure.test/run-tests
           'gesso.live.browser.refinement-exploration-parity-test)
     :cljs (t/run-tests
            'gesso.live.browser.refinement-exploration-parity-test)))

#?(:cljs (set! *main-cli-fn* -main))

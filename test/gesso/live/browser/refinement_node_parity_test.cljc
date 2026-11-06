(ns gesso.live.browser.refinement-node-parity-test
  "Cross-host semantic corpus for the actual browser observation, projection,
   and independent Choreo witness checker. Every case executes the same CLJC
   functions on JVM and Node/CLJS; the test harness compares the *whole* stable
   result vector, including typed rejections and unresolved obligations.

   This is a host-relative pure-semantic comparison, not evidence of actual
   Chromium behavior, network delivery, XTDB commits, or global proof."
  (:require [gesso.choreo.core :as choreo]
            [gesso.choreo.machine :as machine]
            [gesso.choreo.project :as project]
            [gesso.live.browser.adapter :as adapter]
            [gesso.live.browser.observation :as observation]
            [gesso.live.browser.refinement :as refinement]
            [gesso.live.browser.refinement-correspondence :as correspondence]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :as t :refer-macros [deftest is testing]])))

(def corpus-prefix "GESSO_BROWSER_REFINEMENT_PARITY ")
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

(defn- witnessed-steps []
  [{:op :send :role :server :to :browser :event :notice/one
    :via :http :payload {} :as :one}
   {:op :deliver :message :one}
   {:op :send :role :server :to :browser :event :notice/two
    :via :http :payload {} :as :two}
   {:op :deliver :message :two}])

(defn- inputs []
  (let [ch (scenario)
        initial (adapter/initial-state)
        start {:event :execution/start
               :execution-id :case
               :execution (machine/start (project/project ch :browser))}
        began (:state (refinement/step-projected initial start))
        generation (adapter/execution-generation began :case)]
    {:initial initial
     :events [start
              {:event :machine/message :execution-id :case
               :generation generation :message-id :physical-one
               :envelope (machine/message :server :browser :notice/one
                                          {} {:via :http})}
              {:event :live/invalidated :fragment-id :board}
              {:event :machine/message :execution-id :case
               :generation generation :message-id :physical-two
               :envelope (machine/message :server :browser :notice/two
                                          {} {:via :http})}]}))

(defn- check! [events ch schedule alignment]
  (correspondence/check-correspondence
   (adapter/initial-state) events ch schedule alignment))

(defn- sorted-tags [tags]
  (mapv identity (sort-by str tags)))

(defn- rejected [thunk]
  (try
    (thunk)
    [:unexpected-acceptance]
    (catch #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core.ExceptionInfo) e
      [:rejected (-> e ex-data :error/kind)])))

(defn- report-signature [report]
  [:matched (:status report)
   (:browser-event-count report)
   (:browser-candidate-count report)
   (:alignment report)
   (mapv #(get-in % [:distributed-observation :event]) (:matches report))
   (mapv :realization-index (:matches report))
   (sorted-tags (:browser-missing-witnesses report))
   (sorted-tags (:missing-physical-witnesses report))
   (:global-trace-admitted? report)
   (:browser-host-refinement-established? report)
   (:proof-authority report)])

(defn- trace-signature [trace]
  [:trace
   (mapv #(get-in % [:refinement :normalized-event]) (:transitions trace))
   (mapv #(get-in % [:distributed-observation :event])
         (:distributed-candidates trace))
   (mapv #(get-in % [:observation :classification]) (:transitions trace))
   (sorted-tags (:missing-witnesses trace))
   (:global-trace-admitted? trace)
   (:proof-authority trace)])

(defn portable-corpus
  "Deterministic vectors only: print representation is stable across CLJ/CLJS.
   The body deliberately calls public production functions for every case."
  []
  (let [{:keys [initial events]} (inputs)
        ch (scenario)
        schedule (witnessed-steps)
        full (check! events ch schedule [0 1])
        duplicate (assoc-in events [3 :message-id] :physical-one)
        unrelated (choreo/->choreography
                   {:initial :one
                    :states {:one (choreo/communicate :server :browser
                                                        :notice/one :two {:via :http})
                             :two (choreo/communicate :server :browser
                                                        :notice/foreign :work {:via :http})
                             :work (choreo/local :browser :browser/work :done)
                             :done (choreo/return :done)}})
        unrelated-schedule (assoc-in schedule [2 :event] :notice/foreign)
        same-semantic-different-physical
        (assoc-in events [1 :message-id] :another-physical-one)
        pure-trace (refinement/trace-projected initial events)
        missing-alignment (rejected #(check! events ch schedule [0]))
        reused-alignment (rejected #(check! events ch schedule [0 0]))
        source-substitution
        (rejected #(check! events unrelated unrelated-schedule [0 1]))
        false-schedule (rejected #(check! events ch
                                          [{:op :deliver :message :not-sent}]
                                          [0 1]))
        forged-report (rejected
                       #(correspondence/recheck-correspondence
                         initial events ch schedule [0 1]
                         (assoc full :proof-authority :certified)))
        changed-payload (assoc-in events [1 :envelope :payload] {:forged? true})
        changed-report (check! changed-payload ch schedule [])
        silent-events [{:event :live/invalidated :fragment-id :board}]
        silent (check! silent-events ch [] [])
        advance (machine/receive
                 (machine/start (project/project ch :browser))
                 (machine/message :server :browser :notice/one {} {:via :http}))
        imported (refinement/step-projected
                  initial {:event :execution/start :execution-id :imported
                           :execution advance})]
    [[:checked-two-receives (report-signature full)]
     [:actual-browser-trace (trace-signature pure-trace)]
     [:duplicate-physical-id
      (report-signature (check! duplicate ch schedule [0]))]
     [:incomplete-alignment missing-alignment]
     [:reused-alignment reused-alignment]
     [:reordered-alignment (rejected #(check! events ch schedule [1 0]))]
     [:substituted-choreo-source source-substitution]
     [:invalid-independent-schedule false-schedule]
     [:forged-correspondence-authority forged-report]
     [:wrong-receive-payload (report-signature changed-report)]
     [:stuttering-only (report-signature silent)]
     [:physical-id-is-not-semantic-identity
      (= full (check! same-semantic-different-physical ch schedule [0 1]))]
     [:imported-preexisting-history
      [(count (get-in imported [:refinement :distributed-candidates]))
       (sorted-tags (get-in imported [:refinement :missing-witnesses]))]]
     [:observer-rejects-substituted-state
      (let [event (first events)
            {:keys [effects]} (observation/step-observed initial event)]
        (rejected #(observation/observe-transition
                    initial event initial effects)))]]))

(def ^:private expected-case-ids
  [:checked-two-receives
   :actual-browser-trace
   :duplicate-physical-id
   :incomplete-alignment
   :reused-alignment
   :reordered-alignment
   :substituted-choreo-source
   :invalid-independent-schedule
   :forged-correspondence-authority
   :wrong-receive-payload
   :stuttering-only
   :physical-id-is-not-semantic-identity
   :imported-preexisting-history
   :observer-rejects-substituted-state])

(deftest node-browser-refinement-corpus-is-complete-and-deterministic
  (let [cases (portable-corpus)
        by-id (into {} cases)]
    (is (= expected-case-ids (mapv first cases)))
    (is (= (count cases) (count (distinct (map first cases)))))
    (is (= cases (portable-corpus)))
    (is (= :conditionally-matched-model-witness
           (get-in by-id [:checked-two-receives 1])))
    (is (= 2 (get-in by-id [:checked-two-receives 3])))
    (is (= [:notice/one :notice/two]
           (get-in by-id [:checked-two-receives 5])))
    (is (= [:notice/one :notice/two]
           (get-in by-id [:actual-browser-trace 2])))
    (is (= [:incomplete-candidate-alignment]
           (subvec (get by-id :incomplete-alignment) 1)))
    (is (= [:rejected :invalid-or-reordered-alignment]
           (get by-id :reused-alignment)))
    (is (= [:rejected :candidate-witness-mismatch]
           (get by-id :substituted-choreo-source)))
    (is (= [:rejected :invalid-independent-witness]
           (get by-id :invalid-independent-schedule)))
    (is (= [:rejected :correspondence-report-mismatch]
           (get by-id :forged-correspondence-authority)))
    (is (= true (get by-id :physical-id-is-not-semantic-identity)))
    (is (= 0 (get-in by-id [:imported-preexisting-history 0])))
    (is (= [:rejected :transition-mismatch]
           (get by-id :observer-rejects-substituted-state)))
    (is (= :none
           (last (get by-id :checked-two-receives))))))

(deftest conditional-reports-keep-physical-obligations-on-both-hosts
  (let [by-id (into {} (portable-corpus))
        full (get by-id :checked-two-receives)
        physical (nth full 8)]
    (is (= :matched (first full)))
    (is (some #{:actual-transport-send-and-delivery-identity} physical))
    (is (some #{:all-schedules-trace-refinement-not-established} physical))
    (is (= false (nth full 9)))
    (is (= false (nth full 10)))
    (is (= :none (nth full 11)))))

(defn -main [& _]
  ;; In the standalone Node test invocation the corpus is additionally compared
  ;; byte-for-byte with the JVM corpus by the caller. The ordinary Gesso Node
  ;; browser gate will include this namespace when registered by the harness.
  (println corpus-prefix (pr-str (portable-corpus)))
  #?(:clj (clojure.test/run-tests 'gesso.live.browser.refinement-node-parity-test)
     :cljs (t/run-tests 'gesso.live.browser.refinement-node-parity-test)))

#?(:cljs (set! *main-cli-fn* -main))

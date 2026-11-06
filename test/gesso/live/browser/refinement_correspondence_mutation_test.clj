(ns gesso.live.browser.refinement-correspondence-mutation-test
  "Adversarial browser/Choreo correspondence regressions. A checked model
   schedule is an independent *shape/order* witness, never a physical delivery
   attestation, global trace certificate, or source-instance credential."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.choreo.core :as choreo]
            [gesso.choreo.machine :as machine]
            [gesso.choreo.project :as project]
            [gesso.live.browser.adapter :as adapter]
            [gesso.live.browser.refinement-correspondence :as correspondence]))

(defn- error-kind [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (:error/kind (ex-data e)))))

(defn- choreography
  ([] (choreography :audit/close))
  ([close-event]
   (choreo/->choreography
    {:initial :open
     :states {:open (choreo/communicate :server :browser :audit/open
                                         :close {:via :http})
              :close (choreo/communicate :server :browser close-event
                                          :work {:via :http})
              :work (choreo/local :browser :audit/render :finished)
              :finished (choreo/return :complete)}})))

(defn- witness
  ([] (witness :audit/close))
  ([close-event]
   [{:op :send :role :server :to :browser :event :audit/open
     :via :http :payload {} :as :open}
    {:op :deliver :message :open}
    {:op :send :role :server :to :browser :event close-event
     :via :http :payload {} :as :close}
    {:op :deliver :message :close}]))

(defn- incoming [generation message-id event]
  {:event :machine/message
   :execution-id :case
   :generation generation
   :message-id message-id
   :envelope (machine/message :server :browser event {} {:via :http})})

(defn- browser-inputs
  ([] (browser-inputs :audit/close))
  ([close-event]
   (let [ch (choreography close-event)
         start {:event :execution/start :execution-id :case
                :execution (machine/start (project/project ch :browser))}
         started (first (adapter/step (adapter/initial-state) start))
         generation (adapter/execution-generation started :case)]
     {:initial (adapter/initial-state)
      :events [start
               (incoming generation :transport-open :audit/open)
               {:event :live/invalidated :fragment-id :board}
               (incoming generation :transport-close close-event)]})))

(defn- check!
  ([initial events ch schedule indices]
   (correspondence/check-correspondence initial events ch schedule indices))
  ([initial events indices]
   (check! initial events (choreography) (witness) indices)))

(defn- baseline []
  (let [{:keys [initial events]} (browser-inputs)]
    (check! initial events [0 1])))

(deftest canonical-receive-pair-has-no-global-or-physical-proof-authority
  (let [report (baseline)]
    (is (= 2 (:browser-candidate-count report)))
    (is (= 4 (:browser-event-count report)))
    (is (= [0 1] (:alignment report)))
    (is (= [:audit/open :audit/close]
           (mapv #(get-in % [:distributed-observation :event]) (:matches report))))
    (is (= [0 1] (mapv :realization-index (:matches report))))
    (is (every? #(= :model-witness-shape-and-order-only (:correspondence %))
                (:matches report)))
    (is (= :conditionally-matched-model-witness (:status report)))
    (is (= :none (:proof-authority report)))
    (is (false? (:global-trace-admitted? report)))
    (is (false? (:browser-host-refinement-established? report)))
    (is (contains? (:missing-physical-witnesses report)
                   :actual-transport-send-and-delivery-identity))
    (is (contains? (:missing-physical-witnesses report)
                   :browser-projection-to-global-execution-identity))
    (is (contains? (:missing-physical-witnesses report)
                   :authoritative-operation-and-xtdb-commit-correspondence))
    (is (contains? (:missing-physical-witnesses report)
                   :all-schedules-trace-refinement-not-established))))

(deftest reused-and-reordered-indices-cannot-represent-two-deliveries
  (let [{:keys [initial events]} (browser-inputs)]
    (doseq [indices [[0 0] [1 0] [1 1] [0 2] [0 -1] [-1 1]
                     [0 nil] [nil 1] [0 1.0] [0 :later]]]
      (is (= :invalid-or-reordered-alignment
             (error-kind #(check! initial events indices)))
          (str "Bad alignment " (pr-str indices))))
    (doseq [indices [[] [0] [0 1 2] nil '(0 1)]]
      (is (= :incomplete-candidate-alignment
             (error-kind #(check! initial events indices)))
          (str "Incomplete alignment " (pr-str indices))))))

(deftest duplicates-do-not-invent-independent-communications
  (let [{:keys [initial events]} (browser-inputs)
        duplicate-id (assoc-in events [3 :message-id] :transport-open)
        duplicate-event (assoc-in events [3 :envelope :event] :audit/open)]
    ;; Neither a repeated physical identifier nor an out-of-order identical
    ;; semantic message is a second successfully executed receive occurrence.
    (doseq [changed [duplicate-id duplicate-event]]
      (let [partial (check! initial changed [0])]
        (is (= 1 (:browser-candidate-count partial)))
        (is (= :audit/open
               (get-in partial [:matches 0 :distributed-observation :event])))
        (is (= :incomplete-candidate-alignment
               (error-kind #(check! initial changed [0 1]))))
        (is (false? (:global-trace-admitted? partial)))))))

(deftest duplicate-abstract-events-still-need-distinct-witness-occurrences
  (let [ch (choreography :audit/open)
        schedule (witness :audit/open)
        {:keys [initial events]} (browser-inputs :audit/open)
        report (check! initial events ch schedule [0 1])]
    (is (= 2 (:browser-candidate-count report)))
    (is (= [:audit/open :audit/open]
           (mapv #(get-in % [:distributed-observation :event]) (:matches report))))
    (is (= [0 1] (mapv :realization-index (:matches report))))
    (is (= :invalid-or-reordered-alignment
           (error-kind #(check! initial events ch schedule [0 0]))))
    (is (false? (:global-trace-admitted? report)))))

(deftest reordered-browser-receives-cannot-claim-ordered-global-alignment
  (let [{:keys [initial events]} (browser-inputs)
        reordered (-> events
                      (assoc 1 (nth events 3))
                      (assoc 3 (nth events 1)))
        partial (check! initial reordered [0])]
    (is (= 1 (:browser-candidate-count partial)))
    (is (= [:audit/open]
           (mapv #(get-in % [:distributed-observation :event]) (:matches partial))))
    (is (= :incomplete-candidate-alignment
           (error-kind #(check! initial reordered [0 1]))))
    (is (false? (:global-trace-admitted? partial)))))

(deftest substituted-global-sources-and-invalid-schedules-reject
  (let [{:keys [initial events]} (browser-inputs)]
    (is (= :candidate-witness-mismatch
           (error-kind #(check! initial events
                                (choreography :audit/foreign)
                                (witness :audit/foreign) [0 1]))))
    (is (= :invalid-independent-witness
           (error-kind #(check! initial events (choreography)
                                (assoc-in (witness) [2 :event] :audit/foreign)
                                [0 1]))))
    (is (= :invalid-independent-witness
           (error-kind #(check! initial events (choreography)
                                (vec (reverse (witness))) [0 1]))))
    (is (= :invalid-independent-witness
           (error-kind #(check! initial events (choreography)
                                [{:op :deliver :message :not-emitted}] [0 1]))))))

(deftest visible-payload-substitution-does-not-inherit-a-model-witness
  (let [{:keys [initial events]} (browser-inputs)
        changed (assoc-in events [1 :envelope :payload] {:forged? true})
        partial (check! initial changed [])]
    ;; This unauthorized receive is rejected before machine history advances;
    ;; its successor is not reached either. No candidate can be fabricated.
    (is (= 0 (:browser-candidate-count partial)))
    (is (= [] (:matches partial)))
    (is (false? (:global-trace-admitted? partial)))
    (doseq [indices [[0] [0 1]]]
      (is (= :incomplete-candidate-alignment
             (error-kind #(check! initial changed indices)))))))

(deftest missing-candidates-do-not-inherit-full-observation-coverage
  (let [{:keys [initial events]} (browser-inputs)
        prefix (subvec events 0 2)
        report (check! initial prefix [0])
        silent (check! (adapter/initial-state)
                       [{:event :live/invalidated :fragment-id :board}]
                       (choreography) [] [])]
    (is (= 1 (:browser-candidate-count report)))
    (is (= [0] (:alignment report)))
    (is (false? (:global-trace-admitted? report)))
    (is (contains? (:unverified-premises report)
                   :all-schedules-trace-refinement-not-established))
    (is (empty? (:matches silent)))
    (is (= 0 (:browser-candidate-count silent)))
    (is (false? (:global-trace-admitted? silent)))
    (is (= :none (:proof-authority silent)))))

(deftest copied-reports-are-inspectable-not-upgradeable-credentials
  (let [{:keys [initial events]} (browser-inputs)
        result (check! initial events [0 1])
        source (pr-str result)
        copied (read-string source)]
    (is (= result copied))
    (is (= result (correspondence/recheck-correspondence
                   initial events (choreography) (witness) [0 1] copied)))
    (doseq [forged [(assoc result :proof-authority :certified)
                   (assoc result :global-trace-admitted? true)
                   (assoc result :browser-host-refinement-established? true)
                   (assoc result :status :globally-proved)
                   (assoc result :unverified-premises #{})
                   (assoc result :missing-physical-witnesses #{})
                   (assoc result :correspondence-property :universal-refinement)
                   (assoc-in result [:matches 0 :correspondence] :physical-identity)
                   (assoc result :certificate {:valid? true})]]
      (is (= :correspondence-report-mismatch
             (error-kind #(correspondence/recheck-correspondence
                           initial events (choreography) (witness) [0 1] forged)))
          (str "Forged report " (pr-str forged))))))

(deftest changed-browser-instance-and-report-order-invalidate-copied-evidence
  (let [{:keys [initial events]} (browser-inputs)
        report (baseline)
        other-id (mapv #(if (= :case (:execution-id %))
                          (assoc % :execution-id :other)
                          %)
                       events)
        swapped-matches (update report :matches (comp vec reverse))]
    ;; Moving only event IDs without changing the imported machine is not a
    ;; valid browser instance. Rechecking must not adopt the old report.
    (is (= :correspondence-report-mismatch
           (error-kind #(correspondence/recheck-correspondence
                         initial other-id (choreography) (witness) [0 1] report))))
    (is (= :correspondence-report-mismatch
           (error-kind #(correspondence/recheck-correspondence
                         initial events (choreography) (witness) [0 1]
                         swapped-matches))))))

(deftest physical-message-id-alone-is-not-a-source-identity-proof
  (let [{:keys [initial events]} (browser-inputs)
        changed (assoc-in events [1 :message-id] :different-physical-id)
        before (check! initial events [0 1])
        after (check! initial changed [0 1])]
    ;; The model only observes the semantic receive. Physical ID substitutions
    ;; may therefore leave the diagnostic model report equal; this is a named
    ;; missing witness, not a correspondence guarantee.
    (is (= before after))
    (is (contains? (:missing-physical-witnesses after)
                   :actual-transport-send-and-delivery-identity))
    (is (= :none (:proof-authority after)))
    (is (false? (:browser-host-refinement-established? after)))))

(ns gesso.assurance.example.physical-single-node-live-publication-test
  "v2325: Actual Biff-adopted XTDB/Live HTTP command outcomes must remain
   truthful when advisory postcommit notification fails. These tests do NOT
   certify authenticated commit evidence, durable publication, or a browser."
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.example.physical-single-node-live :as subject]
            [gesso.live.core :as live]))

(def ^:private reserve "/__gesso_proving/slot/reserve")
(def ^:private release "/__gesso_proving/slot/release")
(def ^:private publication-header "x-gesso-proving-live-publication")

(defn- request [uri]
  {:request-method :post :uri uri})

(defn- rejected-reason [thunk]
  (try (thunk)
       :not-rejected
       (catch clojure.lang.ExceptionInfo e
         (:reason (ex-data e)))))

(deftest postcommit-publication-exception-preserves-committed-http-outcome
  (subject/with-live-proving-application!
   (fn [{:keys [system handler read! delegate-handler]}]
     (is (identical? handler (:biff.ring/handler system))
         "Use the handler installed at original Biff startup")
     (is (not (identical? handler delegate-handler)))
     (let [emissions (atom 0)
           result (with-redefs [live/emit! (fn [& _]
                                            (swap! emissions inc)
                                            (throw (ex-info "SSE source unavailable"
                                                            {:transient true})))]
                    (handler (request reserve)))]
       (is (= 200 (:status result))
           "A committed reservation must not look like a failed write")
       (is (= "unknown" (get-in result [:headers publication-header]))
           "Cannot claim a client received a failed/uncertain notification")
       (is (= "true" (get-in result [:headers "HX-Refresh"]))
           "Request a full HTMX reread without resubmitting the write")
       (is (= 1 @emissions) "Do not retry a postcommit publication")
       (is (= 1 (:revision (read!)))
           "An advisory failure must never replay the actual transaction")
       (is (= "reserved" (:status (read!)))))
     (is (= :operation-precondition-not-met
            (rejected-reason #(handler (request reserve))))
         "A failed Live publish must not roll back the committed operation")
     (is (= 1 (:revision (read!)))))))

(deftest missing-or-invalid-publication-result-does-not-falsify-commit
  (subject/with-live-proving-application!
   (fn [{:keys [handler read!]}]
     (let [attempts (atom 0)
           result (with-redefs [live/emit! (fn [& _]
                                            (swap! attempts inc)
                                            {:status :unsupported})]
                    (handler (request reserve)))]
       (is (= 200 (:status result)))
       (is (= "unknown" (get-in result [:headers publication-header])))
       (is (= "true" (get-in result [:headers "HX-Refresh"])))
       (is (= 1 @attempts))
       (is (= 1 (:revision (read!))))))))

(deftest accepted-and-queued-invalidation-do-not-claim-client-delivery
  (subject/with-live-proving-application!
   (fn [{:keys [handler read!]}]
     (doseq [[operation returned-status revision]
             [[reserve :emitted 1] [release :queued 2]]]
       (let [calls (atom [])
             result (with-redefs [live/emit!
                                  (fn [_ invalidation]
                                    (swap! calls conj invalidation)
                                    {:status returned-status})]
                      (handler (request operation)))]
         (is (= 200 (:status result)))
         (is (= "source-accepted"
                (get-in result [:headers publication-header])))
         (is (nil? (get-in result [:headers "HX-Refresh"]))
             "A successful source acceptance should not force a reload")
         (is (= 1 (count @calls)))
         (is (= :updated (:change/kind (first @calls))))
         (is (= :gesso-proving-reservation (:topic (first @calls))))
         (is (= revision (:revision (read!)))))))))

(deftest rejected-physical-command-does-not-attempt-live-publication
  (subject/with-live-proving-application!
   (fn [{:keys [handler read!]}]
     (let [calls (atom 0)]
       (with-redefs [live/emit! (fn [& _]
                                  (swap! calls inc)
                                  (throw (ex-info "Should not publish" {})))]
         (is (= :operation-precondition-not-met
                (rejected-reason #(handler (request release))))))
       (is (zero? @calls))
       (is (zero? (:revision (read!))))))))

(deftest real-live-emitter-accepts-a-postcommit-advisory
  ;; Positive control: exercise the actual source without a mocked emitter.
  ;; Emission acceptance is NOT evidence that a browser has received it.
  (subject/with-live-proving-application!
   (fn [{:keys [system handler read! live-report]}]
     (is (identical? handler (:biff.ring/handler system)))
     (is (= :htmx-2.0.7-full-page-refresh-requested
            (:uncertain-notification-recovery live-report)))
     (let [result (handler (request reserve))]
       (is (= 200 (:status result)))
       (is (= "source-accepted"
              (get-in result [:headers publication-header])))
       (is (nil? (get-in result [:headers "HX-Refresh"])))
       (is (= 1 (:revision (read!))))))))

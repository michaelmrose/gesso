(ns gesso.assurance.example.physical-single-node-restart-test
  "v2348: The installed reservation handler across three actual JVMs.
   Hard halt after a protected write and source acceptance, before ack.
   Reopen with the managed worker; verify a third JVM sees terminal state.
   This is not a power-loss or exactly-once delivery guarantee."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.example.physical-single-node-live :as fixture]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.example.physical-single-node-session :as session]
            [gesso.live.outbox.acknowledgment :as ack]
            [gesso.live.outbox.cycle :as cycle]
            [manifold.stream :as stream]
            [xtdb.api :as xt])
  (:import [java.net HttpURLConnection URL]
           [java.nio.file Files Path]
           [java.util.concurrent TimeUnit]))

(def ^:private slot-id "reservation-hard-restart")
(def ^:private policy
  {:catalog-id :slot/reservation-lifecycle
   :policy-id :proving/hard-restart
   :grants {:actor/writer #{:slot/reserve :slot/release}}
   :read-principals #{:actor/writer}})
(def ^:private reserve "/__gesso_proving/slot/reserve")
(def ^:private halt-code 73)

(defn- require! [ok reason]
  (when-not ok (throw (ex-info "Reservation restart check failed" {:reason reason}))))

(defn- headers [credential]
  {"cookie" (:cookie credential)
   "x-gesso-proving-csrf" (:csrf-token credential)})

(defn- reserve! [base credential]
  (let [^HttpURLConnection c (.openConnection (URL. (str base reserve)))]
    (try
      (.setRequestMethod c "POST")
      (.setConnectTimeout c 3000)
      (.setReadTimeout c 3000)
      (doseq [[k v] (headers credential)] (.setRequestProperty c k v))
      (require! (= 200 (.getResponseCode c)) :protected-http-write)
      (with-open [in (.getInputStream c)] (slurp in))
      (require! (= "intent-recorded" (.getHeaderField c "x-gesso-proving-live-publication"))
                :publication-boundary)
      (finally (.disconnect c)))))

(defn- read-intent! [node]
  ;; Retry only XTDB's typed schema replan conflict, as in host-restart-child.
  (loop [remaining 8]
    (let [r (try {:rows (xt/q node ["SELECT * FROM gesso_live_outbox"])}
                 (catch Throwable e {:failure e}))]
      (if-let [e (:failure r)]
        (if (and (pos? remaining) (= :prepared-query-out-of-date (:xtdb.error/code (ex-data e))))
          (do (Thread/sleep 30) (recur (dec remaining)))
          (throw e))
        (let [[row :as rows] (:rows r)]
          (require! (= 1 (count rows)) :expected-one-intent)
          (require! (and (= physical/outbox-source-id (:gesso.live.outbox/source-id row))
                         (= slot-id (:gesso.live.outbox/subject-id row))
                         (= :slot/reserve (:gesso.live.outbox/operation row)))
                    :unexpected-intent-identity)
          row)))))

(defn- check-domain! [app]
  (let [row ((:read! app))]
    (require! (and (= "reserved" (:status row)) (= 1 (:revision row)))
              :domain-must-not-be-replayed)))

(defn- open-stream! [app credential]
  (let [r ((:handler app) {:request-method :get :uri fixture/events-uri
                           :headers (headers credential)})]
    (require! (= 200 (:status r)) :protected-sse-admission)
    (:body r)))

(defn- witness! [body]
  (let [frame @(stream/try-take! body ::closed 5000 ::timeout)]
    (require! (and (string? frame) (str/includes? frame "event: live-update"))
              :actual-live-frame)))

(defn- report! [output phase fields]
  (spit output (pr-str (assoc fields :phase phase
                                    :pid (.pid (java.lang.ProcessHandle/current))))))

(defn- crash! [root output]
  (session/with-session-policy!
   policy
   (fn [{:keys [source issue!]}]
     (fixture/with-durable-granted-live-proving-application!
      slot-id source :biff-jetty-http1 {:mode :create :root root}
      (fn [{:keys [node loopback-base-url process-outbox!] :as app}]
        (let [credential (issue! :actor/writer)
              body (open-stream! app credential)]
          (try
            (reserve! loopback-base-url credential)
            (check-domain! app)
            (require! (= :pending (:gesso.live.outbox/status (read-intent! node))) :pending-before-dispatch)
            (with-redefs [ack/acknowledge-dispatch!
                          (fn [n _receipt]
                            ;; The real synchronous source has accepted the
                            ;; invalidation; no acknowledgment CAS has run.
                            (witness! body)
                            (let [row (read-intent! n)
                                  lease (:gesso.live.outbox/lease-id row)]
                              (require! (and (= :leased (:gesso.live.outbox/status row))
                                             (uuid? lease)
                                             (nil? (:gesso.live.outbox/dispatch-receipt-id row)))
                                        :pre-ack-physical-state)
                              (report! output :crash {:intent-id (:xt/id row) :lease-id lease
                                                     :status :leased :revision 1 :frame? true})
                              ;; Nonzero distinct code; no finally or shutdown hooks.
                              (.halt (Runtime/getRuntime) halt-code)))]
              (process-outbox! {:worker-id "reservation-before-halt" :lease-millis 3000})
              (throw (ex-info "Expected halt was not reached" {})))
            (finally (stream/close! body)))))))))

(defn- eventually [pred]
  (loop [n 0]
    (cond (pred) true (>= n 1000) false
          :else (do (Thread/sleep 20) (recur (inc n))))))

(defn- recover! [root output]
  (let [before (edn/read-string (slurp (str root "/crash.edn")))
        ready (promise)
        real-cycle @#'cycle/process-cycle!]
    (with-redefs [cycle/process-cycle!
                  (fn [& args]
                    ;; Only the first cycle's start is gated; all discovery,
                    ;; acquisition, actual delivery and acknowledgment are real.
                    (require! (= true (deref ready 15000 false)) :recovery-subscription-not-ready)
                    (apply real-cycle args))]
      (session/with-session-policy!
       policy
       (fn [{:keys [source issue!]}]
         (fixture/with-managed-durable-granted-live-proving-application!
          slot-id source :biff-jetty-http1 {:mode :reopen :root root}
          {:worker-id "reservation-after-halt" :poll-ms 20}
          (fn [{:keys [node outbox-worker-status] :as app}]
            (try
              (check-domain! app)
              (let [row (read-intent! node)]
                (require! (and (= (:intent-id before) (:xt/id row))
                               (= :leased (:gesso.live.outbox/status row))
                               (= (:lease-id before) (:gesso.live.outbox/lease-id row))
                               (nil? (:gesso.live.outbox/dispatch-receipt-id row)))
                          :reopen-before-reclaim))
              (let [body (open-stream! app (issue! :actor/writer))]
                (try
                  (deliver ready true)
                  (witness! body)
                  (require! (eventually #(= 1 (:total-dispatch-accepted (outbox-worker-status))))
                            :managed-worker-did-not-acknowledge)
                  (check-domain! app)
                  (let [row (read-intent! node)
                        lease (:gesso.live.outbox/lease-id row)
                        receipt (:gesso.live.outbox/dispatch-receipt-id row)]
                    (require! (and (= (:intent-id before) (:xt/id row))
                                   (= :dispatch-accepted (:gesso.live.outbox/status row))
                                   (uuid? lease) (not= (:lease-id before) lease)
                                   (= "reservation-after-halt" (:gesso.live.outbox/lease-owner row))
                                   (uuid? receipt)) :reclaimed-intent)
                    (report! output :recover {:intent-id (:xt/id row) :lease-id lease
                                              :receipt-id receipt :status :dispatch-accepted
                                              :revision 1 :frame? true}))
                  (finally (stream/close! body))))
              (finally (deliver ready true))))))))))

(defn- verify! [root output]
  (session/with-session-policy!
   policy
   (fn [{:keys [source]}]
     (fixture/with-durable-granted-live-proving-application!
      slot-id source :biff-jetty-http1 {:mode :reopen :root root}
      (fn [{:keys [node process-outbox!] :as app}]
        (check-domain! app)
        (let [row (read-intent! node)
              resends (:processed (process-outbox! {:worker-id "reservation-terminal-check"}))]
          (require! (and (= :dispatch-accepted (:gesso.live.outbox/status row))
                         (= 0 resends)) :terminal-no-replay)
          (report! output :verify {:intent-id (:xt/id row)
                                   :receipt-id (:gesso.live.outbox/dispatch-receipt-id row)
                                   :status :dispatch-accepted :revision 1 :resends resends})))))))

(defn -main [phase root output]
  (try
    ((case phase "crash" crash! "recover" recover! "verify" verify!)
     (.toPath (io/file root)) output)
    (shutdown-agents)
    (System/exit 0)
    (catch Throwable e
      (.printStackTrace e)
      (System/exit 1))))

(defn- child! [^Path root phase expected-exit]
  (let [output (.resolve root (str phase ".edn"))
        log (.resolve root (str phase ".log"))
        cmd [(str (System/getProperty "java.home") "/bin/java") "-Xmx1024m"
             "--add-opens=java.base/java.nio=ALL-UNNAMED" "--enable-native-access=ALL-UNNAMED"
             "-Dio.netty.tryReflectionSetAccessible=true" "-Dio.netty.noUnsafe=false"
             "-cp" (System/getProperty "java.class.path")
             "clojure.main" "-m" "gesso.assurance.example.physical-single-node-restart-test"
             phase (str root) (str output)]
        builder (doto (ProcessBuilder. (into-array String cmd))
                  (.redirectErrorStream true) (.redirectOutput (.toFile log)))
        child (.start builder)]
    (try
      (let [finished? (.waitFor child 100 TimeUnit/SECONDS)
            exit (if finished? (.exitValue child) :timed-out)]
        (when-not (= expected-exit exit)
          (let [contents (slurp (.toFile log))]
            (throw (ex-info "Reservation restart child failed"
                            {:phase phase :exit exit :expected expected-exit
                             :log (subs contents (max 0 (- (count contents) 6000)))}))))
        (edn/read-string (slurp (.toFile output))))
      (finally
        (when (.isAlive child)
          (.destroyForcibly child)
          (.waitFor child 5 TimeUnit/SECONDS))))))

(deftest protected-reservation-reclaims-after-hard-halt-across-three-jvms
  (let [root (Files/createTempDirectory "gesso-reservation-hard-restart-"
                                       (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (let [a (child! root "crash" halt-code)
            b (child! root "recover" 0)
            c (child! root "verify" 0)]
        (is (= [:crash :recover :verify] (mapv :phase [a b c])))
        (is (every? integer? (map :pid [a b c])))
        (is (= 3 (count (set (map :pid [a b c])))))
        (is (uuid? (:intent-id a)))
        (is (= (:intent-id a) (:intent-id b) (:intent-id c)))
        (is (= [:leased :dispatch-accepted :dispatch-accepted] (mapv :status [a b c])))
        (is (= [1 1 1] (mapv :revision [a b c])))
        (is (and (:frame? a) (:frame? b)))
        (is (every? uuid? (map :lease-id [a b])))
        (is (not= (:lease-id a) (:lease-id b)))
        (is (uuid? (:receipt-id b)))
        (is (= (:receipt-id b) (:receipt-id c)))
        (is (= 0 (:resends c))))
      (finally
        (doseq [entry (reverse (file-seq (.toFile root)))] (io/delete-file entry true))))))

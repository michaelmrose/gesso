(ns gesso.model.formal-check
  "Execute the finite prepared-transaction safety checks through Recife/TLC.

   This is an internal, JVM-only verification backend, not a second transaction
   interpreter. Each modeled action calls formal-transition/transition, which
   calls tx-model/step; independent safety obligations come from
   formal-obligation. The input MUST be an admitted prepared-subject. The
   caller, not this backend, is responsible for the completeness and
   relevance of its chosen prestate and plans.

   A successful result means only that TLC accepted all reachable states of
   THIS finite model under THESE assumptions. It is diagnostic data, NOT a
   durable certificate, executor identity, application approval, or proof of
   production XTDB/CLJS correspondence. Consumers MUST NOT grant stronger
   assurance merely because a returned map has :status :checked.

   Recife 0.22.0 does not faithfully transport arbitrary Clojure strings or
   underscores in keyword names, or nil document values. A reversible finite
   token codec is therefore applied only at the TLC boundary; the real
   transition and predicates always receive decoded Clojure values. Model
   checks are serialized because Recife controls process-wide model-checking
   machinery. Session data is removed on
   completion, failure, or interruption. Failed runs preserve a bounded,
   process-local diagnostic record reachable only through an opaque handle.
   Exception data exposes only a redacted summary, not model documents or
   raw TLC states. No application callbacks execute."
  (:require [clojure.walk :as walk]
            [gesso.model.formal-obligation :as obligation]
            [gesso.model.formal-transition :as formal]
            [recife.core :as recife]
            [recife.helpers :as helpers])
  (:import [java.util UUID]))

(def assurance-scope
  {:subject :gesso.model.formal-check/prepared-serial-tlc-v1
   :derived-from #{:gesso.model.formal-transition/prepared-serial-v1
                   :gesso.model.formal-obligation/prepared-serial-safety-v1}
   :checker :recife-0.22.0/tlc
   :checked #{:finite-model-safety-invariant-exploration}
   :assumed #{:complete-selected-finite-prestate-and-plans
              :serial-atomic-reference-semantics
              :faithful-reversible-tlc-token-encoding
              :correct-recife-tlc-execution}
   :not-established #{:complete-application-operation-enumeration
                      :generic-inductive-theorem
                      :actual-xtdb-implementation-refinement
                      :authorization-or-application-policy
                      :server-browser-composition
                      :independent-application-certificate}})

;; Recife reloads operator namespaces while compiling TLC overrides; ordinary
;; def would erase live sessions during model initialization.
(defonce ^:private sessions (atom {}))
(defonce ^:private checker-lock (Object.))

;; A finite local registry, not a release artifact, authentication credential,
;; or certificate. Handle possession grants access to potentially sensitive
;; raw subject/trace data; never expose inspect-failure over HTTP or in logs.
;; Older records are discarded so repeated failed checks cannot grow without
;; bound. A process restart discards all diagnostics.
(def ^:private max-failure-records 16)
(defonce ^:private failure-records (atom {:order [] :entries {}}))

(defn inspect-failure
  "Inspect the raw diagnostic for one opaque handle IN THIS JVM PROCESS.

   Returned data can contain the complete selected prestate, prepared plans,
   TLC traces, and backend messages. Treat it as confidential local diagnostic
   material: never serialize it to browser responses or ordinary logs.
   Unknown/expired handles return nil. A handle is not a certification token."
  [handle]
  (when (instance? UUID handle)
    (get-in @failure-records [:entries handle])))

(defn- remember-failure! [entry]
  (let [handle (UUID/randomUUID)]
    (swap! failure-records
           (fn [{:keys [order entries]}]
             (let [order (conj (vec order) handle)
                   excess (max 0 (- (count order) max-failure-records))
                   dropped (take excess order)]
               {:order (vec (drop excess order))
                :entries (-> (apply dissoc entries dropped)
                             (assoc handle entry))})))
    handle))

(defn- fail! [kind message]
  (throw (ex-info message {:error/type ::invalid-formal-check
                           :error/kind kind})))

(defn- values-matching [pred data]
  (let [found (volatile! #{})]
    (walk/postwalk (fn [x] (when (pred x) (vswap! found conj x)) x) data)
    (sort-by pr-str @found)))

(defn- lossy-keyword? [x]
  (and (keyword? x)
       (or (some-> (namespace x) (.contains "_"))
           (.contains (name x) "_"))))

(defn- codec-for [subject]
  (let [values [(:plans subject) (:initial-state subject)]
        all-keywords (set (values-matching keyword? values))
        ;; Encode nil unconditionally: tx-model/step may introduce nil
        ;; :failed-precondition in an observation even if no submitted plan or
        ;; initial document contained nil. Recife/TLA cannot reliably carry
        ;; unboxed Clojure nil inside nested map values or observations.
        originals (concat [nil]
                          (values-matching string? values)
                          (values-matching lossy-keyword? values))
        forward
        (loop [xs (seq originals) next-index 0 taken all-keywords mapping {}]
          (if (seq xs)
            (let [x (first xs)
                  candidate (keyword "gessoformalcodec" (str "v" next-index))]
              (if (contains? taken candidate)
                (recur xs (inc next-index) taken mapping)
                (recur (next xs) (inc next-index) (conj taken candidate)
                       (assoc mapping x candidate))))
            mapping))
        backward (into {} (map (fn [[original token]] [token original])) forward)]
    {:encode (fn [value]
               (walk/postwalk
                (fn [x]
                  (cond
                    (nil? x) (get forward nil)
                    (string? x) (or (get forward x)
                                    (fail! :unadmitted-string
                                           "A model transition produced an unadmitted string."))
                    (keyword? x) (get forward x x)
                    :else x))
                value))
     :decode (fn [value]
               (walk/postwalk (fn [x] (get backward x x)) value))}))

(defn- translate-state [state transform]
  (-> state
      (update ::formal/documents transform)
      (update ::formal/observations transform)))

(defn- session! [state]
  (or (get @sessions (::session state))
      (fail! :missing-session "No active formal checker session for model state.")))

(defn- attempt [state index]
  (let [{:keys [subject codec]} (session! state)]
    (if (< index (count (:actions subject)))
      (let [action (nth (:actions subject) index)]
        (if (some #{action} (formal/enabled-actions subject state))
          (-> (formal/transition subject
                                (translate-state state (:decode codec))
                                action)
              (translate-state (:encode codec)))
          state))
      state)))

;; Processes are static, but their *meaning* is the actual supplied admitted
;; subject, bound to one short-lived opaque in-process session. All six slots
;; are necessary because formal-transition admits 1..6 plans.
(recife/defproc attempt-0 (fn [state] (attempt state 0)))
(recife/defproc attempt-1 (fn [state] (attempt state 1)))
(recife/defproc attempt-2 (fn [state] (attempt state 2)))
(recife/defproc attempt-3 (fn [state] (attempt state 3)))
(recife/defproc attempt-4 (fn [state] (attempt state 4)))
(recife/defproc attempt-5 (fn [state] (attempt state 5)))

(defn- holds? [state obligation-id]
  (let [{:keys [subject codec]} (session! state)]
    (obligation/holds? subject obligation-id
                       (translate-state state (:decode codec)))))

(helpers/definvariant action-accounting [state]
  (holds? state :action-accounting))
(helpers/definvariant observation-chain [state]
  (holds? state :observation-chain))
(helpers/definvariant rejected-is-atomic [state]
  (holds? state :rejected-is-atomic))
(helpers/definvariant committed-write-set [state]
  (holds? state :committed-write-set))
(helpers/definvariant outcome-check-consistency [state]
  (holds? state :outcome-check-consistency))

(def ^:private components
  #{attempt-0 attempt-1 attempt-2 attempt-3 attempt-4 attempt-5
    action-accounting observation-chain rejected-is-atomic
    committed-write-set outcome-check-consistency})

(defn- recognized-obligation [reported-name]
  ;; Recife/TLC invariant names may be namespaced; only known obligation IDs
  ;; are safe to attribute. An unknown name is not guessed from its spelling.
  (when (keyword? reported-name)
    (some (fn [id]
            (when (= (name id) (name reported-name)) id))
          obligation/obligation-ids)))

(defn- trace-states [result]
  (let [trace (:trace result)]
    (when (and (vector? trace)
               (every? (fn [item]
                         (and (vector? item) (= 2 (count item))
                              (integer? (first item)) (map? (second item))))
                       trace))
      trace)))

(defn- safe-trace-prefix [codec result]
  ;; Index and status are framework control data. Never put the domain
  ;; documents, command arguments, target IDs or unredacted states into an
  ;; exception, since an application can accidentally serialize ex-data.
  (when-let [states (trace-states result)]
    (mapv
     (fn [[index encoded]]
       (let [state ((:decode codec) encoded)
             observations (::formal/observations state)]
         {:state-index index
          :observations (when (vector? observations)
                          (mapv (fn [o]
                                  ;; Do not trust arbitrary return values in a
                                  ;; purported TLC result to be safe metadata.
                                  (cond-> {}
                                    (and (integer? (:plan-index o))
                                         (<= 0 (:plan-index o) 5))
                                    (assoc :plan-index (:plan-index o))
                                    (contains? #{:committed :rejected} (:status o))
                                    (assoc :status (:status o))))
                                (take 6 observations)))}))
     (take 32 states))))

(defn- failure-class [result]
  (cond
    (not (map? result)) :malformed-checker-result
    (not (every? #(contains? result %)
                 [:trace :trace-info :distinct-states]))
    :malformed-checker-result
    (and (or (vector? (:trace result))
             (= :violation-detected (:trace result)))
         (= :invariant (get-in result [:trace-info :violation :type])))
    :invariant-counterexample
    (= :error (:trace result)) :backend-error
    (or (vector? (:trace result))
        (= :violation-detected (:trace result)))
    :model-counterexample
    (= :ok (:trace result)) :inconclusive-checker-result
    :else :unrecognized-checker-result))

(defn- failure-summary [subject codec result class]
  (let [reported (get-in result [:trace-info :violation])
        trace-prefix (safe-trace-prefix codec result)]
    (cond-> {:failure/class class
             :subject-kind (:kind subject)
             :checker :recife-0.22.0/tlc
             :selected-plan-count (count (:plans subject))
             :obligations (set obligation/obligation-ids)
             :assumed (:assumed assurance-scope)
             :not-established (:not-established assurance-scope)
             :source-boundary :prepared-transaction-finite-model}
      (and (map? reported) (keyword? (:type reported)))
      (assoc :violation/type (:type reported))
      (recognized-obligation (:name reported))
      (assoc :failed-obligation (recognized-obligation (:name reported)))
      (seq trace-prefix)
      (assoc :transition-prefix trace-prefix)
      (and (map? result) (pos-int? (:distinct-states result)))
      (assoc :distinct-states (:distinct-states result)))))

(defn- reject-result! [subject codec result]
  (let [class (failure-class result)
        summary (failure-summary subject codec result class)
        handle (remember-failure! {:summary summary
                                   :subject subject
                                   :raw-result result
                                   :decoded-trace
                                   (when-let [states (trace-states result)]
                                     (mapv (fn [[index state]]
                                             [index ((:decode codec) state)])
                                           states))})]
    ;; Keep the historic :error/kind so existing application callers continue
    ;; to fail closed. :failure/class tells them WHY checking failed.
    (throw (ex-info "Recife/TLC did not establish the selected finite obligations."
                    {:error/type ::invalid-formal-check
                     :error/kind :model-check-failed
                     :failure/class class
                     :diagnostic/summary summary
                     :diagnostic/handle handle}))))

(defn- reject-backend-exception! [subject error]
  (let [class (cond
                (instance? InterruptedException error) :checker-interrupted
                (instance? java.util.concurrent.TimeoutException error) :checker-timeout
                :else :backend-exception)
        summary {:failure/class class
                 :subject-kind (:kind subject)
                 :checker :recife-0.22.0/tlc
                 :source-boundary :recife-tlc-execution
                 :selected-plan-count (count (:plans subject))}
        handle (remember-failure! {:summary summary :subject subject
                                   :backend-exception error})]
    (when (= class :checker-interrupted)
      (.interrupt (Thread/currentThread)))
    ;; Preserve the original message for compatibility with existing JVM
    ;; diagnostic consumers. It is not included in structured safe ex-data.
    (throw (ex-info (or (.getMessage error) "Recife/TLC backend failed.")
                    {:error/type ::invalid-formal-check
                     :error/kind :model-check-failed
                     :failure/class class
                     :diagnostic/summary summary
                     :diagnostic/handle handle}
                    error))))

(defn check-prepared!
  "Run Recife/TLC over ALL enabled orders of one admitted finite subject.

   Accepts only the exact data returned by formal/prepared-subject; recreates
   the canonical subject to reject caller-forged plan/action/scope fields.
   No options can suppress obligations or change transition semantics.
   Failure, counterexample, absent checker result, or malformed result throws.
   Return data is for immediate diagnostic explanation, never certification.
   Timeouts must be enforced by the enclosing bounded JVM test runner."
  [subject]
  (when-not (and (map? subject) (= (:kind subject)
                                     :gesso.model.formal-transition/prepared-serial-v1)
                 (map? (::formal/documents (:initial-state subject)))
                 (vector? (:plans subject)))
    (fail! :invalid-subject "Expected an admitted prepared-transaction subject."))
  (let [canonical (formal/prepared-subject
                   (::formal/documents (:initial-state subject))
                   (:plans subject))]
    (when-not (= canonical subject)
      (fail! :modified-subject "Prepared subject does not match canonical admission."))
    (locking checker-lock
      (let [session-id (keyword "gessoformalsession" (str "s" (UUID/randomUUID)))
            codec (codec-for subject)
            initial (-> (formal/initial-state subject)
                        (translate-state (:encode codec))
                        (assoc ::session session-id))]
        (swap! sessions assoc session-id {:subject subject :codec codec})
        (try
          (let [result (try
                         (let [raw (recife/run-model initial components
                                                     {:no-deadlock true :workers 1})]
                           (if (instance? clojure.lang.IDeref raw) @raw raw))
                         (catch Exception error
                           (reject-backend-exception! subject error)))]
            ;; TLC success requires an explicit and complete terminal result.
            ;; Every other shape is a failure, with a redacted explanation and
            ;; an optional locally inspectable raw diagnostic.
            (when-not (and (map? result)
                           (every? #(contains? result %)
                                   [:trace :trace-info :distinct-states])
                           (= :ok (:trace result))
                           (nil? (:trace-info result))
                           (pos-int? (:distinct-states result)))
              (reject-result! subject codec result))
            {:status :checked-finite-model
             :evidence-class :bounded-reference-model-check-not-certificate
             :subject-kind (:kind subject)
             :obligations (set obligation/obligation-ids)
             :checker :recife-0.22.0/tlc
             :distinct-states (:distinct-states result)
             :assumed (:assumed assurance-scope)
             :not-established (:not-established assurance-scope)})
          (finally
            (swap! sessions dissoc session-id)))))))

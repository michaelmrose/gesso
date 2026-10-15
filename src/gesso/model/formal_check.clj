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
   completion, failure, or interruption. No application callbacks execute."
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
          (let [raw (recife/run-model initial components
                                      {:no-deadlock true :workers 1})
                result (if (instance? clojure.lang.IDeref raw) @raw raw)]
            ;; A missing :trace-info is not the same as an explicit nil.
            ;; Recife uses nil here for a completed invariant-safe check;
            ;; accepting absent metadata could silently upgrade an incomplete
            ;; checker response into a successful bounded result.
            (when-not (and (map? result)
                           (every? #(contains? result %)
                                   [:trace :trace-info :distinct-states])
                           (= :ok (:trace result))
                           (nil? (:trace-info result))
                           (pos-int? (:distinct-states result)))
              (fail! :model-check-failed
                     "Recife/TLC did not establish the selected finite obligations."))
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

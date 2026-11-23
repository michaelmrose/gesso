(ns gesso.assurance.release.traffic-fence
  "v2267: An enforcing, host-local, synchronous Ring traffic fence.

   This is a narrow runtime MECHANISM for R5.1 Part XI: no business handler
   can run through the issued Ring entrypoint before an explicitly qualified
   compare-and-activate transition. A single immutable handler/guard binding
   is selected atomically. Protected requests pin that binding under a read
   lease until the synchronous Ring invocation finishes; activation, retirement
   and closure hold the exclusive write lease. A stale candidate cannot
   overwrite a newer selected generation or retire its replacement.

   The host supplies :qualify!, the authoritative release-eligibility CHECK.
   Its provenance is a TRUSTED PREMISE, not authenticated in this file. A
   successful callback is not proof of candidate admission, installed Biff
   adoption, writer coverage, browser assets, compatible overlap, or release
   authority. Until an independent Gesso issuer connects the original source
   and physical release gates, this remains an opt-in host mechanism. The
   returned Ring handler MUST actually be installed as the only public
   protected entrypoint; this file cannot discover bypass routes or workers.

   Explicitly synchronous Ring only: deferred responses, body streams, SSE,
   jobs, and transferred callbacks outlive the lexical read lease and require
   a separate lifetime protocol. No attempt is made to stop resource owners,
   roll back committed business effects, or authorize future requests from a
   cached assessment. The per-request :guard! must check current permissions."
  (:import [java.util IdentityHashMap]
           [java.util.concurrent.locks ReentrantReadWriteLock]))

(def report-type :gesso.assurance.release.traffic-fence/host-local-state)
(def report-version 1)
(def ^:private max-staged 64)
(def ^:private max-generations 65536)
(def ^:private unavailable
  {:status 503
   :headers {"cache-control" "no-store"}
   :body "Protected application traffic is unavailable"})

(deftype ^:private CandidateToken []
  Object
  (toString [_] "#<gesso.release/opaque-staged-candidate>"))

(defn- reject! [reason]
  (throw (ex-info "Host-local traffic fence cannot perform this transition."
                  {:gesso.release.traffic-fence/rejection true :reason reason})))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))))

(defn- qualifier-input! [input]
  (when-not (and (plain-map? input)
                 (= #{:qualify!} (set (keys input)))
                 (fn? (:qualify! input)))
    (reject! :trusted-host-qualifier-required))
  (:qualify! input))

(defn- candidate-input! [candidate]
  (when-not (and (plain-map? candidate)
                 (= #{:generation :handler :guard!} (set (keys candidate)))
                 (qualified-keyword? (:generation candidate))
                 (fn? (:handler candidate))
                 (fn? (:guard! candidate)))
    (reject! :invalid-immutable-dispatch-candidate))
  candidate)

(defn issue-local-ring-traffic-fence!
  "Issue one opaque host-local synchronous Ring dispatcher.

   :prepare! stages an immutable candidate, capturing the CURRENT epoch.
   :activate! atomically selects that candidate only if its epoch is current
   and the host's independent :qualify! returns exactly true for that precise
   handler/guard pair (their object references appear in the qualifier input). Qualification
   failure consumes the staged token and keeps the previous binding (whose
   own per-request guard still independently controls its usability).
   Both staged and historically used generation inventories are bounded.
   :retire! requires the EXACT active candidate token; it drains synchronous
   requests before denying subsequent traffic. :close! permanently denies all
   traffic and drops unactivated candidates. Every transition checks for
   reentrant read->write upgrade and fails rather than deadlocking.

   The host MUST NOT call activation from arbitrary application requests.
   None of these methods is a source-bound release credential."
  [input]
  (let [qualify! (qualifier-input! input)
        ^ReentrantReadWriteLock lock (ReentrantReadWriteLock. true)
        candidates (IdentityHashMap.)
        state (atom {:phase :inactive :epoch 0 :active nil :used #{}})
        with-write (fn [f]
                     (when (pos? (.getReadHoldCount lock))
                       (reject! :lifecycle-transition-during-protected-request))
                     ;; A host qualification callback is executed under the
                     ;; write lease. Reentrant close/retire/prepare within it
                     ;; would invalidate the checked epoch mid-activation.
                     (when (.isWriteLockedByCurrentThread lock)
                       (reject! :reentrant-lifecycle-transition))
                     (let [l (.writeLock lock)]
                       (.lock l)
                       (try (f) (finally (.unlock l)))))
        with-read (fn [f]
                    (let [l (.readLock lock)]
                      (.lock l)
                      (try (f) (finally (.unlock l)))))
        summary (fn []
                  (let [{:keys [phase epoch active]} @state]
                    {:type report-type :version report-version
                     :phase phase :epoch epoch
                     :selected-generation (:generation active)
                     :staged-count (.size candidates)
                     :scope :one-host-synchronous-ring-entrypoint-only
                     :host-qualifier-provenance :trusted-host-callback-conditional
                     :real-biff-installation :not-established
                     :async-response-drain :not-established
                     :release-authority :none
                     :installation-eligibility :not-issued
                     :activation-eligibility :not-issued
                     :completion-claim :not-issued
                     :runtime-effect-permission :none}))]
    {:handler
     (fn [request]
       ;; Do not permit a qualifier to call into live traffic while holding
       ;; the exclusive activation lock. Retire waits for the complete
       ;; synchronous handler invocation, never for a detached async effect.
       (if (.isWriteLockedByCurrentThread lock)
         unavailable
         (with-read
           (fn []
             (let [selected (:active @state)]
               (if (and selected
                        (true? (try ((:guard! selected) request)
                                    (catch Throwable error
                                      (when (instance? InterruptedException error)
                                        (.interrupt (Thread/currentThread)))
                                      false))))
                 ((:handler selected) request)
                 unavailable))))))

     :prepare!
     (fn [candidate]
       (with-write
         (fn []
           (let [candidate (candidate-input! candidate)
                 {:keys [phase epoch used]} @state
                 generation (:generation candidate)]
             (when (= :closed phase)
               (reject! :permanently-closed))
             (when (or (contains? used generation)
                       (some #(= generation (:generation %))
                             (iterator-seq (.iterator (.values candidates)))))
               (reject! :reused-or-already-staged-generation))
             (when (>= (.size candidates) max-staged)
               (reject! :too-many-staged-generations))
             (let [token (CandidateToken.)]
               (.put candidates token (assoc candidate :expected-epoch epoch))
               token)))))

     :cancel!
     (fn [token]
       (with-write
         (fn []
           (when-not (and (instance? CandidateToken token)
                          (.containsKey candidates token))
             (reject! :unknown-or-consumed-candidate))
           (.remove candidates token)
           {:status :cancelled :authority :none})))

     :activate!
     (fn [token]
       (with-write
         (fn []
           (when-not (and (instance? CandidateToken token)
                          (.containsKey candidates token))
             (reject! :unknown-or-consumed-candidate))
           ;; Consume the token BEFORE calling untrusted host integration,
           ;; including when qualification fails or throws.
           (let [candidate (.remove candidates token)
                 {:keys [phase epoch active used]} @state
                 generation (:generation candidate)]
             (when (= :closed phase)
               (reject! :permanently-closed))
             (when-not (= epoch (:expected-epoch candidate))
               (reject! :stale-predecessor-generation))
             (when (contains? used generation)
               (reject! :reused-generation))
             (when (>= (count used) max-generations)
               (reject! :generation-inventory-exhausted))
             (when (= epoch Long/MAX_VALUE)
               (reject! :generation-counter-exhausted))
             (let [qualification
                   (try {:status :observed
                         :result (qualify! {:candidate-generation generation
                                            ;; The callback must inspect the
                                            ;; ACTUAL functions, not only labels.
                                            :candidate-handler (:handler candidate)
                                            :candidate-guard! (:guard! candidate)
                                            :previous-generation (:generation active)
                                            :expected-epoch epoch
                                            :scope :one-host-synchronous-ring-entrypoint-only})}
                        (catch Throwable error
                          (when (instance? InterruptedException error)
                            (.interrupt (Thread/currentThread)))
                          {:status :unavailable}))]
               (when-not (= :observed (:status qualification))
                 (reject! :qualification-unavailable))
               (when-not (true? (:result qualification))
                 (reject! :qualification-not-established))
               ;; Exactly ONE atomic binding replacement under the write
               ;; lease. Epoch increments even if the public label repeats.
               (reset! state {:phase :active :epoch (inc epoch)
                              :active (assoc candidate :token token)
                              :used (conj used generation)})
               {:status :host-selected-only
                :generation generation :epoch (inc epoch)
                :release-authority :none
                :activation-eligibility :not-issued})))))

     :retire!
     (fn [token]
       (with-write
         (fn []
           (let [{:keys [phase epoch active] :as now} @state]
             (when-not (and (= :active phase)
                            (instance? CandidateToken token)
                            (identical? token (:token active)))
               (reject! :not-the-current-active-generation))
             (when (= epoch Long/MAX_VALUE)
               (reject! :generation-counter-exhausted))
             (reset! state (assoc now :phase :inactive :epoch (inc epoch)
                                  :active nil))
             {:status :retired :generation (:generation active)
              :authority :none}))))

     :close!
     (fn []
       (with-write
         (fn []
           (when-not (= :closed (:phase @state))
             (.clear candidates)
             (reset! state (assoc @state :phase :closed :active nil)))
           {:status :closed :authority :none})))

     :summary! (fn [] (with-read summary))}))

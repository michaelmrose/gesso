(ns gesso.script.gesso-node-single-pass-gate-test
  "Permanent source-executed integrity checks for the v1459 one-run Node browser
   gate. Exercise the original runner definitions, not a copy of its cache
   protocol. Process and JVM-corpus acquisition are the only stubbed seams;
   this test never loads the Babashka (-main) entrypoint or builds Node output."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.jvm-cljs-semantics-test :as portable]
            [gesso.live.browser.refinement-node-parity-test :as refinement]))

(def ^:private node-file "target/gesso-test/node/browser-tests.js")
(def ^:private portable-prefix "GESSO_PORTABLE_SEMANTICS_CORPUS ")
(def ^:private refinement-prefix "GESSO_BROWSER_REFINEMENT_PARITY ")
(def ^:private portable-cases (delay (portable/shared-corpus-results)))
(def ^:private refinement-cases (delay (refinement/portable-corpus)))

(def ^:private selected-definitions
  '#{test-work-dir gate-failure-key gate-failure!
     portable-corpus-line-prefix portable-minimum-cases
     browser-refinement-corpus-line-prefix browser-refinement-case-ids
     *node-browser-captured-run*
     require-portable-corpus! require-browser-refinement-corpus!
     run-node-portable-correspondence!
     run-node-browser-refinement-correspondence!
     run-node-browser-tests!})

(defn- source-forms []
  (with-open [r (java.io.PushbackReader. (io/reader "script/gesso_test.clj"))]
    (loop [forms []]
      (let [form (read r false ::eof)]
        (if (= ::eof form) forms (recur (conj forms form)))))))

(defn- declared [form]
  (when (and (seq? form) (#{'def 'defn} (first form)))
    (second form)))

;; The runner intentionally looks up the captured-run Var in this exact
;; namespace. Creating it for the duration of this test namespace's suite is
;; necessary to exercise production's real cache path; an arbitrary excerpt
;; namespace would silently exercise its uncached fallback instead.
(def ^:private source-gate
  (delay
    (let [forms (source-forms)
          present (set (keep declared forms))
          missing (remove present selected-definitions)]
      (when (seq missing)
        (throw (ex-info "Required single-pass gate definition is missing."
                        {:missing (vec missing)})))
      (when (find-ns 'gesso.script.gesso-test)
        (throw (ex-info "Cannot replace an already loaded runner namespace."
                        {:namespace 'gesso.script.gesso-test})))
      (let [target (create-ns 'gesso.script.gesso-test)]
        (binding [*ns* target]
          (clojure.core/refer 'clojure.core)
          (alias 'str 'clojure.string)
          (alias 'edn 'clojure.edn)
          (doseq [symbol '[run-command-captured-with-timeout!
                           jvm-portable-corpus! jvm-browser-refinement-corpus!
                           run-node-test-groups!]]
            (intern target symbol
                    (fn [& _]
                      (throw (ex-info "Unstubbed external boundary"
                                      {:boundary symbol})))))
          (doseq [form forms
                  :when (contains? selected-definitions (declared form))]
            (eval form)))
        target))))

(use-fixtures
  :once
  (fn [tests]
    (try
      (tests)
      (finally
        (when (= @source-gate (find-ns 'gesso.script.gesso-test))
          (remove-ns 'gesso.script.gesso-test))))))

(defn- gate-var [symbol]
  (or (ns-resolve @source-gate symbol)
      (throw (ex-info "Original runner definition missing." {:symbol symbol}))))
(defn- gate [symbol & args]
  (apply (var-get (gate-var symbol)) args))
(defn- rejection [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo error (ex-data error))))

(defn- markers
  ([] (markers @portable-cases @refinement-cases))
  ([p r]
   (str "Testing actual Node browser adapter\n"
        portable-prefix (pr-str p) "\n"
        refinement-prefix (pr-str r) "\n"
        "Ran 25 tests containing 1052 assertions.\n0 failures, 0 errors.\n")))

(defn- events-of [events tag]
  (filter #(= tag (first %)) @events))

(defn- simulated-gate
  "Run the source-executed production gate against ordered captured Node
   results. A corpus checker still runs the ORIGINAL parsing and comparison
   code, and each independently obtained JVM result is tracked in events."
  [outputs events thunk]
  (let [remaining (atom (vec outputs))
        boundaries
        {'run-node-test-groups!
         (fn [groups]
           (swap! events conj [:groups groups])
           (gate 'run-node-portable-correspondence! node-file))
         'run-command-captured-with-timeout!
         (fn [command timeout]
           (swap! events conj [:node command timeout])
           (when (empty? @remaining)
             (throw (ex-info "Unexpected additional Node execution."
                             {:node-runs :exhausted})))
           (let [out (first @remaining)]
             (swap! remaining subvec 1)
             (if (instance? Throwable out)
               (throw out)
               {:out out :err ""})))
         'jvm-portable-corpus!
         (fn []
           (swap! events conj [:jvm :portable])
           (pr-str @portable-cases))
         'jvm-browser-refinement-corpus!
         (fn []
           (swap! events conj [:jvm :refinement])
           (pr-str @refinement-cases))}
        substituted (into {} (map (fn [[symbol f]] [(gate-var symbol) f])
                                  boundaries))]
    (with-redefs-fn substituted thunk)))

(defn- run-browser! []
  (with-out-str (gate 'run-node-browser-tests!)))

(deftest source-definitions-and-required-host-boundaries-are-real
  (let [forms (source-forms)
        present (set (keep declared forms))]
    (is (= '(-main) (last forms)))
    (is (every? present selected-definitions))
    (is (= 'gesso.script.gesso-test (ns-name @source-gate)))
    (is (true? (:dynamic (meta (gate-var '*node-browser-captured-run*)))))
    (is (nil? (var-get (gate-var '*node-browser-captured-run*))))
    (is (= 14 (count @refinement-cases)))
    (is (<= 941 (count @portable-cases)))))

(deftest exactly-one-node-result-feeds-both-independent-comparisons
  (let [events (atom [])
        result (simulated-gate [(markers)] events run-browser!)]
    (is (str/includes? result "Portable JVM/Node semantic corpus: PASS"))
    (is (str/includes? result "Browser refinement JVM/Node corpus: PASS"))
    (is (= [[:groups [:browser]]
            [:node ["node" node-file] 180000]
            [:jvm :portable]
            [:jvm :refinement]] @events))
    (is (= 1 (count (events-of events :node))))
    (is (nil? (var-get (gate-var '*node-browser-captured-run*))))))

(deftest second-invocation-must-acquire-fresh-node-and-jvm-evidence
  (let [events (atom [])
        result (simulated-gate [(markers) (markers)] events
                               #(do (run-browser!) (run-browser!)))]
    (is (str/includes? result "Browser refinement JVM/Node corpus: PASS"))
    (is (= 2 (count (events-of events :node))))
    (is (= 2 (count (filter #{[:jvm :portable]} @events))))
    (is (= 2 (count (filter #{[:jvm :refinement]} @events))))
    (is (= [:groups :node :jvm :jvm :groups :node :jvm :jvm]
           (mapv first @events)))
    (is (nil? (var-get (gate-var '*node-browser-captured-run*))))))

(deftest second-invocation-cannot-use-first-gates-matching-result
  (let [events (atom [])
        changed (assoc-in @refinement-cases [10 1] [:stuttering-but-substituted])
        failure (simulated-gate [(markers) (markers @portable-cases changed)]
                                events
                                #(do (run-browser!)
                                     (rejection run-browser!)))]
    (is (= 10 (:first-mismatched-index failure)))
    (is (= (nth @refinement-cases 10) (:jvm-case failure)))
    (is (= (nth changed 10) (:node-case failure)))
    (is (= 2 (count (events-of events :node))))
    (is (= 2 (count (filter #{[:jvm :refinement]} @events))))))

(deftest portable-mismatch-blocks-refinement-without-node-retry
  (let [events (atom [])
        changed (assoc-in @portable-cases [0 1] [:ok :altered])
        failure (simulated-gate [(markers changed @refinement-cases)]
                                events #(rejection run-browser!))]
    (is (= 0 (:first-mismatched-index failure)))
    (is (= (first @portable-cases) (:jvm-case failure)))
    (is (= (first changed) (:node-case failure)))
    (is (= 1 (count (events-of events :node))))
    (is (= 0 (count (filter #{[:jvm :refinement]} @events))))))

(deftest refinement-mismatch-cannot-be-repaired-by-second-node-run
  (let [events (atom [])
        changed (assoc-in @refinement-cases [10 1] [:stuttering-but-substituted])
        failure (simulated-gate [(markers @portable-cases changed)]
                                events #(rejection run-browser!))]
    (is (= 10 (:first-mismatched-index failure)))
    (is (= 1 (count (events-of events :node))))
    (is (= 1 (count (filter #{[:jvm :portable]} @events))))
    (is (= 1 (count (filter #{[:jvm :refinement]} @events))))))

(deftest missing-marker-in-first-captured-result-fails-closed
  (let [events (atom [])
        missing (str/replace (markers)
                             (str portable-prefix (pr-str @portable-cases) "\n") "")
        failure (simulated-gate [missing] events #(rejection run-browser!))]
    (is (= 0 (:matching-lines failure)))
    (is (= :node (:host failure)))
    (is (= 1 (count (events-of events :node))))
    (is (empty? (events-of events :jvm)))))

(deftest missing-refinement-marker-fails-after-portable-comparison
  (let [events (atom [])
        missing (str/replace (markers)
                             (str refinement-prefix (pr-str @refinement-cases) "\n") "")
        failure (simulated-gate [missing] events #(rejection run-browser!))]
    (is (= 0 (:matching-lines failure)))
    (is (= :node (:host failure)))
    (is (= 1 (count (events-of events :node))))
    (is (= [[:jvm :portable]] (vec (events-of events :jvm))))))

(deftest duplicated-refinement-marker-fails-instead-of-choosing-one
  (let [events (atom [])
        raw (str (markers) refinement-prefix (pr-str @refinement-cases) "\n")
        failure (simulated-gate [raw] events #(rejection run-browser!))]
    (is (= 2 (:matching-lines failure)))
    (is (= 1 (count (events-of events :node))))
    (is (= [[:jvm :portable]] (vec (events-of events :jvm))))))

(deftest failed-first-comparison-never-poisons-subsequent-gate
  (let [events (atom [])
        bad-output (str/replace (markers)
                                (str portable-prefix (pr-str @portable-cases) "\n") "")
        failure-and-pass
        (simulated-gate [bad-output (markers)] events
                        #(let [failed (rejection run-browser!)
                               succeeded (run-browser!)]
                           [failed succeeded]))]
    (is (= 0 (:matching-lines (first failure-and-pass))))
    (is (str/includes? (second failure-and-pass)
                       "Browser refinement JVM/Node corpus: PASS"))
    (is (= 2 (count (events-of events :node))))
    (is (= 1 (count (filter #{[:jvm :refinement]} @events))))
    (is (nil? (var-get (gate-var '*node-browser-captured-run*))))))

(deftest missing-or-swapped-scoped-evidence-is-not-reacquired
  (let [cache (gate-var '*node-browser-captured-run*)
        events (atom [])
        f (fn [cache-value output]
            (simulated-gate [] events
                            #(with-bindings {cache (atom cache-value)}
                               (rejection (fn []
                                            (with-out-str
                                              (gate 'run-node-browser-refinement-correspondence!
                                                    output)))))))]
    (is (= node-file (:requested-path (f nil node-file))))
    (let [failure (f {:path node-file :result {:out (markers) :err ""}}
                     "swapped/browser.js")]
      (is (= node-file (:captured-path failure)))
      (is (= "swapped/browser.js" (:requested-path failure))))
    (is (empty? (events-of events :node)))
    (is (empty? (events-of events :jvm)))))

(deftest standalone-refinement-executes-its-own-node-once
  (let [events (atom [])
        result (simulated-gate [(markers)] events
                               #(with-out-str
                                  (gate 'run-node-browser-refinement-correspondence!
                                        node-file)))]
    (is (str/includes? result "Browser refinement JVM/Node corpus: PASS"))
    (is (= [[:node ["node" node-file] 180000] [:jvm :refinement]] @events))))

(deftest duplicate-capture-within-one-gate-fails-closed
  (let [events (atom [])
        outcome
        (simulated-gate [(markers) (markers)] events
                        #(with-redefs-fn
                           {(gate-var 'run-node-test-groups!)
                            (fn [_]
                              (gate 'run-node-portable-correspondence! node-file)
                              (gate 'run-node-portable-correspondence! node-file))}
                           (fn [] (rejection run-browser!))))]
    (is (= node-file (:previous-path outcome)))
    (is (= node-file (:current-path outcome)))
    (is (= 2 (count (events-of events :node))))
    (is (zero? (count (filter #{[:jvm :refinement]} @events))))))

(ns gesso.script.gesso-node-refinement-gate-integrity-test
  "Source-executed v1458 regression tests for the mandatory Node/CLJS browser
   refinement correspondence gate. Read original runner forms, but NEVER load
   its Babashka entrypoint or launch a real build. Compare against the real
   14-case CLJC corpus, not a fabricated all-green example. The native Node
   build remains a separate required gate."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gesso.live.browser.refinement-node-parity-test :as parity]))

;; Minimal JDK filesystem adapter for source-executed Babashka runner forms.
(defn path [& parts]
  (java.nio.file.Paths/get (str (first parts))
                           (into-array String (map str (rest parts)))))
(defn parent [p] (.getParent (path p)))
(defn create-dirs [p]
  (java.nio.file.Files/createDirectories
   (path p) (make-array java.nio.file.attribute.FileAttribute 0)))

(def ^:private exercised
  '#{test-work-dir generated-test-src
     fail! gate-failure-key gate-failure!
     required-node-browser-test-namespaces
     portable-choreo-test-namespace? node-test-plan
     portable-corpus-line-prefix portable-semantics-test-namespace
     browser-refinement-corpus-line-prefix browser-refinement-case-ids
     browser-refinement-corpus-namespace
     node-runner-name node-runner-namespace require-lines quoted-test-args
     write-node-runner! require-browser-refinement-corpus!
     jvm-browser-refinement-corpus!
     run-node-browser-refinement-correspondence!
     run-node-test-groups! run-node-browser-tests!})

(defn- source-forms []
  (with-open [reader (java.io.PushbackReader. (io/reader "script/gesso_test.clj"))]
    (loop [acc []]
      (let [form (read reader false ::eof)]
        (if (= ::eof form) acc (recur (conj acc form)))))))

(defn- declared [form]
  (when (and (seq? form) (#{'def 'defn} (first form)))
    (second form)))

(def ^:private original-gate
  (delay
    (let [forms (source-forms)
          declared-names (set (keep declared forms))
          missing (remove declared-names exercised)
          target (create-ns 'gesso.script.node-refinement-source-excerpt)]
      (when (seq missing)
        (throw (ex-info "Required source gate definitions are missing."
                        {:missing (vec missing)})))
      (binding [*ns* target]
        (clojure.core/refer 'clojure.core)
        (alias 'str 'clojure.string)
        (alias 'edn 'clojure.edn)
        (alias 'fs 'gesso.script.gesso-node-refinement-gate-integrity-test)
        ;; Only host boundaries are replaced, never the gate definitions.
        (doseq [name '[cljs-test-namespaces cljs-test-sdeps
                       run-command-captured-with-timeout!
                       run-command! cljs-main-command delete-tree-if-exists!
                       run-node-portable-correspondence!]]
          (intern target name
                  (fn [& _] (throw (ex-info "Unstubbed host boundary" {:name name})))))
        (doseq [form forms
                :when (contains? exercised (declared form))]
          (eval form)))
      target)))

(defn- gate-var [name]
  (or (ns-resolve @original-gate name)
      (throw (ex-info "Original gate definition unavailable." {:name name}))))
(defn- gate [name & args] (apply (var-get (gate-var name)) args))
(defn- stubbed [changes thunk]
  (with-redefs-fn
    (into {} (map (fn [[symbol f]] [(gate-var symbol) f]) changes))
    thunk))
(defn- rejected [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo e (ex-data e))))

(def ^:private real-cases (delay (parity/portable-corpus)))
(defn- prefix [] (var-get (gate-var 'browser-refinement-corpus-line-prefix)))
(defn- line [cases] (str (prefix) (pr-str cases) "\n"))
(defn- check [out host] (gate 'require-browser-refinement-corpus! out host))
(defn- failure [out host] (rejected #(check out host)))

(deftest source-excerpt-is-actual-runner-and-never-runs-main
  (let [forms (source-forms)
        present (set (keep declared forms))]
    (is (= '(-main) (last forms)))
    (is (every? present exercised))
    (is (not= 'gesso.script.gesso-test (ns-name @original-gate)))
    (is (identical? @original-gate @original-gate))))

(deftest required-test-namespace-is-not-discoverable-by-accident
  (let [target 'gesso.live.browser.refinement-node-parity-test
        required (var-get (gate-var 'required-node-browser-test-namespaces))
        all (vec (conj required 'gesso.choreo.fake-test))]
    (is (contains? required target))
    (is (= target (var-get (gate-var 'browser-refinement-corpus-namespace))))
    (is (some #{target}
              (:browser (stubbed {'cljs-test-namespaces (constantly all)}
                                 #(gate 'node-test-plan)))))
    (let [rejection (stubbed {'cljs-test-namespaces
                              (constantly (remove #{target} all))}
                             #(rejected (fn [] (gate 'node-test-plan))))]
      (is (= [target] (:missing-namespaces rejection))))
    (is (= 14 (count (var-get (gate-var 'browser-refinement-case-ids)))))))

(deftest generated-browser-runner-emits-both-corpora-and-no-choreo-markers
  (let [tmp (java.nio.file.Files/createTempDirectory
             "gesso-v1458-runner-"
             (make-array java.nio.file.attribute.FileAttribute 0))
        directory (str tmp)
        source-ns (var-get (gate-var 'required-node-browser-test-namespaces))]
    (try
      (stubbed {'generated-test-src directory}
               #(do
                  (gate 'write-node-runner! :browser (sort source-ns))
                  (gate 'write-node-runner! :choreo ['gesso.choreo.fake-test])))
      (let [browser (slurp (str tmp "/gesso/test_node_browser_runner.cljs"))
            choreo (slurp (str tmp "/gesso/test_node_choreo_runner.cljs"))]
        (is (= 1 (count (re-seq #"GESSO_BROWSER_REFINEMENT_PARITY " browser))))
        (is (= 1 (count (re-seq #"GESSO_PORTABLE_SEMANTICS_CORPUS " browser))))
        (is (str/includes? browser
                           "gesso.live.browser.refinement-node-parity-test/portable-corpus"))
        (is (str/includes? browser "(t/run-tests"))
        (is (str/includes? browser "gesso.live.browser.refinement-node-parity-test"))
        (is (not (str/includes? choreo "GESSO_BROWSER_REFINEMENT_PARITY")))
        (is (not (str/includes? choreo "GESSO_PORTABLE_SEMANTICS_CORPUS"))))
      (finally
        (doseq [file (reverse (file-seq (.toFile tmp)))]
          (io/delete-file file true))))))

(deftest exact-live-corpus-accepted-without-granting-authority
  (let [cases @real-cases
        normalized (check (str "Testing actual CLJC semantics\n" (line cases)) :node)]
    (is (= 14 (count cases)))
    (is (= (var-get (gate-var 'browser-refinement-case-ids))
           (mapv first cases)))
    (is (= (pr-str cases) normalized))
    (is (= (pr-str cases) (check (line cases) :jvm)))
    (is (= false (get-in cases [0 1 9])))
    (is (= false (get-in cases [0 1 10])))
    (is (= :none (get-in cases [0 1 11])))))

(deftest missing-and-duplicated-markers-fail-even-with-passing-test-summary
  (let [cases @real-cases
        marker (line cases)
        passed "Ran 25 tests containing 1052 assertions.\n0 failures, 0 errors.\n"]
    (doseq [[out count] [[passed 0]
                         [(str "other-marker " (pr-str cases) "\n" passed) 0]
                         [(str marker marker passed) 2]]]
      (let [error (failure out :node)]
        (is (= count (:matching-lines error)))
        (is (= :node (:host error)))))))

(deftest malformed-incomplete-reordered-and-inert-cases-fail-closed
  (let [cases @real-cases
        bad [[:empty []]
             [:truncated (pop cases)]
             [:reordered (vec (reverse cases))]
             [:duplicate-id (assoc-in cases [1 0] (ffirst cases))]
             [:missing-physical-premise (assoc-in cases [0 1 8] [])]
             [:forged-global-authority (assoc-in cases [0 1 9] true)]
             [:changed-proof-authority (assoc-in cases [0 1 11] :certified)]
             [:unrecognized-rejection (assoc cases 4 [:reused-alignment [:ok]])]]]
    (doseq [[label mutated] bad]
      (testing (name label)
        (let [error (failure (line mutated) :node)]
          (is (= :node (:host error)))
          (is (= 14 (count (:expected-case-ids error)))))))
    (doseq [raw ["[" "{broken" "#=(+ 1 2)"]]
      (let [error (failure (str (prefix) raw "\n") :jvm)]
        (is (= :jvm (:host error)))
        (is (contains? error :cause))))))

(deftest real-source-jvm-command-cannot-substitute-a-second-node-run
  (let [calls (atom [])
        printed (line @real-cases)
        result (stubbed {'cljs-test-sdeps (constantly "{:aliases {}}")
                         'run-command-captured-with-timeout!
                         (fn [command timeout]
                           (swap! calls conj [command timeout])
                           {:out printed :err ""})}
                        #(gate 'jvm-browser-refinement-corpus!))
        [command timeout] (first @calls)]
    (is (= (pr-str @real-cases) result))
    (is (= 1 (count @calls)))
    (is (= "clojure" (first command)))
    (is (= 180000 timeout))
    (is (str/includes? (last command) "refinement-node-parity-test/portable-corpus"))
    (is (not= "node" (first command)))))

(deftest valid-shaped-but-changed-node-result-fails-exact-comparison
  (let [cases @real-cases
        altered (assoc-in cases [10 1] [:forged-but-valid-shaped])
        calls (atom [])
        failure (stubbed
                 {'run-command-captured-with-timeout!
                  (fn [command timeout]
                    (swap! calls conj [command timeout])
                    {:out (line altered) :err ""})
                  'jvm-browser-refinement-corpus! (constantly (pr-str cases))}
                 #(rejected (fn []
                              (gate 'run-node-browser-refinement-correspondence!
                                    "compiled/browser-tests.js"))))]
    (is (= 10 (:first-mismatched-index failure)))
    (is (= (nth cases 10) (:jvm-case failure)))
    (is (= (nth altered 10) (:node-case failure)))
    (is (= 14 (:jvm-case-count failure)))
    (is (= 14 (:node-case-count failure)))
    (is (= [["node" "compiled/browser-tests.js"]] (mapv first @calls)))
    (is (= [180000] (mapv second @calls)))))

(deftest missing-node-result-fails-before-jvm-witness-can-be-consulted
  (let [jvm-calls (atom 0)
        failure (stubbed
                 {'run-command-captured-with-timeout!
                  (fn [_ _] {:out "0 failures, 0 errors\n" :err ""})
                  'jvm-browser-refinement-corpus!
                  (fn [] (swap! jvm-calls inc) (pr-str @real-cases))}
                 #(rejected (fn []
                              (gate 'run-node-browser-refinement-correspondence!
                                    "node.js"))))]
    (is (= 0 (:matching-lines failure)))
    (is (zero? @jvm-calls))))

(deftest source-gate-enforces-both-comparisons-in-order
  (let [events (atom [])
        emitted (stubbed
                 {'node-test-plan (constantly {:browser ['gesso.live.browser.refinement-node-parity-test]})
                  'delete-tree-if-exists! (fn [& _] nil)
                  'write-node-runner! (fn [group ns]
                                        (swap! events conj [:runner group ns])
                                        {:namespace 'gesso.test-node-browser-runner})
                  'cljs-main-command (fn [& xs] (into ["clojure"] xs))
                  'run-command! (fn [command] (swap! events conj [:compile command]))
                  'run-node-portable-correspondence! (fn [file] (swap! events conj [:portable file]))
                  'run-node-browser-refinement-correspondence!
                  (fn [file] (swap! events conj [:refinement file]) nil)}
                 #(gate 'run-node-browser-tests!))]
    (is (nil? emitted))
    (is (= [:runner :compile :portable :refinement] (mapv first @events)))
    (is (= "target/gesso-test/node/browser-tests.js" (second (nth @events 2))))
    (is (= (second (nth @events 2)) (second (nth @events 3)))))
  (let [events (atom [])
        thrown (stubbed
                 {'node-test-plan (constantly {:browser ['gesso.live.browser.refinement-node-parity-test]})
                  'delete-tree-if-exists! (fn [& _] nil)
                  'write-node-runner! (fn [& _] {:namespace 'gesso.test-node-browser-runner})
                  'cljs-main-command (fn [& xs] (into ["clojure"] xs))
                  'run-command! (fn [_] (swap! events conj :compiled))
                  'run-node-portable-correspondence!
                  (fn [_] (swap! events conj :portable)
                    (throw (ex-info "Original portable gate failed" {:old-gate :failed})))
                  'run-node-browser-refinement-correspondence!
                  (fn [_] (swap! events conj :refinement))}
                 #(rejected (fn [] (gate 'run-node-browser-tests!))))]
    (is (= :failed (:old-gate thrown)))
    (is (= [:compiled :portable] @events))))

(deftest failed-refinement-comparison-prevents-gate-success
  (let [events (atom [])
        failure (stubbed
                 {'node-test-plan (constantly {:browser ['gesso.live.browser.refinement-node-parity-test]})
                  'delete-tree-if-exists! (fn [& _] nil)
                  'write-node-runner! (fn [& _] {:namespace 'gesso.test-node-browser-runner})
                  'cljs-main-command (fn [& xs] (into ["clojure"] xs))
                  'run-command! (fn [_] (swap! events conj :compiled))
                  'run-node-portable-correspondence!
                  (fn [_] (swap! events conj :portable))
                  'run-node-browser-refinement-correspondence!
                  (fn [_] (swap! events conj :refinement)
                    (throw (ex-info "JVM/Node refinement corpus mismatch"
                                    {:refinement :mismatch}))) }
                 #(rejected (fn [] (gate 'run-node-browser-tests!))))]
    (is (= :mismatch (:refinement failure)))
    (is (= [:compiled :portable :refinement] @events))))

(deftest provenance-report-is-never-accepted-as-a-fresh-proof-token
  ;; Even a syntactically well-formed report must pass against independently
  ;; derived JVM evidence. Caller-supplied reports have no direct fast path.
  (let [calls (atom [])
        cases @real-cases]
    (is (nil?
         (stubbed
          {'run-command-captured-with-timeout!
           (fn [command timeout]
             (swap! calls conj [:node command timeout])
             {:out (line cases) :err ""})
           'jvm-browser-refinement-corpus!
           (fn [] (swap! calls conj [:jvm]) (pr-str cases))}
          #(gate 'run-node-browser-refinement-correspondence! "node.js"))))
    (is (= [:node :jvm] (mapv first @calls)))))

(ns gesso.script.gesso-node-three-corpus-gate-test
  "Permanent source-executed checks for the v1463 three-corpus Node gate.
   Evaluate original runner definitions, but stub only host execution and
   build boundaries. The tests never invoke the Babashka (-main) entry point.
   An independently compiled Node gate remains a separate integration check."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.jvm-cljs-semantics-test :as portable]
            [gesso.live.browser.refinement-node-parity-test :as refinement]
            [gesso.live.browser.refinement-exploration-parity-test :as exploration]))

;; The production script requires babashka.fs; these are only the narrow
;; filesystem boundaries used by its original runner-generation definition.
(defn path [& parts]
  (java.nio.file.Paths/get (str (first parts))
                           (into-array String (map str (rest parts)))))
(defn parent [p] (.getParent (path p)))
(defn create-dirs [p]
  (java.nio.file.Files/createDirectories
   (path p) (make-array java.nio.file.attribute.FileAttribute 0)))

(def ^:private node-file "target/gesso-test/node/browser-tests.js")
(def ^:private portable-prefix "GESSO_PORTABLE_SEMANTICS_CORPUS ")
(def ^:private refinement-prefix "GESSO_BROWSER_REFINEMENT_PARITY ")
(def ^:private exploration-prefix "GESSO_BROWSER_EXPLORATION_PARITY ")
(def ^:private portable-cases (delay (portable/shared-corpus-results)))
(def ^:private refinement-cases (delay (refinement/portable-corpus)))
(def ^:private exploration-cases (delay (exploration/portable-corpus)))

(def ^:private selected-definitions
  '#{test-work-dir generated-test-src fail! gate-failure-key gate-failure!
     required-node-browser-test-namespaces
     portable-semantics-test-namespace portable-corpus-line-prefix
     portable-minimum-cases browser-refinement-corpus-line-prefix
     browser-refinement-case-ids browser-refinement-corpus-namespace
     browser-exploration-corpus-line-prefix browser-exploration-case-ids
     browser-exploration-corpus-namespace
     node-test-plan node-runner-name node-runner-namespace
     require-lines quoted-test-args write-node-runner!
     require-portable-corpus! jvm-portable-corpus!
     require-browser-refinement-corpus! jvm-browser-refinement-corpus!
     require-browser-exploration-corpus! jvm-browser-exploration-corpus!
     *node-browser-captured-run*
     run-node-portable-correspondence!
     run-node-browser-refinement-correspondence!
     run-node-browser-exploration-correspondence!
     run-node-test-groups! run-node-browser-tests!})

(defn- source-forms []
  (with-open [r (java.io.PushbackReader. (io/reader "script/gesso_test.clj"))]
    (loop [acc []]
      (let [f (read r false ::eof)]
        (if (= ::eof f) acc (recur (conj acc f)))))))

(defn- declaration [form]
  (when (and (seq? form) (#{'def 'defn} (first form)))
    (second form)))

(def ^:private source-gate
  (delay
    (let [forms (source-forms)
          present (set (keep declaration forms))
          missing (remove present selected-definitions)]
      (when (seq missing)
        (throw (ex-info "Production gate definitions missing."
                        {:missing (vec missing)})))
      (when (find-ns 'gesso.script.gesso-test)
        (throw (ex-info "Runner namespace unexpectedly occupied."
                        {:namespace 'gesso.script.gesso-test})))
      (let [target (create-ns 'gesso.script.gesso-test)]
        (binding [*ns* target]
          (clojure.core/refer 'clojure.core)
          (alias 'str 'clojure.string)
          (alias 'edn 'clojure.edn)
          (alias 'fs 'gesso.script.gesso-node-three-corpus-gate-test)
          ;; Only genuine host/process and test-discovery seams are stubbed.
          (doseq [symbol '[cljs-test-namespaces portable-choreo-test-namespace?
                           delete-tree-if-exists! node-test-plan!
                           write-node-runner-stub! run-command!
                           run-command-captured-with-timeout! cljs-main-command
                           cljs-test-sdeps]]
            (intern target symbol
                    (fn [& _]
                      (throw (ex-info "Unstubbed host boundary"
                                      {:boundary symbol})))))
          (doseq [form forms
                  :when (contains? selected-definitions (declaration form))]
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
      (throw (ex-info "Original runner Var absent" {:symbol symbol}))))
(defn- gate [symbol & args]
  (apply (var-get (gate-var symbol)) args))
(defn- rejected [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- stubbed [changes thunk]
  (with-redefs-fn
    (into {} (map (fn [[symbol f]] [(gate-var symbol) f]) changes)) thunk))

(defn- corpus-output
  ([] (corpus-output @portable-cases @refinement-cases @exploration-cases))
  ([p r e]
   (str "Testing native Node semantics\n"
        portable-prefix (pr-str p) "\n"
        refinement-prefix (pr-str r) "\n"
        exploration-prefix (pr-str e) "\n"
        "Ran 367 tests containing 2917 assertions.\n0 failures, 0 errors.\n")))

(defn- simulate
  "Exercise production run-node-browser-tests! and its ORIGINAL group runner.
   Only CLI compilation/process execution and independent JVM corpus acquisition
   are replaced. Every browser comparison uses the real production parser."
  [outputs events thunk]
  (let [remaining (atom (vec outputs))
        host {'node-test-plan (constantly {:browser
                                          ['gesso.live.browser.refinement-exploration-parity-test]
                                          :choreo ['gesso.choreo.machine-test]})
              'delete-tree-if-exists!
              (fn [path] (swap! events conj [:delete path]))
              'write-node-runner!
              (fn [group namespaces]
                (swap! events conj [:generate group namespaces])
                {:namespace 'gesso.test-node-browser-runner})
              'cljs-main-command (fn [& args] (into [:cljs] args))
              'run-command! (fn [command]
                              (swap! events conj [:compile command]))
              'run-command-captured-with-timeout!
              (fn [command timeout]
                (swap! events conj [:node command timeout])
                (when (empty? @remaining)
                  (throw (ex-info "Unexpected second Node execution"
                                  {:node-exhausted true})))
                (let [out (first @remaining)]
                  (swap! remaining subvec 1)
                  (if (instance? Throwable out) (throw out)
                      {:out out :err ""})))
              'jvm-portable-corpus!
              (fn [] (swap! events conj [:jvm :portable])
                (pr-str @portable-cases))
              'jvm-browser-refinement-corpus!
              (fn [] (swap! events conj [:jvm :refinement])
                (pr-str @refinement-cases))
              'jvm-browser-exploration-corpus!
              (fn [] (swap! events conj [:jvm :exploration])
                (pr-str @exploration-cases))}]
    (stubbed host thunk)))

(defn- browser! []
  (with-out-str (gate 'run-node-browser-tests!)))
(defn- count-event [events tag]
  (count (filter #(= tag (first %)) @events)))

(deftest source-is-exactly-the-delivered-runner
  (let [forms (source-forms)
        definitions (set (keep declaration forms))]
    (is (= '(-main) (last forms)))
    (is (every? definitions selected-definitions))
    (is (= 'gesso.script.gesso-test (ns-name @source-gate)))
    (is (true? (:dynamic (meta (gate-var '*node-browser-captured-run*)))))
    (is (nil? (var-get (gate-var '*node-browser-captured-run*))))
    (is (contains? (var-get (gate-var 'required-node-browser-test-namespaces))
                   'gesso.live.browser.refinement-exploration-parity-test))
    (is (= 18 (count @exploration-cases)))
    (is (= 14 (count @refinement-cases)))
    (is (<= 941 (count @portable-cases)))))

(deftest generated-node-runner-owns-the-only-main-entrypoint
  (let [tmp (java.nio.file.Files/createTempDirectory
             "gesso-v1464-runner-"
             (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (with-redefs-fn
        {(gate-var 'generated-test-src) (str tmp)}
        #(gate 'write-node-runner! :browser
               ['gesso.assurance.proof.jvm-cljs-semantics-test
                'gesso.live.browser.refinement-node-parity-test
                'gesso.live.browser.refinement-exploration-parity-test]))
      (let [source (slurp (str tmp "/gesso/test_node_browser_runner.cljs"))]
        (is (= 1 (count (re-seq #"GESSO_PORTABLE_SEMANTICS_CORPUS " source))))
        (is (= 1 (count (re-seq #"GESSO_BROWSER_REFINEMENT_PARITY " source))))
        (is (= 1 (count (re-seq #"GESSO_BROWSER_EXPLORATION_PARITY " source))))
        (is (str/includes? source "(set! *main-cli-fn* main)"))
        (is (str/includes? source "refinement-exploration-parity-test/portable-corpus"))
        (is (not (re-find #"(?m)^\(main\)$" source)))
        (is (= 1 (count (re-seq #"\(t/run-tests" source))))
        (is (= 1 (count (re-seq #"\(defn main" source)))))
      (finally
        (doseq [file (reverse (file-seq (.toFile tmp)))]
          (io/delete-file file true))))))

(deftest missing-required-exploration-suite-fails-before-compilation
  (let [required (var-get (gate-var 'required-node-browser-test-namespaces))
        without (disj required
                      'gesso.live.browser.refinement-exploration-parity-test)
        observed (rejected
                  #(stubbed {'cljs-test-namespaces (constantly
                                                    (conj without
                                                          'gesso.choreo.machine-test))
                             'portable-choreo-test-namespace?
                             (fn [ns] (str/starts-with? (str ns) "gesso.choreo."))}
                            (fn [] (gate 'node-test-plan))))]
    (is (= ['gesso.live.browser.refinement-exploration-parity-test]
           (:missing-namespaces observed)))))

(deftest complete-three-corpus-output-passes-only-once
  (let [events (atom [])
        output (simulate [(corpus-output)] events browser!)]
    (is (str/includes? output "Portable JVM/Node semantic corpus: PASS"))
    (is (str/includes? output "Browser refinement JVM/Node corpus: PASS"))
    (is (str/includes? output "Browser exploration JVM/Node corpus: PASS"))
    (is (= 1 (count-event events :node)))
    (is (= [[:jvm :portable] [:jvm :exploration] [:jvm :refinement]]
           (vec (filter #(= :jvm (first %)) @events))))
    (is (= ["node" node-file] (second (first (filter #(= :node (first %)) @events)))))
    (is (nil? (var-get (gate-var '*node-browser-captured-run*))))))

(deftest second-gate-refreshes-all-three-comparisons
  (let [events (atom [])
        output (simulate [(corpus-output) (corpus-output)] events
                         #(do (browser!) (browser!)))]
    (is (str/includes? output "Browser exploration JVM/Node corpus: PASS"))
    (is (= 2 (count-event events :node)))
    (is (= 2 (count (filter #{[:jvm :portable]} @events))))
    (is (= 2 (count (filter #{[:jvm :refinement]} @events))))
    (is (= 2 (count (filter #{[:jvm :exploration]} @events))))
    (is (nil? (var-get (gate-var '*node-browser-captured-run*))))))

(deftest exploration-mismatch-fails-after-portable-without-retrying-node
  (let [events (atom [])
        mutated (assoc-in @exploration-cases [2 1] [:source-substituted])
        failure (simulate [(corpus-output @portable-cases
                                          @refinement-cases mutated)]
                          events #(rejected browser!))]
    (is (= 2 (:first-mismatched-index failure)))
    (is (= (nth @exploration-cases 2) (:jvm-case failure)))
    (is (= (nth mutated 2) (:node-case failure)))
    (is (= 1 (count-event events :node)))
    (is (= [[:jvm :portable] [:jvm :exploration]]
           (vec (filter #(= :jvm (first %)) @events))))))

(deftest missing-exploration-marker-rejects-successful-node-summary
  (let [events (atom [])
        incomplete (str/replace (corpus-output)
                                (str exploration-prefix
                                     (pr-str @exploration-cases) "\n") "")
        failure (simulate [incomplete] events #(rejected browser!))]
    (is (= :node (:host failure)))
    (is (= 0 (:matching-lines failure)))
    (is (= 1 (count-event events :node)))
    (is (= [[:jvm :portable]]
           (vec (filter #(= :jvm (first %)) @events))))))

(deftest duplicate-exploration-marker-rejects-even-with-passing-assertions
  (let [events (atom [])
        duplicate (str (corpus-output)
                       exploration-prefix (pr-str @exploration-cases) "\n")
        failure (simulate [duplicate] events #(rejected browser!))]
    (is (= :node (:host failure)))
    (is (= 2 (:matching-lines failure)))
    (is (= 1 (count-event events :node)))))

(deftest malformed-reordered-or-inert-exploration-corpus-fails
  (let [original @exploration-cases
        altered [[:truncated (pop original)]
                 [:reordered (vec (reverse original))]
                 [:duplicate-id (assoc-in original [1 0] (ffirst original))]
                 [:inert (assoc-in original [1 1 0] :bounded-no-counterexamples)]
                 [:forged-authority (assoc-in original [0 1 12] true)]
                 [:lost-premise (assoc-in original [0 1 14] [])]]]
    (doseq [[kind cases] altered]
      (testing (name kind)
        (let [failure (rejected #(gate 'require-browser-exploration-corpus!
                                       (str exploration-prefix
                                            (pr-str cases) "\n") :node))]
          (is (= :node (:host failure)))
          (is (= (count cases) (:case-count failure))))))
    (doseq [raw ["[" "{malformed" "#=(+ 2 3)"]]
      (let [failure (rejected #(gate 'require-browser-exploration-corpus! 
                                     (str exploration-prefix raw "\n") :jvm))]
        (is (= :jvm (:host failure)))
        (is (contains? failure :cause))))))

(deftest changed-physical-identity-cannot-be-hidden-by-exact-shape
  (let [original @exploration-cases
        modified (assoc-in original [6 1 0] false)
        events (atom [])
        failure (simulate [(corpus-output @portable-cases
                                          @refinement-cases modified)]
                          events #(rejected browser!))]
    (is (= :node (:host failure)))
    (is (= 18 (count (:expected-case-ids failure))))
    (is (= 1 (count-event events :node)))))

(deftest absent-exploration-checker-is-an-explicit-gate-failure
  (let [var (gate-var 'run-node-browser-exploration-correspondence!)
        root (var-get var)
        target @source-gate
        events (atom [])]
    (try
      (ns-unmap target 'run-node-browser-exploration-correspondence!)
      (let [failure (simulate [(corpus-output)] events #(rejected browser!))]
        (is (= 'gesso.live.browser.refinement-exploration-parity-test
               (:required-namespace failure)))
        (is (= 1 (count-event events :node)))
        (is (= [[:jvm :portable]]
               (vec (filter #(= :jvm (first %)) @events)))))
      (finally
        (intern target 'run-node-browser-exploration-correspondence! root)))))

(deftest failure-does-not-reuse-previous-node-corpus
  (let [events (atom [])
        missing (str/replace (corpus-output)
                             (str exploration-prefix
                                  (pr-str @exploration-cases) "\n") "")
        outcome (simulate [missing (corpus-output)] events
                          #(let [failure (rejected browser!)
                                 success (browser!)]
                             [failure success]))]
    (is (= 0 (:matching-lines (first outcome))))
    (is (str/includes? (second outcome)
                       "Browser exploration JVM/Node corpus: PASS"))
    (is (= 2 (count-event events :node)))
    (is (nil? (var-get (gate-var '*node-browser-captured-run*))))))

(deftest jvm-exploration-evidence-is-not-a-second-node-execution
  (let [calls (atom [])
        serialized (pr-str @exploration-cases)
        actual (stubbed
                {'cljs-test-sdeps (constantly "{:aliases {}}")
                 'run-command-captured-with-timeout!
                 (fn [command timeout]
                   (swap! calls conj [command timeout])
                   {:out (str exploration-prefix serialized "\n") :err ""})}
                #(gate 'jvm-browser-exploration-corpus!))
        [command timeout] (first @calls)]
    (is (= serialized actual))
    (is (= 1 (count @calls)))
    (is (= "clojure" (first command)))
    (is (= 180000 timeout))
    (is (str/includes? (last command)
                       "refinement-exploration-parity-test/portable-corpus"))
    (is (not= "node" (first command)))))

(deftest standalone-exploration-uses-independent-node-process
  (let [events (atom [])
        output (simulate [(corpus-output)] events
                         #(with-out-str
                            (gate 'run-node-browser-exploration-correspondence!
                                  node-file)))]
    (is (str/includes? output "Browser exploration JVM/Node corpus: PASS"))
    (is (= [[:node ["node" node-file] 180000] [:jvm :exploration]]
           (vec (filter #(#{:node :jvm} (first %)) @events))))))

(deftest cached-output-path-substitution-rejects-before-host-access
  (let [cache (gate-var '*node-browser-captured-run*)
        events (atom [])
        failure (simulate [] events
                          #(with-bindings
                             {cache (atom {:path node-file
                                           :result {:out (corpus-output) :err ""}})}
                             (rejected (fn []
                                         (gate 'run-node-browser-exploration-correspondence!
                                               "swapped/browser.js")))))]
    (is (= node-file (:captured-path failure)))
    (is (= "swapped/browser.js" (:requested-path failure)))
    (is (zero? (count-event events :node)))
    (is (zero? (count-event events :jvm)))))

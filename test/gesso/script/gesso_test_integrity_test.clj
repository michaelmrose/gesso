(ns gesso.script.gesso-test-integrity-test
  "Permanent v1328 source-executed integration checks for the v1327 Node/JVM
   correspondence gate. The Babashka entrypoint calls (-main) on load; these
   JVM tests read its original Clojure forms and evaluate ONLY selected gate
   definitions in an isolated namespace. They never launch a real build,
   mutate production source, or substitute a hand-written gate implementation.
   The ordinary Node and Chromium gates must still be run on their own hosts."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.jvm-cljs-semantics-test :as portable]))

;; Narrow filesystem adapter used only by extracted original harness forms.
;; This keeps the JVM regression independent of Babashka installation while
;; still executing the exact discovery/runner-generating Clojure code.
(defn path [& parts]
  (java.nio.file.Paths/get (str (first parts))
                          (into-array String (map str (rest parts)))))
(defn parent [p] (.getParent (path p)))
(defn create-dirs [p]
  (java.nio.file.Files/createDirectories
   (path p) (make-array java.nio.file.attribute.FileAttribute 0)))
(defn relativize [root target]
  (.relativize (path root) (path target)))
(defn file-name [p] (.getFileName (path p)))
(defn glob [root pattern]
  (let [suffix (second (re-find #"(\.clj[sc]?)$" pattern))]
    (->> (file-seq (io/file root))
         (filter #(.isFile ^java.io.File %))
         (filter #(and (str/ends-with? (.getName ^java.io.File %) (str "_test" suffix))
                       (or (= suffix ".cljs") (= suffix ".cljc")
                           (= suffix ".clj"))))
         (mapv #(str (.toPath ^java.io.File %))))))

(def ^:private exercised-definitions
  '#{test-work-dir generated-test-src jvm-wall-clock-timeout-ms
     required-node-browser-test-namespaces portable-semantics-test-namespace
     portable-corpus-line-prefix portable-minimum-cases
     prepared-standalone-gates prepared-jvm-runtime
     prepared-subprocess-runtime prepared-jvm-session-command
     prepared-inventory-scopes
     fail! gate-failure! gate-failure? gate-failure-key
     jvm-test-file-pattern jvm-namespace-declaration-pattern
     test-namespace cljs-test-namespaces jvm-test-namespaces
     portable-choreo-test-namespace? node-test-plan
     node-runner-name node-runner-namespace require-lines quoted-test-args
     write-node-runner! require-portable-corpus! jvm-portable-corpus!
     run-node-portable-correspondence! run-node-test-groups!
     command-gates shard-gate-job prepared-runtime-descriptors
     prepared-jvm-job prepared-gate-job shard-prepared-inventory})

(defn- original-forms []
  (with-open [reader (java.io.PushbackReader. (io/reader "script/gesso_test.clj"))]
    (loop [result []]
      (let [form (read reader false ::eof)]
        (if (= ::eof form) result (recur (conj result form)))))))

(defn- declared-name [form]
  (when (and (seq? form) (#{'def 'defn} (first form)))
    (second form)))

(def ^:private gate-namespace
  (delay
    (let [forms (original-forms)
          names (set (keep declared-name forms))
          missing (remove names exercised-definitions)
          isolated (create-ns 'gesso.script.gesso-test-integrity-excerpt)]
      (when (seq missing)
        (throw (ex-info "Tested harness definition is missing."
                        {:missing (vec missing)})))
      (binding [*ns* isolated]
        (clojure.core/refer 'clojure.core)
        (alias 'str 'clojure.string)
        (alias 'edn 'clojure.edn)
        (alias 'fs 'gesso.script.gesso-test-integrity-test)
        ;; These process seams are deliberately stubbed: source code is not
        ;; imported from Babashka or executed as a CLI entry point.
        (doseq [name '[cljs-test-sdeps run-command-captured-with-timeout!
                       delete-tree-if-exists! cljs-main-command run-command!
                       formal-test-namespaces quick-test-namespaces]]
          (intern isolated name (fn [& _] (throw (ex-info "Unstubbed test seam" {:name name})))))
        (doseq [form forms
                :when (contains? exercised-definitions (declared-name form))]
          (eval form)))
      isolated)))

(defn- gate-var [name]
  (or (ns-resolve @gate-namespace name)
      (throw (ex-info "Gate implementation is unavailable." {:name name}))))

(defn- gate [name & args]
  (apply (var-get (gate-var name)) args))

(defn- stubbed [substitutions body]
  (with-redefs-fn (into {} (map (fn [[k v]] [(gate-var k) v]) substitutions)) body))

(defn- rejected [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(def ^:private corpus (delay (portable/shared-corpus-results)))
(defn- corpus-line [cases]
  (str (var-get (gate-var 'portable-corpus-line-prefix))
       (pr-str cases) "\n"))
(defn- corpus-output [cases]
  (str "Testing portable semantics\n" (corpus-line cases)
       "Ran 22 tests containing 1011 assertions.\n0 failures, 0 errors.\n"))
(defn- corpus-check [output]
  (gate 'require-portable-corpus! output :node))

(deftest harness-entrypoint-is-never-evaluated-by-these-tests
  (let [forms (original-forms)
        included (set (keep declared-name forms))]
    (is (= '(-main) (last forms)))
    (is (every? included exercised-definitions))
    (is (identical? @gate-namespace @gate-namespace))
    (is (not= 'gesso.script.gesso-test (ns-name @gate-namespace)))))

(deftest node-discovery-requires-the-permanent-correspondence-namespace
  (let [plan (gate 'node-test-plan)
        target 'gesso.assurance.proof.jvm-cljs-semantics-test]
    (is (some #{target} (:browser plan)))
    (is (= target (var-get (gate-var 'portable-semantics-test-namespace))))
    (is (= (set (:browser plan))
           (var-get (gate-var 'required-node-browser-test-namespaces))))
    (is (= (count (:browser plan)) (count (distinct (:browser plan)))))
    (is (= (count (:choreo plan)) (count (distinct (:choreo plan)))))
    (is (seq (:choreo plan)))
    (is (= (:browser plan) (vec (sort (:browser plan)))))
    (is (= (:choreo plan) (vec (sort (:choreo plan)))))))

(deftest missing-required-test-namespace-fails-before-compilation
  (let [all (gate 'cljs-test-namespaces)
        target 'gesso.assurance.proof.jvm-cljs-semantics-test
        failure (stubbed {'cljs-test-namespaces (fn [] (remove #{target} all))}
                         #(rejected (fn [] (gate 'node-test-plan))))]
    (is (= [target] (:missing-namespaces failure)))
    (is (not (:gesso.script.gesso-test-integrity-excerpt/gate-failure failure)))))

(deftest missing-choreo-corpus-fails-before-compilation
  (let [all (gate 'cljs-test-namespaces)
        failure (stubbed {'cljs-test-namespaces
                          (fn [] (remove (fn [s] (str/starts-with? (str s) "gesso.choreo.")) all))}
                         #(rejected (fn [] (gate 'node-test-plan))))]
    (is (= {} failure))))

(deftest node-discovery-does-not-accidentally-select-all-cljs-tests
  (let [plan (gate 'node-test-plan)
        all (gate 'cljs-test-namespaces)]
    (is (<= (count (:browser plan)) (count all)))
    (is (every? (set all) (:browser plan)))
    (is (every? (set all) (:choreo plan)))
    (is (every? #(str/starts-with? (str %) "gesso.choreo.") (:choreo plan)))))

(deftest generated-node-runner-must-serialize-full-shared-corpus
  (let [tmp (java.nio.file.Files/createTempDirectory
             "gesso-v1328-runner-" (make-array java.nio.file.attribute.FileAttribute 0))
        directory (str tmp)
        plan (gate 'node-test-plan)]
    (try
      (stubbed {'generated-test-src directory}
               #(do
                  (gate 'write-node-runner! :browser (:browser plan))
                  (gate 'write-node-runner! :choreo (:choreo plan))))
      (let [browser (slurp (str tmp "/gesso/test_node_browser_runner.cljs"))
            choreo (slurp (str tmp "/gesso/test_node_choreo_runner.cljs"))]
        (is (str/includes? browser "shared-corpus-results"))
        (is (str/includes? browser "GESSO_PORTABLE_SEMANTICS_CORPUS"))
        (is (str/includes? browser "gesso.assurance.proof.jvm-cljs-semantics-test"))
        (is (str/includes? browser "(t/run-tests"))
        (is (not (str/includes? choreo "GESSO_PORTABLE_SEMANTICS_CORPUS")))
        (is (not (str/includes? choreo "shared-corpus-results")))
        (is (str/includes? choreo "(t/run-tests")))
      (finally
        (doseq [f (reverse (file-seq (.toFile tmp)))]
          (io/delete-file f true))))))

(deftest portable-corpus-exactly-requires-one-marker
  (is (= (pr-str @corpus) (corpus-check (corpus-output @corpus))))
  ;; v1326 established a floor, not a fixed corpus size. New portable cases
  ;; must not break the integrity gate merely because coverage grew.
  (is (<= (var-get (gate-var 'portable-minimum-cases)) (count @corpus)))
  (doseq [[out expected]
          [["" 0]
           ["some unrelated test result\n" 0]
           [(str (corpus-line @corpus) (corpus-line @corpus)) 2]]]
    (is (= expected (:matching-lines (rejected #(corpus-check out)))))))

(deftest truncated-and-incomplete-corpora-fail-closed
  (let [minimum (var-get (gate-var 'portable-minimum-cases))]
    (doseq [cases [[]
                   (subvec @corpus 0 (dec minimum))
                   (vec (repeat minimum ["duplicate" [:ok true]]))]]
      (is (some? (:minimum-cases (rejected #(corpus-check (corpus-output cases))))))))
  (is (= (count @corpus)
         (count (edn/read-string (corpus-check (corpus-output @corpus))))))
  (is (nil? (rejected #(corpus-check (corpus-output (conj @corpus ["extra" [:ok true]])))))))

(deftest malformed-corpus-edn-rejected
  (doseq [data ["[" "{broken" "#=(+ 1 2)"]]
    (let [failure (rejected #(corpus-check (str "GESSO_PORTABLE_SEMANTICS_CORPUS " data "\n")))]
      (is (= :node (:host failure)))
      (is (contains? failure :cause)))))

(deftest malformed-corpus-entries-rejected
  (let [mutations [(assoc @corpus 0 nil)
                   (assoc @corpus 0 [123 [:ok true]])
                   (assoc @corpus 0 ["new" :ok])
                   (assoc @corpus 0 ["new" [:other true]])
                   (assoc @corpus 0 ["new"])
                   (assoc-in @corpus [0 0] (first (second @corpus)))]]
    (doseq [cases mutations]
      (is (some? (:minimum-cases (rejected #(corpus-check (corpus-output cases)))))))))

(deftest host-is-recorded-for-invalid-corpus
  (let [node (rejected #(gate 'require-portable-corpus! "bad" :node))
        jvm (rejected #(gate 'require-portable-corpus! "bad" :jvm))]
    (is (= :node (:host node)))
    (is (= :jvm (:host jvm)))))

(deftest exact-correspondence-is-required-before-gate-success
  (let [node (corpus-output @corpus)
        events (atom [])]
    (is (nil? (stubbed {'run-command-captured-with-timeout!
                        (fn [cmd ms] (swap! events conj [cmd ms]) {:out node :err ""})
                        'jvm-portable-corpus! (fn [] (pr-str @corpus))}
                       #(gate 'run-node-portable-correspondence! "test-browser.js"))))
    (is (= [["node" "test-browser.js"]] (mapv first @events)))
    (is (= [180000] (mapv second @events)))))

(deftest first-and-last-mismatches-are-localized
  (let [size (count @corpus)]
    (doseq [index [0 1 (quot (dec size) 2) (dec size)]]
      (let [cases (assoc-in @corpus [index 1] [:ok :intentionally-wrong])
          failure (stubbed {'run-command-captured-with-timeout!
                            (fn [& _] {:out (corpus-output cases) :err ""})
                            'jvm-portable-corpus! (fn [] (pr-str @corpus))}
                           #(rejected (fn []
                                        (gate 'run-node-portable-correspondence! "node.js"))))]
        (is (= index (:first-mismatched-index failure)))
        (is (= size (:jvm-case-count failure)))
        (is (= size (:node-case-count failure)))
        (is (= (nth @corpus index) (:jvm-case failure)))
        (is (= (nth cases index) (:node-case failure)))))))

(deftest missing-tail-after-minimum-still-fails-correspondence
  (let [size (count @corpus)
        minimum (var-get (gate-var 'portable-minimum-cases))]
    (when (> size minimum)
      (let [cases (pop @corpus)
            failure (stubbed {'run-command-captured-with-timeout!
                              (fn [& _] {:out (corpus-output cases) :err ""})
                              'jvm-portable-corpus! (fn [] (pr-str @corpus))}
                             #(rejected (fn []
                                          (gate 'run-node-portable-correspondence! "node.js"))))]
        (is (= size (:jvm-case-count failure)))
        (is (= (dec size) (:node-case-count failure)))
        (is (nil? (:first-mismatched-index failure)))))))

(deftest no-silent-success-on-node-parse-failure
  (is (some? (stubbed {'run-command-captured-with-timeout!
                      (fn [& _] {:out "Testing only\n" :err ""})
                      'jvm-portable-corpus! (fn [] (pr-str @corpus))}
                     #(rejected (fn []
                                  (gate 'run-node-portable-correspondence! "node.js")))))))

(deftest source-produced-jvm-command-still-uses-original-corpus-namespace
  (let [seen (atom nil)
        result (stubbed {'cljs-test-sdeps (fn [] "{:aliases {:gesso-cljs-test {}}}")
                         'run-command-captured-with-timeout!
                         (fn [cmd ms]
                           (reset! seen [cmd ms])
                           {:out (corpus-output @corpus) :err ""})}
                        #(gate 'jvm-portable-corpus!))]
    (is (= (pr-str @corpus) result))
    (is (= "clojure" (first (first @seen))))
    (is (= 180000 (second @seen)))
    (is (str/includes? (last (first @seen)) "shared-corpus-results"))
    (is (str/includes? (last (first @seen)) "gesso.assurance.proof.jvm-cljs-semantics-test"))))

(deftest node-group-execution-compares-browser-and-not-choreo
  (let [calls (atom [])]
    (stubbed {'node-test-plan (fn [] {:browser ['gesso.assurance.proof.jvm-cljs-semantics-test]
                                     :choreo ['gesso.choreo.type-test]})
              'test-work-dir "target/gesso-v1328-mock"
              'delete-tree-if-exists! (fn [& _] nil)
              'write-node-runner! (fn [group namespaces]
                                    (swap! calls conj [:runner group namespaces])
                                    {:namespace 'test.generated})
              'cljs-main-command (fn [& xs] (into ["clojure"] xs))
              'run-command! (fn [cmd] (swap! calls conj [:command cmd]))
              'run-node-portable-correspondence! (fn [path]
                                                   (swap! calls conj [:compare path]))}
             #(gate 'run-node-test-groups! [:browser :choreo]))
    (is (= 2 (count (filter #(= :runner (first %)) @calls))))
    (is (= 3 (count (filter #(= :command (first %)) @calls))))
    (is (= 1 (count (filter #(= :compare (first %)) @calls))))
    (is (= "target/gesso-v1328-mock/node/browser-tests.js"
           (second (first (filter #(= :compare (first %)) @calls)))))
    (is (= ["node" "target/gesso-v1328-mock/node/choreo-tests.js"]
           (second (last @calls))))))

(deftest absent-node-browser-group-rejected-before-subprocess
  (let [commands (atom 0)
        failure (stubbed {'node-test-plan (fn [] {:browser []})
                          'delete-tree-if-exists! (fn [& _] nil)
                          'run-command! (fn [& _] (swap! commands inc))}
                         #(rejected (fn [] (gate 'run-node-test-groups! [:browser]))))]
    (is (= :browser (:group-id failure)))
    (is (zero? @commands))))

(deftest public-command-scopes-preserve-node-gate
  (doseq [scope ["all" "cljs" "node" "node:browser"]]
    (let [gates (gate 'command-gates scope)]
      (is (= :integrity (first gates)))
      (is (= 1 (count (filter #{:node:browser} gates))))
      (is (= 1 (count (filter #{:integrity} gates))))))
  (is (not (some #{:node:browser} (gate 'command-gates "node:choreo")))))

(deftest prepared-ygg-inventory-preserves-node-gate-once
  (doseq [scope ["all" "cljs" "node" "node:browser"]]
    (let [inventory (stubbed {'jvm-test-namespaces
                              (fn [] ['gesso.model.fake-test])}
                             #(gate 'shard-prepared-inventory scope))
          jobs (:jobs inventory)
          node-jobs (filter #(= :node:browser (:gate %)) jobs)]
      (is (= 1 (count node-jobs)))
      (is (= "gate:prepared" (get-in (first node-jobs) [:command 2])))
      (is (not (some #(= :integrity (:gate %)) jobs)))
      (is (= (count jobs) (count (set (map :id jobs)))))))
  (let [only (gate 'shard-prepared-inventory "node:browser")]
    (is (= #{:node:browser} (set (map :gate (:jobs only))))))
  (is (some? (rejected #(gate 'shard-prepared-inventory "unsupported")))))

(deftest all-original-sources-are-still-required-for-cross-host-evidence
  (is (contains? (set (gate 'cljs-test-namespaces))
                 'gesso.assurance.proof.jvm-cljs-semantics-test))
  (is (contains? (set (gate 'jvm-test-namespaces))
                 'gesso.script.gesso-test-integrity-test))
  (is (= :node:browser (-> (gate 'shard-prepared-inventory "node:browser")
                           :jobs first :gate))))

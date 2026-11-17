(ns gesso.script.check-choreo-node-inventory
  "Fail-closed source audit for the branch-scoped portable Choreo Node runner.
   Use the same .cljs/.cljc discovery convention as script/gesso_test.clj;
   do not evaluate the runner or load its CLJS dependencies."
  (:require [babashka.fs :as fs]
            [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def runner-path
  "test/gesso/choreo/typed_nominal_transition_node_runner.cljs")

(defn fail! [message data]
  (throw (ex-info message (assoc data :choreo-node-inventory/failure true))))

(defn exactly-one [label xs]
  (let [xs (vec xs)]
    (when-not (= 1 (count xs))
      (fail! "Missing or duplicate runner source form."
             {:form label :found (count xs)}))
    (first xs)))

(defn read-forms [source]
  (with-open [reader (java.io.PushbackReader.
                      (java.io.StringReader. source))]
    (binding [*read-eval* false]
      (loop [forms []]
        (let [form (read {:read-cond :allow
                          :features #{:cljs}
                          :eof ::end} reader)]
          (if (= ::end form)
            forms
            (recur (conj forms form))))))))

(defn declared-test-ns [path]
  ;; Read only the initial declaration; forms later in a portable test may
  ;; use aliases and reader features that require its namespace to be loaded.
  (with-open [reader (java.io.PushbackReader. (io/reader (str path)))]
    (let [form (binding [*read-eval* false]
                 (read {:read-cond :allow
                        :features #{:cljs}
                        :eof nil} reader))]
      (when-not (and (seq? form)
                     (= 'ns (first form))
                     (symbol? (second form)))
        (fail! "Portable test lacks a readable leading ns declaration."
               {:file (str path)}))
      (str (second form)))))

(defn portable-inventory []
  (let [files (->> (file-seq (io/file "test/gesso/choreo"))
                   (filter #(.isFile %))
                   (filter #(re-matches #".*_test\.clj[cs]$" (.getName %)))
                   (sort-by str)
                   vec)
        entries (mapv
                 (fn [path]
                   (let [relative (str (fs/relativize "test/gesso/choreo" path))
                         expected (str "gesso.choreo."
                                       (-> relative
                                           (str/replace #"\.clj[cs]$" "")
                                           (str/replace #"[\\/]" ".")
                                           (str/replace "_" "-")))
                         declared (declared-test-ns path)]
                     (when-not (= expected declared)
                       (fail! "Portable test namespace disagrees with file path."
                              {:file (str path)
                               :expected expected
                               :declared declared}))
                     declared))
                 files)]
    (when (or (empty? files)
              (not= (count files) (count (distinct entries))))
      (fail! "Empty or duplicate portable Choreo namespace inventory."
             {:file-count (count files)
              :namespaces entries}))
    (set entries)))

(defn source-form? [form op name]
  (and (seq? form)
       (= op (first form))
       (= name (second form))))

(defn quoted-namespace [value]
  (when-not (and (seq? value)
                 (= 'quote (first value))
                 (= 2 (count value))
                 (symbol? (second value)))
    (fail! "Node runner must call literal, quoted namespaces."
           {:observed value}))
  (str (second value)))

(defn check-set! [label expected observed]
  (when (or (not= expected (set observed))
            (not= (count expected) (count observed)))
    (fail! "Portable Node runner inventory does not match repository discovery."
           {:source label
            :missing (sort (remove (set observed) expected))
            :unexpected (sort (remove expected observed))
            :expected-count (count expected)
            :observed-count (count observed)})))

(defn audit! [runner-source inventory]
  (let [forms (read-forms runner-source)
        ns-form (exactly-one
                 :ns
                 (filter #(and (seq? %) (= 'ns (first %))) forms))
        requires (exactly-one
                  :require
                  (filter #(and (seq? %) (= :require (first %)))
                          (drop 2 ns-form)))
        imported (->> (rest requires)
                      (map first)
                      (filter symbol?)
                      (map str)
                      (filter #(str/starts-with? % "gesso.choreo."))
                      vec)
        required-form (exactly-one
                       :required-namespaces
                       (filter #(source-form? % 'def 'required-namespaces)
                               forms))
        required (nth required-form 2 nil)
        _ (when-not (and (set? required)
                         (every? string? required))
            (fail! "Runner required-namespaces must be a literal string set."
                   {:value required}))
        main-form (exactly-one
                   :main
                   (filter #(source-form? % 'defn 'main) forms))
        invocations (->> (tree-seq coll? seq main-form)
                         (filter #(and (seq? %)
                                       (= 't/run-tests (first %))))
                         vec)
        run-form (exactly-one :t/run-tests invocations)
        _ (when-not (= '(t/empty-env ::reporter) (second run-form))
            ;; Compare function shape rather than fully qualified ::reporter:
            ;; Babashka resolves auto-qualified reader keywords in its own ns.
            (when-not (= 't/empty-env (first (second run-form)))
              (fail! "Node runner no longer uses the expected test environment."
                     {:run-form run-form})))
        called (mapv quoted-namespace (drop 2 run-form))]
    (check-set! :requires inventory imported)
    (check-set! :required-namespaces inventory required)
    (check-set! :run-tests inventory called)
    (count inventory)))

(defn expect-rejected! [label thunk]
  (try
    (thunk)
    (fail! "Self-test mutation unexpectedly passed."
           {:mutation label :audit-unexpected-pass true})
    (catch clojure.lang.ExceptionInfo e
      (when (or (not (:choreo-node-inventory/failure (ex-data e)))
                (:audit-unexpected-pass (ex-data e)))
        (throw e)))))

(defn replace-once [source target replacement]
  (when-not (= 2 (count (str/split source
                                  (re-pattern (java.util.regex.Pattern/quote target))
                                  -1)))
    (fail! "Self-test source mutation is missing or ambiguous."
           {:target target}))
  (str/replace source target replacement))

(defn self-test! [source inventory]
  (audit! source inventory)
  (let [sample (first (sort inventory))
        extra "gesso.choreo.prospective-test"]
    (expect-rejected! :new-suite
                      #(audit! source (conj inventory extra)))
    (expect-rejected! :deleted-suite
                      #(audit! source (disj inventory sample)))
    (expect-rejected!
     :omitted-execution
     #(audit! (replace-once source
                           (str "               '" sample "\n")
                           "") inventory))
    (expect-rejected!
     :omitted-import
     #(audit! (replace-once source
                           (str "            [" sample "]\n")
                           "") inventory))
    (expect-rejected!
     :omitted-required-set
     #(audit! (replace-once source
                           (str "    \"" sample "\"\n")
                           "") inventory)))
  (println "Portable Choreo Node inventory negative cases: PASS (5 mutations)"))

(defn -main [& args]
  (when-not (contains? #{[] ["--self-test"]} (vec args))
    (fail! "Usage: bb script/check_choreo_node_inventory.clj [--self-test]"
           {:args args}))
  (let [source (slurp runner-path)
        inventory (portable-inventory)]
    (when (= ["--self-test"] (vec args))
      (self-test! source inventory))
    (println "Portable Choreo Node inventory: PASS ("
             (audit! source inventory) "namespaces)")))

(apply -main *command-line-args*)

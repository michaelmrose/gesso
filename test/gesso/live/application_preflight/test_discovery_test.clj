(ns gesso.live.application-preflight.test-discovery-test
  "Guard the independent registration of application-preflight regression tests.

   A nested deftest was found during the application-preflight decomposition:
   it looked like a test in the source, but clojure.test did not register it
   when the namespace loaded. Check the actual source forms against the
   registered test Vars rather than maintaining a second list of test names."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def ^:private preflight-test-root
  "test/gesso/live/application_preflight")

(def ^:private remaining-integration-test
  "test/gesso/live/application_preflight_test.clj")

(defn- preflight-test-files
  []
  (let [directory (io/file preflight-test-root)]
    (when-not (.isDirectory directory)
      (throw
       (ex-info "Application-preflight test directory is missing."
                {:directory preflight-test-root})))
    (let [nested-files (->> (file-seq directory)
                            (filter #(.isFile %))
                            (filter #(str/ends-with? (.getName %) "_test.clj")))]
      (->> (conj (vec nested-files) (io/file remaining-integration-test))
           (sort-by #(.getPath %))
           vec))))

(defn- namespace-for-test-file
  [file]
  (let [path (-> (.getPath file)
                 (str/replace #"\\" "/"))]
    (when-not (and (str/starts-with? path "test/")
                   (str/ends-with? path "_test.clj"))
      (throw (ex-info "Unexpected application-preflight test path."
                      {:path path})))
    (-> (subs path (count "test/") (- (count path) (count ".clj")))
        (str/replace "/" ".")
        (str/replace "_" "-")
        symbol)))

(defn- source-forms
  [file ns-name]
  ;; Read in the namespace's normal reader context so auto-resolved keywords
  ;; use the aliases established when the namespace was required.
  (let [ns-object (the-ns ns-name)
        eof (Object.)]
    (with-open [reader (clojure.lang.LineNumberingPushbackReader.
                        (io/reader file))]
      (binding [*ns* ns-object
                *read-eval* false]
        (loop [forms []]
          (let [form (read {:eof eof} reader)]
            (if (identical? eof form)
              forms
              (recur (conj forms form)))))))))

(defn- deftest-form?
  [value]
  (and (seq? value)
       (symbol? (first value))
       (contains? #{'deftest 'clojure.test/deftest}
                  (first value))))

(defn- all-deftest-names
  [form]
  ;; Do not macroexpand: a regression here is about the literal source tree.
  ;; Collect nested deftest forms too, to detect tests that would only register
  ;; as a side effect of running some enclosing test.
  (let [names (transient [])]
    (letfn [(visit [node]
              ;; Quoted source examples are data, not executable test forms.
              (when-not (and (seq? node) (= 'quote (first node)))
                (when (deftest-form? node)
                  (conj! names (second node)))
                (when (coll? node)
                  (doseq [child (if (map? node)
                                  (mapcat identity node)
                                  node)]
                    (visit child)))))]
      (visit form))
    (persistent! names)))

(deftest discovery-detects-nested-test-declarations
  (is (= ['visible]
         (all-deftest-names '(deftest visible (is true)))))
  (is (= ['outer 'hidden]
         (all-deftest-names
          '(deftest outer
             (let [x 1]
               (deftest hidden (is (= x 1))))))))
  (is (empty?
       (all-deftest-names
        '(quote (deftest example-only (is true))))))
  (is (empty? (all-deftest-names '(defn helper [] true)))))

(deftest every-preflight-deftest-is-top-level-and-registered
  (let [files (preflight-test-files)]
    (is (some #(= remaining-integration-test (.getPath %)) files))
    (is (<= 2 (count files)) "Both the main and extracted suites must exist.")
    (doseq [file files]
      (let [namespace (namespace-for-test-file file)]
        (testing (str namespace)
          (require namespace)
          (let [forms (source-forms file namespace)
                top-level (->> forms
                               (filter deftest-form?)
                               (map second)
                               vec)
                any-level (vec (mapcat all-deftest-names forms))
                registered (->> (ns-publics namespace)
                                (keep (fn [[name var]]
                                        (when (:test (meta var)) name)))
                                set)]
            (is (= (count any-level) (count top-level))
                (str "Every deftest must occur at top level: " file))
            (is (= (count top-level) (count (set top-level)))
                (str "Duplicate deftest names: " file))
            (is (= (set top-level) registered)
                (str "Source tests must register independently: " file))))))))

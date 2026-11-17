(ns gesso.script.repo-integrity-metadata-test
  "Regression coverage for the metadata-aware namespace reader used by the
   actual Fish repository-integrity preflight. The Babashka expression is
   extracted from that script, not reimplemented for this test. This is not
   a substitute for executing the full Fish/Ygg preflight."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private integrity-script "script/repo_integrity.fish")
(def ^:private typed-pilots
  ["test/gesso/choreo/typed_binding_pilot.clj"
   "test/gesso/choreo/typed_identity_pilot.clj"])

(defn- script-source []
  (slurp integrity-script))

(defn- reader-expression []
  ;; Capture the exact -e program which the Fish fallback executes. If the
  ;; script stops invoking the checked reader, this regression fails closed.
  (let [source (script-source)
        match (re-find
               #"(?s)set -l parsed_ns \(command bb -e '(.+?)' -- \"\$file\" 2>/dev/null\)"
               source)]
    (when-not match
      (throw (ex-info "Metadata-aware namespace reader is missing from preflight."
                      {:script integrity-script})))
    (second match)))

(defn- read-namespace-via-preflight [path]
  ;; bb is already a prerequisite of script/gesso_test.clj and the Fish gate.
  ;; Run the exact program used in production, including its argument handling.
  (sh/sh "bb" "-e" (reader-expression) "--" (str path)))

(defn- canonical-path [tree declared extension]
  (str tree "/"
       (-> (str declared)
           (str/replace "." "/")
           (str/replace "-" "_"))
       "." extension))

(defn- with-temporary-source [contents f]
  (let [path (java.nio.file.Files/createTempFile
              "gesso-repo-integrity-metadata-" ".clj"
              (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (spit (.toFile path) contents)
      (f (str path))
      (finally
        (io/delete-file (.toFile path) true)))))

(deftest preflight-recognizes-both-real-typed-pilot-namespaces
  (let [script (script-source)]
    (is (str/includes? script "if string match -qr '^\\^' -- \"$actual_ns\""))
    (is (str/includes? script "set actual_ns (string trim -- \"$parsed_ns\")")))
  (let [outputs (mapv read-namespace-via-preflight typed-pilots)
        declared (mapv (comp str/trim :out) outputs)]
    (is (= [0 0] (mapv :exit outputs)) (pr-str outputs))
    (is (= ["gesso.choreo.typed-binding-pilot"
            "gesso.choreo.typed-identity-pilot"]
           declared))
    (is (= typed-pilots
           (mapv #(canonical-path "test" % "clj") declared)))
    (is (= (count declared) (count (distinct declared)))
        "The two source files must not appear to declare one namespace.")))

(deftest preflight-reader-rejects-invalid-metadata-bearing-forms
  (doseq [[label source]
          [[:non-symbol-name "(ns ^{:typed.clojure true} 42)"]
           [:not-an-ns "(do (ns ^{:typed.clojure true} wrong.namespace))"]
           [:truncated-form "(ns ^{:typed.clojure true} missing.namespace"]
           [:reader-eval "#=(+ 1 2)"]]]
    (testing (name label)
      (with-temporary-source
        source
        (fn [path]
          (let [{:keys [exit out err]} (read-namespace-via-preflight path)]
            (is (not= 0 exit) (str "Unexpected success: " out))
            (is (str/blank? out))
            (is (not (str/blank? err)))))))))

(deftest preflight-reader-accepts-ordinary-symbol-and-metadata
  (doseq [[source expected]
          [["(ns plain.sample)" "plain.sample"]
           ["(ns ^{:typed.clojure true} typed.sample)" "typed.sample"]
           ["(ns ^:typed.clojure another.sample)" "another.sample"]]]
    (with-temporary-source
      source
      (fn [path]
        (let [{:keys [exit out err]} (read-namespace-via-preflight path)]
          (is (zero? exit) err)
          (is (= expected (str/trim out))))))))

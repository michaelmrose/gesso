(ns gesso.assurance.typed-induction-dependency-node-check
  "Opt-in original-source Typed Clojure checks for induction graph node assembly.

   The original source produces descriptive raw graph nodes first; ONLY the
   dedicated admit-node step attaches live AdmissionHandle objects. A handle
   remains structural, not proof, theorem admission, or runtime authority.
   This pilot assumes an internal raw-subject HMap; checking arbitrary
   untrusted catalog declarations and source-relative graph closure remains
   the responsibility of the original runtime validators."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.proof.induction-dependency :as induction-deps]
            [gesso.assurance.subject]
            [typed.clojure]))

(def ^:private config {:max-processors 1 :check-form-eval :never})
(def ^:private source-ns 'gesso.assurance.proof.induction-dependency)
(def ^:private admission-handle 'gesso.assurance.admission.AdmissionHandle)
(def ^:private subject-interface
  '(typed.clojure/Map typed.clojure/Any typed.clojure/Any))
(def ^:private raw-subject
  `(typed.clojure/HMap
    :mandatory {:kind typed.clojure/Kw :id typed.clojure/Kw
                :interface ~subject-interface}
    :complete? true))
(def ^:private rule-subject
  `(typed.clojure/HMap
    :mandatory {:kind (typed.clojure/Val :rule) :id typed.clojure/Kw
                :interface ~subject-interface}
    :complete? true))
(def ^:private edge-type
  '(typed.clojure/HMap
    :mandatory {:id typed.clojure/Kw :fingerprint typed.clojure/Str}
    :complete? true))
(def ^:private raw-node-type
  `(typed.clojure/HMap
    :mandatory {:id typed.clojure/Kw :subject ~raw-subject
                :depends-on (typed.clojure/Vec ~edge-type)}
    :complete? true))
(def ^:private admitted-node-type
  `(typed.clojure/HMap
    :mandatory {:id typed.clojure/Kw :subject ~raw-subject
                :subject-handle ~admission-handle
                :depends-on (typed.clojure/Vec ~edge-type)}
    :complete? true))
(def ^:private subject-description
  '(typed.clojure/HMap
    :mandatory {:fingerprint typed.clojure/Str} :complete? false))

(defn- original-definitions []
  (let [file (or (io/resource "gesso/assurance/proof/induction_dependency.clj")
                 (throw (ex-info "Induction dependency source missing" {})))]
    (with-open [reader (java.io.PushbackReader. (io/reader file))]
      (loop [acc []]
        (let [form (read {:eof ::end} reader)]
          (if (= ::end form) acc (recur (conj acc form))))))))

(defn- original [forms name]
  (let [matches (filter #(and (seq? %) (#{'defn 'defn-} (first %))
                              (= name (second %))) forms)]
    (when-not (= 1 (count matches))
      (throw (ex-info "Missing or duplicate original induction function"
                      {:name name :matches (count matches)})))
    (first matches)))

(defn- fn-ann! [name signature]
  (eval (list 'typed.clojure/ann name
              (list 'typed.clojure/IFn signature))))

(defn- install-contracts! []
  (eval (list 'typed.clojure/ann-datatype admission-handle '[]))
  (doseq [[name signature]
          [['gesso.assurance.subject/describe-subject
            ['typed.clojure/Any '-> subject-description]]
           ['gesso.assurance.admission/admit-subject!
            [raw-subject '-> admission-handle]]
           ['gesso.assurance.proof.induction-dependency/raw-node
            ['typed.clojure/Kw subject-interface '-> rule-subject]]
           ['gesso.assurance.proof.induction-dependency/fingerprint
            [raw-subject '-> 'typed.clojure/Str]]
           ['gesso.assurance.proof.induction-dependency/edge
            [raw-subject '-> edge-type]]
           ;; Avoid pretending sort-by's numeric-key checker annotation
           ;; establishes ordering of string-encoded source IDs.
           ['gesso.assurance.proof.induction-dependency/ordered-dependencies
            [(list 'typed.clojure/Seqable raw-subject)
             '-> (list 'typed.clojure/Seq raw-subject)]]
           ['gesso.assurance.proof.induction-dependency/make-node
            [raw-subject (list 'typed.clojure/Seqable raw-subject)
             '-> raw-node-type]]
           ['gesso.assurance.proof.induction-dependency/admit-node
            [raw-node-type '-> admitted-node-type]]]]
    (fn-ann! name signature)))

(defn- check! [checker label form]
  (let [{:keys [ex type-errors]} (checker form :check-config config)]
    (when (or ex (seq type-errors))
      (throw (ex-info "Induction dependency source failed static checking"
                      {:label label :errors (mapv str type-errors)} ex)))
    true))

(defn- reject! [checker label form]
  (let [{:keys [ex type-errors]} (checker form :check-config config)
        kinds (mapv :type-error type-errors)]
    (when-not (and (nil? ex) (seq kinds)
                   (every? #{:typed.clojure/type-mismatch-error
                             :typed.clojure/app-type-error} kinds))
      (throw (ex-info "Invalid induction node escaped static rejection"
                      {:label label :actual kinds
                       :exception (some-> ex ex-message)} ex)))
    true))

(defn- change-result [definition update-result]
  (let [original-result (last definition)]
    (when-not (map? original-result)
      (throw (ex-info "Original induction node is no longer a map"
                      {:function (second definition)})))
    (let [changed (update-result original-result)]
      (when (= changed original-result)
        (throw (ex-info "Negative mutation did not change source" {})))
      (apply list (concat (butlast definition) [changed])))))

(defn- check-builder-handoff! [forms]
  ;; If this call moves or changes, the checker must not continue checking
  ;; a now-disconnected helper and claiming production path coverage.
  (let [hits (atom [])]
    (walk/postwalk
     (fn [form]
       (when (and (seq? form) (= 'mapv (first form))
                  (= 'admit-node (second form)))
         (swap! hits conj form))
       form)
     (original forms 'build-induction-dependency-graph!))
    (when-not (= ['(mapv admit-node nodes)] @hits)
      (throw (ex-info "Production induction graph handle handoff changed"
                      {:found @hits}))))
  true)

(defn- check-production! [checker forms]
  (binding [*ns* (the-ns source-ns)]
    (doseq [name '[raw-node fingerprint edge make-node admit-node]]
      (check! checker [:original name] (original forms name)))
    (check-builder-handoff! forms)
    (check! checker :valid-raw-node-construction
            '(make-node (raw-node :typed/child {}) [(raw-node :typed/parent {})]))
    (check! checker :valid-raw-to-admitted
            '(admit-node (make-node (raw-node :typed/child {}) [])))
    (doseq [[label name alteration]
            [[:wrong-raw-kind 'raw-node #(assoc % :kind :proof)]
             [:raw-authority-injection 'raw-node #(assoc % :authority :granted)]
             [:raw-missing-interface 'raw-node #(dissoc % :interface)]
             [:edge-no-fingerprint 'edge #(dissoc % :fingerprint)]
             [:edge-wrong-id 'edge #(assoc % :id '(fingerprint raw))]
             [:raw-node-no-subject 'make-node #(dissoc % :subject)]
             [:raw-node-fake-handle 'make-node #(assoc % :subject-handle 'raw)]
             [:raw-node-pretend-proof 'make-node #(assoc % :proof-status :proved)]
             [:raw-node-bad-edges 'make-node #(assoc % :depends-on '[raw])]]]
      (reject! checker label
               (change-result (original forms name) alteration)))
    ;; A deliberate error in the original admit-node body is caught without
    ;; granting proof authority to the legitimate AdmissionHandle.
    (let [source (original forms 'admit-node)
          invalid (walk/postwalk
                   (fn [form]
                     (if (and (seq? form) (= 'admission/admit-subject! (first form)))
                       '(:subject node)
                       form))
                   source)]
      (when (= source invalid)
        (throw (ex-info "Original induction issuer disappeared" {})))
      (reject! checker :admit-node-returns-raw-subject invalid))
    (reject! checker :unadmitted-to-admit-node '(admit-node {}))
    (reject! checker :admitted-to-make-node
             '(make-node (admit-node (make-node (raw-node :typed/id {}) [])) []))
    true))

(defn- private-fn [name]
  (or (ns-resolve source-ns name)
      (throw (ex-info "Induction dependency private function missing"
                      {:name name}))))

(defn- check-runtime! []
  (let [raw (fn [id] {:kind :rule :id id :interface {:role :induction-source}})
        sources (mapv raw [:typed/z :typed/a :typed/m :typed/a])
        ordered (private-fn 'ordered-dependencies)
        make-node (private-fn 'make-node)
        admit-node (private-fn 'admit-node)]
    (doseq [xs [sources [] (vec (reverse sources)) [(raw :typed/z)]]]
      (when-not (= (vec (sort-by (comp str :id) xs)) (vec (ordered xs)))
        (throw (ex-info "Induction graph node ordering changed" {:input xs}))))
    (let [raw-node (make-node (raw :typed/child) sources)
          current (admit-node raw-node)
          expected-edges (mapv (private-fn 'edge)
                               (sort-by (comp str :id) sources))]
      (when-not (and (= #{:id :subject :depends-on}
                        (set (keys raw-node)))
                     (= #{:id :subject :subject-handle :depends-on}
                        (set (keys current)))
                     (= (dissoc current :subject-handle) raw-node)
                     (= expected-edges (:depends-on current))
                     (admission/admitted? (:subject-handle current))
                     (identical? (:subject-handle current)
                                 (admission/require-admitted-subject!
                                  (:subject-handle current) (:subject current))))
        (throw (ex-info "Induction graph node admission changed" {})))))
  true)

(defn -main [& _]
  (install-contracts!)
  (let [checker (requiring-resolve 'typed.clj.checker/check-form-info)
        forms (original-definitions)]
    (check-production! checker forms)
    (check-runtime!)
    (println "Typed induction dependency assembly passed: five original")
    (println "source bodies, source-linked builder handoff, rejected negative")
    (println "mutations and live structural admission; no theorem claim.")
    :ok))

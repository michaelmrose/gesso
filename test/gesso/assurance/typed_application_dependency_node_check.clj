(ns gesso.assurance.typed-application-dependency-node-check
  "Opt-in source-backed typing of application dependency graph node creation.

   Check complete original raw-node, fingerprint, edge, and node definitions
   under contracts for their expected internal inputs. In particular node
   must issue a real AdmissionHandle for :subject-handle and produce exactly
   the HMap later consumed by the dependency graph validator.

   The raw subject's conformance to this input contract, the semantic
   validity of its fingerprint, and authority of any issued handle are NOT
   established by these annotations; existing runtime admission and graph
   validation remain mandatory. The small ordered-sources runtime helper is
   independently compared to its original sort-by expression because this
   Typed Clojure release's sort-by annotation accepts only numeric keys."
  (:require [clojure.java.io :as io]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.proof.application-dependency :as app]
            [gesso.assurance.subject]
            [typed.clojure]))

(def ^:private config {:max-processors 1 :check-form-eval :never})
(def ^:private source-ns 'gesso.assurance.proof.application-dependency)
(def ^:private admission-handle 'gesso.assurance.admission.AdmissionHandle)
(def ^:private interface '(typed.clojure/Map typed.clojure/Any typed.clojure/Any))
(def ^:private raw-subject
  `(typed.clojure/HMap
    :mandatory {:kind (typed.clojure/Val :rule)
                :id typed.clojure/Kw
                :interface ~interface}
    :complete? true))
(def ^:private edge-type
  '(typed.clojure/HMap
    :mandatory {:id typed.clojure/Kw :fingerprint typed.clojure/Str}
    :complete? true))
(def ^:private node-type
  `(typed.clojure/HMap
    :mandatory {:id typed.clojure/Kw
                :subject ~raw-subject
                :subject-handle ~admission-handle
                :depends-on (typed.clojure/Vec ~edge-type)}
    :complete? true))
(def ^:private subject-description
  '(typed.clojure/HMap :mandatory {:fingerprint typed.clojure/Str}
    :complete? false))

(defn- original-definitions []
  (let [file (or (io/resource "gesso/assurance/proof/application_dependency.clj")
                 (throw (ex-info "Application dependency source not found" {})))]
    (with-open [r (java.io.PushbackReader. (io/reader file))]
      (loop [forms []]
        (let [form (read {:eof ::end} r)]
          (if (= form ::end) forms (recur (conj forms form))))))))

(defn- original [forms name]
  (let [matches (filter #(and (seq? %) (#{'defn 'defn-} (first %))
                              (= name (second %))) forms)]
    (when-not (= 1 (count matches))
      (throw (ex-info "Missing or duplicate original node constructor"
                      {:name name :matches (count matches)})))
    (first matches)))

(defn- fn-ann! [sym signature]
  (eval (list 'typed.clojure/ann sym
              (list 'typed.clojure/IFn signature))))

(defn- install-contracts! []
  (eval (list 'typed.clojure/ann-datatype admission-handle '[]))
  (doseq [[name signature]
          [['gesso.assurance.subject/describe-subject
            ['typed.clojure/Any '-> subject-description]]
           ['gesso.assurance.admission/admit-subject!
            [raw-subject '-> admission-handle]]
           ['gesso.assurance.proof.application-dependency/raw-node
            ['typed.clojure/Kw interface '-> raw-subject]]
           ['gesso.assurance.proof.application-dependency/fingerprint
            [raw-subject '-> 'typed.clojure/Str]]
           ['gesso.assurance.proof.application-dependency/edge
            [raw-subject '-> edge-type]]
           ;; Typed Clojure's bundled sort-by signature rejects string sort
           ;; keys. This helper stays a separately exercised runtime seam.
           ['gesso.assurance.proof.application-dependency/ordered-sources
            [(list 'typed.clojure/Seqable raw-subject)
             '-> (list 'typed.clojure/Seq raw-subject)]]
           ['gesso.assurance.proof.application-dependency/node
            [raw-subject (list 'typed.clojure/Seqable raw-subject)
             '-> node-type]]]]
    (fn-ann! name signature)))

(defn- check! [check label expression]
  (let [{:keys [ex type-errors]} (check expression :check-config config)]
    (when (or ex (seq type-errors))
      (throw (ex-info "Application dependency production source failed typing"
                      {:label label :errors (mapv str type-errors)} ex)))
    true))

(defn- reject! [check label expression]
  (let [{:keys [ex type-errors]} (check expression :check-config config)
        kinds (mapv :type-error type-errors)]
    (when-not (and (nil? ex)
                   (seq kinds)
                   (every? #{:typed.clojure/type-mismatch-error
                             :typed.clojure/app-type-error} kinds))
      (throw (ex-info "Malformed application node escaped static rejection"
                      {:label label :actual kinds
                       :exception (some-> ex ex-message)} ex)))
    true))

(defn- change-result [definition alter]
  (let [last-expression (last definition)]
    (when-not (map? last-expression)
      (throw (ex-info "Original node constructor result no longer a map"
                      {:function (second definition)})))
    (let [changed (alter last-expression)]
      (when (= changed last-expression)
        (throw (ex-info "Static negative mutation did not change source"
                        {:function (second definition)})))
      (apply list (concat (butlast definition) [changed])))))

(defn- check-production! [check forms]
  (binding [*ns* (the-ns source-ns)]
    (doseq [name '[raw-node fingerprint edge node]]
      (check! check [:original name] (original forms name)))
    (doseq [[label function alteration]
            [[:raw-kind 'raw-node #(assoc % :kind :certified)]
             [:raw-with-authority 'raw-node #(assoc % :authority :granted)]
             [:raw-missing-id 'raw-node #(dissoc % :id)]
             [:edge-wrong-id 'edge #(assoc % :id '(admission/admit-subject! raw))]
             [:edge-wrong-fingerprint 'edge #(assoc % :fingerprint '(:interface raw))]
             [:edge-missing-fingerprint 'edge #(dissoc % :fingerprint)]
             [:node-raw-handle 'node #(assoc % :subject-handle 'raw)]
             [:node-wrong-id 'node #(assoc % :id (list 'admission/admit-subject! 'raw))]
             [:node-raw-edges 'node #(assoc % :depends-on '[raw])]
             [:node-missing-handle 'node #(dissoc % :subject-handle)]
             [:node-fake-authority 'node #(assoc % :authority :granted)]
             [:node-missing-subject 'node #(dissoc % :subject)]]]
      (let [source (original forms function)]
        (reject! check label (change-result source alteration))))
    ;; These are checked calls to the actual original node constructor.
    (check! check :valid-composition
            '(node (raw-node :typed/node {}) [(raw-node :typed/parent {})]))
    (reject! check :raw-untyped-node-call '(node {} []))
    (reject! check :wrong-sources '(node (raw-node :typed/node {}) [{}]))
    true))

(defn- private-var [name]
  (or (ns-resolve source-ns name)
      (throw (ex-info "Application dependency helper missing" {:name name}))))

(defn- check-ordering-runtime! []
  ;; The only executable ordering code in this revision is unchanged from
  ;; the original sorted-by-string source behavior. Preserve id ordering,
  ;; duplicate IDs, and empty inputs while validating constructor results.
  (let [raw (fn [id] {:kind :rule :id id :interface {:role :source}})
        raw-nodes (mapv raw [:typed/z :typed/a :typed/m :typed/a])
        ordered (private-var 'ordered-sources)
        edge (private-var 'edge)
        node (private-var 'node)]
    (doseq [values [raw-nodes [] (vec (reverse raw-nodes))
                    [(raw :typed/z)] [(raw :typed/a) (raw :typed/a)]]]
      (let [expected (sort-by (comp str :id) values)
            actual (ordered values)]
        (when-not (= (vec expected) (vec actual))
          (throw (ex-info "Application node ordering changed"
                          {:input values})))))
    (let [child (raw :typed/child)
          made (node child raw-nodes)
          expected-edges (mapv edge (sort-by (comp str :id) raw-nodes))]
      (when-not (and (= (:id child) (:id made))
                     (= child (:subject made))
                     (admission/admitted? (:subject-handle made))
                     (identical? (:subject-handle made)
                                 (admission/require-admitted-subject!
                                  (:subject-handle made) child))
                     (= expected-edges (:depends-on made))
                     (= #{:id :subject :subject-handle :depends-on}
                        (set (keys made))))
        (throw (ex-info "Application node creation runtime contract changed" {})))))
  true)

(defn -main [& _]
  (install-contracts!)
  (let [check (requiring-resolve 'typed.clj.checker/check-form-info)
        forms (original-definitions)]
    (check-production! check forms)
    (check-ordering-runtime!)
    (println "Typed application dependency constructor passed: four real")
    (println "source bodies, valid composition, 14 invalid source/call cases,")
    (println "and original-order/runtime-admission checks. Ordered-sources")
    (println "remains runtime-checked; no proof or authority is inferred.")
    :ok))

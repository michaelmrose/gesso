(ns gesso.model.mapped-projection-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-projection :as projection]
            [gesso.model.mapped-preparation :as preparation]
            [gesso.model.mapped-frame :as frame]
            [gesso.assurance.proof.mapped-prestate-semantics :as prestate]
            [gesso.assurance.proof.mapped-prestate-lowering :as lowering]
            [gesso.assurance.proof.mapped-prestate-rules :as rules]
            [gesso.assurance.proof.mapped-prestate-partition :as partition]
            [gesso.model.certified-plan :as plan]
            [gesso.model.inductive-plan :as inductive]
            [gesso.model.tx :as tx]
            [gesso.model.command :as command]
            [gesso.model.inductive-plan-test]
            [gesso.live.outbox :as outbox]
            [gesso.live.consistency.xtdb :as live-xtdb]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.live.optimistic.resolved-operation :as adapter]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def ^:private source
  {:subject {:kind :operation :id :example/projection :interface {:operation :example/projection}}
   :state-types {:example/count {:type :integer}
                 :example/enabled {:type :boolean}
                 :example/owner {:type :string :nullable? true}
                 :example/one {:type :keyword :nullable? true}
                 :example/two {:type :keyword :nullable? true}}
   :input-types {} :guard {:readers #{} :expression [:literal true]}
   :reads #{} :writes #{} :replacements {}
   :frame #{:example/count :example/enabled :example/owner :example/one :example/two}})

(def ^:private targets
  {:example/count [:document :requests "r1" :item_count]
   :example/enabled [:document :requests "r1" :enabled]
   :example/owner [:nullable-document :assignments "a1" :owner]
   :example/one [:journal-slot "p1" :example/command]
   :example/two [:journal-slot "p2" :example/command]})

(def ^:private rows {:requests [{:xt/id "r1" :item-count 2 :enabled false}]
                     :assignments []})

(defn- record
  ([id principal] (record id principal {}))
  ([id principal arguments]
   (resolution/command-record {:command-id (identity/command-id id)
                               :principal (identity/principal principal)
                               :operation :example/command :arguments arguments}
                              {:result :accepted} "t1")))

(defn- project [docs journal]
  (projection/project-state! source targets docs journal #(get-in % [:gesso.command/outcome :result])))

(deftest mapped-projection-decodes-closed-row-sets
  (is (= {:example/count 2 :example/enabled false :example/owner nil
          :example/one nil :example/two nil} (project rows [])))
  (is (= {:example/count 2 :example/enabled false :example/owner "p1"
          :example/one :accepted :example/two nil}
         (project (assoc rows :assignments [{:xt/id "a1" :owner "p1"}]) [(record "c1" "p1")])))
  (is (nil? (:example/owner (project (assoc rows :assignments [{:xt/id "a1" :owner nil}]) []))))
  (is (= :accepted (:example/two (project rows [(record "c2" "p2")])))))

(deftest mapped-projection-refuses-ambiguous-incomplete-or-ill-typed-rows
  (let [a (record "c1" "p1")
        reason (fn [docs journal]
                 (try (project docs journal) nil
                      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (doseq [[docs journal expected]
            [[(dissoc rows :assignments) [] :invalid-projection-input]
             [(assoc rows :extra []) [] :invalid-projection-input]
             [(assoc rows :requests []) [] :missing-required-document]
             [(assoc rows :requests (vec (repeat 2 (first (:requests rows))))) [] :duplicate-row]
             [(update rows :requests conj {:xt/id "r2"}) [] :unexpected-row]
             [(update-in rows [:requests 0] dissoc :enabled) [] :missing-mapped-field]
             [(assoc rows :assignments [{:xt/id "a1"}]) [] :missing-mapped-field]
             [(assoc-in rows [:requests 0 :enabled] nil) [] :ill-typed-present-value]
             [(assoc-in rows [:requests 0 :item-count] "2") [] :ill-typed-present-value]
             [rows [a (record "c2" "p1")] :duplicate-row]
             [rows [a (record "c1" "p2")] :duplicate-command-id]
             [rows [(record "c3" "p3")] :unexpected-row]]]
      (is (= expected (reason docs journal)) (pr-str expected)))
    (is (thrown? clojure.lang.ExceptionInfo (project rows [(assoc a :xt/id "wrong")])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (projection/project-state! source targets rows [a] (constantly nil))))
    (is (= :optional-field-unsupported
           (try (projection/project-state! (assoc-in source [:state-types :example/count :optional?] true)
                                           targets rows [] (constantly :accepted))
                nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

(deftest put-candidate-derives-poststate-and-checks-frame
  (let [original (assoc source :writes #{:example/count :example/one}
                       :replacements {:example/count [:literal 3] :example/one [:literal :accepted]}
                       :frame #{:example/enabled :example/owner :example/two})
        puts [[:put-docs :requests {:xt/id "r1" :item-count 3 :enabled false}]
              [:put-docs :gesso_commands (record "c1" "p1")]]
        check (fn [ops journal] (projection/check-put-candidate! original targets rows journal
                                          #(get-in % [:gesso.command/outcome :result]) {} ops))
        reason (fn [ops journal] (try (check ops journal) nil
                                      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
        r (check puts [])]
    (is (= 2 (get-in r [:before :example/count])))
    (is (= 3 (get-in r [:after :example/count])))
    (is (= :accepted (get-in r [:after :example/one])))
    (is (= :none (:authority r)))
    (is (false? (:transaction-submitted? r)))
    (is (= :candidate-relation-mismatch (reason (assoc-in puts [0 2 :item-count] 4) [])))
    (is (= :candidate-relation-mismatch (reason (assoc-in puts [0 2 :enabled] true) [])))
    (is (= :candidate-write-target-mismatch (reason (pop puts) [])))
    (is (= :candidate-write-target-mismatch (reason (conj puts (first puts)) [])))
    (is (= :candidate-write-target-mismatch (reason (conj puts [:put-docs :assignments {:xt/id "a1" :owner "p1"}]) [])))
    (is (= :unsupported-candidate-operation (reason (conj puts ["ASSERT TRUE"]) [])))
    (is (= :occupied-journal-slot (reason puts [(record "old" "p1")])))
    (is (= :candidate-write-target-mismatch (reason (assoc-in puts [1 2] (record "c1" "p2")) [])))
    (is (= (:before r) (:after (projection/check-put-candidate! source targets rows []
                                          (constantly :accepted) {} []))))))

(deftest mapped-preparations-bind-complete-command-and-consume-once
  (let [id :example/projection
        original (assoc source :writes #{:example/count :example/one}
                        :replacements {:example/count [:literal 3] :example/one [:literal :accepted]}
                        :frame #{:example/enabled :example/owner :example/two})
        catalog {:declared-operation-ids #{id} :operations [{:id id :construction original}]}
        {:keys [prepare! admit! discard! pending?]} (preparation/build-admission!
                    catalog targets #(get-in % [:gesso.command/outcome :result]))
        row (record "c1" "p1") binding (:gesso.command/binding row)
        snapshot {:document-rows rows :records {} :state (project rows [])}
        puts [[:put-docs :requests {:xt/id "r1" :item-count 3 :enabled false}]
              [:put-docs :gesso_commands row]]
        prepare #(prepare! id binding snapshot {})
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= :missing-preparation (reason #(admit! id "c1" puts))))
    (is (= :unknown-operation (reason #(prepare! :example/unknown binding snapshot {}))))
    (is (= :binding-slot-mismatch (reason #(prepare! id (:gesso.command/binding (record "c1" "p2")) snapshot {}))))
    (is (= :preparation-state-mismatch (reason #(prepare! id binding (assoc-in snapshot [:state :example/count] 9) {}))))
    (prepare)
    (is (pending? id "c1"))
    (is (= :duplicate-pending-preparation (reason prepare)))
    (is (= :missing-preparation (reason #(admit! id "other" puts))))
    (is (pending? id "c1"))
    (is (= :candidate-binding-mismatch
           (reason #(admit! id "c1" (assoc-in puts [1 2 :gesso.command/binding :arguments] {:different true})))))
    (is (not (pending? id "c1")))
    (is (= :missing-preparation (reason #(admit! id "c1" puts))))
    (prepare)
    (discard! id "c1")
    (is (not (pending? id "c1")))
    (prepare)
    (let [gate (promise)
          contender #(future @gate (or (reason (fn [] (admit! id "c1" puts))) :admitted))
          a (contender) b (contender)]
      (deliver gate true)
      (is (= #{:admitted :missing-preparation} #{(deref a 5000 :timeout) (deref b 5000 :timeout)})))
    (is (not (pending? id "c1")))
    (prepare)
    (is (= :candidate-relation-mismatch (reason #(admit! id "c1" (assoc-in puts [0 2 :item-count] 4)))))
    (is (not (pending? id "c1")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (preparation/build-admission! (assoc catalog :declared-operation-ids #{}) targets identity/identity)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (preparation/build-admission! (update catalog :operations conj (first (:operations catalog))) targets identity/identity)))))

(deftest complete-document-frame-rejects-hidden-mutations
  (let [id :example/projection
        original (assoc source :writes #{:example/count :example/one :example/owner}
                        :replacements {:example/count [:literal 3] :example/one [:literal :accepted]
                                       :example/owner [:literal "p1"]}
                        :frame #{:example/enabled :example/two})
        catalog {:declared-operation-ids #{id} :operations [{:id id :construction original}]}
        effects {id {[:document :requests "r1"] {:updated-at "t1"}
                     [:document :assignments "a1"] {:created-at "t1"}}}
        decoder #(get-in % [:gesso.command/outcome :result])
        docs (update-in rows [:requests 0] assoc :hidden {:x 1} :nullable nil :updated-at "t0")
        snapshot {:document-rows docs :records {} :state (project docs [])}
        row (record "c1" "p1")
        puts [[:put-docs :requests (assoc (first (:requests docs)) :item-count 3 :updated-at "t1")]
              [:put-docs :assignments {:xt/id "a1" :owner "p1" :created-at "t1"}]
              [:put-docs :gesso_commands row]]
        check (fn [ops] (let [{:keys [prepare! admit!]} (preparation/build-admission! catalog targets decoder effects)]
                          (prepare! id (:gesso.command/binding row) snapshot {})
                          (admit! id "c1" ops)))
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= :checked-supplied-complete-rows (:document-frame (check puts))))
    (doseq [bad [(assoc-in puts [0 2 :hidden :x] 2)
                 (update-in puts [0 2] dissoc :hidden)
                 (update-in puts [0 2] dissoc :nullable)
                 (assoc-in puts [0 2 :extra] nil)]]
      (is (= :unmapped-document-frame-mismatch (reason #(check bad)))))
    (is (= :supplemental-document-value-mismatch (reason #(check (assoc-in puts [0 2 :updated-at] "bad")))))
    (is (= :created-document-field-mismatch (reason #(check (assoc-in puts [1 2 :extra] true)))))
    (is (= :supplemental-document-value-mismatch (reason #(check (update-in puts [1 2] dissoc :created-at)))))
    (is (= :aliased-document-key (reason #(check (assoc-in puts [0 2 :item_count] 3)))))
    (is (= :incomplete-document-effects
           (reason #(preparation/build-admission! catalog targets decoder {id {}}))))
    (is (= :overlapping-document-effects
           (reason #(preparation/build-admission! catalog targets decoder
                       (assoc-in effects [id [:document :requests "r1"] :item_count] 3)))))))

(deftest framed-admission-binds-guards-to-consumed-preparation
  (let [id :example/projection
        original (assoc source :writes #{:example/count :example/one :example/owner}
                        :replacements {:example/count [:literal 3] :example/one [:literal :accepted]
                                       :example/owner [:literal "p1"]}
                        :frame #{:example/enabled :example/two})
        catalog {:declared-operation-ids #{id} :operations [{:id id :construction original}]}
        effects {id {[:document :requests "r1"] {} [:document :assignments "a1"] {}}}
        decoder #(get-in % [:gesso.command/outcome :result])
        docs (update-in rows [:requests 0] assoc :hidden "original")
        snapshot {:document-rows docs :records {} :state (project docs [])}
        row (record "c1" "p1")
        puts [[:put-docs :requests (assoc (first (:requests docs)) :item-count 3)]
              [:put-docs :assignments {:xt/id "a1" :owner "p1"}]
              [:put-docs :gesso_commands row]]
        mapping (plan/check-target-mapping! original targets)
        guards-for #(frame/formatted-prestate-assertions! mapping % [])
        guards (guards-for docs)
        {:keys [prepare! admit! pending?]} (preparation/build-admission! catalog targets decoder effects)
        prepare #(prepare! id (:gesso.command/binding row) snapshot {})
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (doseq [[prefix expected]
            [[[] :missing-frame-prestate-assertion]
             [nil :invalid-framed-admission]
             [(seq guards) :invalid-framed-admission]
             [(pop guards) :missing-frame-prestate-assertion]
             [(guards-for (assoc-in docs [:requests 0 :hidden] "different"))
              :missing-frame-prestate-assertion]
             [(guards-for (assoc docs :assignments [{:xt/id "a1" :owner "p1"}]))
              :missing-frame-prestate-assertion]]]
      (prepare)
      (is (= expected (reason #(admit! id "c1" puts prefix))))
      (is (not (pending? id "c1")))
      (is (= :missing-preparation (reason #(admit! id "c1" puts guards)))))
    (prepare)
    (is (= :unmapped-document-frame-mismatch
           (reason #(admit! id "c1" (assoc-in puts [0 2 :hidden] "different") guards))))
    (prepare)
    (let [checked (admit! id "c1" puts (conj guards ["ASSERT TRUE"]))]
      (is (= :checked-supplied-prewrite-guards (:frame-prestate checked)))
      (is (= :checked-supplied-complete-rows (:document-frame checked)))
      (is (= :none (:authority checked)))
      (is (= :not-established (:atomicity checked)))
      (is (false? (:transaction-submitted? checked))))
    (prepare)
    (is (nil? (:frame-prestate (admit! id "c1" puts))))
    (let [{:keys [prepare! admit!]} (preparation/build-admission! catalog targets decoder)]
      (prepare! id (:gesso.command/binding row) snapshot {})
      (is (= :invalid-framed-admission (reason #(admit! id "c1" puts guards)))))))

(deftest framed-envelope-preserves-the-exact-original-footprint
  (let [id :example/projection
        original (assoc source :writes #{:example/count :example/one :example/owner}
                        :replacements {:example/count [:literal 3] :example/one [:literal :accepted]
                                       :example/owner [:literal "p1"]}
                        :frame #{:example/enabled :example/two})
        catalog {:declared-operation-ids #{id} :operations [{:id id :construction original}]}
        effects {id {[:document :requests "r1"] {} [:document :assignments "a1"] {}}}
        decoder #(get-in % [:gesso.command/outcome :result])
        docs (update-in rows [:requests 0] assoc :hidden "original")
        snapshot {:document-rows docs :records {} :state (project docs [])}
        row (record "c1" "p1")
        base {:api :execute-tx :tx-options {}
              :tx-ops [["ASSERT TRUE"]
                       [:put-docs :requests (assoc (first (:requests docs)) :item-count 3)]
                       [:put-docs :assignments {:xt/id "a1" :owner "p1"}]
                       [:put-docs :gesso_commands row]
                       [:put-docs :outbox {:xt/id "intent"}]]}
        identity {:alternative id :command-id "c1"}
        seen (atom [])
        footprint! (fn [ops]
                     (swap! seen conj ops)
                     (when-not (= (:tx-ops base) ops)
                       (throw (ex-info "Unexpected footprint" {:reason :invalid-footprint})))
                     identity)
        registry (preparation/build-admission! catalog targets decoder effects)
        prepare #((:prepare! registry) id (:gesso.command/binding row) snapshot {})
        build #(#'inductive/frame-submission! registry footprint! #{:outbox} %)
        admit #(#'inductive/admit-framed-transaction! registry footprint! #{:outbox} base %)
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= :missing-preparation (reason #(build base))))
    (prepare)
    (let [actual (build base)
          prefix ((:frame-guards! registry) id "c1")
          ops (:tx-ops actual)
          n (count prefix)]
      (is (= 5 n))
      (is (= (into prefix (:tx-ops base)) ops))
      (is (= (dissoc base :tx-ops) (dissoc actual :tx-ops)))
      (is ((:pending? registry) id "c1"))
      (is (= :invalid-footprint (reason #(build (update base :tx-ops conj [:put-docs :outbox {:xt/id "other"}])))))
      (doseq [bad [(assoc actual :api :submit-tx)
                   (assoc actual :tx-options {:database :other})
                   (update actual :tx-ops pop)
                   (assoc-in actual [:tx-ops n 0] "ASSERT FALSE")
                   (assoc-in actual [:tx-ops (dec (count ops)) 2 :xt/id] "other")]]
        (is (= :changed-framed-transaction (reason #(admit bad))))
        (is ((:pending? registry) id "c1")))
      (doseq [bad [base
                   (assoc actual :tx-ops (into (subvec prefix 1) (:tx-ops base)))
                   (assoc actual :tx-ops (into [["ASSERT TRUE"]] ops))
                   (assoc actual :tx-ops (into (vec (reverse prefix)) (:tx-ops base)))]]
        (is (= :noncanonical-frame-prefix (reason #(admit bad))))
        (is (not ((:pending? registry) id "c1")))
        (prepare))
      ;; A preview is not a reservation of the preparation. Re-registration
      ;; with changed complete rows cannot admit the old generated prefix.
      ((:discard! registry) id "c1")
      (let [different (assoc-in docs [:requests 0 :hidden] "new")]
        ((:prepare! registry) id (:gesso.command/binding row)
         {:document-rows different :records {} :state (project different [])} {}))
      (is (= :unmapped-document-frame-mismatch (reason #(admit actual))))
      (prepare)
      (let [result (admit actual)]
        (is (= identity (:identity result)))
        (is (= :exact-generated-prefix (get-in result [:candidate :frame-prefix])))
        (is (= :none (get-in result [:candidate :authority])))
        (is (false? (get-in result [:candidate :transaction-submitted?]))))
      (is (= :missing-preparation (reason #(admit actual))))
      (is (= 1 (count (remove #(= (:tx-ops base) %) @seen))))
      (is (not-any? #(= ops %) @seen)))))

(deftest durable-witness-encoding-preserves-typed-scalar-valuations
  (let [encode (deref (ns-resolve 'gesso.model.inductive-plan 'encode-valuation))
        decode (deref (ns-resolve 'gesso.model.inductive-plan 'decode-valuation!))
        valuation {:example/nil nil :example/false false :example/true true
                   :example/integer 123456789012345678901234567890N
                   :example/negative -7 :example/text "x\n:keyword"
                   :example/keyword :example/value :example/unqualified :value}
        encoded (encode valuation)
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= valuation (decode encoded)))
    (is (= [] (encode {})))
    (is (= {} (decode [])))
    (is (not= (encode {}) (encode {:example/nil nil})))
    (is (not= (encode {:example/nil nil}) (encode {:example/nil false})))
    (is (= encoded (encode (into {} (reverse (seq valuation))))))
    (is (= (encode {:example/number 1}) (encode {:example/number 1N})))
    (doseq [bad [(conj encoded (first encoded)) (vec (reverse encoded))
                 [["example" "value" "integer" "01"]]
                 [["example" "value" "boolean" "maybe"]]
                 [["example" "value" "nil" "extra"]]
                 [["example" "value" "unknown"]]]]
      (is (= :invalid-transition-witness (reason #(decode bad)))))
    (is (= :invalid-transition-witness
           (reason #(encode {(keyword "example" (apply str (repeat 2049 "x"))) 1}))))))

(deftest mapped-input-selectors-derive-closed-typed-values
  (let [derive (deref (ns-resolve 'gesso.model.inductive-plan 'derive-inputs!))
        check (deref (ns-resolve 'gesso.model.inductive-plan 'check-input-derivations!))
        original (assoc source :input-types {:input/flag {:type :boolean}
                                             :input/count {:type :integer}
                                             :input/principal {:type :string}
                                             :input/nullable {:type :string :nullable? true}})
        id :example/projection
        catalog {:operations [{:id id :construction original}]}
        selectors {:input/flag [:argument :flag] :input/count [:state :example/count]
                   :input/principal [:principal] :input/nullable [:argument :nullable]}
        binding (:gesso.command/binding (record "c1" "p1" {:flag false :nullable nil}))
        before (project rows [])
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= {id selectors} (check catalog {:input-derivations {id selectors}})))
    (is (= {:input/flag false :input/count 2 :input/principal "p1" :input/nullable nil}
           (derive original selectors binding before)))
    (is (= :missing-mapped-input-source
           (reason #(derive original selectors (update binding :arguments dissoc :nullable) before))))
    (is (= :missing-mapped-input-source
           (reason #(derive original selectors binding (dissoc before :example/count)))))
    (is (= :ill-typed-present-value
           (reason #(derive original selectors (assoc-in binding [:arguments :flag] "false") before))))
    (doseq [selector [[:literal true] [:principal :extra] [:argument] [:state :unknown/field] (fn [] true)]]
      (is (= :invalid-mapped-input-selector
             (reason #(check catalog {:input-derivations {id (assoc selectors :input/flag selector)}})))))))

(deftest selected-publication-executor-submits-the-framed-canonical-plan
  (let [id :example/projection
        operation :example/command
        original (assoc source :input-types {:input/allowed {:type :boolean}}
                        :guard {:readers #{:input/allowed} :expression [:read :input/allowed]}
                        :reads #{:input/allowed}
                        :writes #{:example/count :example/one :example/owner}
                        :replacements {:example/count [:literal 3] :example/one [:literal :accepted]
                                       :example/owner [:literal "p1"]}
                        :frame #{:example/enabled :example/two})
        invariant {:readers #{:example/count}
                   :expression [:>= [:read :example/count] [:literal 0]]}
        catalog {:id :example/framed-publication
                 :initial-condition invariant :invariants {:example/nonnegative invariant}
                 :declared-operation-ids #{id} :operations [{:id id :construction original}]}
        proof ((deref (ns-resolve 'gesso.model.inductive-plan-test 'proof-package)) catalog)
        before (assoc (first (:requests rows)) :revision 0 :created-at "t0" :updated-at "t0" :hidden "keep")
        after (assoc before :item-count 3 :revision 1 :updated-at "t1")
        assignment {:xt/id "a1" :owner "p1" :revision 0 :created-at "t1" :updated-at "t1"}
        docs {:requests [before] :assignments []}
        version {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at}
        targets* #{[:requests "r1"] [:assignments "a1"]}
        tables #{:requests :assignments :gesso_commands}
        operations #{[:requests "r1" :update] [:assignments "a1" :create]}
        checks #{[:requests "r1" :revision 0] [:requests "r1" :updated-at "t0"]}
        values (into #{} (mapcat (fn [[table doc]] (map (fn [[k v]] [table (:xt/id doc) k v]) doc)))
                     [[:requests after] [:assignments assignment]])
        effects {id {[:document :requests "r1"] {:revision 1 :updated-at "t1"}
                     [:document :assignments "a1"] (select-keys assignment [:revision :created-at :updated-at])}}
        seen (atom [])
        ;; The footprint callback is a selected trusted fixture; the real
        ;; canonical model contracts independently constrain all domain puts.
        admission-source {:inductive-source proof :target-map targets
                    :input-derivations {id {:input/allowed [:argument :allowed]}}
                    :decode-outcome! #(get-in % [:gesso.command/outcome :result])
                    :transaction-footprint! (fn [ops]
                                              (swap! seen conj ops)
                                              {:alternative id
                                               :command-id (:xt/id (first (mapcat #(drop 2 %)
                                                               (filter #(= :gesso_commands (second %)) ops))))})
                    :auxiliary-tables #{:gesso_live_outbox} :document-effects effects}
        admission (inductive/build-inductive-mapped-admission! admission-source)
        constructs (atom 0)
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (let [release-policy (atom nil)]
      (try
        (with-open [node (xtn/start-node {})]
      (let [call (fn [command-id]
                   (let [binding {:command-id (identity/command-id command-id) :principal (identity/principal "p1")
                                  :operation operation :arguments {:allowed true}}
                         cmd (assoc binding :execution-id (identity/execution-id "e1"))
                         prepare (with-meta (fn [spec changes]
                                              (outbox/prepare! (assoc spec :intent-ids [(random-uuid)]
                                                                     :source-id "frame-test" :operation operation
                                                                     :command-id command-id) changes))
                                   {::publication/binding (constantly binding) ::publication/topics #{:request}})]
                     (assoc cmd :command cmd :ctx {:xtdb/connectable node publication/preparation-key prepare})))
            mode (atom :missing)
            opts {:operation operation :submission-mode! (constantly :submit)
                  :mapped-admission admission :may-disclose? (constantly true)
                  :construction-contract {:required-guard-tables #{} :allowed-guard-tables #{}
                                          :required-write-tables tables :allowed-write-tables tables}
                  :target-contract! (fn [& _] {:required-guard-targets #{} :allowed-guard-targets #{}
                                              :required-write-targets targets* :allowed-write-targets targets*})
                  :command-operation-contract! (fn [& _] {:required-command-operations operations :allowed-command-operations operations})
                  :predicate-contract! (fn [& _] {:required-guard-checks #{} :allowed-guard-checks #{}
                                                 :required-update-checks checks :allowed-update-checks checks})
                  :post-state-contract! (fn [& _] {:required-post-state-values values :allowed-post-state-values values})
                  :outcome-contract! (fn [& _] {:expected-outcome {:result :accepted}})
                  :construct! (fn [invocation]
                                (swap! constructs inc)
                                (when-not (= :missing @mode)
                                  (admission :prepare! id (select-keys invocation [:command-id :principal :operation :arguments])
                                             {:document-rows docs :records {} :state (project docs [])}
                                             {:input/allowed (not= :wrong-input @mode)}))
                                (when (= :stale @mode)
                                  (xt/execute-tx node [[:put-docs :requests (assoc before :hidden "intervening")]]))
                                {:spec {:purpose :gesso.live/refresh}
                                 :model-plan {:commands [(command/update-command :requests :update before after version)
                                                         (command/create :assignments assignment version)]
                                              :changes [{:topic :request :id "r1"}]}
                                 :outcome {:result :accepted} :now "t1"})
                  :project! (fn [& _] (throw (ex-info "Unexpected browser projection" {})))}
            exec (publication/build-executor opts)
            claimed (atom [])
            seen-envelopes (atom [])
            policy (fn [envelope]
                     (swap! seen-envelopes conj envelope)
                     (is (nil? (publication/claim-framed-submission! (constantly true) node envelope)))
                     (is (nil? (publication/claim-framed-submission! admission (Object.) envelope)))
                     (is (nil? (publication/claim-framed-submission! admission node (assoc envelope :api :submit-tx))))
                     (is (nil? (publication/claim-framed-submission! admission node (update envelope :tx-ops pop))))
                     (is (nil? @(future (publication/claim-framed-submission! admission node envelope))))
                     (when-let [checked (publication/claim-framed-submission! admission node envelope)]
                       (swap! claimed conj checked)
                       (is (nil? (publication/claim-framed-submission! admission node envelope)))
                       true))
            _ (reset! release-policy (live-xtdb/register-submission-policy! node policy))
            request-row #(first (xt/q node ["SELECT * FROM requests WHERE _id = ?" "r1"]))
            empty-writes? #(every? empty? (map (fn [table] (xt/q node [(str "SELECT _id FROM " table)]))
                                               ["assignments" "gesso_commands" "gesso_live_outbox" "gesso_transition_evidence"]))]
        (is (= :incomplete-mapped-input-derivations
               (reason #(inductive/build-inductive-mapped-admission! (dissoc admission-source :input-derivations)))))
        (is (= :invalid-mapped-input-selector
               (reason #(inductive/build-inductive-mapped-admission!
                         (assoc-in admission-source [:input-derivations id :input/allowed] [:state :example/missing])))))
        (is (= :changed-mapped-admission-registration
               (reason #(inductive/require-current-mapped-admission!
                         admission (assoc-in admission-source [:input-derivations id :input/allowed] [:state :example/enabled])))))
        (is (= :mapped-admission-requires-closed-contract
               (reason #(publication/build-executor (dissoc opts :outcome-contract!)))))
        (is (= :unregistered-mapped-admission
               (reason #(publication/build-executor (assoc opts :mapped-admission (fn [& _] true))))))
        (xt/execute-tx node [[:put-docs :requests before]])
        (with-redefs [adapter/project-resolution! (fn [_ result _] result)]
          (is (= :missing-preparation (reason #(exec (call "missing")))))
          (is (empty-writes?))
          (reset! mode :wrong-input)
          (is (= :mapped-input-derivation-mismatch (reason #(exec (call "wrong-input")))))
          (is (empty-writes?))
          (is (not (admission :pending? id "wrong-input")))
          (reset! mode :stale)
          (is (= :xtdb/assert-failed
                 (try (exec (call "stale")) nil (catch Exception e (:xtdb.error/code (ex-data e))))))
          (is (= "intervening" (:hidden (request-row))))
          (is (empty-writes?))
          (is (not (admission :pending? id "stale")))
          (xt/execute-tx node [[:put-docs :requests before]])
          (reset! mode :valid)
          (let [execute live-xtdb/execute-tx-from!
                result (with-redefs [live-xtdb/execute-tx-from!
                                     (fn [ctx ops & [opts]]
                                       (let [receipt (execute ctx ops opts)]
                                         ;; Change the current state after the genuine receipt.
                                         ;; The verification must still see the committed snapshot.
                                         (xt/execute-tx node [[:put-docs :requests (assoc after :item-count 4)]])
                                         receipt))]
                         (exec (call "valid")))]
            (is (= :incorporated (:status result)))
            (is (= :checked-commit-basis-projection
                   (get-in result [:transaction :mapped-readback :physical-correspondence])))
            (is (= (live-xtdb/tx-result-basis (get-in result [:transaction :tx-result]))
                   (get-in result [:transaction :mapped-readback :basis])))
            (is (= 4 (:item-count (request-row))))
            (is (= :checked-preceding-projection
                   (get-in result [:transaction :mapped-readback :prestate :physical-correspondence])))
            (let [replay (exec (call "valid"))
                  report (get-in result [:transaction :mapped-readback :correspondence])]
              (is (= :checked-historical-projection (get-in replay [:mapped-readback :physical-correspondence])))
              (is (= (get-in result [:transaction :mapped-readback :prestate])
                     (get-in replay [:mapped-readback :prestate])))
              (is (= report (get-in replay [:mapped-readback :correspondence])))
              (is (= :none (:authority report)))
              (is (= :transition-checked-only (:status report)))
              (is (= :not-established-for-step (:initialization report)))
              (is (contains? (:open-obligations report) :initialization-correspondence-for-step))
              (is (contains? (:open-obligations report) :complete-intervening-history-coverage))
              (is (= :checked-command-and-prestate-selectors (get-in report [:transition :inputs])))
              (is (= :checked-supplied-witness (get-in report [:transition :relation]))))
            (is (= 4 (:item-count (request-row))))
            (xt/execute-tx node [[:put-docs :requests after]]))
          (is (= after (request-row)))
          (is (= [assignment] (xt/q node ["SELECT * FROM assignments"])))
          (is (= [{:xt/id "valid"}] (xt/q node ["SELECT _id FROM gesso_commands"])))
          (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))
          (let [witness (first (xt/q node ["SELECT * FROM gesso_transition_evidence"]))
                checked (last @claimed)
                journal (xt/q node ["SELECT * FROM gesso_commands"])
                current-docs {:requests [after] :assignments [assignment]}
                verify #(inductive/check-historical-mapped-poststate!
                         admission (first journal) % current-docs journal)]
            (is (= witness (inductive/mapped-transition-evidence! admission checked)))
            (is (= 3 (:profile-version witness)))
            (is (= [["input" "allowed" "boolean" "true"]] (:input-witness witness)))
            (is (some #{["example" "owner" "nil"]} (:prestate-witness witness)))
            (is (= :checked-supplied-witness (:abstract-relation (verify witness))))
            (is (= :checked-command-and-prestate-selectors (:input-correspondence (verify witness))))
            (let [altered (assoc-in (first journal) [:gesso.command/binding :arguments :allowed] false)]
              (is (= :mapped-input-derivation-mismatch
                     (reason #(inductive/check-historical-mapped-poststate!
                               admission altered witness current-docs [altered])))))
            (is (= :invalid-historical-mapped-evidence
                   (reason #(verify (assoc witness :profile-version 1)))))
            (is (= :invalid-transition-witness
                   (reason #(verify (update witness :input-witness conj (first (:input-witness witness)))))))
            (is (= :historical-mapped-poststate-mismatch
                   (reason #(verify (assoc-in witness [:input-witness 0 3] "false")))))
            ;; A self-consistent digest is not a semantic admission.
            (let [forged (inductive/mapped-transition-evidence!
                          admission (assoc-in checked [:candidate :inputs :input/allowed] false))]
              (is (= :mapped-input-derivation-mismatch (reason #(verify forged))))
              (let [altered (assoc-in (first journal) [:gesso.command/binding :arguments :allowed] false)]
                (is (= :candidate-relation-mismatch
                       (reason #(inductive/check-historical-mapped-poststate!
                                 admission altered forged current-docs [altered])))))))
          (is (not (admission :pending? id "valid")))
          (is (= :replay (:status (exec (call "valid")))))
          (is (= 4 @constructs))
          (is (= 2 (count @claimed)))
          (is (every? #(= :exact-generated-prefix (get-in % [:candidate :frame-prefix])) @claimed))
          (let [envelope (last @seen-envelopes)]
            (is (nil? (publication/claim-framed-submission! admission node envelope)))
            (is (= :submission-policy-refused
                   (reason #(live-xtdb/execute-tx! node (:tx-ops envelope)))))
            (is (= :submission-policy-refused
                   (reason #(live-xtdb/submit-tx! node (:tx-ops envelope)))))
            (is (= 2 (count @claimed)))
            (is (= after (request-row)))
            (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_commands"]))))
            (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))))
          ;; Reset the isolated fixture through raw XTDB for a second command.
          (let [outbox-id (:xt/id (first (xt/q node ["SELECT _id FROM gesso_live_outbox"])))]
            (xt/execute-tx node [[:put-docs :requests before]
                                 [:delete-docs :assignments "a1"]
                                 [:delete-docs :gesso_commands "valid"]
                                 [:delete-docs :gesso_live_outbox outbox-id]]))
          (let [q live-xtdb/q-from
                failure (with-redefs [live-xtdb/q-from
                                      (fn [provider query opts]
                                        (let [result (q provider query opts)]
                                          (if (and (:snapshot-token opts)
                                                   (= ["SELECT * FROM requests"] query))
                                            (assoc-in result [0 :item-count] 99)
                                            result)))]
                          (try (exec (call "mismatch")) nil (catch Exception e e)))]
            (is (= :committed (:commit/status (ex-data failure))))
            (is (= :post-commit-correspondence (:failure/stage (ex-data failure))))
            (is (= :mapped-poststate-mismatch (:reason (ex-data (ex-cause failure)))))
            (is (= after (request-row)))
            (is (= [{:xt/id "mismatch"}] (xt/q node ["SELECT _id FROM gesso_commands"])))
            (is (= #{"valid" "mismatch"}
                   (set (map :xt/id (xt/q node ["SELECT _id FROM gesso_transition_evidence"])))))
            (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))
            ;; A transient failed observation is recoverable by a good read.
            (with-redefs [live-xtdb/q-from
                          (fn [provider query opts]
                            (let [result (q provider query opts)]
                              (if (and (:snapshot-token opts) (= ["SELECT * FROM requests"] query))
                                (assoc-in result [0 :item-count] 99) result)))]
              (is (= :historical-mapped-poststate-mismatch (reason #(exec (call "mismatch"))))))
            (with-redefs [adapter/project-resolution! (fn [_ result _] result)]
              (is (= :replay (:status (exec (call "mismatch"))))))
            (let [witness (first (xt/q node ["SELECT * FROM gesso_transition_evidence WHERE _id = ?" "mismatch"]))
                  prior-constructs @constructs
                  prior-claims (count @claimed)]
              ;; Non-disclosure precedes evidence inspection.
              (xt/execute-tx node [[:delete-docs :gesso_transition_evidence "mismatch"]])
              (is (= :missing-or-displaced-mapped-transition-evidence
                     (reason #(exec (call "mismatch")))))
              (let [denied (publication/build-executor (assoc opts :may-disclose? (constantly false)))]
                (is (= :unavailable
                       (try (denied (call "mismatch")) nil
                            (catch Exception e (get-in (ex-data e) [:error/details :status]))))))
              (xt/execute-tx node [[:put-docs :gesso_transition_evidence witness]])
              (is (= :missing-or-displaced-mapped-transition-evidence
                     (reason #(exec (call "mismatch")))))
              ;; Raw fixture writes emulate corruption outside mediation.
              (xt/execute-tx node [[:delete-docs :gesso_commands "mismatch"]
                                   [:put-docs :gesso_commands (record "corrupt" "p1" {:allowed true})]
                                   [:put-docs :gesso_transition_evidence (assoc witness :xt/id "corrupt")]
                                   [:put-docs :requests (assoc after :item-count 99)]])
              (xt/execute-tx node [[:put-docs :requests after]])
              (is (= :historical-mapped-poststate-mismatch (reason #(exec (call "corrupt")))))
              ;; A valid poststate and internally valid saved transition are
              ;; insufficient when the actual preceding state differs.
              (xt/execute-tx node [[:delete-docs :gesso_commands "corrupt"]
                                   [:delete-docs :assignments "a1"]
                                   [:put-docs :requests (assoc before :item-count 99)]])
              (xt/execute-tx node [[:put-docs :gesso_commands (record "precorrupt" "p1" {:allowed true})]
                                   [:put-docs :gesso_transition_evidence (assoc witness :xt/id "precorrupt")]
                                   [:put-docs :requests after]
                                   [:put-docs :assignments assignment]])
              (is (= :mapped-prestate-mismatch (reason #(exec (call "precorrupt")))))
              (is (= prior-constructs @constructs))
              (is (= prior-claims (count @claimed))))
            ;; A lost execute receipt must use the same historical replay
            ;; gate in recovery, not an ordinary journal-only lookup.
            (xt/execute-tx node [[:put-docs :requests before]
                                 [:delete-docs :assignments "a1"]
                                 [:delete-docs :gesso_commands "precorrupt"]])
            (let [execute live-xtdb/execute-tx-from!
                  result (with-redefs [live-xtdb/execute-tx-from!
                                       (fn [ctx ops & [opts]]
                                         (execute ctx ops opts)
                                         (throw (ex-info "Lost receipt" {})))
                                       adapter/project-resolution! (fn [_ result _] result)]
                           (exec (call "recover")))]
              (is (= :replay (:status result)))
              (is (= :checked-historical-projection (get-in result [:mapped-readback :physical-correspondence])))
              (is (= after (request-row)))))))
        (finally (when-let [release @release-policy] (release)))))))

(deftest full-mapped-prefix-guards-empty-and-occupied-journal-slots
  (let [mapping (plan/check-target-mapping! source targets)
        occupied (record "old" "p2" {:nested {:value "original" :flags [true false]}})
        candidate (record "fresh" "p1")
        guards (frame/formatted-prestate-assertions! mapping rows [occupied])
        description (prestate/describe! source targets rows [occupied])
        ops (conj guards [:put-docs :gesso_commands candidate])
        result (fn [node tx-ops]
                 (try (xt/execute-tx node tx-ops) :committed
                      (catch Exception e
                        (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
                          :assert-failed (throw e)))))
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= 7 (count guards)))
    (is (frame/require-formatted-prestate-assertions! mapping rows [occupied] guards))
    (is (= :missing-frame-prestate-assertion
           (reason #(frame/require-formatted-prestate-assertions! mapping rows [occupied] (pop guards)))))
    (is (= :duplicate-journal-frame-slot
           (reason #(frame/formatted-prestate-assertions! mapping rows [occupied (record "duplicate" "p2")]))))
    (is (= :unsupported-journal-frame-value
           (reason #(frame/formatted-prestate-assertions! mapping rows [(assoc-in occupied [:gesso.command/outcome :extra] nil)]))))
    (with-open [node (xtn/start-node {})]
      (xt/execute-tx node [[:put-docs :requests (first (:requests rows))]])
      (let [clear! #(xt/execute-tx node [[:delete-docs :gesso_commands "old" "duplicate" "intruder"]])
            fresh #(xt/q node ["SELECT * FROM gesso_commands WHERE _id = ?" "fresh"])
            observed-match? #(try
                               (:matches? (prestate/observe! source targets rows [occupied] description
                                            {:requests (xt/q node ["SELECT * FROM requests"])
                                             :assignments (xt/q node ["SELECT * FROM assignments"])}
                                            (xt/q node ["SELECT * FROM gesso_commands"])))
                               (catch clojure.lang.ExceptionInfo e
                                 (if (= :duplicate-row (:reason (ex-data e))) false (throw e))))]
        (doseq [changed [[]
                         [(assoc-in occupied [:gesso.command/outcome :result] :rejected)]
                         [(assoc-in occupied [:gesso.command/binding :arguments :nested :value] "changed")]
                         [(assoc-in occupied [:gesso.command/outcome :extra] true)]
                         [(assoc-in occupied [:gesso.command/binding :arguments :nested :flags] [false true])]
                         [occupied (record "duplicate" "p2")]
                         [occupied (record "intruder" "p1")]]]
          (clear!)
          (when (seq changed) (xt/execute-tx node [(into [:put-docs :gesso_commands] changed)]))
          (is (false? (observed-match?)))
          (is (= :assert-failed (result node ops)))
          (is (empty? (fresh))))
        (clear!)
        (xt/execute-tx node [[:put-docs :gesso_commands occupied]])
        (is (true? (observed-match?)))
        (is (= :committed (result node ops)))
        (is (= [candidate] (fresh)))))))

(deftest independent-mapped-prestate-predicates-preserve-decoder-input
  (let [docs (assoc-in rows [:requests 0 :supplemental] "original")
        occupied (record "old" "p2" {:nested {:value "original" :flags [true false]}})
        prepared [occupied]
        description (prestate/describe! source targets docs prepared)
        observe #(prestate/observe! source targets docs prepared description %1 %2)
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
        document [:document :requests "r1"]
        nullable [:document :assignments "a1"]
        empty-slot [:journal-slot "p1" :example/command]
        occupied-slot [:journal-slot "p2" :example/command]]
    (is (= {document [:exact-row (first (:requests docs))]
            nullable [:absent] empty-slot [:absent] occupied-slot [:exact-row occupied]}
           (:predicates description)))
    (let [r (observe docs prepared)]
      (is (:matches? r))
      (is (= :not-established (:sql-refinement r)))
      (is (= :not-issued (:certificate r))))
    (doseq [[actual-docs actual-journal expected]
            [[(assoc-in docs [:requests 0 :item-count] 3) prepared #{document}]
             [(assoc-in docs [:requests 0 :supplemental] "different") prepared #{document}]
             [(assoc docs :requests []) prepared #{document}]
             [(assoc docs :assignments [{:xt/id "a1" :owner "p1"}]) prepared #{nullable}]
             [docs [] #{occupied-slot}]
             [docs [(assoc-in occupied [:gesso.command/outcome :result] :rejected)] #{occupied-slot}]
             [docs [(assoc-in occupied [:gesso.command/binding :arguments :nested :flags] [false true])] #{occupied-slot}]
             [docs [occupied (record "new" "p1")] #{empty-slot}]]]
      (let [r (observe actual-docs actual-journal)]
        (is (false? (:matches? r)))
        (is (= expected (:mismatched-targets r)))))
    (doseq [bad [(update description :predicates dissoc empty-slot)
                 (assoc-in description [:predicates occupied-slot] [:absent])
                 (assoc description :mapping-fingerprint "wrong")]]
      (is (= :prestate-description-mismatch
             (reason #(prestate/observe! source targets docs prepared bad docs prepared)))))
    (is (= :duplicate-row (reason #(observe (update docs :requests conj (first (:requests docs))) prepared))))
    (is (= :duplicate-row (reason #(observe docs [occupied (record "duplicate" "p2")]))))
    (is (= :unexpected-row (reason #(observe docs [occupied (record "other" "p3")]))))
    (is (= :invalid-closed-row-sets (reason #(observe (dissoc docs :assignments) prepared))))
    (is (= :missing-required-row
           (reason #(prestate/describe! source targets (assoc docs :requests []) prepared))))
    ;; Finite comparison of the independent row predicate with the existing
    ;; typed projection, including a change irrelevant to the projection.
    (doseq [n [1 2 3] enabled [false true] extra ["original" "different"]
            journal [prepared [] [(record "new" "p1")]]]
      (let [candidate (assoc docs :requests [{:xt/id "r1" :item-count n
                                              :enabled enabled :supplemental extra}])
            r (observe candidate journal)]
        (is (= (and (= docs candidate) (= prepared journal)) (:matches? r)))
        (when (:matches? r)
          (is (= (project docs prepared) (project candidate journal))))))))

(deftest source-relative-prefix-correspondence
  (let [mapping (plan/check-target-mapping! source targets)
        occupied (record "old" "p2" {:nested {:value "original" :flags [true false]}})
        journal [occupied]
        prefix (frame/formatted-prestate-assertions! mapping rows journal)
        check #(lowering/check-prefix! source targets rows journal %)
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
        report (with-redefs [frame/formatted-prestate-assertions! (fn [& _] (throw (ex-info "Unexpected generator call" {})))]
                 (check prefix))]
    (is (= :checked-exact-prefix (:status report)))
    (is (= (:fingerprint mapping) (:mapping-fingerprint report)))
    (is (= 7 (:assertion-count report)))
    (is (= prefix (into [] (mapcat :assertions) (:groups report))))
    (is (= (:predicates (prestate/describe! source targets rows journal))
           (into {} (map (juxt :target :predicate)) (:groups report))))
    (is (= :not-established (:sql-refinement report)))
    (is (= :none (:authority report)))
    (is (= :not-issued (:certificate report)))
    (is (contains? (:open-obligations report) :transaction-internal-read-basis))
    (doseq [i (range (count prefix))]
      (is (= :noncanonical-lowering-prefix
             (reason #(check (into (subvec prefix 0 i) (subvec prefix (inc i)))))))
      (is (= :noncanonical-lowering-prefix
             (reason #(check (assoc prefix i ["ASSERT TRUE"]))))))
    (doseq [bad [(seq prefix) (vec (reverse prefix)) (conj prefix (first prefix))
                 (assoc-in prefix [0 1] "wrong-id")
                 (assoc-in prefix [(dec (count prefix)) (dec (count (peek prefix)))] "wrong-value")]]
      (is (= :noncanonical-lowering-prefix (reason #(check bad)))))
    (is (= :noncanonical-lowering-prefix
           (reason #(lowering/check-prefix! source targets (assoc-in rows [:requests 0 :item-count] 4) journal prefix))))
    (is (= :noncanonical-lowering-prefix
           (reason #(lowering/check-prefix! source targets rows [] prefix))))
    (doseq [[docs records] [[rows []]
                            [(assoc rows :assignments [{:xt/id "a1" :owner "p1"}]) journal]
                            [rows [(record "new" "p1") occupied]]]]
      (let [generated (frame/formatted-prestate-assertions! mapping docs records)]
        (is (= :checked-exact-prefix (:status (lowering/check-prefix! source targets docs records generated))))))
    (is (= :unsupported-value
           (reason #(lowering/check-prefix! source targets (assoc-in rows [:requests 0 :extra] nil) journal prefix))))))

(deftest restricted-prestate-rules-model-exact-decoded-row-equality
  (let [mapping (plan/check-target-mapping! source targets)
        occupied (record "old" "p2" {:nested {:value "original" :flags [true false] :empty {}}})
        journal [occupied]
        prefix (frame/formatted-prestate-assertions! mapping rows journal)
        description (rules/derive! source targets rows journal prefix)
        request [:document :requests "r1"]
        nullable [:document :assignments "a1"]
        empty-slot [:journal-slot "p1" :example/command]
        slot [:journal-slot "p2" :example/command]
        expected {request (:requests rows) nullable [] empty-slot [] slot journal}
        observe #(rules/evaluate! source targets rows journal prefix description %)
        report (observe expected)
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
        nested [:gesso.command/binding :arguments :nested]
        original-nested (get-in occupied nested)]
    (is (:rules-pass? report))
    (is (:exact-match? report))
    (is (:instance-correspondence? report))
    (is (= :not-established (:sql-refinement report)))
    (is (= :not-issued (:certificate report)))
    (is (= :none (:authority report)))
    (is (contains? (:open-obligations report) :transaction-internal-read-basis))
    (is (= prefix (into [] (mapcat :assertions) (:groups description))))
    (let [group (first (filter #(= slot (:target %)) (:groups description)))]
      (is (some #{[:map-size nested 3]} (:rules group)))
      (is (some #{[:equals (conj nested :empty) {}]} (:rules group))))
    (doseq [bad [(dissoc expected request) (assoc expected :unexpected [])
                 (assoc expected slot nil) (assoc expected slot [nil])]]
      (is (= :invalid-selected-bags (reason #(observe bad)))))
    (doseq [bad [(update description :groups pop)
                 (update-in description [:groups 0 :rules] pop)
                 (assoc description :sql-refinement :proved)]]
      (is (= :description-mismatch
             (reason #(rules/evaluate! source targets rows journal prefix bad expected)))))
    (doseq [bad [(assoc expected request [])
                 (assoc expected request (vec (repeat 2 (first (:requests rows)))))
                 (assoc-in expected [request 0 :item-count] 3)
                 (update-in expected [request 0] dissoc :enabled)
                 (assoc-in expected [request 0 :extra] nil)
                 (assoc expected nullable [{:xt/id "a1" :owner "p1"}])
                 (assoc expected empty-slot [(record "intruder" "p1")])
                 (assoc expected slot [])
                 (assoc expected slot [occupied occupied])]]
      (let [r (observe bad)]
        (is (false? (:rules-pass? r)))
        (is (false? (:exact-match? r)))
        (is (:instance-correspondence? r))))
    (doseq [v [original-nested (dissoc original-nested :value)
               (assoc original-nested :extra nil)
               (assoc original-nested :value nil)
               (assoc original-nested :flags [false true])
               (assoc original-nested :empty [])
               (assoc original-nested :empty "")
               (assoc original-nested :empty nil)
               {} [] nil false "original" ["original" [true false] {}]]]
      (let [candidate (assoc-in occupied nested v)
            r (observe (assoc expected slot [candidate]))]
        (is (= (= occupied candidate) (:rules-pass? r)))
        (is (:instance-correspondence? r))))))

(deftest decoded-map-partitions-account-for-known-and-residual-entries
  (let [expected {:z {:value false} :a []}
        actual (assoc expected :m nil)
        witness (partition/partition-map expected actual)
        decode #(mapv (:dictionary witness) %)]
    (is (= :partition-constructed (:status witness)))
    (is (= [0 2] (:known witness)))
    (is (= [1] (:residual witness)))
    (is (= (:actual witness) (into (:known witness) (:residual witness))))
    (is (= expected (into {} (decode (:known witness)))))
    (is (= actual (into {} (decode (:actual witness)))))
    (is (= 2 (:expected-cardinality witness)))
    (is (= 3 (:actual-cardinality witness)))
    (is (false? (:cardinality-premises-satisfied? witness)))
    (let [exact (partition/partition-map expected expected)]
      (is (:cardinality-premises-satisfied? exact))
      (is (empty? (:residual exact)))
      (is (= (:known exact) (:actual exact))))
    (is (= {:status :not-established :reason :unmatched-child-values
            :missing-fields #{:a} :different-fields #{:z}}
           (partition/partition-map expected {:z {:value true} :m []})))
    (doseq [v [nil [] "map" {"z" {:value false} :a []}]]
      (is (= :unsupported-map-shape (:reason (partition/partition-map expected v)))))
    (is (:cardinality-premises-satisfied? (partition/partition-map {} {})))
    (is (false? (:cardinality-premises-satisfied? (partition/partition-map {} {:x nil}))))))

(deftest map-partition-observations-cover-original-rule-sites
  (let [mapping (plan/check-target-mapping! source targets)
        occupied (record "old" "p2" {:nested {:value "original" :flags [true false] :empty {}}})
        journal [occupied]
        prefix (frame/formatted-prestate-assertions! mapping rows journal)
        request [:document :requests "r1"]
        slot [:journal-slot "p2" :example/command]
        bags {request (:requests rows) [:document :assignments "a1"] []
              [:journal-slot "p1" :example/command] [] slot journal}
        observe #(partition/observe! source targets rows journal prefix %)
        report (observe bags)
        sites (fn [r] (into {} (map (juxt (juxt :target :path) :partition)) (:sites r)))
        expected-sites (set (for [{:keys [target rules]} (:groups (rules/derive! source targets rows journal prefix))
                                  [kind path n] rules :when (= :map-size kind)]
                              {:target target :path path :cardinality n}))
        nested [:gesso.command/binding :arguments :nested]]
    (is (= expected-sites (set (map #(dissoc % :partition) (:sites report)))))
    (is (= :all-map-cardinality-premises-observed (:status report)))
    (doseq [{:keys [cardinality partition]} (:sites report)]
      (is (= cardinality (count (:known partition)) (count (:actual partition))))
      (is (empty? (:residual partition)))
      (is (= (:known partition) (:actual partition))))
    (let [r (observe (assoc-in bags [request 0 :extra] true))
          p (get (sites r) [request []])]
      (is (= :map-cardinality-premises-open (:status r)))
      (is (= :partition-constructed (:status p)))
      (is (= 1 (count (:residual p))))
      (is (false? (:cardinality-premises-satisfied? p))))
    (let [r (observe (assoc-in bags (into [slot 0] (conj nested :extra)) true))]
      (is (= :partition-constructed (get-in (sites r) [[slot nested] :status])))
      (is (= 1 (count (get-in (sites r) [[slot nested] :residual]))))
      (is (= :unmatched-child-values (get-in (sites r) [[slot []] :reason]))))
    (is (= :non-singleton-row-bag (get-in (sites (observe (assoc bags slot []))) [[slot nested] :reason])))
    (is (thrown? clojure.lang.ExceptionInfo (observe (dissoc bags slot))))
    (is (= :none (:authority report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-established (:sql-refinement report)))
    (is (contains? (:open-obligations report) :recursive-child-equality-correspondence))
    (is (contains? (:open-obligations report) :kernel-checked-partition-instance-composition))))

(deftest mapped-prefix-refuses-stale-valid-time-observations
  ;; Fixed historical intervals avoid sleeps and clock-boundary races.
  ;; The external query clock is not the internal transaction clock.
  (let [start (java.time.Instant/parse "2020-01-01T00:00:00Z")
        boundary (java.time.Instant/parse "2021-01-01T00:00:00Z")
        later (java.time.Instant/parse "2022-01-01T00:00:00Z")
        request (first (:requests rows))
        changed-request (assoc request :item-count 3)
        assignment {:xt/id "a1" :owner "p1"}
        occupied (record "old" "p2")
        changed-occupied (assoc-in occupied [:gesso.command/outcome :result] :rejected)
        ;; Restore the mixed empty/nonempty argument shapes that exposed
        ;; the original v2562 positive-control failure.
        intruder (record "intruder" "p1" {:nested {:value "intruder"}})
        marker {:xt/id "probe" :written true}
        mapping (plan/check-target-mapping! source targets)]
    (with-open [node (xtn/start-node {})]
      (xt/execute-tx node [[:put-docs {:into :requests :valid-from start} request]
                           [:put-docs {:into :gesso_commands :valid-from start} occupied]])
      (xt/execute-tx node [[:put-docs {:into :requests :valid-from boundary} changed-request]
                           [:put-docs {:into :assignments :valid-from boundary} assignment]
                           [:put-docs {:into :gesso_commands :valid-from boundary} changed-occupied intruder]
                           [:put-docs :guard_probe {:xt/id "sentinel"}]])
      (let [token (:snapshot-token (first (xt/q node "SHOW SNAPSHOT_TOKEN")))
            observe (fn [opts]
                      [{:requests (xt/q node ["SELECT * FROM requests WHERE _id = ?" "r1"] opts)
                        :assignments (xt/q node ["SELECT * FROM assignments WHERE _id = ?" "a1"] opts)}
                       (xt/q node ["SELECT * FROM gesso_commands ORDER BY _id"] opts)])
            old-basis {:snapshot-token token :current-time start}
            later-basis {:snapshot-token token :current-time later}
            [old-docs old-journal :as old] (observe old-basis)
            [current-docs current-journal :as current] (observe later-basis)
            attempt (fn [docs journal]
                      (let [prefix (frame/formatted-prestate-assertions! mapping docs journal)]
                        (is (= :checked-exact-prefix
                               (:status (lowering/check-prefix! source targets docs journal prefix))))
                        (try (xt/execute-tx node (conj prefix [:put-docs :guard_probe marker]))
                             :committed
                             (catch Exception e
                               (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
                                 :assert-failed (throw e))))))
            written #(xt/q node ["SELECT * FROM guard_probe WHERE _id = ?" "probe"])]
        (is (and (string? token) (seq token)))
        (is (= [rows [occupied]] old))
        (is (= [{:requests [changed-request] :assignments [assignment]}
                [intruder changed-occupied]] current))
        (is (= current (observe {})))
        (is (not= (project old-docs old-journal) (project current-docs current-journal)))
        ;; Isolate each stale coordinate. No writer races or elapsed-time
        ;; waits are needed: each prepared value was visible at the earlier
        ;; valid clock on the very same system snapshot.
        (doseq [[label docs journal]
                [[:request (assoc current-docs :requests (:requests old-docs)) current-journal]
                 [:nullable (assoc current-docs :assignments (:assignments old-docs)) current-journal]
                 [:occupied current-docs [intruder occupied]]
                 [:empty-slot current-docs [changed-occupied]]
                 [:all old-docs old-journal]]]
          (let [description (prestate/describe! source targets docs journal)
                reference (prestate/observe! source targets docs journal description current-docs current-journal)]
            (is (false? (:matches? reference)) (name label))
            (is (= :assert-failed (attempt docs journal)) (name label))
            (is (empty? (written)) (name label))))
        (is (= old (observe old-basis)))
        (is (= current (observe later-basis)))
        (is (= :committed (attempt current-docs current-journal)))
        (is (= [marker] (written)))
        (is (= current (observe {})))))))

(deftest mapped-prefix-distinguishes-empty-structs-after-schema-evolution
  (let [mapping (plan/check-target-mapping! source targets)
        empty-args (record "one" "p1")
        nested-empty (record "two" "p2" {:nested {:empty {}}})
        journal [empty-args nested-empty]
        args-path [:gesso.command/binding :arguments]
        nested-path (into args-path [:nested :empty])
        prefix (frame/formatted-prestate-assertions! mapping rows journal)
        marker {:xt/id "probe" :written true}
        attempt! (fn [node ops]
                   (try (xt/execute-tx node ops) :committed
                        (catch Exception e
                          (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
                            :assert-failed (throw e)))))]
    (is (= :checked-exact-prefix
           (:status (lowering/check-prefix! source targets rows journal prefix))))
    (is (= 2 (:version (lowering/check-prefix! source targets rows journal prefix))))
    (with-open [node (xtn/start-node {})]
      (xt/execute-tx node [[:put-docs :requests (first (:requests rows))]
                           [:put-docs :gesso_commands empty-args nested-empty]])
      (let [actual #(xt/q node "SELECT * FROM gesso_commands ORDER BY _id")
            written #(xt/q node ["SELECT * FROM guard_probe WHERE _id = ?" "probe"])
            ops (conj prefix [:put-docs :guard_probe marker])]
        (is (= journal (actual)))
        (is (= :committed (attempt! node prefix)))
        (doseq [[row path] [[empty-args args-path] [nested-empty nested-path]]
                [label value] [[:nonempty-map {:extra "bad"}]
                               [:empty-string ""] [:map-looking-string "{}"]
                               [:empty-vector []] [:empty-set #{}]
                               [:empty-binary (byte-array 0)] [:null nil]]]
          (xt/execute-tx node [[:put-docs :gesso_commands (assoc-in row path value)]])
          (is (= :assert-failed (attempt! node ops)) (str path " " label))
          (is (empty? (written)))
          (xt/execute-tx node [[:put-docs :gesso_commands row]])
          (is (= journal (actual)))
          (is (= :committed (attempt! node prefix)) (str "restored " path " " label)))
        (doseq [[row path] [[empty-args args-path] [nested-empty nested-path]]]
          (xt/execute-tx node [[:put-docs :gesso_commands (update-in row (pop path) dissoc (peek path))]])
          (is (= :assert-failed (attempt! node ops)))
          (is (empty? (written)))
          (xt/execute-tx node [[:put-docs :gesso_commands row]]))
        ;; Null-only physical fields are omitted by the decoded XTDB row.
        ;; The supported predicate is equality of visible decoded entries.
        (xt/execute-tx node [[:put-docs :gesso_commands (assoc-in empty-args args-path {:extra nil})]])
        (is (= journal (actual)))
        (is (= :committed (attempt! node ops)))
        (is (= [marker] (written)))))))

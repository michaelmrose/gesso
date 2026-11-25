(ns gesso.model.mapped-projection-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-projection :as projection]
            [gesso.model.mapped-preparation :as preparation]
            [gesso.model.mapped-frame :as frame]
            [gesso.model.certified-plan :as plan]
            [gesso.model.inductive-plan :as inductive]
            [gesso.model.tx :as tx]))

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

(defn- record [id principal]
  (resolution/command-record {:command-id (identity/command-id id)
                              :principal (identity/principal principal)
                              :operation :example/command :arguments {}}
                             {:result :accepted} "t1"))

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
        guards-for #(mapv tx/format-assertion (frame/prestate-assertions! mapping %))
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
      (is (= 3 n))
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

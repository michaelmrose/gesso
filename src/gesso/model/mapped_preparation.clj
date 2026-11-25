(ns gesso.model.mapped-preparation
  "Local preparation admission for mapped durable candidates. A trusted
   assembly supplies the original operation catalog, mapping, outcome decoder
   and pinned rows. The framed admission arity checks restricted lowered
   pre-write guards; general SQL/auxiliary-effect checking, physical
   provenance, executor issuance and transaction authority remain open."
  (:require [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-projection :as projection]
            [gesso.model.mapped-frame :as frame]
            [gesso.model.tx :as tx]))

(defn- reject! [reason]
  (throw (ex-info "Invalid mapped command preparation."
                  {:error/type ::invalid-preparation :reason reason})))

(defn build-admission!
  "Build a private, host-local pending-preparation registry. The original
   constructions and mapping are captured at assembly, not supplied by a
   submitter. A preparation binds a complete typed command binding and one
   mapped journal-writing alternative. Duplicate pending registrations fail;
   admission atomically consumes the selected preparation even on failure.
   Trusted callers may discard abandoned preparations. New attempts require
   new preparations; durable replay/slot uniqueness remain XTDB concerns.
   Only pass the mapped puts after checking the full transaction footprint.
   The four-argument admit! additionally requires the formatted pre-write
   guard prefix to match the same consumed preparation. The three-argument
   form checks supplied replacements only; it is not framed admission.
   frame-guards! previews the generated prefix without consuming it;
   admit-framed! consumes and checks that exact prefix against the same
   captured rows used for candidate validation."
  ([catalog target-map decode-outcome!]
   (build-admission! catalog target-map decode-outcome! nil))
  ([catalog target-map decode-outcome! document-effects]
  (let [entries (:operations catalog)
        ids (mapv :id entries)]
    (when-not (and (vector? entries) (seq entries) (fn? decode-outcome!)
                   (= (count ids) (count (distinct ids)))
                   (= (set ids) (:declared-operation-ids catalog)))
      (reject! :invalid-operation-selection))
    (let [operations (into {} (map (fn [{:keys [id construction]}]
                                     (when-not (= id (get-in construction [:subject :id]))
                                       (reject! :operation-subject-mismatch))
                                     [id {:construction construction
                                          :mapping (plan/check-target-mapping! construction target-map)}])) entries)
          _ (when (some? document-effects)
              (when-not (and (map? document-effects) (= (set ids) (set (keys document-effects))))
                (reject! :incomplete-document-effect-catalog))
              (doseq [[id {:keys [mapping]}] operations]
                (frame/check-effects! mapping (get document-effects id))))
          pending (atom {})
          operation! (fn [id] (or (get operations id) (reject! :unknown-operation)))
          prepare! (fn [id binding snapshot inputs]
                     (let [binding (resolution/require-binding binding)
                           {:keys [construction mapping]} (operation! id)
                           slot [:journal-slot (identity/raw-value (:principal binding)) (:operation binding)]
                           k [(resolution/record-id binding) id]
                           journal (vec (vals (:records snapshot)))]
                       (when-not (= #{slot} (set (filter #(= :journal-slot (first %)) (:write-targets mapping))))
                         (reject! :binding-slot-mismatch))
                       (when-not (and (map? (:records snapshot))
                                      (= (:records snapshot) (into {} (map (juxt :xt/id clojure.core/identity)) journal))
                                      (= (:state snapshot) (projection/project-state! construction target-map
                                                             (:document-rows snapshot) journal decode-outcome!)))
                         (reject! :preparation-state-mismatch))
                       (swap! pending (fn [m]
                                        (when (contains? m k) (reject! :duplicate-pending-preparation))
                                        (assoc m k {:binding binding :snapshot snapshot :inputs inputs})))
                       nil))
          lower-frame! (fn [id snapshot]
                         (when-not (some? document-effects)
                           (reject! :invalid-framed-admission))
                         (mapv tx/format-assertion
                               (frame/prestate-assertions! (:mapping (operation! id))
                                                           (:document-rows snapshot))))
          frame-guards! (fn [id command-id]
                          (let [entry (or (get @pending [command-id id])
                                          (reject! :missing-preparation))]
                            (lower-frame! id (:snapshot entry))))
          admit-candidate! (fn [id command-id puts frame-mode formatted-guards]
                   (let [k [command-id id]
                         [prior _] (swap-vals! pending dissoc k)
                         {:keys [binding snapshot inputs]} (or (get prior k) (reject! :missing-preparation))
                         {:keys [construction mapping]} (operation! id)
                         records (mapcat #(drop 2 %) (filter #(and (= :put-docs (first %))
                                                                   (= :gesso_commands (second %))) puts))]
                     (when-not (and (= 1 (count records))
                                    (= binding (:gesso.command/binding (resolution/require-record (first records)))))
                       (reject! :candidate-binding-mismatch))
                     (let [checked (projection/check-put-candidate! construction target-map
                                     (:document-rows snapshot) (vec (vals (:records snapshot))) decode-outcome! inputs puts)]
                       (when-not (and (= (:fingerprint mapping) (:mapping-fingerprint checked))
                                      (= (:state snapshot) (:before checked)))
                         (reject! :preparation-state-mismatch))
                       (when (some? document-effects)
                         (frame/check-replacements! mapping (get document-effects id) (:document-rows snapshot) puts))
                       (when frame-mode
                         (when-not (and (some? document-effects) (vector? formatted-guards)
                                        (every? vector? formatted-guards))
                           (reject! :invalid-framed-admission))
                         (if (= :exact frame-mode)
                           (when-not (= (lower-frame! id snapshot) formatted-guards)
                             (reject! :noncanonical-frame-prefix))
                           (frame/require-prestate-assertions! mapping (:document-rows snapshot) formatted-guards)))
                       (cond-> checked
                         (some? document-effects)
                         (assoc :document-frame :checked-supplied-complete-rows)
                         frame-mode
                         (assoc :frame-prestate :checked-supplied-prewrite-guards)
                         (= :exact frame-mode)
                         (assoc :frame-prefix :exact-generated-prefix)))))
          admit! (fn
                   ([id command-id puts] (admit-candidate! id command-id puts nil nil))
                   ([id command-id puts formatted-guards]
                    (admit-candidate! id command-id puts :contains formatted-guards)))]
      {:prepare! prepare! :admit! admit!
       :frame-guards! frame-guards!
       :admit-framed! (fn [id command-id puts prefix]
                        (admit-candidate! id command-id puts :exact prefix))
       :discard! (fn [id command-id] (swap! pending dissoc [command-id id]) nil)
       :pending? (fn [id command-id] (contains? @pending [command-id id]))}))))

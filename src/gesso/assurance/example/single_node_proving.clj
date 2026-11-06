(ns gesso.assurance.example.single-node-proving
  "v1450: small, reusable, SINGLE-DECLARED-MACHINE assurance example.

   Application authors express a two-operation reservation lifecycle and
   business invariants with ordinary Gesso source data. The example performs
   genuine source-derived abstract closure and operation-interface composition.
   It deliberately does not invent deployed routes, an XTDB writer inventory,
   an execution witness, or a proof/certification credential.

   A caller with a separately assembled, issued and host-observed application
   may supply the ORIGINAL writer-source to obtain the full v1448 conditional
   report. This example alone never establishes physical completeness,
   universal kernel soundness or browser/XTDB correspondence."
  (:require [gesso.assurance.proof.application-assurance-report :as report]
            [gesso.assurance.proof.application-dependency :as dependency]
            [gesso.assurance.proof.operation-interface-composition :as composition]
            [gesso.assurance.proof.operation-interface-provenance :as provenance]))

(def ^:private state-types
  {:slot/status {:type :enum :name :slot/status
                 :values #{:available :reserved}}
   :slot/active-claims {:type :integer}
   :slot/audit-tag {:type :keyword :nullable? true}})

(def ^:private available-status
  {:readers #{:slot/status}
   :expression [:in [:read :slot/status]
                [:literal #{:available :reserved}]]})

(def ^:private nonnegative-claims
  {:readers #{:slot/active-claims}
   :expression [:>= [:read :slot/active-claims] [:literal 0]]})

(def ^:private at-most-one-claim
  {:readers #{:slot/active-claims}
   :expression [:<= [:read :slot/active-claims] [:literal 1]]})

(defn- transition [id from to claims]
  {:subject {:kind :operation :id id :interface {:operation id}}
   :state-types state-types
   :input-types {}
   :guard {:readers #{:slot/status}
           :expression [:= [:read :slot/status] [:literal from]]}
   :reads #{:slot/status}
   :writes #{:slot/status :slot/active-claims}
   :replacements {:slot/status [:literal to]
                  :slot/active-claims [:literal claims]}
   :frame #{:slot/audit-tag}})

(defn reservation-catalog
  "An explicit, closed *abstract* operation catalog. Each operation writes a
   literal claim count, making the at-most-one-claim business rule accessible
   to Gesso's supported literal-preservation inference rules. No separate
   Recife/Quint/TLA specification is authored by the application developer.

   This is not an enumeration of installed handlers or physical XTDB writers."
  []
  {:id :slot/reservation-lifecycle
   :declared-operation-ids #{:slot/reserve :slot/release}
   :initial-condition
   {:readers #{:slot/status :slot/active-claims}
    :expression [:and (:expression available-status)
                 (:expression nonnegative-claims)
                 (:expression at-most-one-claim)]}
   :invariants {:slot/status-domain available-status
                :slot/nonnegative-claims nonnegative-claims
                :slot/at-most-one-claim at-most-one-claim}
   :operations
   [{:id :slot/reserve
     :construction (transition :slot/reserve :available :reserved 1)}
    {:id :slot/release
     :construction (transition :slot/release :reserved :available 0)}]})

(defn assemble-abstract-assurance!
  "Check the exact application declarations and derive conditional operation
   interfaces and provenance from the original source. The closure handle is
   live only within this JVM; reports are descriptive, not proof authority."
  []
  (let [catalog (reservation-catalog)
        graph (dependency/build-application-dependency-graph! catalog)
        closure (dependency/check-application-dependencies! catalog graph)
        composed (composition/compose-operation-interfaces!
                  catalog graph closure)
        manifest (provenance/describe-composition-provenance!
                  composed catalog graph closure)]
    {:catalog catalog
     :graph graph
     :closure closure
     :composition composed
     :provenance manifest}))

(defn describe-conditional-application!
  "Given an ORIGINAL v1446 writer-source from an actually issued Gesso
   application, recheck and join it to the reservation catalog. The caller
   must obtain that source from the application construction/host boundary;
   synthetic writer summaries are neither accepted nor created here.

   Returns a NONAUTHORITATIVE v1448 application report with all physical and
   deployment assumptions still explicit."
  [writer-source]
  (let [{:keys [catalog graph closure]} (assemble-abstract-assurance!)]
    (report/describe-application-assurance!
     catalog graph closure writer-source)))

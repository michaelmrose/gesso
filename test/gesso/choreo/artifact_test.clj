(ns gesso.choreo.artifact-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.walk :as walk]
   [gesso.choreo.artifact :as artifact]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.correspondence :as correspondence]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.project :as project]
   [gesso.choreo.proof :as proof]))

(defn- error-data
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo ex
      (ex-data ex))))

(defn- all-boundary-choreography
  []
  (choreo/->choreography
   {:name :example/artifact-all-boundaries
    :initial :prepare
    :states
    {:prepare
     (choreo/local
      :browser
      :prepare
      :command
      {:outputs #{:request-id}})

     :command
     (choreo/communicate
      :browser
      :authority
      :request/claim
      :claim
      {:via :http
       :required #{:request-id}
       :correlation #{:request-id}})

     :claim
     (choreo/authoritative
      :authority
      :request/claim
      :decide
      {:requires #{:request-id}
       :outputs #{:outcome :revision}})

     :decide
     (choreo/branch
      :authority
      :outcome
      {:confirmed :settled
       :rejected :rejected})

     :settled
     (choreo/communicate
      :authority
      :browser
      :request/settled
      :observe
      {:via :sse
       :required #{:outcome :revision}})

     :observe
     (choreo/await
      :browser
      {:browser/observed :show}
      {:event-contracts
       {:browser/observed
        {:required #{:basis}}}})

     :show
     (choreo/local
      :browser
      :show
      :done
      {:requires #{:basis}})

     :rejected
     (choreo/communicate
      :authority
      :browser
      :request/rejected
      :done
      {:required #{:outcome :revision}})

     :done
     (choreo/return :done)}}))

(defn- equivalent-choreography-with-reordered-state-map
  []
  (let [original
        (all-boundary-choreography)

        states
        (:states original)]
    (choreo/->choreography
     {:name (:name original)
      :initial (:initial original)
      :states
      (array-map
       :done (get states :done)
       :rejected (get states :rejected)
       :show (get states :show)
       :observe (get states :observe)
       :settled (get states :settled)
       :decide (get states :decide)
       :claim (get states :claim)
       :command (get states :command)
       :prepare (get states :prepare))})))

(defn- same-runtime-semantics-with-different-source-metadata
  []
  (let [original
        (all-boundary-choreography)]
    (choreo/->choreography
     (assoc original
            :name :example/artifact-renamed
            :metadata {:source :different-file}))))

(defn- changed-runtime-semantics
  []
  (choreo/->choreography
   {:name :example/artifact-changed
    :initial :prepare
    :states
    {:prepare
     (choreo/local
      :browser
      :prepare-different-action
      :done)

     :done
     (choreo/return :done)}}))



(defn- artifact-bound-protocol
  [outcome]
  (choreo/->choreography
   {:name :example/artifact-bound-evidence
    :initial :prepare
    :states
    {:prepare
     (choreo/local
      :browser
      :prepare
      :send
      {:outputs #{:request-id}})

     :send
     (choreo/communicate
      :browser
      :server
      :request/command
      :apply
      {:via :http
       :required #{:request-id}
       :correlation #{:request-id}})

     :apply
     (choreo/authoritative
      :server
      :request/apply
      :done
      {:requires #{:request-id}
       :outputs #{:status}})

     :done
     (choreo/return outcome)}}))

(def artifact-bound-good-witness
  [{:op :local
    :role :browser
    :action :prepare
    :outputs {:request-id 1}}

   {:op :send
    :role :browser
    :to :server
    :event :request/command
    :via :http
    :payload {:request-id 1}
    :as :command}

   {:op :deliver
    :message :command}

   {:op :authoritative
    :role :server
    :operation :request/apply
    :outputs {:status :accepted}}])

(def artifact-bound-incomplete-witness
  [{:op :local
    :role :browser
    :action :prepare
    :outputs {:request-id 1}}])

(defn- recursively-contains-key?
  [value target-key]
  (let [found?
        (volatile! false)]
    (walk/postwalk
     (fn [node]
       ;; Avoid contains? here: sorted maps with numeric runtime locators can
       ;; throw when asked to compare a keyword target against numeric keys.
       (when (and (map? node)
                  (some #(= target-key %)
                        (keys node)))
         (vreset! found? true))
       node)
     value)
    @found?))

(defn- recursively-contains-executable-plan?
  [value]
  (let [found?
        (volatile! false)]
    (walk/postwalk
     (fn [node]
       (when (project/executable-plan? node)
         (vreset! found? true))
       node)
     value)
    @found?))

(defn- sidecar-locations
  [sidecar]
  (:locations sidecar))

(defn- historical-v3-certificate
  "Construct the exact historical v3 structural-certificate shape from one
   current v4 certificate. Historical recognition belongs to proof.cljc; an
   ArtifactSet sidecar must nevertheless bind only the current certificate
   version emitted with its executable plans."
  [current-certificate]
  (-> current-certificate
      (assoc
       :gesso.choreo/version
       proof/projection-structural-certificate-v3-version

       :properties
       proof/projection-structural-certificate-v3-properties

       :nonclaims
       proof/projection-structural-certificate-v3-nonclaims

       :valid?
       (and
        (proof/valid? (:boundary-proof current-certificate))
        (proof/valid? (:successor-proof current-certificate))
        (proof/valid? (:completion-proof current-certificate))
        (proof/valid? (:observable-origin-proof current-certificate))))
      (dissoc :runtime-origin-proof)
      (assoc :failures [])))

(deftest artifact-vocabulary-is-explicit-and-versioned
  (is (= 2 artifact/artifact-version))
  (is (= :gesso.choreo/artifact-set
         artifact/artifact-set-type))
  (is (= :gesso.choreo/diagnostic-proof-sidecar
         artifact/diagnostic-sidecar-type)))

(deftest emission-produces-sibling-executable-and-diagnostic-products
  (let [choreography
        (all-boundary-choreography)

        direct-plans
        (project/project-all choreography)

        emitted
        (artifact/emit-artifacts choreography)

        plans
        (:executable-plans emitted)

        sidecar
        (:diagnostic-proof-sidecar emitted)]
    (is (artifact/artifact-set? emitted))
    (is (= direct-plans plans))
    (is (= #{:browser :authority}
           (set (keys plans))))
    (is (every? project/executable-plan?
                (vals plans)))

    (is (artifact/diagnostic-sidecar? sidecar))
    (is (= :example/artifact-all-boundaries
           (:choreography-name sidecar)))
    (is (= 4
           proof/projection-structural-certificate-version))
    (is (= proof/projection-structural-certificate-version
           (:projection-structural-certificate-version sidecar)))
    (is (= (:projection-structural-certificate-version sidecar)
           (get-in sidecar [:proof :gesso.choreo/version])))
    (is (proof/projection-structural-certificate?
         (:proof sidecar)))
    (is (proof/structural-certificate-valid?
         (:proof sidecar)))
    (is (= proof/projection-structural-certificate-properties
           (get-in sidecar [:proof :properties])))

    (testing "source identity belongs to diagnostics, not the executable plans"
      (doseq [[_role plan] plans]
        (is (false?
             (recursively-contains-key?
              plan
              :name)))))

    (testing "the sidecar is a sibling, not executable-plan payload"
      (doseq [[_role plan] plans]
        (is (false?
             (recursively-contains-key?
              plan
              :diagnostic-proof-sidecar)))
        (is (false?
             (recursively-contains-key?
              plan
              :proof)))
        (is (false?
             (recursively-contains-key?
              plan
              :obligations)))))))

(deftest executable-digests-bind-only-the-exact-runtime-artifact
  (let [original
        (artifact/emit-artifacts
         (all-boundary-choreography))

        reordered
        (artifact/emit-artifacts
         (equivalent-choreography-with-reordered-state-map))

        metadata-only
        (artifact/emit-artifacts
         (same-runtime-semantics-with-different-source-metadata))

        changed
        (artifact/emit-artifacts
         (changed-runtime-semantics))

        original-browser
        (get-in original [:executable-plans :browser])

        reordered-browser
        (get-in reordered [:executable-plans :browser])

        metadata-browser
        (get-in metadata-only [:executable-plans :browser])

        changed-browser
        (get-in changed [:executable-plans :browser])

        original-digest
        (artifact/executable-digest original-browser)]

    (testing "digest is an explicit SHA-256 content identity"
      (is (string? original-digest))
      (is (re-matches #"[0-9a-f]{64}"
                      original-digest)))

    (testing "equivalent executable plans have the same digest regardless of source map insertion order"
      (is (= original-browser
             reordered-browser))
      (is (= original-digest
             (artifact/executable-digest
              reordered-browser))))

    (testing "compiler/source metadata that does not survive projection cannot perturb the executable digest"
      (is (= original-browser
             metadata-browser))
      (is (= original-digest
             (artifact/executable-digest
              metadata-browser)))
      (is (= :example/artifact-all-boundaries
             (get-in original
                     [:diagnostic-proof-sidecar
                      :choreography-name])))
      (is (= :example/artifact-renamed
             (get-in metadata-only
                     [:diagnostic-proof-sidecar
                      :choreography-name]))))

    (testing "a runtime-semantic change changes the digest"
      (is (not= original-browser
                changed-browser))
      (is (not= original-digest
                (artifact/executable-digest
                 changed-browser))))))

(deftest sidecar-binds-the-current-structural-certificate-to-the-exact-plans
  (let [emitted
        (artifact/emit-artifacts
         (all-boundary-choreography))

        plans
        (:executable-plans emitted)

        sidecar
        (:diagnostic-proof-sidecar emitted)

        certificate
        (:proof sidecar)]
    (is (= 2
           (:gesso.choreo/version sidecar)))
    (is (= proof/projection-structural-certificate-version
           (:projection-structural-certificate-version sidecar)))
    (is (proof/projection-structural-certificate?
         certificate))
    (is (proof/structural-certificate-valid?
         certificate))
    (is (= proof/projection-structural-certificate-properties
           (:properties certificate)))

    (testing "the sidecar carries all five current structural sub-proofs"
      (is (= proof/projection-boundary-property
             (get-in certificate [:boundary-proof :property])))
      (is (= proof/projection-successor-property
             (get-in certificate [:successor-proof :property])))
      (is (= proof/projection-completion-property
             (get-in certificate [:completion-proof :property])))
      (is (= proof/projection-observable-origin-property
             (get-in certificate [:observable-origin-proof :property])))
      (is (= proof/projection-runtime-origin-property
             (get-in certificate [:runtime-origin-proof :property])))
      (is (proof/valid?
           (:boundary-proof certificate)))
      (is (proof/valid?
           (:successor-proof certificate)))
      (is (proof/valid?
           (:completion-proof certificate)))
      (is (proof/valid?
           (:observable-origin-proof certificate)))
      (is (proof/valid?
           (:runtime-origin-proof certificate))))

    (testing "the certificate remains diagnostic while digests bind it to exact runtime artifacts"
      (is (= (set (keys plans))
             (set (keys (:executable-digests sidecar)))))
      (doseq [[role plan] plans]
        (is (= (artifact/executable-digest plan)
               (get-in sidecar
                       [:executable-digests role]))))
      (is (artifact/sidecar-matches?
           plans
           sidecar)))))

(deftest sidecar-rejects-stale-or-invalid-structural-proof-artifacts
  (let [emitted
        (artifact/emit-artifacts
         (all-boundary-choreography))

        plans
        (:executable-plans emitted)

        sidecar
        (:diagnostic-proof-sidecar emitted)

        stale-version
        (assoc sidecar
               :projection-structural-certificate-version
               1)

        invalid-certificate
        (assoc-in sidecar
                  [:proof :valid?]
                  false)]

    (testing "the sidecar format is tied to the current structural-certificate version"
      (is (false?
           (artifact/diagnostic-sidecar?
            stale-version)))
      (is (false?
           (artifact/sidecar-matches?
            plans
            stale-version))))

    (testing "an invalid structural certificate cannot be laundered by matching executable digests"
      (is (false?
           (artifact/diagnostic-sidecar?
            invalid-certificate)))
      (is (false?
           (artifact/sidecar-matches?
            plans
            invalid-certificate)))

      (let [data
            (error-data
             #(artifact/require-sidecar-match!
               plans
               invalid-certificate))]
        (is (= :gesso.choreo.artifact/error
               (:error/type data)))
        (is (= :invalid-diagnostic-sidecar
               (:error/kind data)))))))

(deftest sidecar-cannot-relabel-a-historical-certificate-as-current
  (let [emitted
        (artifact/emit-artifacts
         (all-boundary-choreography))

        plans
        (:executable-plans emitted)

        sidecar
        (:diagnostic-proof-sidecar emitted)

        current-certificate
        (:proof sidecar)

        historical-v3
        (historical-v3-certificate
         current-certificate)

        relabelled
        (assoc sidecar
               ;; Keep the sidecar's declared version current while replacing
               ;; the nested certificate with a genuinely valid historical v3
               ;; artifact. A sidecar validator must bind these identities
               ;; together rather than merely validating each independently.
               :projection-structural-certificate-version
               proof/projection-structural-certificate-version
               :proof
               historical-v3)]

    (is (proof/projection-structural-certificate? historical-v3))
    (is (proof/structural-certificate-valid? historical-v3))
    (is (= proof/projection-structural-certificate-v3-version
           (:gesso.choreo/version historical-v3)))
    (is (not= (:projection-structural-certificate-version relabelled)
              (get-in relabelled [:proof :gesso.choreo/version])))

    (testing "a DiagnosticProofSidecar certifies the current proof contract, not merely any recognized historical certificate"
      (is (false?
           (artifact/diagnostic-sidecar?
            relabelled)))
      (is (false?
           (artifact/sidecar-matches?
            plans
            relabelled)))

      (let [data
            (error-data
             #(artifact/require-sidecar-match!
               plans
               relabelled))]
        (is (= :gesso.choreo.artifact/error
               (:error/type data)))
        (is (= :invalid-diagnostic-sidecar
               (:error/kind data)))))))

(deftest sidecar-is-bound-to-every-emitted-executable-plan
  (let [emitted
        (artifact/emit-artifacts
         (all-boundary-choreography))

        plans
        (:executable-plans emitted)

        sidecar
        (:diagnostic-proof-sidecar emitted)

        digests
        (:executable-digests sidecar)]
    (is (= (set (keys plans))
           (set (keys digests))))

    (doseq [[role plan] plans]
      (is (= (artifact/executable-digest plan)
             (get digests role))))

    (is (artifact/sidecar-matches?
         plans
         sidecar))

    (is (= sidecar
           (artifact/require-sidecar-match!
            plans
            sidecar)))))

(deftest corrupt-or-mismatched-sidecar-is-rejected-without-changing-the-plan
  (let [emitted
        (artifact/emit-artifacts
         (all-boundary-choreography))

        plans
        (:executable-plans emitted)

        browser-plan
        (get plans :browser)

        sidecar
        (:diagnostic-proof-sidecar emitted)

        corrupt-sidecar
        (assoc-in sidecar
                  [:executable-digests :browser]
                  (apply str (repeat 64 "0")))]

    (is (false?
         (artifact/sidecar-matches?
          plans
          corrupt-sidecar)))

    (let [data
          (error-data
           #(artifact/require-sidecar-match!
             plans
             corrupt-sidecar))]
      (is (= :gesso.choreo.artifact/error
             (:error/type data)))
      (is (= :sidecar-digest-mismatch
             (:error/kind data)))
      (is (= :browser
             (:role data))))

    (testing "diagnostics cannot alter what the production machine executes"
      (is (project/executable-plan? browser-plan))
      (is (machine/execution?
           (machine/start browser-plan)))
      (is (= browser-plan
             (get plans :browser))))))

(deftest sidecar-carries-semantic-to-runtime-diagnostics-that-executable-plans-do-not
  (let [emitted
        (artifact/emit-artifacts
         (all-boundary-choreography))

        plans
        (:executable-plans emitted)

        sidecar
        (:diagnostic-proof-sidecar emitted)

        locations
        (sidecar-locations sidecar)]
    (is (vector? locations))
    (is (seq locations))

    (doseq [location locations]
      (is (keyword? (:role location)))
      (is (nat-int? (:runtime-locator location)))
      (is (contains? location :semantic-state))
      (is (vector? (:proof-obligation-id location))))

    (testing "semantic source identities are diagnostics, while runtime state identity stays compact"
      (doseq [[_role plan] plans]
        (is (every? nat-int?
                    (keys (:states plan))))
        (is (nat-int? (:initial plan)))))

    (testing "the sidecar identifies both communication endpoints where proof obligations do"
      (let [command-locations
            (filterv
             #(= :command
                 (:semantic-state %))
             locations)]
        (is (= #{:sender :receiver}
               (set (map :endpoint
                         command-locations))))))))

(deftest diagnostic-sidecar-does-not-own-or-embed-production-plans
  (let [emitted
        (artifact/emit-artifacts
         (all-boundary-choreography))

        sidecar
        (:diagnostic-proof-sidecar emitted)]
    (is (false?
         (contains? sidecar
                    :executable-plans)))
    (is (false?
         (recursively-contains-executable-plan?
          sidecar)))))

(deftest artifact-predicates-reject-lookalikes
  (is (false?
       (artifact/artifact-set? {})))
  (is (false?
       (artifact/diagnostic-sidecar? {})))

  (let [emitted
        (artifact/emit-artifacts
         (all-boundary-choreography))

        sidecar
        (:diagnostic-proof-sidecar emitted)]
    (is (false?
         (artifact/diagnostic-sidecar?
          (assoc sidecar
                 :gesso.choreo/version
                 999))))))

(deftest diagnostic-sidecar-top-level-shape-is-closed
  (let [emitted
        (artifact/emit-artifacts
         (all-boundary-choreography))

        plans
        (:executable-plans emitted)

        sidecar
        (:diagnostic-proof-sidecar emitted)]
    (is (= #{:gesso.choreo/type
             :gesso.choreo/version
             :executable-plan-version
             :proof-version
             :projection-structural-certificate-version
             :choreography-name
             :executable-digests
             :locations
             :proof}
           (set (keys sidecar))))

    (doseq [extra-key
            [:adapter/extra
             :runtime/control
             :executable-plans]]
      (let [lookalike
            (assoc sidecar extra-key :must-not-be-accepted)]
        (is (false?
             (artifact/diagnostic-sidecar?
              lookalike)))
        (is (false?
             (artifact/sidecar-matches?
              plans
              lookalike)))

        (let [data
              (error-data
               #(artifact/require-sidecar-match!
                 plans
                 lookalike))]
          (is (= :gesso.choreo.artifact/error
                 (:error/type data)))
          (is (= :invalid-diagnostic-sidecar
                 (:error/kind data))))))))

(deftest artifact-set-top-level-shape-is-closed
  (let [emitted
        (artifact/emit-artifacts
         (all-boundary-choreography))]
    (is (= #{:gesso.choreo/type
             :gesso.choreo/version
             :executable-plans
             :diagnostic-proof-sidecar}
           (set (keys emitted))))

    (doseq [extra-key
            [:adapter/extra
             :runtime/control
             :proof]]
      (is (false?
           (artifact/artifact-set?
            (assoc emitted
                   extra-key
                   :must-not-be-accepted)))))))

(deftest require-sidecar-match-validates-the-plan-map-before-digest-comparison
  (let [emitted
        (artifact/emit-artifacts
         (all-boundary-choreography))

        browser-plan
        (get-in emitted
                [:executable-plans :browser])

        malformed-plans
        {:not-the-browser-role browser-plan}

        matching-lookalike-sidecar
        (assoc
         (:diagnostic-proof-sidecar emitted)
         :executable-digests
         {:not-the-browser-role
          (artifact/executable-digest browser-plan)})]

    ;; The public boolean checker already knows this plan map is invalid because
    ;; the map key and ExecutablePlan :role disagree.  The throwing checker must
    ;; enforce the same precondition before comparing digests.
    (is (false?
         (artifact/sidecar-matches?
          malformed-plans
          matching-lookalike-sidecar)))

    (doseq [plans
            [malformed-plans
             [:not :a-plan-map]]]
      (let [data
            (error-data
             #(artifact/require-sidecar-match!
               plans
               matching-lookalike-sidecar))]
        (is (= :gesso.choreo.artifact/error
               (:error/type data)))
        (is (= :invalid-executable-plans
               (:error/kind data)))))))

(deftest executable-digest-requires-an-executable-plan
  (let [data
        (error-data
         #(artifact/executable-digest
           {:not :an-executable-plan}))]
    (is (= :gesso.choreo.artifact/error
           (:error/type data)))
    (is (= :invalid-executable-plan
           (:error/kind data)))))


(deftest lockstep-correspondence-evidence-is-bound-to-the-exact-artifact-set
  (let [choreography
        (artifact-bound-protocol :done)

        emitted
        (artifact/emit-artifacts choreography)

        evidence
        (artifact/check-artifact-witness
         emitted
         choreography
         artifact-bound-good-witness
         {:require-complete? true})]

    (is (artifact/artifact-correspondence-evidence? evidence))
    (is (artifact/artifact-correspondence-valid? evidence))
    (is (artifact/evidence-matches? emitted evidence))
    (is (= :lockstep (:mode evidence)))
    (is (= correspondence/correspondence-property
           (get-in evidence [:correspondence :property])))
    (is (= artifact/artifact-correspondence-evidence-nonclaims
           (:nonclaims evidence)))
    (is (= :done
           (get-in evidence [:correspondence :global-outcome])))
    (is (= (get-in emitted
                   [:diagnostic-proof-sidecar
                    :executable-digests])
           (:executable-digests evidence)))
    (is (= (get-in emitted
                   [:diagnostic-proof-sidecar
                    :proof])
           (:structural-certificate evidence)))
    (is (= evidence
           (artifact/require-evidence-match!
            emitted
            evidence)))))

(deftest weak-correspondence-evidence-is-bound-to-the-exact-artifact-set
  (let [choreography
        (artifact-bound-protocol :done)

        emitted
        (artifact/emit-artifacts choreography)

        evidence
        (artifact/check-artifact-weak-witness
         emitted
         choreography
         artifact-bound-good-witness
         {:require-complete? true})]

    (is (artifact/artifact-correspondence-evidence? evidence))
    (is (artifact/artifact-correspondence-valid? evidence))
    (is (artifact/evidence-matches? emitted evidence))
    (is (= :weak (:mode evidence)))
    (is (= correspondence/weak-correspondence-property
           (get-in evidence [:correspondence :property])))))

(deftest failed-concrete-witness-can-remain-correctly-artifact-bound
  (let [choreography
        (artifact-bound-protocol :done)

        emitted
        (artifact/emit-artifacts choreography)

        evidence
        (artifact/check-artifact-witness
         emitted
         choreography
         artifact-bound-incomplete-witness
         {:require-complete? true})]

    (is (artifact/artifact-correspondence-evidence? evidence))
    (is (false?
         (artifact/artifact-correspondence-valid? evidence)))
    (is (artifact/evidence-matches? emitted evidence))
    (is (false? (:valid? evidence)))
    (is (seq (get-in evidence
                     [:correspondence :failures])))))

(deftest identical-executable-plans-cannot-hide-different-global-terminal-semantics
  (let [done
        (artifact-bound-protocol :done)

        rejected
        (artifact-bound-protocol :rejected)

        done-artifacts
        (artifact/emit-artifacts done)

        rejected-artifacts
        (artifact/emit-artifacts rejected)

        done-digests
        (get-in done-artifacts
                [:diagnostic-proof-sidecar
                 :executable-digests])

        rejected-digests
        (get-in rejected-artifacts
                [:diagnostic-proof-sidecar
                 :executable-digests])

        done-proof
        (get-in done-artifacts
                [:diagnostic-proof-sidecar
                 :proof])

        rejected-proof
        (get-in rejected-artifacts
                [:diagnostic-proof-sidecar
                 :proof])

        data
        (error-data
         #(artifact/check-artifact-witness
           done-artifacts
           rejected
           artifact-bound-good-witness
           {:require-complete? true}))]

    ;; Projection intentionally erases the authored global terminal outcome, so
    ;; these two programs can have byte-for-byte equivalent local executable
    ;; artifacts. Digest equality alone is therefore insufficient global-program
    ;; identity for theorem-facing evidence composition.
    (is (= done-digests rejected-digests))
    (is (not= done-proof rejected-proof))
    (is (= :gesso.choreo.artifact/error
           (:error/type data)))
    (is (= :artifact-proof-choreography-mismatch
           (:error/kind data)))))

(deftest changed-runtime-artifact-is-rejected-before-correspondence-replay
  (let [certified
        (artifact-bound-protocol :done)

        changed
        (choreo/->choreography
         {:name :example/artifact-bound-runtime-change
          :initial :prepare
          :states
          {:prepare
           (choreo/local
            :browser
            :different-action
            :done)

           :done
           (choreo/return :done)}})

        emitted
        (artifact/emit-artifacts certified)

        data
        (error-data
         #(artifact/check-artifact-witness
           emitted
           changed
           []))]

    (is (= :gesso.choreo.artifact/error
           (:error/type data)))
    (is (= :artifact-choreography-mismatch
           (:error/kind data)))))

(deftest artifact-correspondence-evidence-contract-fails-closed
  (let [choreography
        (artifact-bound-protocol :done)

        emitted
        (artifact/emit-artifacts choreography)

        evidence
        (artifact/check-artifact-witness
         emitted
         choreography
         artifact-bound-good-witness
         {:require-complete? true})

        rejected-artifacts
        (artifact/emit-artifacts
         (artifact-bound-protocol :rejected))]

    (is (= #{:gesso.choreo/type
             :gesso.choreo/version
             :property
             :classification
             :valid?
             :mode
             :artifact-version
             :correspondence-version
             :executable-digests
             :structural-certificate
             :correspondence
             :nonclaims}
           (set (keys evidence))))

    (doseq [lookalike
            [(assoc evidence :mode :weak)
             (assoc evidence :gesso.choreo/version 999)
             (assoc evidence :artifact-version 999)
             (assoc evidence :correspondence-version 999)
             (assoc evidence :nonclaims #{})
             (assoc evidence :extra :not-allowed)
             (assoc-in evidence
                       [:structural-certificate :valid?]
                       false)]]
      (is (false?
           (artifact/artifact-correspondence-evidence?
            lookalike))))

    (is (false?
         (artifact/evidence-matches?
          rejected-artifacts
          evidence)))

    ;; The executable plans are intentionally identical for the terminal-only
    ;; change, so require-evidence-match! reaches the independent proof binding
    ;; check rather than failing first on digest identity.
    (let [data
          (error-data
           #(artifact/require-evidence-match!
             rejected-artifacts
             evidence))]
      (is (= :gesso.choreo.artifact/error
             (:error/type data)))
      (is (= :evidence-proof-mismatch
             (:error/kind data))))))


(ns gesso.choreo.artifact-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.walk :as walk]
   [gesso.choreo.artifact :as artifact]
   [gesso.choreo.core :as choreo]
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

(deftest artifact-vocabulary-is-explicit-and-versioned
  (is (= 1 artifact/artifact-version))
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
    (is (proof/result? (:proof sidecar)))
    (is (proof/valid? (:proof sidecar)))

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

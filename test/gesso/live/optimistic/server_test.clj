(ns gesso.live.optimistic.server-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.machine :as machine]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.server :as server]))

;; -----------------------------------------------------------------------------
;; Fixtures / helpers
;; -----------------------------------------------------------------------------

(def request-scope
  [:request "request-1"])

(def request-wire-scope
  (protocol/wire-scope request-scope))

(def pending-card
  [:details
   {:data-request-card "request-1"
    :open true}
   [:summary "Claimed"]
   [:div "Confirming…"]])

(def canonical-card
  [:details
   {:data-request-card "request-1"}
   [:summary "Claimed"]
   [:div "Authoritative"]])

(defn descriptor
  ([]
   (descriptor nil))
  ([opts]
   (server/->optimistic
    (merge
     {:transition :request/claim
      :scope request-scope
      :target "request-card-1"
      :base-revision 7
      :template-name "request-1-claim"
      :pending-label "Claiming…"
      :content pending-card}
     opts))))

(defn command
  ([]
   (command nil))
  ([opts]
   (merge
    {:transition :request/claim
     :scope request-scope
     :base-revision 7}
    opts)))

(defn request-ctx
  ([]
   (request-ctx "exec-1"))
  ([execution-id]
   {:headers
    {server/execution-request-header execution-id}
    :user/id "user-1"}))

(defn settlement
  ([]
   (settlement nil))
  ([opts]
   (server/->settlement
    (merge
     {:execution-id "exec-1"
      :scope request-scope
      :outcome :confirmed
      :revision 8}
     opts))))

(defn attrs
  [node]
  (when (and (vector? node)
             (map? (second node)))
    (second node)))

(defn children
  [node]
  (let [xs (rest node)]
    (if (map? (first xs))
      (rest xs)
      xs)))

;; -----------------------------------------------------------------------------
;; Public identities
;; -----------------------------------------------------------------------------

(deftest public-identity-test
  (testing "server facade reflects protocol identities rather than redefining them"
    (is (= protocol/version server/protocol-version))
    (is (= protocol/execution-header-name
           server/execution-request-header))
    (is (= protocol/projection-modes
           server/projection-modes))
    (is (= protocol/settlement-outcomes
           server/settlement-outcomes)))

  (testing "server attr aliases are exactly the protocol-owned attrs"
    (is (= protocol/protocol-attr server/protocol-attr))
    (is (= protocol/transition-attr server/transition-attr))
    (is (= protocol/template-attr server/template-attr))
    (is (= protocol/target-attr server/target-attr))
    (is (= protocol/scope-attr server/scope-attr))
    (is (= protocol/base-revision-attr server/base-revision-attr))
    (is (= protocol/revision-attr server/revision-attr))
    (is (= protocol/pending-label-attr server/pending-label-attr))
    (is (= protocol/projection-mode-attr server/projection-mode-attr))
    (is (= protocol/settlement-attr server/settlement-attr))
    (is (= protocol/execution-attr server/execution-attr))
    (is (= protocol/outcome-attr server/outcome-attr))
    (is (= protocol/command-applied-attr server/command-applied-attr))
    (is (= protocol/reason-attr server/reason-attr))
    (is (= protocol/canonical-attr server/canonical-attr))))

;; -----------------------------------------------------------------------------
;; Descriptor construction
;; -----------------------------------------------------------------------------

(deftest new-template-name-test
  (testing "generated names are browser-safe and unique"
    (let [a (server/new-template-name)
          b (server/new-template-name)]
      (is (str/starts-with? a server/default-template-prefix))
      (is (str/starts-with? b server/default-template-prefix))
      (is (not= a b)))))

(deftest target-sync-test
  (testing "target-scoped synchronization follows normalized HTMX targets"
    (is (= "closest [data-request-card]:drop"
           (server/target-sync
            "closest [data-request-card]")))
    (is (= "#request-card-1:drop"
           (server/target-sync "request-card-1")))
    (is (= "#request-card-1:abort"
           (server/target-sync "request-card-1" :abort))))

  (testing "invalid target/strategy values are rejected"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":target must be a non-blank string"
         (server/target-sync nil)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"name must not be blank"
         (server/target-sync "request-card-1" "")))))

(deftest optimistic-descriptor-test
  (let [prepared
        (descriptor
         {:attrs {:class "action-source"}
          :template-attrs {:class "optimistic-template"}})]
    (testing "descriptor stores semantic application values and normalized browser mechanics"
      (is (server/optimistic? prepared))
      (is (= server/descriptor-type
             (:gesso.live.optimistic/type prepared)))
      (is (= protocol/version (:protocol-version prepared)))
      (is (= "request/claim" (:transition prepared)))
      (is (= request-scope (:scope prepared)))
      (is (= 7 (:base-revision prepared)))
      (is (= "#request-card-1" (:target prepared)))
      (is (= pending-card (:content prepared)))
      (is (= :provisional (:projection-mode prepared)))
      (is (= "request-1-claim" (:template-name prepared)))
      (is (= "Claiming…" (:pending-label prepared)))
      (is (= "#request-card-1:drop" (:sync prepared)))
      (is (= {:class "action-source"} (:attrs prepared)))
      (is (= {:class "optimistic-template"}
             (:template-attrs prepared))))

    (testing "ensure-optimistic preserves an already-prepared descriptor"
      (is (identical? prepared
                      (server/ensure-optimistic prepared))))))

(deftest descriptor-normalization-test
  (testing "generated identity is supplied when template-name is omitted"
    (let [prepared
          (server/->optimistic
           {:transition :request/claim
            :scope request-scope
            :target "request-card-1"
            :content pending-card})]
      (is (str/starts-with?
           (:template-name prepared)
           server/default-template-prefix))))

  (testing "projection mode is explicit and validated"
    (is (= :pending
           (:projection-mode
            (descriptor {:projection-mode :pending}))))
    (is (= :full
           (:projection-mode
            (descriptor {:projection-mode :full}))))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid Gesso Live optimistic projection mode"
         (descriptor {:projection-mode :invented}))))

  (testing "explicit sync overrides or disables the target default"
    (is (= "closest form:drop"
           (:sync (descriptor {:sync "closest form:drop"}))))
    (is (nil? (:sync (descriptor {:sync nil}))))
    (is (nil? (:sync (descriptor {:sync false})))))

  (testing "opaque semantic scopes remain semantic in the server descriptor"
    (is (= {:request/id "request-1"}
           (:scope
            (descriptor
             {:scope {:request/id "request-1"}}))))))

(deftest optimistic-validation-test
  (testing "transition, scope, target, and content are required"
    (doseq [[opts pattern]
            [[{:transition nil} #"name must not be blank"]
             [{:scope nil} #"scope is required"]
             [{:target nil} #":target must be a non-blank string"]
             [{:content nil} #"requires :content"]]]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           pattern
           (descriptor opts)))))

  (testing "projection content must be one rooted Hiccup element"
    (doseq [invalid
            [[:<> [:div "one"] [:div "two"]]
             [[:div "one"] [:div "two"]]
             (list [:div "one"])
             "not hiccup"]]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"one rooted Hiccup element"
           (descriptor {:content invalid})))))

  (testing "attrs and template-attrs must be maps"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":attrs must be a map"
         (descriptor {:attrs [:not-a-map]})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":template-attrs must be a map"
         (descriptor {:template-attrs [:not-a-map]}))))

  (testing "pending label and sync are strict when supplied"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":pending-label must be a non-blank string"
         (descriptor {:pending-label ""})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":sync must be"
         (descriptor {:sync ""})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":sync must be"
         (descriptor {:sync :drop})))))

;; -----------------------------------------------------------------------------
;; Browser-facing optimistic markup
;; -----------------------------------------------------------------------------

(deftest source-attrs-test
  (let [source
        (server/source-attrs
         (descriptor
          {:attrs
           {:class "claim-button"
            :data-app-action "claim"}}))]
    (is (= {:class "claim-button"
            :data-app-action "claim"
            :data-gesso-optimistic-protocol "2"
            :data-gesso-optimistic-transition "request/claim"
            :data-gesso-optimistic-template "request-1-claim"
            :data-gesso-optimistic-target "#request-card-1"
            :data-gesso-optimistic-scope request-wire-scope
            :data-gesso-optimistic-base-revision "i:7"
            :data-gesso-optimistic-label "Claiming…"
            :data-gesso-optimistic-mode "provisional"}
           source))))

(deftest source-attrs-protect-framework-vocabulary-test
  (let [prepared
        (descriptor
         {:attrs
          (zipmap
           protocol/reserved-attrs
           (repeat "wrong"))})
        source (server/source-attrs prepared)]
    (testing "caller attrs cannot forge protocol identity"
      (is (= "2" (get source protocol/protocol-attr)))
      (is (= "request/claim"
             (get source protocol/transition-attr)))
      (is (= "request-1-claim"
             (get source protocol/template-attr)))
      (is (= "#request-card-1"
             (get source protocol/target-attr)))
      (is (= request-wire-scope
             (get source protocol/scope-attr)))
      (is (= "i:7"
             (get source protocol/base-revision-attr)))
      (is (= "Claiming…"
             (get source protocol/pending-label-attr)))
      (is (= "provisional"
             (get source protocol/projection-mode-attr))))

    (testing "attrs irrelevant to a command source are removed instead of preserved"
      (doseq [k [protocol/revision-attr
                 protocol/settlement-attr
                 protocol/execution-attr
                 protocol/outcome-attr
                 protocol/command-applied-attr
                 protocol/reason-attr
                 protocol/canonical-attr]]
        (is (not (contains? source k)))))))

(deftest optimistic-template-test
  (let [node
        (server/template
         (descriptor
          {:template-attrs
           {:class "optimistic-template"
            protocol/canonical-attr "wrong"}}))]
    (is (= :template (first node)))
    (is (= pending-card (nth node 2)))
    (is (= {:class "optimistic-template"
            :data-gesso-optimistic-protocol "2"
            :data-gesso-optimistic-transition "request/claim"
            :data-gesso-optimistic-template "request-1-claim"
            :data-gesso-optimistic-scope request-wire-scope
            :data-gesso-optimistic-mode "provisional"}
           (attrs node)))
    (is (not (contains? (attrs node)
                        protocol/canonical-attr)))))

(deftest render-parts-test
  (let [parts
        (server/render-parts
         {:transition :request/claim
          :scope request-scope
          :target "request-card-1"
          :content pending-card})
        prepared (:optimistic parts)
        template-name (:template-name prepared)]
    (testing "one preparation supplies all UI pieces"
      (is (server/optimistic? prepared))
      (is (= "#request-card-1:drop" (:sync parts)))
      (is (= template-name
             (get (:source-attrs parts)
                  protocol/template-attr)))
      (is (= template-name
             (get-in parts [:template 1 protocol/template-attr]))))

    (testing "source/template helpers reject raw opts to prevent split generated identities"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"require a prepared descriptor"
           (server/source-attrs
            {:transition :request/claim
             :scope request-scope
             :target "request-card-1"
             :content pending-card})))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"require a prepared descriptor"
           (server/template
            {:transition :request/claim
             :scope request-scope
             :target "request-card-1"
             :content pending-card}))))))

;; -----------------------------------------------------------------------------
;; Canonical authority
;; -----------------------------------------------------------------------------

(deftest canonical-attrs-test
  (testing "canonical authority is explicit and carries typed revision identity"
    (is (= {:data-gesso-optimistic-protocol "2"
            :data-gesso-optimistic-scope request-wire-scope
            :data-gesso-optimistic-revision "i:8"
            :data-gesso-optimistic-canonical "true"}
           (server/canonical-attrs
            {:scope request-scope
             :revision 8}))))

  (testing "revision is optional but scope is not"
    (is (= {:data-gesso-optimistic-protocol "2"
            :data-gesso-optimistic-scope request-wire-scope
            :data-gesso-optimistic-canonical "true"}
           (server/canonical-attrs
            {:scope request-scope})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"scope is required"
         (server/canonical-attrs {})))))

(deftest canonical-test
  (let [node
        (server/canonical
         {:scope request-scope
          :revision "rev-8"}
         [:article
          {:id "request-card-1"
           protocol/scope-attr "wrong"
           protocol/canonical-attr "false"
           protocol/revision-attr "s:wrong"}
          "Canonical"])]
    (testing "application attrs survive while all protocol attrs are authoritative"
      (is (= :article (first node)))
      (is (= "Canonical" (nth node 2)))
      (is (= {:id "request-card-1"
              :data-gesso-optimistic-protocol "2"
              :data-gesso-optimistic-scope request-wire-scope
              :data-gesso-optimistic-revision "s:rev-8"
              :data-gesso-optimistic-canonical "true"}
             (attrs node)))))

  (testing "canonical requires exactly one rooted Hiccup element"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"one rooted Hiccup element"
         (server/canonical
          {:scope request-scope}
          [:<> [:div "a"] [:div "b"]])))))

;; -----------------------------------------------------------------------------
;; Request execution identity
;; -----------------------------------------------------------------------------

(deftest request-execution-id-test
  (testing "explicit adapter injection has highest precedence"
    (is (= "explicit"
           (server/request-execution-id
            {:gesso.live.optimistic/execution-id "explicit"
             :headers
             {server/execution-request-header "header"}}))))

  (testing "normal lower-case Ring headers are accepted"
    (is (= "exec-1"
           (server/request-execution-id
            (request-ctx)))))

  (testing "legacy/case-preserving adapters and nested request maps are tolerated"
    (is (= "upper"
           (server/request-execution-id
            {:headers
             {"Gesso-Optimistic-Execution" "upper"}})))
    (is (= "nested"
           (server/request-execution-id
            {:request
             {:headers
              {server/execution-request-header "nested"}}}))))

  (testing "missing identity remains nil for non-optimistic requests"
    (is (nil? (server/request-execution-id {})))))

(deftest require-request-execution-id-test
  (is (= "exec-1"
         (server/require-request-execution-id
          (request-ctx))))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":execution-id must be a non-blank string"
       (server/require-request-execution-id {}))))

;; -----------------------------------------------------------------------------
;; Semantic settlements
;; -----------------------------------------------------------------------------

(deftest settlement-construction-test
  (doseq [[outcome applied?]
          [[:confirmed true]
           [:reconciled true]
           [:rejected false]
           [:failed false]]]
    (testing (str "command-applied? is derived for " outcome)
      (let [prepared
            (settlement
             {:outcome outcome
              :reason :request/updated
              :consistency-token "token-8"})]
        (is (server/settlement? prepared))
        (is (= server/settlement-type
               (:gesso.live.optimistic/type prepared)))
        (is (= "exec-1" (:execution-id prepared)))
        (is (= request-scope (:scope prepared)))
        (is (= outcome (:outcome prepared)))
        (is (= 8 (:revision prepared)))
        (is (= applied? (:command-applied? prepared)))
        (is (= "request/updated" (:reason prepared)))
        (is (= "token-8" (:consistency-token prepared))))))

  (testing "ensure-settlement preserves a prepared settlement"
    (let [prepared (settlement)]
      (is (identical? prepared
                      (server/ensure-settlement prepared))))))

(deftest settlement-validation-test
  (testing "transition is deliberately not settlement correlation"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"does not accept redundant :transition"
         (settlement {:transition :request/claim}))))

  (testing "callers cannot supply command-applied?"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"does not accept :command-applied"
         (settlement {:command-applied? false}))))

  (testing "execution id, scope, and outcome are required"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":execution-id must be a non-blank string"
         (settlement {:execution-id nil})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"scope is required"
         (settlement {:scope nil})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid Gesso Live optimistic settlement outcome"
         (settlement {:outcome :maybe})))))

(deftest settlement-for-request-test
  (testing "request identity fills the settlement execution id"
    (is (= "exec-1"
           (:execution-id
            (server/settlement-for-request
             (request-ctx)
             {:scope request-scope
              :outcome :confirmed
              :revision 8})))))

  (testing "explicit settlement identity wins when deliberately supplied"
    (is (= "explicit"
           (:execution-id
            (server/settlement-for-request
             (request-ctx)
             {:execution-id "explicit"
              :scope request-scope
              :outcome :confirmed}))))))

(deftest settlement-marker-test
  (let [marker
        (server/settlement-marker
         (settlement
          {:outcome :reconciled
           :reason :request/conflict
           :consistency-token "secret-transport-token"}))
        marker-attrs (attrs marker)]
    (testing "marker carries only browser settlement semantics"
      (is (= :template (first marker)))
      (is (= {:data-gesso-optimistic-protocol "2"
              :data-gesso-optimistic-settlement "true"
              :data-gesso-optimistic-execution "exec-1"
              :data-gesso-optimistic-scope request-wire-scope
              :data-gesso-optimistic-outcome "reconciled"
              :data-gesso-optimistic-revision "i:8"
              :data-gesso-optimistic-command-applied "true"
              :data-gesso-optimistic-reason "request/conflict"}
             marker-attrs)))

    (testing "consistency token transport is intentionally absent from DOM markup"
      (is (not-any?
           #(str/includes? (name %) "consistency")
           (keys marker-attrs)))
      (is (not (str/includes?
                (pr-str marker)
                "secret-transport-token"))))))

(deftest with-settlement-test
  (let [rendered
        (server/with-settlement
         (settlement {:outcome :confirmed})
         canonical-card
         nil
         [:aside "extra"])
        rendered-children (vec (children rendered))
        marker (nth rendered-children 0)
        canonical-node (nth rendered-children 1)
        extra (nth rendered-children 2)]
    (testing "response is an inert display-contents wrapper"
      (is (= :div (first rendered)))
      (is (= {:style {:display "contents"}}
             (attrs rendered))))

    (testing "marker and canonical root derive from the same settlement"
      (is (= "exec-1"
             (get (attrs marker)
                  protocol/execution-attr)))
      (is (= request-wire-scope
             (get (attrs marker)
                  protocol/scope-attr)))
      (is (= request-wire-scope
             (get (attrs canonical-node)
                  protocol/scope-attr)))
      (is (= "i:8"
             (get (attrs canonical-node)
                  protocol/revision-attr)))
      (is (= "true"
             (get (attrs canonical-node)
                  protocol/canonical-attr))))

    (testing "nil extras are omitted and real extras follow canonical content"
      (is (= [:aside "extra"] extra))
      (is (= 3 (count rendered-children))))))

;; -----------------------------------------------------------------------------
;; Command payload / projected server endpoint
;; -----------------------------------------------------------------------------

(deftest command-payload-test
  (testing "command payload derives execution id from request context"
    (is (= {:execution-id "exec-1"
            :transition "request/claim"
            :scope request-wire-scope
            :base-revision 7
            :consistency-token "read-token"}
           (server/command-payload
            (request-ctx)
            (command
             {:consistency-token "read-token"})))))

  (testing "execution id may be injected explicitly for an adapter/test"
    (is (= "explicit"
           (:execution-id
            (server/command-payload
             {}
             (command {:execution-id "explicit"}))))))

  (testing "optional command values are omitted when absent"
    (is (= #{:execution-id :transition :scope}
           (set
            (keys
             (server/command-payload
              (request-ctx)
              {:transition :request/claim
               :scope request-scope})))))))

(deftest begin-command-test
  (let [execution
        (server/begin-command
         (request-ctx)
         (command))
        action (machine/pending-action execution)
        context (server/command-context execution)]
    (testing "server projection reaches exactly its application FX boundary"
      (is (machine/waiting-fx? execution))
      (is (= "exec-1" (:execution-id execution)))
      (is (= :fx (:kind action)))
      (is (= optimistic-choreo/server-execute-machine
             (:machine action))))

    (testing "received command is correlated, merged, and bound as a complete payload"
      (is (= "exec-1" (:execution-id context)))
      (is (= request-wire-scope (:scope context)))
      (is (= "request/claim" (:transition context)))
      (is (= 7 (:base-revision context)))
      (is (= {:execution-id "exec-1"
              :transition "request/claim"
              :scope request-wire-scope
              :base-revision 7}
             (:command context))))))

(deftest biff-fx-context-test
  (let [execution
        (server/begin-command
         (request-ctx)
         (command))
        fx-ctx
        (server/biff-fx-context
         {:user/id "user-1"
          :scope "app-value-that-protocol-may-overwrite"}
         execution)]
    (testing "ordinary application ctx and protocol context are merged"
      (is (= "user-1" (:user/id fx-ctx)))
      (is (= request-wire-scope (:scope fx-ctx)))
      (is (= "request/claim" (:transition fx-ctx))))

    (testing "application FX gets both explicit optimistic aliases"
      (is (= "exec-1"
             (:gesso.live.optimistic/execution-id fx-ctx)))
      (is (= (:command (server/command-context execution))
             (:gesso.live.optimistic/command fx-ctx))))))

(deftest run-biff-fx-test
  (let [seen (atom nil)
        execution
        (server/begin-command
         (request-ctx)
         (command))
        result
        (server/run-biff-fx
         {:user/id "user-1"}
         execution
         (fn [ctx]
           (reset! seen ctx)
           {:status :claimed}))]
    (is (= {:status :claimed} result))
    (is (= "user-1" (:user/id @seen)))
    (is (= "exec-1"
           (:gesso.live.optimistic/execution-id @seen))))

  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":fx-machine must be callable"
       (server/run-biff-fx
        {}
        (server/begin-command
         (request-ctx)
         (command))
        :not-callable))))

;; -----------------------------------------------------------------------------
;; Settlement send boundary
;; -----------------------------------------------------------------------------

(deftest prepare-settlement-send-test
  (let [execution
        (server/begin-command
         (request-ctx)
         (command))
        semantic-settlement
        (settlement
         {:outcome :confirmed
          :revision 8
          :reason :request/claimed
          :consistency-token "tx-8"})
        prepared
        (server/prepare-settlement-send
         execution
         semantic-settlement
         canonical-card)
        action (:action prepared)
        payload (:payload action)]
    (testing "prepared value owns a server execution waiting at the HTTP send boundary"
      (is (server/prepared-server-send? prepared))
      (is (machine/waiting-send? (:execution prepared)))
      (is (= :send (:kind action)))
      (is (= optimistic-choreo/browser-role (:to action)))
      (is (= protocol/settlement-event (:event action)))
      (is (= :http (:via action))))

    (testing "send payload is constructed from one correlated settlement"
      (is (= "exec-1" (:execution-id payload)))
      (is (= request-wire-scope (:scope payload)))
      (is (= :confirmed (:outcome payload)))
      (is (= true (:command-applied? payload)))
      (is (= 8 (:revision payload)))
      (is (= "request/claimed" (:reason payload)))
      (is (= "tx-8" (:consistency-token payload)))
      (is (= canonical-card (:canonical payload))))

    (testing "semantic values remain available for HTTP rendering"
      (is (= semantic-settlement (:settlement prepared)))
      (is (= canonical-card (:canonical prepared))))))

(deftest prepare-settlement-send-correlation-test
  (let [execution
        (server/begin-command
         (request-ctx)
         (command))]
    (testing "settlement execution id must match the choreography execution"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"execution id does not match"
           (server/prepare-settlement-send
            execution
            (settlement {:execution-id "other"})
            canonical-card))))

    (testing "settlement scope must match the command's opaque scope identity"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"scope does not match"
           (server/prepare-settlement-send
            execution
            (settlement {:scope [:request "other"]})
            canonical-card))))

    (testing "canonical settlement content must be one rooted element"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"one rooted Hiccup element"
           (server/prepare-settlement-send
            execution
            (settlement)
            [:<> [:div "a"] [:div "b"]]))))))

(deftest prepared-response-hiccup-test
  (let [prepared
        (server/prepare-settlement-send
         (server/begin-command
          (request-ctx)
          (command))
         (settlement)
         canonical-card)
        response
        (server/prepared-response-hiccup
         prepared
         [:aside "extra"])
        response-children (vec (children response))
        canonical-node (nth response-children 1)]
    (is (= :div (first response)))
    (is (= "true"
           (get (attrs canonical-node)
                protocol/canonical-attr)))
    (is (= request-wire-scope
           (get (attrs canonical-node)
                protocol/scope-attr)))
    (is (= [:aside "extra"]
           (nth response-children 2)))))

(deftest complete-settlement-send-test
  (let [prepared
        (server/prepare-settlement-send
         (server/begin-command
          (request-ctx)
          (command))
         (settlement)
         canonical-card)
        completed
        (server/complete-settlement-send prepared)]
    (testing "completion advances the projected server endpoint to terminal"
      (is (machine/completed? (:execution completed)))
      (is (not (server/prepared-server-send? completed)))))

  (testing "only a prepared waiting-send value may be completed"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Expected a prepared optimistic server settlement send"
         (server/complete-settlement-send {})))))

;; -----------------------------------------------------------------------------
;; High-level command runner
;; -----------------------------------------------------------------------------

(deftest run-command-test
  (let [seen-fx-ctx (atom nil)
        seen-settle (atom nil)
        prepared
        (server/run-command
         (request-ctx)
         (command)
         {:fx-machine
          (fn [fx-ctx]
            (reset! seen-fx-ctx fx-ctx)
            {:request/id "request-1"
             :request/status :claimed})

          :settle
          (fn [fx-ctx fx-result]
            (reset! seen-settle
                    {:fx-ctx fx-ctx
                     :fx-result fx-result})
            {:outcome :confirmed
             :revision 8
             :reason :request/claimed
             :consistency-token "tx-8"
             :canonical canonical-card})})]
    (testing "runner returns the prepared HTTP settlement send"
      (is (server/prepared-server-send? prepared))
      (is (= :confirmed
             (get-in prepared [:settlement :outcome])))
      (is (= "tx-8"
             (get-in prepared
                     [:settlement :consistency-token]))))

    (testing "Biff FX receives application and protocol context"
      (is (= "user-1"
             (:user/id @seen-fx-ctx)))
      (is (= "exec-1"
             (:gesso.live.optimistic/execution-id
              @seen-fx-ctx)))
      (is (= "request/claim"
             (:transition @seen-fx-ctx)))
      (is (= request-wire-scope
             (:scope @seen-fx-ctx))))

    (testing "settle receives the exact same FX context and terminal result"
      (is (identical?
           @seen-fx-ctx
           (:fx-ctx @seen-settle)))
      (is (= {:request/id "request-1"
              :request/status :claimed}
             (:fx-result @seen-settle))))

    (testing "server, not settle, controls execution-id and semantic scope correlation"
      (is (= "exec-1"
             (get-in prepared
                     [:settlement :execution-id])))
      (is (= request-scope
             (get-in prepared
                     [:settlement :scope]))))))

(deftest run-command-does-not-trust-settle-correlation-test
  (let [prepared
        (server/run-command
         (request-ctx)
         (command)
         {:fx-machine (fn [_] :done)
          :settle
          (fn [_ _]
            ;; These keys are deliberately ignored by run-command's select-keys.
            {:execution-id "forged"
             :scope [:request "forged"]
             :transition :forged
             :command-applied? false
             :outcome :confirmed
             :canonical canonical-card})})]
    (is (= "exec-1"
           (get-in prepared [:settlement :execution-id])))
    (is (= request-scope
           (get-in prepared [:settlement :scope])))
    (is (= true
           (get-in prepared
                   [:settlement :command-applied?])))))

(deftest run-command-validation-test
  (testing "fx-machine and settle must be callable"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":fx-machine must be callable"
         (server/run-command
          (request-ctx)
          (command)
          {:fx-machine :no
           :settle (fn [_ _] {})})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":settle must be callable"
         (server/run-command
          (request-ctx)
          (command)
          {:fx-machine (fn [_] nil)
           :settle :no}))))

  (testing "settle must return a map containing canonical content"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":settle-result must be a map"
         (server/run-command
          (request-ctx)
          (command)
          {:fx-machine (fn [_] nil)
           :settle (fn [_ _] :not-a-map)})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"requires :canonical"
         (server/run-command
          (request-ctx)
          (command)
          {:fx-machine (fn [_] nil)
           :settle
           (fn [_ _]
             {:outcome :confirmed})})))))

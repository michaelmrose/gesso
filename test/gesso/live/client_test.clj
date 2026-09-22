(ns gesso.live.client-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.client :as client]
   [manifold.deferred :as d]
   [manifold.stream :as s]))

;; -----------------------------------------------------------------------------
;; Fixtures / helpers
;; -----------------------------------------------------------------------------

(def fragment-a
  [:div
   {:id "fragment-a"
    :hx-swap-oob "true"}
   "A"])

(def fragment-b
  [:div
   {:id "fragment-b"
    :hx-swap-oob "true"}
   "B"])

(defn- test-channel
  ([]
   (test-channel
    nil))
  ([options]
   (client/channel
    (merge
     {:id
      :test/client-channel

      :event
      "client-oob"

      :client
      (fn [ctx]
        {:client/user-id
         (:test/user-id ctx)

         :client/scopes
         (:test/scopes ctx)

         :test/label
         (:test/label ctx)})}
     options))))

(defn- client-ctx
  ([client-id]
   (client-ctx
    client-id
    nil))
  ([client-id
    {:keys [user-id
            scopes
            label
            params-location]
     :or {user-id
          "user-1"

          scopes
          #{[:store "store-1"]}

          label
          "client"

          params-location
          :query-params}}]
   {:test/user-id
    user-id

    :test/scopes
    scopes

    :test/label
    label

    params-location
    {:client-id
     client-id}}))

(defn- connect!
  ([channel client-id]
   (connect!
    channel
    client-id
    nil))
  ([channel client-id opts]
   (client/stream-response
    channel
    (client-ctx
     client-id
     opts))))

(defn- close-response!
  [response]
  (when-let [stream
             (:body response)]
    (s/close!
     stream))
  nil)

(defn- await-result
  ([deferred]
   (await-result
    deferred
    1000))
  ([deferred timeout-ms]
   (deref
    deferred
    timeout-ms
    ::timeout)))

(defn- eventually
  ([pred]
   (eventually
    pred
    1000))
  ([pred timeout-ms]
   (let [deadline
         (+ (System/currentTimeMillis)
            timeout-ms)]
     (loop []
       (if (pred)
         true
         (if (< (System/currentTimeMillis)
                deadline)
           (do
             (Thread/sleep 5)
             (recur))
           false))))))

(defn- drain!
  [channel client-id]
  (client/drain-fragments!
   channel
   client-id))

(defn- pending-wrapper?
  [node client-id]
  (and
   (vector?
    node)

   (= :div
      (first
       node))

   (= true
      (get-in
       node
       [1
        :data-gesso-live-client-pending]))

   (= client-id
      (get-in
       node
       [1
        :data-gesso-live-client-id]))))

(defn- thrown-data
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      (ex-data
       error))))

;; -----------------------------------------------------------------------------
;; Defaults and channel construction
;; -----------------------------------------------------------------------------

(deftest defaults-test
  (is (= "client-oob"
         client/default-event))

  (is (= {:base-path
          "/gesso/live/client"

          :stream-path
          "/gesso/live/client/stream"

          :pending-path
          "/gesso/live/client/pending"

          :client-id-param
          :client-id}
         client/default-endpoint))

  (is (map?
       client/default-options))

  (is (fn?
       (:client
        client/default-options))))

(deftest channel-default-shape-test
  (let [channel
        (client/channel)]

    (is (uuid?
         (:id
          channel)))

    (is (= "client-oob"
           (:event
            channel)))

    (is (= client/default-endpoint
           (:endpoint
            channel)))

    (is (fn?
         (:client
          channel)))

    (is (instance?
         clojure.lang.IAtom
         (:state
          channel)))

    (let [state
          @(:state
            channel)]

      (is (= {}
             (:clients
              state)))

      (is (= {}
             (:pending
              state)))

      (is (nil?
           (:latest-client-id
            state)))

      (is (number?
           (:created-at
            state)))

      (is (= 0
             (:sent-count
              state)))

      (is (= 0
             (:wakeup-count
              state)))

      (is (= 0
             (:dropped-count
              state))))))

(deftest channel-options-test
  (let [client-fn
        (fn [_ctx]
          {:client/user-id
           "user-x"

           :client/scopes
           #{[:scope :x]}})

        channel
        (client/channel
         {:id
          :custom/channel

          :event
          :custom-wake

          :endpoint
          {:stream-path
           "/custom/stream"

           :pending-path
           "/custom/pending"

           :client-id-param
           :browser}

          :client
          client-fn})]

    (is (= :custom/channel
           (:id
            channel)))

    (is (= "custom-wake"
           (:event
            channel)))

    (is (= {:base-path
            "/gesso/live/client"

            :stream-path
            "/custom/stream"

            :pending-path
            "/custom/pending"

            :client-id-param
            :browser}
           (:endpoint
            channel)))

    (is (identical?
         client-fn
         (:client
          channel)))))

(deftest channel-partial-endpoint-inherits-defaults-test
  (let [channel
        (client/channel
         {:endpoint
          {:stream-path
           "/only-stream-overridden"}})]

    (is (= "/only-stream-overridden"
           (get-in
            channel
            [:endpoint
             :stream-path])))

    (is (= "/gesso/live/client/pending"
           (get-in
            channel
            [:endpoint
             :pending-path])))

    (is (= :client-id
           (get-in
            channel
            [:endpoint
             :client-id-param])))))

(deftest channel-normalizes-event-test
  (is (= "wake"
         (:event
          (client/channel
           {:event
            :wake}))))

  (is (= "wake"
         (:event
          (client/channel
           {:event
            'wake}))))

  (is (= "wake"
         (:event
          (client/channel
           {:event
            "wake"})))))

(deftest channel-requires-client-function-test
  (doseq [value
          [nil
           :not-a-function
           {}
           []]]

    (let [data
          (thrown-data
           #(client/channel
             {:client
              value}))]

      (is (= :client
             (:key
              data)))

      (is (= value
             (:value
              data))))))

(deftest new-client-id-test
  (let [first-id
        (client/new-client-id)

        second-id
        (client/new-client-id)]

    (is (string?
         first-id))

    (is (uuid?
         (parse-uuid
          first-id)))

    (is (string?
         second-id))

    (is (not=
         first-id
         second-id))))

;; -----------------------------------------------------------------------------
;; Listener markup
;; -----------------------------------------------------------------------------

(deftest listener-default-markup-test
  (let [channel
        (test-channel)

        markup
        (with-redefs
         [client/new-client-id
          (constantly
           "client-1")]

          (client/listener
           channel
           {}))

        [root-tag
         root-attrs
         [trigger-tag
          trigger-attrs]]
        markup]

    (is (= :div
           root-tag))

    (is (= :div
           trigger-tag))

    (is (= "gesso-live-client-listener-client-1"
           (:id
            root-attrs)))

    (is (= "sse"
           (:hx-ext
            root-attrs)))

    (is (= "/gesso/live/client/stream?client-id=client-1"
           (:sse-connect
            root-attrs)))

    (is (= "true"
           (:aria-hidden
            root-attrs)))

    (is (= true
           (:data-gesso-live-client-listener
            root-attrs)))

    (is (= "client-1"
           (:data-gesso-live-client-id
            root-attrs)))

    (is (= ":test/client-channel"
           (:data-gesso-live-channel-id
            root-attrs)))

    (is (= "sse:client-oob"
           (:hx-trigger
            trigger-attrs)))

    (is (= "/gesso/live/client/pending?client-id=client-1"
           (:hx-get
            trigger-attrs)))

    (is (= "none"
           (:hx-swap
            trigger-attrs)))

    (is (= true
           (:data-gesso-live-client-pending-trigger
            trigger-attrs)))))

(deftest listener-explicit-client-id-test
  (let [markup
        (client/listener
         (test-channel)
         {}
         {:client/id
          "explicit-client"})

        root-attrs
        (second
         markup)

        trigger-attrs
        (second
         (nth
          markup
          2))]

    (is (= "explicit-client"
           (:data-gesso-live-client-id
            root-attrs)))

    (is (str/includes?
         (:sse-connect
          root-attrs)
         "client-id=explicit-client"))

    (is (str/includes?
         (:hx-get
          trigger-attrs)
         "client-id=explicit-client"))))

(deftest listener-explicit-dom-id-test
  (let [markup
        (client/listener
         (test-channel)
         {}
         {:client/id
          "client-1"

          :id
          "custom-listener"})]

    (is (= "custom-listener"
           (get-in
            markup
            [1
             :id])))))

(deftest listener-custom-root-and-trigger-attrs-test
  (let [markup
        (client/listener
         (test-channel)
         {}
         {:client/id
          "client-1"

          :attrs
          {:class
           "listener"

           :data-app-listener
           true}

          :trigger-attrs
          {:class
           "trigger"

           :hx-include
           "#board-state"}})]

    (is (= "listener"
           (get-in
            markup
            [1
             :class])))

    (is (= true
           (get-in
            markup
            [1
             :data-app-listener])))

    (is (= "trigger"
           (get-in
            markup
            [2
             1
             :class])))

    (is (= "#board-state"
           (get-in
            markup
            [2
             1
             :hx-include])))))

(deftest listener-custom-endpoint-and-param-test
  (let [channel
        (test-channel
         {:endpoint
          {:stream-path
           "/stream?existing=yes"

           :pending-path
           "/pending?existing=yes"

           :client-id-param
           :browser-id}})

        markup
        (client/listener
         channel
         {}
         {:client/id
          "a client/with spaces"})

        root-url
        (get-in
         markup
         [1
          :sse-connect])

        pending-url
        (get-in
         markup
         [2
          1
          :hx-get])]

    (is (= "/stream?existing=yes&browser-id=a+client%2Fwith+spaces"
           root-url))

    (is (= "/pending?existing=yes&browser-id=a+client%2Fwith+spaces"
           pending-url))))

(deftest listener-custom-event-test
  (let [channel
        (test-channel
         {:event
          :notifications})

        markup
        (client/listener
         channel
         {}
         {:client/id
          "client-1"})]

    (is (= "sse:notifications"
           (get-in
            markup
            [2
             1
             :hx-trigger])))))

;; -----------------------------------------------------------------------------
;; Stream response and registration
;; -----------------------------------------------------------------------------

(deftest stream-response-ring-shape-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")]

    (try
      (is (= 200
             (:status
              response)))

      (is (= {"content-type"
              "text/event-stream; charset=utf-8"

              "cache-control"
              "no-cache, no-transform"

              "connection"
              "keep-alive"

              "x-accel-buffering"
              "no"}
             (:headers
              response)))

      (is (some?
           (:body
            response)))

      (finally
        (close-response!
         response)))))

(deftest stream-response-registers-client-descriptor-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1"
         {:user-id
          "user-7"

          :scopes
          [[:store "store-1"]
           [:user "user-7"]]

          :label
          "phone"})]

    (try
      (let [connected
            (client/connected-clients
             channel)

            descriptor
            (get
             connected
             "client-1")]

        (is (= #{"client-1"}
               (set
                (client/connected-client-ids
                 channel))))

        (is (= "client-1"
               (:client/id
                descriptor)))

        (is (= "user-7"
               (:client/user-id
                descriptor)))

        (is (= #{[:store "store-1"]
                 [:user "user-7"]}
               (:client/scopes
                descriptor)))

        (is (= "phone"
               (:test/label
                descriptor)))

        (is (number?
             (:connected-at
              descriptor)))

        (is (= (:connected-at
                descriptor)
               (:last-seen-at
                descriptor)))

        (is (not
             (contains?
              descriptor
              :stream)))

        (is (= "client-1"
               (client/latest-client-id
                channel))))

      (finally
        (close-response!
         response)))))

(deftest stream-response-client-function-may-return-nil-test
  (let [channel
        (client/channel
         {:id
          :nil-client

          :client
          (fn [_ctx]
            nil)})

        response
        (client/stream-response
         channel
         {:query-params
          {:client-id
           "client-1"}})]

    (try
      (let [descriptor
            (get
             (client/connected-clients
              channel)
             "client-1")]

        (is (= "client-1"
               (:client/id
                descriptor)))

        (is (= #{}
               (:client/scopes
                descriptor))))

      (finally
        (close-response!
         response)))))

(deftest stream-response-normalizes-missing-client-scopes-to-empty-set-test
  (let [channel
        (client/channel
         {:client
          (fn [_ctx]
            {:client/user-id
             "user-1"})})

        response
        (client/stream-response
         channel
         {:query-params
          {:client-id
           "client-1"}})]

    (try
      (is (= #{}
             (get-in
              (client/connected-clients
               channel)
              ["client-1"
               :client/scopes])))

      (finally
        (close-response!
         response)))))

(deftest stream-response-emits-initial-wake-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")]

    (try
      (is (= "event: client-oob\ndata: client-1\n\n"
             (await-result
              (s/take!
               (:body
                response)))))

      (is (eventually
           #(= 1
               (:wakeup-count
                (client/state-summary
                 channel)))))

      (finally
        (close-response!
         response)))))

(deftest stream-response-generates-id-when-request-has-none-test
  (let [channel
        (test-channel)

        response
        (with-redefs
         [client/new-client-id
          (constantly
           "generated-client")]

          (client/stream-response
           channel
           {}))]

    (try
      (is (= #{"generated-client"}
             (set
              (client/connected-client-ids
               channel))))

      (is (= "generated-client"
             (client/latest-client-id
              channel)))

      (finally
        (close-response!
         response)))))

(deftest stream-response-reads-client-id-from-supported-request-param-locations-test
  (doseq [location
          [:params
           :query-params
           :path-params
           :form-params]]

    (let [channel
          (test-channel)

          response
          (client/stream-response
           channel
           {location
            {:client-id
             (str
              (name
               location)
              "-client")}})]

      (try
        (is (= #{(str
                  (name
                   location)
                  "-client")}
               (set
                (client/connected-client-ids
                 channel))))

        (finally
          (close-response!
           response))))))

(deftest stream-response-reads-string-param-key-test
  (let [channel
        (test-channel)

        response
        (client/stream-response
         channel
         {:query-params
          {"client-id"
           "string-key-client"}})]

    (try
      (is (= #{"string-key-client"}
             (set
              (client/connected-client-ids
               channel))))

      (finally
        (close-response!
         response)))))

(deftest request-param-location-precedence-is-stable-test
  (let [channel
        (test-channel)

        response
        (client/stream-response
         channel
         {:params
          {:client-id
           "params-client"}

          :query-params
          {:client-id
           "query-client"}

          :path-params
          {:client-id
           "path-client"}

          :form-params
          {:client-id
           "form-client"}})]

    (try
      (is (= #{"params-client"}
             (set
              (client/connected-client-ids
               channel))))

      (finally
        (close-response!
         response)))))

;; -----------------------------------------------------------------------------
;; Stream close / reconnect lifecycle
;; -----------------------------------------------------------------------------

(deftest closing-stream-removes-connected-client-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")]

    (is (= #{"client-1"}
           (set
            (client/connected-client-ids
             channel))))

    (close-response!
     response)

    (is (eventually
         #(empty?
           (client/connected-client-ids
            channel))))))

(deftest closing-old-stream-after-reconnect-does-not-remove-new-stream-test
  (let [channel
        (test-channel)

        old-response
        (connect!
         channel
         "client-1"
         {:label
          "old"})

        new-response
        (connect!
         channel
         "client-1"
         {:label
          "new"})]

    (try
      (is (= "new"
             (get-in
              (client/connected-clients
               channel)
              ["client-1"
               :test/label])))

      (close-response!
       old-response)

      (is (eventually
           #(= "new"
               (get-in
                (client/connected-clients
                 channel)
                ["client-1"
                 :test/label]))))

      (is (= #{"client-1"}
             (set
              (client/connected-client-ids
               channel))))

      (finally
        (close-response!
         new-response)))))

(deftest reconnect-closes-displaced-stream-and-preserves-new-owner-test
  (let [channel
        (test-channel)

        old-response
        (connect!
         channel
         "client-1"
         {:label
          "old"})

        old-stream
        (:body
         old-response)

        new-response
        (connect!
         channel
         "client-1"
         {:label
          "new"})

        new-stream
        (:body
         new-response)]

    (try
      (is (eventually
           #(s/closed?
             old-stream))
          "Registering a replacement for one logical client id must actively close the displaced physical SSE stream.")

      (is (not
           (s/closed?
            new-stream))
          "The replacement physical stream must remain open.")

      (is (= "new"
             (get-in
              (client/connected-clients
               channel)
              ["client-1"
               :test/label]))
          "The replacement descriptor must remain the current logical owner after the displaced stream closes.")

      (is (= #{"client-1"}
             (set
              (client/connected-client-ids
               channel)))
          "The displaced stream's close callback must not unregister the replacement owner.")

      (finally
        ;; old-stream should already be closed by reconnect. Keep cleanup
        ;; idempotent so a failing implementation does not leak the test stream.
        (close-response!
         old-response)
        (close-response!
         new-response)))))

(deftest closing-current-stream-after-reconnect-removes-client-test
  (let [channel
        (test-channel)

        old-response
        (connect!
         channel
         "client-1")

        new-response
        (connect!
         channel
         "client-1")]

    (close-response!
     old-response)

    (is (= #{"client-1"}
           (set
            (client/connected-client-ids
             channel))))

    (close-response!
     new-response)

    (is (eventually
         #(empty?
           (client/connected-client-ids
            channel))))))

(deftest reconnect-updates-latest-client-id-test
  (let [channel
        (test-channel)

        first-response
        (connect!
         channel
         "client-1")

        second-response
        (connect!
         channel
         "client-2")

        third-response
        (connect!
         channel
         "client-1")]

    (try
      (is (= "client-1"
             (client/latest-client-id
              channel)))

      (finally
        (doseq [response
                [first-response
                 second-response
                 third-response]]
          (close-response!
           response))))))

;; -----------------------------------------------------------------------------
;; Targeted send
;; -----------------------------------------------------------------------------

(deftest send-to-one-client-test
  (let [channel
        (test-channel)

        response-1
        (connect!
         channel
         "client-1")

        response-2
        (connect!
         channel
         "client-2")]

    (try
      (let [result
            (client/send-to-client!
             channel
             "client-1"
             fragment-a
             fragment-b)]

        (is (= 1
               (:sent
                result)))

        (is (= 1
               (:woke
                result)))

        (is (true?
             (:woke?
              result)))

        (is (= [:client
                "client-1"]
               (:target
                result)))

        (is (= 2
               (:fragment-count
                result)))

        (is (= [fragment-a
                fragment-b]
               (drain!
                channel
                "client-1")))

        (is (nil?
             (drain!
              channel
              "client-2"))))

      (finally
        (close-response!
         response-1)

        (close-response!
         response-2)))))

(deftest send-to-user-normalizes-user-id-to-string-test
  (let [channel
        (test-channel)

        response-1
        (connect!
         channel
         "client-1"
         {:user-id
          "42"})

        response-2
        (connect!
         channel
         "client-2"
         {:user-id
          42})

        response-3
        (connect!
         channel
         "client-3"
         {:user-id
          "other"})]

    (try
      (let [result
            (client/send-to-user!
             channel
             42
             fragment-a)]

        (is (= 2
               (:sent
                result)))

        (is (= 2
               (:woke
                result)))

        (is (= [fragment-a]
               (drain!
                channel
                "client-1")))

        (is (= [fragment-a]
               (drain!
                channel
                "client-2")))

        (is (nil?
             (drain!
              channel
              "client-3"))))

      (finally
        (doseq [response
                [response-1
                 response-2
                 response-3]]
          (close-response!
           response))))))

(deftest send-to-scope-test
  (let [channel
        (test-channel)

        response-1
        (connect!
         channel
         "client-1"
         {:scopes
          #{[:store "store-1"]
            [:user "user-1"]}})

        response-2
        (connect!
         channel
         "client-2"
         {:scopes
          #{[:store "store-1"]}})

        response-3
        (connect!
         channel
         "client-3"
         {:scopes
          #{[:store "store-2"]}})]

    (try
      (let [result
            (client/send-to-scope!
             channel
             [:store
              "store-1"]
             fragment-a)]

        (is (= 2
               (:sent
                result)))

        (is (= [fragment-a]
               (drain!
                channel
                "client-1")))

        (is (= [fragment-a]
               (drain!
                channel
                "client-2")))

        (is (nil?
             (drain!
              channel
              "client-3"))))

      (finally
        (doseq [response
                [response-1
                 response-2
                 response-3]]
          (close-response!
           response))))))

(deftest broadcast-test
  (let [channel
        (test-channel)

        responses
        [(connect!
          channel
          "client-1")

         (connect!
          channel
          "client-2")

         (connect!
          channel
          "client-3")]]

    (try
      (let [result
            (client/broadcast!
             channel
             fragment-a
             fragment-b)]

        (is (= 3
               (:sent
                result)))

        (is (= 3
               (:woke
                result)))

        (doseq [client-id
                ["client-1"
                 "client-2"
                 "client-3"]]

          (is (= [fragment-a
                  fragment-b]
                 (drain!
                  channel
                  client-id)))))

      (finally
        (doseq [response
                responses]
          (close-response!
           response))))))

(deftest send-to-missing-client-is-clean-no-op-test
  (let [channel
        (test-channel)

        result
        (client/send-to-client!
         channel
         "missing"
         fragment-a)]

    (is (= 0
           (:sent
            result)))

    (is (= 0
           (:woke
            result)))

    (is (false?
         (:woke?
          result)))

    (is (= {}
           (client/pending-counts
            channel)))))

(deftest send-request-shape-is-preserved-in-result-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")

        request
        {:to
         [:client
          "client-1"]

         :fragments
         [fragment-a]

         :application/metadata
         {:reason
          :test}}]

    (try
      (let [result
            (client/send!
             channel
             request)]

        (is (= request
               (:request
                result)))

        (is (= 1
               (:fragment-count
                result))))

      (finally
        (close-response!
         response)))))

(deftest send-rejects-unsupported-target-test
  (let [channel
        (test-channel)]

    (doseq [target
            [nil
             :clients
             [:organization
              "org-1"]
             "client-1"]]

      (let [data
            (thrown-data
             #(client/send!
               channel
               {:to
                target

                :fragments
                [fragment-a]}))]

        (is (= target
               (:to
                data)))

        (is (= [:all
                [:client
                 'client-id]
                [:user
                 'user-id]
                [:scope
                 'scope]]
               (:valid-targets
                data)))))))

;; -----------------------------------------------------------------------------
;; Pending queue semantics
;; -----------------------------------------------------------------------------

(deftest pending-fragments-append-in-send-order-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")]

    (try
      (client/send-to-client!
       channel
       "client-1"
       fragment-a)

      (client/send-to-client!
       channel
       "client-1"
       fragment-b)

      (is (= {"client-1"
              2}
             (client/pending-counts
              channel)))

      (is (= [fragment-a
              fragment-b]
             (drain!
              channel
              "client-1")))

      (is (= {}
             (client/pending-counts
              channel)))

      (finally
        (close-response!
         response)))))

(deftest drain-fragments-is-destructive-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")]

    (try
      (client/send-to-client!
       channel
       "client-1"
       fragment-a)

      (is (= [fragment-a]
             (client/drain-fragments!
              channel
              "client-1")))

      (is (nil?
           (client/drain-fragments!
            channel
            "client-1")))

      (finally
        (close-response!
         response)))))

(deftest pending-work-survives-stream-disconnect-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")]

    (client/send-to-client!
     channel
     "client-1"
     fragment-a
     fragment-b)

    (close-response!
     response)

    (is (eventually
         #(empty?
           (client/connected-client-ids
            channel))))

    (testing "disconnect removes ephemeral stream registration, not queued work"
      (is (= {"client-1"
              2}
             (client/pending-counts
              channel)))

      (is (= [fragment-a
              fragment-b]
             (client/drain-fragments!
              channel
              "client-1"))))))

(deftest pending-work-survives-reconnect-of-same-client-id-test
  (let [channel
        (test-channel)

        old-response
        (connect!
         channel
         "client-1")]

    (client/send-to-client!
     channel
     "client-1"
     fragment-a)

    (close-response!
     old-response)

    (is (eventually
         #(empty?
           (client/connected-client-ids
            channel))))

    (let [new-response
          (connect!
           channel
           "client-1")]

      (try
        (is (= [fragment-a]
               (client/drain-fragments!
                channel
                "client-1")))

        (finally
          (close-response!
           new-response))))))

;; -----------------------------------------------------------------------------
;; Receiver-specific pending rendering
;; -----------------------------------------------------------------------------

(deftest drain-fragment-wraps-pending-oob-content-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")]

    (try
      (client/send-to-client!
       channel
       "client-1"
       fragment-a
       fragment-b)

      (let [rendered
            (client/drain-fragment!
             channel
             {:query-params
              {:client-id
               "client-1"}})]

        (is (pending-wrapper?
             rendered
             "client-1"))

        (is (= [fragment-a
                fragment-b]
               (subvec
                rendered
                2)))

        (is (= {}
               (client/pending-counts
                channel))))

      (finally
        (close-response!
         response)))))

(deftest drain-fragment-renders-functions-at-receive-time-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1"
         {:user-id
          "user-7"

          :scopes
          #{[:store
             "store-1"]}})

        seen
        (atom nil)

        dynamic-fragment
        (fn [ctx]
          (reset!
           seen
           ctx)

          [:div
           {:id
            "dynamic"

            :hx-swap-oob
            "true"}
           (get-in
            ctx
            [:params
             :view])])]

    (try
      (client/send-to-client!
       channel
       "client-1"
       dynamic-fragment)

      (let [rendered
            (client/drain-fragment!
             channel
             {:query-params
              {:client-id
               "client-1"}

              :params
              {:view
               "manager"}})]

        (is (= [:div
                {:id
                 "dynamic"

                 :hx-swap-oob
                 "true"}
                "manager"]
               (nth
                rendered
                2)))

        (is (= "client-1"
               (:gesso.live.client/client-id
                @seen)))

        (is (= :test/client-channel
               (:gesso.live.client/channel-id
                @seen)))

        (is (= "manager"
               (get-in
                @seen
                [:params
                 :view])))

        (is (= "user-7"
               (get-in
                @seen
                [:gesso.live.client/client
                 :client/user-id])))

        (is (= #{[:store
                  "store-1"]}
               (get-in
                @seen
                [:gesso.live.client/client
                 :client/scopes])))

        (is (not
             (contains?
              (:gesso.live.client/client
               @seen)
              :stream))))

      (finally
        (close-response!
         response)))))

(deftest dynamic-fragment-may-return-sequence-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")]

    (try
      (client/send-to-client!
       channel
       "client-1"
       (fn [_ctx]
         [fragment-a
          fragment-b]))

      (let [rendered
            (client/drain-fragment!
             channel
             {:query-params
              {:client-id
               "client-1"}})]

        (is (= [fragment-a
                fragment-b]
               (subvec
                rendered
                2))))

      (finally
        (close-response!
         response)))))

(deftest dynamic-fragment-may-return-nested-sequences-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")]

    (try
      (client/send-to-client!
       channel
       "client-1"
       (fn [_ctx]
         [[fragment-a]
          nil
          [fragment-b]]))

      (let [rendered
            (client/drain-fragment!
             channel
             {:query-params
              {:client-id
               "client-1"}})]

        (is (= [fragment-a
                fragment-b]
               (subvec
                rendered
                2))))

      (finally
        (close-response!
         response)))))

(deftest nil-dynamic-render-consumes-queue-without-response-body-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")]

    (try
      (client/send-to-client!
       channel
       "client-1"
       (fn [_ctx]
         nil))

      (is (nil?
           (client/drain-fragment!
            channel
            {:query-params
             {:client-id
              "client-1"}})))

      (is (= {}
             (client/pending-counts
              channel)))

      (finally
        (close-response!
         response)))))

(deftest drain-fragment-without-client-id-does-not-consume-queue-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")]

    (try
      (client/send-to-client!
       channel
       "client-1"
       fragment-a)

      (is (nil?
           (client/drain-fragment!
            channel
            {})))

      (is (= {"client-1"
              1}
             (client/pending-counts
              channel)))

      (finally
        (close-response!
         response)))))

(deftest drain-fragment-for-disconnected-client-still-renders-request-context-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")

        seen
        (atom nil)]

    (client/send-to-client!
     channel
     "client-1"
     (fn [ctx]
       (reset!
        seen
        ctx)

       fragment-a))

    (close-response!
     response)

    (is (eventually
         #(empty?
           (client/connected-client-ids
            channel))))

    (let [rendered
          (client/drain-fragment!
           channel
           {:query-params
            {:client-id
             "client-1"}

            :params
            {:view
             "customer"}})]

      (is (pending-wrapper?
           rendered
           "client-1"))

      (is (= "client-1"
             (:gesso.live.client/client-id
              @seen)))

      (is (= :test/client-channel
             (:gesso.live.client/channel-id
              @seen)))

      (is (= "customer"
             (get-in
              @seen
              [:params
               :view])))

      (is (not
           (contains?
            @seen
            :gesso.live.client/client))))))

;; -----------------------------------------------------------------------------
;; Send wakeup behavior
;; -----------------------------------------------------------------------------

(deftest send-wakes-each-target-stream-test
  (let [channel
        (test-channel)

        response-1
        (connect!
         channel
         "client-1")

        response-2
        (connect!
         channel
         "client-2")]

    (try
      ;; Consume initial wake frames first.
      (is (string?
           (await-result
            (s/take!
             (:body
              response-1)))))

      (is (string?
           (await-result
            (s/take!
             (:body
              response-2)))))

      (let [result
            (client/broadcast!
             channel
             fragment-a)]

        (is (= 2
               (:woke
                result)))

        (is (= "event: client-oob\ndata: client-1\n\n"
               (await-result
                (s/take!
                 (:body
                  response-1)))))

        (is (= "event: client-oob\ndata: client-2\n\n"
               (await-result
                (s/take!
                 (:body
                  response-2))))))

      (finally
        (close-response!
         response-1)

        (close-response!
         response-2)))))

(deftest rejected-stream-wake-removes-only-current-registration-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")

        rejected-stream
        (s/stream 1)]

    (try
      ;; Consume the real connection wake before forcing a deterministic
      ;; rejected wake on another real Manifold stream.
      (is (string?
           (await-result
            (s/take!
             (:body
              response)))))

      (is (eventually
           #(= 1
               (:wakeup-count
                (client/state-summary
                 channel)))))

      ;; A closed Manifold stream makes the next put resolve false without
      ;; redefining Manifold protocol functions. Install it as the current
      ;; runtime stream while preserving the connected-client descriptor.
      (s/close!
       rejected-stream)

      (is (s/closed?
           rejected-stream))

      (swap!
       (:state
        channel)
       assoc-in
       [:clients
        "client-1"
        :stream]
       rejected-stream)

      (let [result
            (client/send-to-client!
             channel
             "client-1"
             fragment-a)]

        ;; :woke counts attempted connected-stream wakeups. The deferred
        ;; rejection then removes this exact current registration and records
        ;; the dropped wake.
        (is (= 1
               (:woke
                result)))

        (is (eventually
             #(empty?
               (client/connected-client-ids
                channel))))

        (is (eventually
             #(= 1
                 (:dropped-count
                  (client/state-summary
                   channel)))))

        (testing "queued delivery survives rejected wake"
          (is (= [fragment-a]
                 (client/drain-fragments!
                  channel
                  "client-1")))))

      (finally
        (s/close!
         rejected-stream)

        (close-response!
         response)))))

(deftest failed-old-stream-wake-cannot-delete-newer-registration-test
  (let [channel
        (test-channel)

        old-response
        (connect!
         channel
         "client-1"
         {:label
          "old"})

        new-response
        (connect!
         channel
         "client-1"
         {:label
          "new"})

        current-stream
        (:body
         new-response)]

    (try
      ;; Consume each connection's initial wake so subsequent delivery on the
      ;; current stream is unambiguous.
      (is (string?
           (await-result
            (s/take!
             (:body
              old-response)))))

      (is (string?
           (await-result
            (s/take!
             current-stream))))

      ;; Closing an obsolete stream exercises the same-stream identity guard
      ;; through Manifold's real on-closed callback.
      (close-response!
       old-response)

      (is (eventually
           #(= "new"
               (get-in
                (client/connected-clients
                 channel)
                ["client-1"
                 :test/label]))))

      (is (identical?
           current-stream
           (get-in
            @(:state
              channel)
            [:clients
             "client-1"
             :stream])))

      (let [result
            (client/send-to-client!
             channel
             "client-1"
             fragment-a)]

        (is (= 1
               (:woke
                result)))

        (is (= "event: client-oob
data: client-1

"
               (await-result
                (s/take!
                 current-stream))))

        (is (= [fragment-a]
               (client/drain-fragments!
                channel
                "client-1"))))

      (finally
        (close-response!
         old-response)

        (close-response!
         new-response)))))

(deftest connected-clients-never-exposes-runtime-stream-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")]

    (try
      (let [client-map
            (client/connected-clients
             channel)]

        (is (= #{"client-1"}
               (set
                (keys
                 client-map))))

        (is (not
             (contains?
              (get
               client-map
               "client-1")
              :stream))))

      (finally
        (close-response!
         response)))))

(deftest pending-counts-test
  (let [channel
        (test-channel)

        response-1
        (connect!
         channel
         "client-1")

        response-2
        (connect!
         channel
         "client-2")]

    (try
      (client/send-to-client!
       channel
       "client-1"
       fragment-a
       fragment-b)

      (client/send-to-client!
       channel
       "client-2"
       fragment-a)

      (is (= {"client-1"
              2

              "client-2"
              1}
             (client/pending-counts
              channel)))

      (finally
        (close-response!
         response-1)

        (close-response!
         response-2)))))

(deftest sent-count-counts-target-client-deliveries-test
  (let [channel
        (test-channel)

        responses
        [(connect!
          channel
          "client-1")

         (connect!
          channel
          "client-2")]]

    (try
      (client/broadcast!
       channel
       fragment-a
       fragment-b)

      (is (= 2
             (:sent-count
              (client/state-summary
               channel))))

      (client/send-to-client!
       channel
       "client-1"
       fragment-a)

      (is (= 3
             (:sent-count
              (client/state-summary
               channel))))

      (finally
        (doseq [response
                responses]
          (close-response!
           response))))))

(deftest state-summary-test
  (let [channel
        (test-channel)

        response-1
        (connect!
         channel
         "client-1")

        response-2
        (connect!
         channel
         "client-2")]

    (try
      (client/send-to-client!
       channel
       "client-1"
       fragment-a)

      (let [summary
            (client/state-summary
             channel)]

        (is (= :test/client-channel
               (:id
                summary)))

        (is (= "client-oob"
               (:event
                summary)))

        (is (= 2
               (:connected-count
                summary)))

        (is (= #{"client-1"
                 "client-2"}
               (set
                (:connected-client-ids
                 summary))))

        (is (= "client-2"
               (:latest-client-id
                summary)))

        (is (= {"client-1"
                1}
               (:pending-counts
                summary)))

        (is (= 1
               (:sent-count
                summary)))

        (is (number?
             (:wakeup-count
              summary)))

        (is (= 0
               (:dropped-count
                summary)))

        (is (number?
             (:created-at
              summary))))

      (finally
        (close-response!
         response-1)

        (close-response!
         response-2)))))

;; -----------------------------------------------------------------------------
;; Reset
;; -----------------------------------------------------------------------------

(deftest reset-channel-clears-ephemeral-state-test
  (let [channel
        (test-channel)

        response-1
        (connect!
         channel
         "client-1")

        response-2
        (connect!
         channel
         "client-2")]

    (client/broadcast!
     channel
     fragment-a)

    (is (= :reset
           (client/reset-channel!
            channel)))

    (let [summary
          (client/state-summary
           channel)]

      (is (= 0
             (:connected-count
              summary)))

      (is (= []
             (:connected-client-ids
              summary)))

      (is (nil?
           (:latest-client-id
            summary)))

      (is (= {}
             (:pending-counts
              summary)))

      (is (= 0
             (:sent-count
              summary)))

      (is (= 0
             (:wakeup-count
              summary)))

      (is (= 0
             (:dropped-count
              summary)))

      (is (number?
           (:created-at
            summary))))

    ;; Closing already-reset streams should remain harmless.
    (close-response!
     response-1)

    (close-response!
     response-2)))

(deftest reset-channel-closes-each-connected-stream-test
  (let [channel
        (test-channel)

        response-1
        (connect!
         channel
         "client-1")

        response-2
        (connect!
         channel
         "client-2")

        streams
        [(:body
          response-1)

         (:body
          response-2)]]

    (is (every?
         #(not
           (s/closed?
            %))
         streams))

    (is (= :reset
           (client/reset-channel!
            channel)))

    (is (eventually
         #(every?
           s/closed?
           streams)))

    ;; Closing streams that reset already closed remains harmless.
    (close-response!
     response-1)

    (close-response!
     response-2)))

(deftest connected-client-delivery-roundtrip-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1"
         {:user-id
          "user-1"

          :scopes
          #{[:store
             "store-1"]}})]

    (try
      ;; The browser receives one initial wake on connection.
      (is (= "event: client-oob\ndata: client-1\n\n"
             (await-result
              (s/take!
               (:body
                response)))))

      (let [send-result
            (client/send-to-scope!
             channel
             [:store
              "store-1"]
             fragment-a
             (fn [ctx]
               [:div
                {:id
                 "receiver-specific"

                 :hx-swap-oob
                 "true"}
                (str
                 (get-in
                  ctx
                  [:params
                   :mode])
                 ":"
                 (get-in
                  ctx
                  [:gesso.live.client/client
                   :client/user-id]))]))]

        (is (= 1
               (:sent
                send-result)))

        ;; The send wakes the browser to fetch its pending fragments.
        (is (= "event: client-oob\ndata: client-1\n\n"
               (await-result
                (s/take!
                 (:body
                  response)))))

        ;; The pending fetch renders receiver-specific functions with the
        ;; receiving request context rather than the sender's context.
        (let [rendered
              (client/drain-fragment!
               channel
               {:query-params
                {:client-id
                 "client-1"}

                :params
                {:mode
                 "manager"}})]

          (is (pending-wrapper?
               rendered
               "client-1"))

          (is (= fragment-a
                 (nth
                  rendered
                  2)))

          (is (= [:div
                  {:id
                   "receiver-specific"

                   :hx-swap-oob
                   "true"}
                  "manager:user-1"]
                 (nth
                  rendered
                  3)))

          (is (= {}
                 (client/pending-counts
                  channel)))))

      (finally
        (close-response!
         response)))))

;; -----------------------------------------------------------------------------
;; Reconnect delivery roundtrip
;; -----------------------------------------------------------------------------

(deftest reconnect-can-drain-work-queued-before-disconnect-test
  (let [channel
        (test-channel)

        first-response
        (connect!
         channel
         "client-1")]

    ;; Consume the connection wake, then queue work.
    (is (string?
         (await-result
          (s/take!
           (:body
            first-response)))))

    (client/send-to-client!
     channel
     "client-1"
     fragment-a)

    (close-response!
     first-response)

    (is (eventually
         #(empty?
           (client/connected-client-ids
            channel))))

    (is (= {"client-1"
            1}
           (client/pending-counts
            channel)))

    (let [second-response
          (connect!
           channel
           "client-1")]

      (try
        (testing "reconnect produces an initial wake so the browser fetches queued work"
          (is (= "event: client-oob\ndata: client-1\n\n"
                 (await-result
                  (s/take!
                   (:body
                    second-response))))))

        (is (= [:div
                {:data-gesso-live-client-pending
                 true

                 :data-gesso-live-client-id
                 "client-1"}
                fragment-a]
               (client/drain-fragment!
                channel
                {:query-params
                 {:client-id
                  "client-1"}})))

        (finally
          (close-response!
           second-response))))))


;; -----------------------------------------------------------------------------
;; Deterministic lifecycle policy
;; -----------------------------------------------------------------------------

(deftest replacement-preserves-one-at-most-once-pending-delivery-test
  (let [channel
        (test-channel)

        old-response
        (connect!
         channel
         "client-1")]

    (try
      ;; Consume the old connection's initial wake, then queue exactly one OOB
      ;; delivery before replacing its physical stream.
      (is (string?
           (await-result
            (s/take!
             (:body
              old-response)))))

      (client/send-to-client!
       channel
       "client-1"
       fragment-a)

      (let [new-response
            (connect!
             channel
             "client-1")]

        (try
          (is (eventually
               #(s/closed?
                 (:body
                  old-response)))
              "Replacement must retire the displaced physical stream.")

          ;; The reconnect wake tells the browser to fetch whatever is currently
          ;; pending. Replacing transport ownership must neither lose nor clone
          ;; the logical pending entry.
          (is (= "event: client-oob\ndata: client-1\n\n"
                 (await-result
                  (s/take!
                   (:body
                    new-response)))))

          (is (= [fragment-a]
                 (client/drain-fragments!
                  channel
                  "client-1")))

          (is (nil?
               (client/drain-fragments!
                channel
                "client-1"))
              "Pending delivery is destructive/at-most-once, not replayable.")

          (is (= {}
                 (client/pending-counts
                  channel)))

          (finally
            (close-response!
             new-response))))

      (finally
        (close-response!
         old-response)))))

(deftest rejected-wake-preserves-work-for-later-reconnect-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")

        rejected-stream
        (s/stream 1)]

    (try
      ;; Consume the real connection wake before replacing the runtime stream
      ;; with a deterministically closed Manifold stream.
      (is (string?
           (await-result
            (s/take!
             (:body
              response)))))

      (s/close!
       rejected-stream)

      (swap!
       (:state
        channel)
       assoc-in
       [:clients
        "client-1"
        :stream]
       rejected-stream)

      (client/send-to-client!
       channel
       "client-1"
       fragment-a)

      (is (eventually
           #(empty?
             (client/connected-client-ids
              channel)))
          "Rejected physical wake removes the failed current registration.")

      (is (= {"client-1" 1}
             (client/pending-counts
              channel))
          "Wake failure must not pretend the queued OOB payload was delivered.")

      (let [reconnected-response
            (connect!
             channel
             "client-1")]

        (try
          (is (= "event: client-oob\ndata: client-1\n\n"
                 (await-result
                  (s/take!
                   (:body
                    reconnected-response))))
              "Reconnect must wake the browser so surviving pending work can be fetched.")

          (is (= [fragment-a]
                 (client/drain-fragments!
                  channel
                  "client-1")))

          (is (nil?
               (client/drain-fragments!
                channel
                "client-1"))
              "Surviving work is still at-most-once once claimed by a drain.")

          (finally
            (close-response!
             reconnected-response))))

      (finally
        (s/close!
         rejected-stream)

        (close-response!
         response)))))

(deftest pending-render-failure-is-explicitly-at-most-once-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")

        render-count
        (atom 0)

        render-error
        (ex-info
         "fixture pending render failure"
         {:fixture/error
          :pending-render-failure})]

    (try
      (client/send-to-client!
       channel
       "client-1"
       (fn [_ctx]
         (swap!
          render-count
          inc)

         (throw
          render-error)))

      (is (identical?
           render-error
           (try
             (client/drain-fragment!
              channel
              {:query-params
               {:client-id
                "client-1"}})

             nil

             (catch clojure.lang.ExceptionInfo error
               error)))
          "Receiver-side render failure must remain visible to the caller.")

      (is (= 1
             @render-count))

      (is (= {}
             (client/pending-counts
              channel))
          "The pending entry is claimed by drain before rendering; there is no hidden acknowledgement/retry protocol.")

      (is (nil?
           (client/drain-fragment!
            channel
            {:query-params
             {:client-id
              "client-1"}}))
          "A failed render is not silently replayed on the next pending fetch.")

      (is (= 1
             @render-count)
          "At-most-once policy means the failed entry is not rendered twice.")

      (finally
        (close-response!
         response)))))

(deftest reset-linearizes-before-closing-displaced-streams-test
  (let [channel
        (test-channel)

        old-response
        (connect!
         channel
         "client-1")

        old-stream
        (:body
         old-response)

        close-stream-var
        (ns-resolve
         'gesso.live.client
         'close-stream!)

        original-close-stream!
        (var-get
         close-stream-var)

        replacement-response
        (atom nil)

        installed?
        (atom false)]

    (try
      ;; Force a new registration at the exact physical-close seam used by
      ;; reset-channel!. A correct reset first atomically publishes the empty
      ;; post-reset state and only then closes streams displaced by that state
      ;; transition. The registration below is therefore logically after reset
      ;; and must survive.
      ;;
      ;; The pre-v7.380 implementation snapshots state, closes old streams, and
      ;; only afterward reset!s the atom. Under that ordering this new stream is
      ;; left physically open while its just-created registration is silently
      ;; erased -- a real stream leak.
      (with-redefs-fn
       {close-stream-var
        (fn [stream]
          (when
           (and
            (identical?
             stream
             old-stream)

            (compare-and-set!
             installed?
             false
             true))

            (reset!
             replacement-response
             (connect!
              channel
              "client-2")))

          (original-close-stream!
           stream))}

       (fn []
         (is (= :reset
                (client/reset-channel!
                 channel)))))

      (let [new-response
            @replacement-response]

        (is (some?
             new-response)
            "The deterministic close seam must install the racing replacement.")

        (is (not
             (s/closed?
              (:body
               new-response)))
            "A registration logically after reset must remain physically live.")

        (is (= #{"client-2"}
               (set
                (client/connected-client-ids
                 channel)))
            "Reset must not erase a registration created after its atomic linearization point.")

        (is (= "client-2"
               (client/latest-client-id
                channel)))

        (is (= {}
               (client/pending-counts
                channel))
            "Reset still clears all pending work that existed before its linearization point.")

        (close-response!
         new-response))

      (finally
        (close-response!
         old-response)))))

;; -----------------------------------------------------------------------------
;; Pending OOB semantic policy
;; -----------------------------------------------------------------------------

(deftest pending-oob-duplicates-remain-distinct-and-ordered-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")]

    (try
      ;; The generic client layer cannot safely infer that two OOB entries are
      ;; semantically interchangeable. Different swap modes, receiver-specific
      ;; rendering, or deliberate repeated effects can make order observable.
      ;; Therefore pending OOB is an ordered queue of work, not a coalesced map
      ;; of logical target state.
      (client/send-to-client!
       channel
       "client-1"
       fragment-a)

      (client/send-to-client!
       channel
       "client-1"
       fragment-a
       fragment-b)

      (is (= {"client-1" 3}
             (client/pending-counts
              channel)))

      (is (= [fragment-a
              fragment-a
              fragment-b]
             (client/drain-fragments!
              channel
              "client-1")))

      (is (= {}
             (client/pending-counts
              channel)))

      (finally
        (close-response!
         response)))))

(deftest pending-drain-claims-one-batch-before-concurrent-send-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")

        render-entered
        (promise)

        release-render
        (promise)

        drain-future
        (atom nil)]

    (try
      (client/send-to-client!
       channel
       "client-1"
       (fn [_ctx]
         (deliver
          render-entered
          true)

         @release-render

         fragment-a))

      (reset!
       drain-future
       (future
         (client/drain-fragment!
          channel
          {:query-params
           {:client-id
            "client-1"}})))

      (is (= true
             (deref
              render-entered
              1000
              ::timeout))
          "The first pending request must reach receiver-side rendering.")

      (is (= {}
             (client/pending-counts
              channel))
          "A drain claims its current batch before receiver-specific rendering begins.")

      ;; This send is deliberately ordered after the first batch has been
      ;; claimed but before it has finished rendering. It must become future
      ;; pending work rather than being spliced into the already-claimed HTTP
      ;; response.
      (client/send-to-client!
       channel
       "client-1"
       fragment-b)

      (is (= {"client-1" 1}
             (client/pending-counts
              channel)))

      (deliver
       release-render
       true)

      (is (= [:div
              {:data-gesso-live-client-pending
               true

               :data-gesso-live-client-id
               "client-1"}
              fragment-a]
             (deref
              @drain-future
              1000
              ::timeout))
          "The already-claimed response contains only its original batch.")

      (is (= [fragment-b]
             (client/drain-fragments!
              channel
              "client-1"))
          "Work sent after the claim boundary remains queued for the next drain.")

      (is (= {}
             (client/pending-counts
              channel)))

      (finally
        (deliver
         release-render
         true)

        (when-let [f
                   @drain-future]
          (future-cancel
           f))

        (close-response!
         response)))))

(deftest reset-clears-unclaimed-pending-without-replaying-claimed-render-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")

        render-entered
        (promise)

        release-render
        (promise)

        drain-future
        (atom nil)]

    (try
      (client/send-to-client!
       channel
       "client-1"
       (fn [_ctx]
         (deliver
          render-entered
          true)

         @release-render

         fragment-a))

      (reset!
       drain-future
       (future
         (client/drain-fragment!
          channel
          {:query-params
           {:client-id
            "client-1"}})))

      (is (= true
             (deref
              render-entered
              1000
              ::timeout))
          "The drain must claim its batch before reset is introduced.")

      (is (= :reset
             (client/reset-channel!
              channel)))

      (is (= {}
             (client/pending-counts
              channel))
          "Reset clears channel-owned pending state at its linearization point.")

      (deliver
       release-render
       true)

      (is (= [:div
              {:data-gesso-live-client-pending
               true

               :data-gesso-live-client-id
               "client-1"}
              fragment-a]
             (deref
              @drain-future
              1000
              ::timeout))
          "Reset does not create a hidden cancellation/acknowledgement protocol for request-local work already claimed by a drain.")

      (is (= {}
             (client/pending-counts
              channel))
          "Already-claimed work is not replayed into channel state after reset.")

      (let [replacement-response
            (connect!
             channel
             "client-1")]

        (try
          (is (string?
               (await-result
                (s/take!
                 (:body
                  replacement-response))))
              "Reconnect still receives the ordinary initial wake.")

          (is (nil?
               (client/drain-fragments!
                channel
                "client-1"))
              "The claimed pre-reset batch is not replayed after reconnect.")

          (finally
            (close-response!
             replacement-response))))

      (finally
        (deliver
         release-render
         true)

        (when-let [f
                   @drain-future]
          (future-cancel
           f))

        (close-response!
         response)))))

(deftest reset-racing-with-pre-reset-send-cannot-resurrect-pending-work-test
  (let [channel
        (test-channel)

        response
        (connect!
         channel
         "client-1")

        enqueue-var
        (ns-resolve
         'gesso.live.client
         'enqueue-pending!)

        original-enqueue-pending!
        (var-get
         enqueue-var)

        enqueue-entered
        (promise)

        release-enqueue
        (promise)

        send-future
        (atom nil)]

    (try
      ;; Pause send! after it has selected the pre-reset connected-client
      ;; snapshot but before it mutates pending state. Reset then establishes its
      ;; linearization point. Releasing the stale send afterward must not be able
      ;; to recreate pending work for the displaced registration.
      (with-redefs-fn
       {enqueue-var
        (fn [channel' client-id fragments]
          (deliver
           enqueue-entered
           true)

          @release-enqueue

          (original-enqueue-pending!
           channel'
           client-id
           fragments))}

       (fn []
         (reset!
          send-future
          (future
            (client/send-to-client!
             channel
             "client-1"
             fragment-a)))

         (is (= true
                (deref
                 enqueue-entered
                 1000
                 ::timeout))
             "The send must have selected its pre-reset target before reset linearizes.")

         (is (= :reset
                (client/reset-channel!
                 channel)))

         (deliver
          release-enqueue
          true)

         (is (map?
              (deref
               @send-future
               1000
               ::timeout))
             "The racing send must complete instead of deadlocking around reset.")))

      (is (= []
             (client/connected-client-ids
              channel)))

      (is (= {}
             (client/pending-counts
              channel))
          "A send whose selected registration was displaced by reset must not resurrect pre-reset pending work afterward.")

      (finally
        (deliver
         release-enqueue
         true)

        (when-let [f
                   @send-future]
          (future-cancel
           f))

        (close-response!
         response)))))

(deftest reset-racing-with-pre-reset-send-cannot-target-same-id-reconnect-test
  (let [channel
        (test-channel)

        old-response
        (connect!
         channel
         "client-1"
         {:label
          "old"})

        enqueue-var
        (ns-resolve
         'gesso.live.client
         'enqueue-pending!)

        original-enqueue-pending!
        (var-get
         enqueue-var)

        enqueue-entered
        (promise)

        release-enqueue
        (promise)

        send-future
        (atom nil)

        replacement-response
        (atom nil)]

    (try
      ;; A logical client id may be reused after reset, but that does not make
      ;; the replacement physical registration the owner of work selected for
      ;; the displaced stream. Pause the old send after target selection, reset,
      ;; reconnect the same id with a new stream, then let the stale send resume.
      (with-redefs-fn
       {enqueue-var
        (fn [channel' client-id fragments]
          (deliver
           enqueue-entered
           true)

          @release-enqueue

          (original-enqueue-pending!
           channel'
           client-id
           fragments))}

       (fn []
         (reset!
          send-future
          (future
            (client/send-to-client!
             channel
             "client-1"
             fragment-a)))

         (is (= true
                (deref
                 enqueue-entered
                 1000
                 ::timeout))
             "The stale send must have selected the old physical registration before reset linearizes.")

         (is (= :reset
                (client/reset-channel!
                 channel)))

         (reset!
          replacement-response
          (connect!
           channel
           "client-1"
           {:label
            "replacement"}))

         (is (= "replacement"
                (get-in
                 (client/connected-clients
                  channel)
                 ["client-1"
                  :test/label]))
             "The same logical client id now belongs to a new physical stream.")

         (deliver
          release-enqueue
          true)

         (let [stale-result
               (deref
                @send-future
                1000
                ::timeout)]
           (is (= 0
                  (:sent
                   stale-result))
               "Work selected for the displaced stream must not transfer to a same-id replacement registration.")

           (is (= 0
                  (:woke
                   stale-result))
               "A rejected stale enqueue must not wake the replacement stream."))))

      (is (= {}
             (client/pending-counts
              channel))
          "The stale pre-reset send must leave no pending work on the replacement owner.")

      (let [fresh-result
            (client/send-to-client!
             channel
             "client-1"
             fragment-b)]
        (is (= 1
               (:sent
                fresh-result))
            "The replacement registration must still accept work selected after reconnect.")

        (is (= {"client-1"
                1}
               (client/pending-counts
                channel)))

        (is (= [fragment-b]
               (client/drain-fragments!
                channel
                "client-1"))
            "Only fresh work selected for the replacement stream may be drained."))

      (finally
        (deliver
         release-enqueue
         true)

        (when-let [f
                   @send-future]
          (future-cancel
           f))

        (close-response!
         old-response)

        (when-let [response
                   @replacement-response]
          (close-response!
           response))))))

;; -----------------------------------------------------------------------------
;; App-policy boundary
;; -----------------------------------------------------------------------------

(deftest channel-treats-user-and-scope-identities-as-opaque-test
  (let [opaque-scope
        {:organization
         "org-1"

         :location
         ["location"
          7]}

        channel
        (test-channel)

        response
        (connect!
         channel
         "client-1"
         {:user-id
          {:opaque
           "user"}

          :scopes
          #{opaque-scope}})]

    (try
      (testing "user target intentionally normalizes through str"
        (is (= 1
               (:sent
                (client/send-to-user!
                 channel
                 {:opaque
                  "user"}
                 fragment-a)))))

      (client/drain-fragments!
       channel
       "client-1")

      (testing "scope identity itself remains opaque and equality-based"
        (is (= 1
               (:sent
                (client/send-to-scope!
                 channel
                 opaque-scope
                 fragment-b))))

        (is (= [fragment-b]
               (client/drain-fragments!
                channel
                "client-1"))))

      (finally
        (close-response!
         response)))))

(deftest client-layer-does-not-infer-authorization-or-scope-membership-test
  (let [scope
        [:organization
         "org-1"]

        channel
        (test-channel)

        response
        (connect!
         channel
         "client-1"
         {:scopes
          #{scope}})]

    (try
      ;; Registration is the app's authorization boundary. The generic client
      ;; layer merely targets the scopes supplied by the app descriptor.
      (is (= 1
             (:sent
              (client/send-to-scope!
               channel
               scope
               fragment-a))))

      (is (= 0
             (:sent
              (client/send-to-scope!
               channel
               [:organization
                "org-2"]
               fragment-b))))

      (finally
        (close-response!
         response)))))

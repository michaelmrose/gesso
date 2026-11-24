(ns gesso.live.outbox.claim-race-fixture
  "Shared inputs for the legacy and installed physical Claim regressions."
  (:require [gesso.choreo.identity :as identity]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.optimistic.protocol :as protocol]))

(def mapping
  {:subject-keys {:request :request/location-id
                  :request-assignment :request/location-id
                  :audit :request/location-id}})

(def version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})

(defn command [execution]
  (protocol/command {:command-id (identity/command-id "race-2379")
                     :execution-id (identity/execution-id execution)
                     :operation :request/claim :arguments {:request-id "r1"}
                     :observed-basis fixture/rendered-basis}))


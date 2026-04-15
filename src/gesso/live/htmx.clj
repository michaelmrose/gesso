(ns gesso.live.htmx
  "HTMX-facing helpers for gesso.live.

   This namespace owns:
   - wrapper attrs for the live transport connection
   - target attrs for HTMX refresh
   - the canonical request header used for propagated consistency tokens."
  )

(defn token-header-name
  "Return the canonical request header used for the propagated consistency token."
  []
  "x-gesso-live-consistency-token")

(defn fragment-root-attrs
  "Build attrs for the outer live wrapper."
  [{:keys [stream-url]}]
  {:hx-ext "sse"
   :sse-connect stream-url})

(defn fragment-target-attrs
  "Build attrs for the live fragment refresh target."
  [{:keys [id src event swap trigger]}]
  {:id id
   :hx-get src
   :hx-trigger (str trigger ", sse:" event)
   :hx-swap swap})

(ns gesso.choreo.internal.machine-nominal-bindings
  "Production-internal adapter for Choreo machine executions. Kept separate
   from nominal-bindings so the machine can consume nominal values itself
   without creating a namespace dependency cycle. Machine execution maps and
   portable descriptors remain tagged and unchanged."
  (:require [gesso.choreo.machine :as machine]
            [gesso.choreo.internal.nominal-bindings :as bindings]))

(defn execution->nominal-bindings
  "Read a validated machine execution and convert its portable identity
   bindings into internal nominal values, preserving categories and extras."
  [execution]
  (bindings/bindings->nominal (machine/identity-bindings execution)))

(ns gesso.model.typed-atomic-pre-submit-hook
  "v1774: A minimal, inert-by-default local pre-submit callback slot.
   Kept free of assurance framework dependencies so the canonical model
   writer never circularly requires application/XTDB proof machinery.
   This is cooperative JVM instrumentation, NOT a trust or admission gate.")

(def ^:dynamic *before-canonical-submit* nil)

(defn before-canonical-submit!
  "Invoke the opt-in typed full-atomic preflight before the original writer.
   Outside a diagnostic trial no callback is installed and this is a no-op."
  [binding incorporated]
  (when-let [f *before-canonical-submit*]
    (f binding incorporated)))

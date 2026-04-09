(ns gesso.components.background.patterns)
(def orb-grid-background-light-image
  (str
   "linear-gradient(to right, color-mix(in srgb, var(--foreground) 10%, transparent) 1px, transparent 1px), "
   "linear-gradient(to bottom, color-mix(in srgb, var(--foreground) 10%, transparent) 1px, transparent 1px), "
   "radial-gradient(circle at 50% 60%, "
   "color-mix(in srgb, var(--primary) 14%, transparent) 0%, "
   "color-mix(in srgb, var(--accent) 6%, transparent) 40%, "
   "transparent 70%)"))

(def orb-grid-background-dark-image
  (str
   "linear-gradient(to right, color-mix(in srgb, var(--foreground) 14%, transparent) 1px, transparent 1px), "
   "linear-gradient(to bottom, color-mix(in srgb, var(--foreground) 14%, transparent) 1px, transparent 1px), "
   "radial-gradient(circle at 50% 60%, "
   "color-mix(in srgb, var(--primary) 18%, transparent) 0%, "
   "color-mix(in srgb, var(--accent) 10%, transparent) 40%, "
   "transparent 70%)"))

(def orb-grid-background-base
  {:background-size "40px 40px, 40px 40px, 100% 100%"})

(def orb-grid-background-light
  (merge orb-grid-background-base
         {:background-color "var(--background)"
          :background-image orb-grid-background-light-image}))

(def orb-grid-background-dark
  (merge orb-grid-background-base
         {:background-color "var(--background)"
          :background-image orb-grid-background-dark-image}))

(def deep-ocean-glow-dark-image
  (str
   "radial-gradient(70% 55% at 50% 50%, "
   "color-mix(in srgb, var(--primary) 40%, white 8%) 0%, "
   "color-mix(in srgb, var(--primary) 30%, black 12%) 18%, "
   "color-mix(in srgb, var(--primary) 22%, black 28%) 34%, "
   "color-mix(in srgb, var(--background) 72%, black) 50%, "
   "color-mix(in srgb, var(--background) 82%, black) 66%, "
   "color-mix(in srgb, var(--background) 88%, black) 80%, "
   "color-mix(in srgb, var(--background) 94%, black) 92%, "
   "color-mix(in srgb, var(--background) 97%, black) 97%, "
   "color-mix(in srgb, var(--background) 99%, black) 100%), "
   "radial-gradient(160% 130% at 10% 10%, "
   "rgba(0,0,0,0) 38%, "
   "color-mix(in srgb, var(--background) 94%, black) 76%, "
   "color-mix(in srgb, var(--background) 98%, black) 100%), "
   "radial-gradient(160% 130% at 90% 90%, "
   "rgba(0,0,0,0) 38%, "
   "color-mix(in srgb, var(--background) 94%, black) 76%, "
   "color-mix(in srgb, var(--background) 98%, black) 100%)"))

(def deep-ocean-glow-light-image
  (str
   "radial-gradient(70% 55% at 50% 50%, "
   "color-mix(in srgb, var(--primary) 18%, white) 0%, "
   "color-mix(in srgb, var(--primary) 10%, white) 18%, "
   "color-mix(in srgb, var(--accent) 8%, white) 34%, "
   "color-mix(in srgb, var(--background) 92%, var(--foreground) 2%) 50%, "
   "color-mix(in srgb, var(--background) 96%, var(--foreground) 2%) 66%, "
   "color-mix(in srgb, var(--background) 98%, var(--foreground) 1%) 80%, "
   "var(--background) 92%, "
   "var(--background) 97%, "
   "var(--background) 100%), "
   "radial-gradient(160% 130% at 10% 10%, "
   "rgba(255,255,255,0) 38%, "
   "color-mix(in srgb, var(--foreground) 4%, var(--background)) 76%, "
   "color-mix(in srgb, var(--foreground) 7%, var(--background)) 100%), "
   "radial-gradient(160% 130% at 90% 90%, "
   "rgba(255,255,255,0) 38%, "
   "color-mix(in srgb, var(--foreground) 4%, var(--background)) 76%, "
   "color-mix(in srgb, var(--foreground) 7%, var(--background)) 100%)"))

(def deep-ocean-glow-base
  {:background-color "var(--background)"
   :background-repeat "no-repeat"
   :background-size "100% 100%"})

(def deep-ocean-glow-light
  (merge deep-ocean-glow-base
         {:background-image deep-ocean-glow-light-image}))

(def deep-ocean-glow-dark
  (merge deep-ocean-glow-base
         {:background-image deep-ocean-glow-dark-image}))

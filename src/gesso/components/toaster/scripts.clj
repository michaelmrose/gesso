(ns gesso.components.toaster.scripts
  "Client behavior snippets for toaster components.")

(defn dismiss-script
  "Return hyperscript for dismissing the nearest toast.

   This handles manual close behavior."
  []
  "on click
  halt the event
  js(me)
    const toast = me.closest('[data-toast]');
    if (toast) toast.remove();
  end
end")

(defn dismiss-attrs
  "Attrs for a toast close button."
  []
  {:_ (dismiss-script)})

(defn auto-dismiss-script
  "Return hyperscript for optional auto-dismiss.

   The script reads data-duration from the toast element. No duration means no
   timeout. The toast is removed directly; exit animation can be added later."
  []
  "init
  js(me)
    const duration = Number(me.dataset.duration || 0);
    if (duration > 0) {
      window.setTimeout(function () {
        if (me && me.isConnected) {
          me.remove();
        }
      }, duration);
    }
  end
end")

(defn auto-dismiss-attrs
  "Attrs for a toast item with optional auto-dismiss behavior."
  [duration]
  (when (some? duration)
    {:_ (auto-dismiss-script)}))

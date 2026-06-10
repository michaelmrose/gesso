(ns gesso.components.accordion.scripts
  (:require
   [gesso.hyperscript :refer [hs]]))

(defn- state-input-query
  [state-input]
  (str "first <" state-input "/> in document"))

(defn- form-state-input-query
  [state-name]
  (str "first <input[name='" state-name "']/> in form"))

(defn accordion-chevron-script
  "Returns the Hyperscript instructions that keep the accordion chevron in sync
   with a details element's open state.

   The script looks for the chevron inside the current item and rotates it to
   180 degrees when the item is open, restoring it to 0 degrees when closed."
  []
  [[:let 'chev "first <svg[data-accordion-chevron]/> in me"]
   [:if 'chev
    [[:if 'me.open
      [:set 'chev.style.transform "'rotate(180deg)'"]
      [:set 'chev.style.transform "'rotate(0deg)'"]]]]])

(defn accordion-single-script
  "Returns the Hyperscript needed for single-open accordion behavior.

   When collapsible? is true, opening one item closes the rest of the items in
   the same accordion root.

   When collapsible? is false, the same single-open behavior applies, but the
   last open item is prevented from closing so that one item always remains open."
  [collapsible?]
  (if collapsible?
    [[:let 'root "closest <div[data-accordion-root]/>"]
     [:if 'me.open
      [[:for 'd "<details/> in root"
        [:if "d != me"
         [:set 'd.open false]]]]]]
    [[:let 'root "closest <div[data-accordion-root]/>"]
     [:if 'me.open
      [[:for 'd "<details/> in root"
        [:if "d != me"
         [:set 'd.open false]]]]
      [[:let 'anyOpen false]
       [:for 'd "<details/> in root"
        [:if 'd.open
         [:set 'anyOpen true]]]
       [:if "not anyOpen"
        [:set 'me.open true]]]]]))

(defn accordion-state-toggle-script
  "Returns Hyperscript forms that sync the final open state of a :single
   accordion item into a configured state input.

   state-input is a CSS selector, for example:
     \"#board-state input[name=selected]\"

   The synced value is the current details element's data-accordion-value."
  [{:keys [type state-input]}]
  (when (and state-input
             (= :single (or type :multiple)))
    [[:let 'stateInput (state-input-query state-input)]
     [:if 'stateInput
      [[:let 'value "me.dataset.accordionValue"]
       [:if 'me.open
        [[:set 'stateInput.value 'value]]
        [[:if "stateInput.value == value"
          [:set 'stateInput.value "''"]]]]]]]))

(defn accordion-state-submit-script
  "Returns a submit-listener statement that preserves accordion state when a
   form inside an accordion item submits.

   This avoids a common sharp edge:

     user opens item X
     user clicks a form/button inside item X
     server re-renders
     item X collapses because the submitted form did not carry accordion state

   state-name is the submitted parameter name, for example \"selected\". The
   script ensures the submitting form contains a hidden input with that name and
   the current item's data-accordion-value.

   If state-input is also supplied, the stable state input is updated at submit
   time as well."
  [{:keys [type state-input state-name]}]
  (when (and state-name
             (= :single (or type :multiple)))
    [:on "submit from <form/> in me"
     "set form to event.target"
     "set value to me.dataset.accordionValue"

     (when state-input
       [[:let 'stateInput (state-input-query state-input)]
        [:if 'stateInput
         [[:set 'stateInput.value 'value]]]])

     [:let 'input (form-state-input-query state-name)]
     [:if "not input"
      [(str "make an <input/> called input")
       [:set 'input.type "'hidden'"]
       [:set 'input.name (pr-str (str state-name))]
       "call form.appendChild(input)"]]

     [:set 'input.value 'value]]))

(defn accordion-script
  "Builds the full Hyperscript string for an accordion item.

   The returned script always updates the chevron on toggle. For :single
   accordions it also applies the group behavior that closes sibling items,
   respecting the collapsible? setting.

   Optional state sync:
     :state-input  CSS selector for a stable input that should store the
                   currently open item value.
     :state-name   Form field name to inject into descendant forms on submit.

   State sync is currently defined for :single accordions."
  [{:keys [type collapsible? state-input state-name]}]
  (let [type         (or type :multiple)
        collapsible? (if (nil? collapsible?) true collapsible?)]
    (hs
     [:on :toggle
      (when (= type :single)
        (accordion-single-script collapsible?))
      (accordion-chevron-script)
      (accordion-state-toggle-script
       {:type type
        :state-input state-input})]

     (accordion-state-submit-script
      {:type type
       :state-input state-input
       :state-name state-name}))))

(ns gesso.http
  "Small Ring/HTMX response helpers.

   html-response is the canonical Gesso Hiccup -> HTML Ring boundary.  v631
   adds a generic, dynamically scoped pre-serialization guard hook so a larger
   application assembler can make validation mandatory once around a handler
   tree instead of asking every render producer to remember a special response
   helper.

   The hook is intentionally HTTP-generic: this namespace does not depend on
   gesso.live, Choreo, application preflight, or any particular semantic
   validator.  A guard receives the exact Hiccup value immediately before Rum
   serialization.  It must either return a truthy value or throw.  Returning
   nil/false fails closed.

   With no installed guard html-response preserves its historical behavior.
   Application-level code can install a guard for the dynamic extent of a
   handler call with with-html-response-preflight."
  (:require
   [rum.core :as rum]))

(def html-content-type
  "text/html; charset=utf-8")

(def html-response-preflight-error-type
  :gesso.http/html-response-preflight-error)

(def ^:dynamic *html-response-preflight*
  "Dynamically scoped pre-serialization guard used by html-response.

   nil preserves the historical unguarded response path.  When bound, the
   value must be an actual function (or a Var currently containing one).  The
   guard receives the exact Hiccup body before Rum serialization and must
   return truthy or throw.  Guard return data is intentionally not installed in
   the response and cannot rewrite the body through this hook."
  nil)

(defn- callable?
  [x]
  (or (fn? x)
      (and (var? x)
           (fn? @x))))

(defn- call
  [f & args]
  (apply (if (var? f) @f f) args))

(defn- preflight-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type html-response-preflight-error-type
      :error/kind kind}
     data))))

(defn html-response-preflight-bound?
  "True when the current dynamic execution context has an HTML response guard.

   This is diagnostic only.  It does not establish that a particular
   application-level guard is semantically sufficient; the owner that installs
   the guard remains responsible for that contract."
  []
  (some? *html-response-preflight*))

(defn require-html-response-preflight!
  "Run the currently installed HTML response guard for body.

   Returns the guard's truthy result.  Throws when no guard is installed, when
   the bound guard is not function-like, or when the guard explicitly rejects
   by returning nil/false.  Exceptions thrown by the guard itself deliberately
   pass through unchanged so application-level structured diagnostics are not
   hidden behind an HTTP wrapper error."
  [body]
  (let [guard *html-response-preflight*]
    (when-not (some? guard)
      (preflight-error
       :missing-html-response-preflight
       "Gesso HTML response preflight is required in this execution context but no guard is installed."
       {}))
    (when-not (callable? guard)
      (preflight-error
       :invalid-html-response-preflight
       "Gesso HTML response preflight guard must be an actual function or a Var currently containing one."
       {:guard guard}))
    (let [result (call guard body)]
      (when-not result
        (preflight-error
         :html-response-preflight-rejected
         "Gesso HTML response preflight rejected the rendered body."
         {}))
      result)))

(defn with-html-response-preflight
  "Invoke thunk with guard installed for every nested html-response call.

   Both guard and thunk must be actual functions (or Vars currently containing
   functions); Clojure's broader IFn values such as keywords/maps are rejected.
   The binding is thread/dynamic-extent scoped and restores any outer binding
   afterward.

   This helper installs a guard; it does not itself render or inspect HTML.
   Application preflight can therefore bind one semantic validator around an
   ordinary handler tree while gesso.http remains independent of Live/Choreo."
  [guard thunk]
  (when-not (callable? guard)
    (preflight-error
     :invalid-html-response-preflight
     "Gesso HTML response preflight guard must be an actual function or a Var currently containing one."
     {:guard guard}))
  (when-not (callable? thunk)
    (preflight-error
     :invalid-html-response-thunk
     "Gesso HTML response preflight requires an actual function or a Var currently containing one as its thunk."
     {:thunk thunk}))
  (binding [*html-response-preflight* guard]
    (call thunk)))

(defn html-response
  "Render a Hiccup/Rum body to a Ring HTML response.

   When an HTML response preflight guard is dynamically installed, the exact
   Hiccup body is validated immediately before Rum serialization.  The guard
   cannot replace the rendered body through this API.  With no guard installed,
   historical rendering behavior is unchanged."
  [body]
  (when (some? *html-response-preflight*)
    (require-html-response-preflight! body))
  {:status 200
   :headers {"content-type" html-content-type}
   :body (rum/render-static-markup body)})

(defn no-content
  "Return an empty 204 Ring response."
  []
  {:status 204
   :headers {}
   :body ""})

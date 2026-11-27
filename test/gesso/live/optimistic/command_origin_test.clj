(ns gesso.live.optimistic.command-origin-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.optimistic.server :as server]
            [gesso.live.optimistic.server-test :as fixture]))

(deftest origin-exists-only-for-the-exact-active-invocation
  (let [seen (atom nil)
        origin (atom nil)
        prepared (fixture/base-server
                  (fn selected [invocation]
                    (reset! seen invocation)
                    (reset! origin (server/current-command-origin invocation))
                    (is (= @origin (server/current-command-origin invocation selected)))
                    (is (nil? (server/current-command-origin invocation (fn [_]))))
                    (is (nil? (server/current-command-origin invocation nil)))
                    (let [copy (into {} invocation)]
                      (is (= invocation copy))
                      (is (not (identical? invocation copy)))
                      (is (nil? (server/current-command-origin copy)))
                      (is (nil? (server/current-command-origin copy selected))))
                    (is (nil? @(future (server/current-command-origin invocation))))
                    (with-redefs [server/operation-context identity]
                      (is (nil? (server/current-command-origin invocation))))
                    (fixture/confirmed-result)))]
    (server/run-command prepared fixture/trusted-ctx (fixture/command-envelope)
                        (fn [boundary]
                          (is (nil? (server/current-command-origin (server/operation-context boundary))))
                          (assoc (:ctx boundary) :enriched true)))
    (is (= (assoc (select-keys @seen [:principal :command-id :execution-id :operation :arguments])
                  :origin :registered-server-invocation
                  :authentication :conditional-on-trusted-principal-resolver)
           @origin))
    (is (= fixture/trusted-principal (:principal @origin)))
    (is (true? (get-in @seen [:ctx :enriched])))
    (is (nil? (server/current-command-origin @seen)))))

(deftest nested-and-exceptional-execution-do-not-leak-origin
  (let [outer-invocation (atom nil)
        inner-invocation (atom nil)
        failure (ex-info "operation failed" {})
        inner (fixture/base-server
               (fn [invocation]
                 (reset! inner-invocation invocation)
                 (is (some? (server/current-command-origin invocation)))
                 (is (nil? (server/current-command-origin @outer-invocation)))
                 (throw failure)))
        outer (fixture/base-server
               (fn [invocation]
                 (reset! outer-invocation invocation)
                 (let [origin (server/current-command-origin invocation)]
                   (is (some? origin))
                   (is (identical? failure
                                   (try (server/run-command inner fixture/trusted-ctx (fixture/command-envelope))
                                        (catch Exception e e))))
                   (is (= origin (server/current-command-origin invocation)))
                   (is (nil? (server/current-command-origin @inner-invocation))))
                 (throw failure)))]
    (is (identical? failure
                    (try (server/run-command outer fixture/trusted-ctx (fixture/command-envelope))
                         (catch Exception e e))))
    (is (nil? (server/current-command-origin @outer-invocation)))
    (is (nil? (server/current-command-origin @inner-invocation)))))

(deftest exact-configured-resolver-and-registry-attribution
  (let [calls (atom [])
        principal (into {} fixture/trusted-principal)
        seen (atom nil)
        prepared (fixture/base-server
                  (fn selected [invocation]
                    (reset! seen invocation)
                    (is (identical? principal (:principal invocation)))
                    (is (= principal (:principal (server/current-command-origin invocation selected))))
                    (fixture/confirmed-result))
                  {:principal-fn (fn [ctx] (swap! calls conj ctx) principal)})]
    (is (server/prepared-send?
         (server/run-command prepared fixture/trusted-ctx (fixture/command-envelope)
                             (fn [boundary]
                               (is (identical? prepared (:server boundary)))
                               (is (identical? principal (:principal boundary)))
                               (assoc (:ctx boundary) :authenticated-principal fixture/other-principal)))))
    (is (= 1 (count @calls)))
    (is (identical? fixture/trusted-ctx (first @calls)))
    (is (= fixture/other-principal (get-in @seen [:ctx :authenticated-principal])))
    (is (nil? (server/current-command-origin @seen)))))

(deftest replaced-resolution-boundaries-do-not-issue-command-origin
  (let [seen (atom [])
        execute! (fn [invocation]
                   (swap! seen conj {:principal (:principal invocation)
                                     :origin (server/current-command-origin invocation)})
                   (fixture/confirmed-result))
        prepared (fixture/base-server execute!)
        validate (ns-resolve 'gesso.live.optimistic.server 'require-principal!)
        registry (ns-resolve 'gesso.live.optimistic.server 'resolve-operation)
        server-check (ns-resolve 'gesso.live.optimistic.server 'require-server!)
        real-registry @registry
        replacement (assoc prepared :principal-fn (constantly fixture/other-principal))]
    (doseq [[v f expected]
            [[validate (constantly fixture/other-principal) fixture/trusted-principal]
             [registry (fn [s op] (into {} (real-registry s op))) fixture/trusted-principal]
             [server-check (constantly replacement) fixture/other-principal]]]
      (is (server/prepared-send?
           (with-redefs-fn {v f}
             #(server/run-command prepared fixture/trusted-ctx (fixture/command-envelope)))))
      (is (= {:principal expected :origin nil} (peek @seen))))
    ;; Changes during context enrichment also invalidate origin attribution.
    (let [original @validate]
      (try
        (is (server/prepared-send?
             (server/run-command prepared fixture/trusted-ctx (fixture/command-envelope)
                                 (fn [boundary]
                                   (alter-var-root validate (constantly identity))
                                   (:ctx boundary)))))
        (is (nil? (:origin (peek @seen))))
        (finally (alter-var-root validate (constantly original)))))
    (is (server/prepared-send?
         (server/run-command prepared fixture/trusted-ctx (fixture/command-envelope))))
    (is (= fixture/trusted-principal (get-in (peek @seen) [:origin :principal])))))

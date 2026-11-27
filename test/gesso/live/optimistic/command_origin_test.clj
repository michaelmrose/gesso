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

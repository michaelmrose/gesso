(ns gesso.live.outbox.publication-manifest-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.publication-manifest :as manifest]
            [xtdb.api :as xt]
            [xtdb.basis :as basis]
            [xtdb.node :as xtn]))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest manifest-reads-await-the-submitted-not-completed-frontier
  (let [submitted {"xtdb" [42]}
        token (basis/->tx-basis-str submitted)
        expected {:xt/id "source" :profile-version 1 :descriptor "original"}
        reads (atom [])
        writes (atom [])]
    ;; Deterministically reproduce a stale default read on a reopened node.
    ;; The explicit token must be present on the actual manifest read, not
    ;; merely on an unrelated readiness query.
    (with-redefs [xt/status (constantly {:latest-submitted-msg-ids submitted
                                       :latest-processed-msg-ids {"xtdb" [3]}})
                  xtdb/q (fn [_ query opts]
                           (swap! reads conj [query opts])
                           (if (= {:await-token token} opts) [expected] []))
                  xtdb/execute-tx! (fn [& args] (swap! writes conj args))]
      (is (nil? (manifest/require-compatible! :node "source" "original")))
      (is (= :publication-manifest-mismatch
             (reason #(manifest/require-compatible! :node "source" "different"))))
      (is (= :publication-manifest-mismatch
             (reason #(manifest/require-compatible! :node "source" nil))))
      (is (= 3 (count @reads)))
      (is (every? #(= {:await-token token} (second %)) @reads))
      (is (empty? @writes)))))

(deftest invalid-or-failed-frontier-observation-never-enrolls
  (let [calls (atom [])
        failed (ex-info "provider unavailable" {})]
    (with-redefs [xtdb/q (fn [& _] (swap! calls conj :read))
                  xtdb/execute-tx! (fn [& _] (swap! calls conj :write))]
      (doseq [submitted [nil {} {"other" [42]} {"xtdb" []}
                         {"xtdb" [nil]} {"xtdb" "42"} {"xtdb" [42] "" [1]}]]
        (with-redefs [xt/status (constantly {:latest-submitted-msg-ids submitted})]
          (is (= :invalid-startup-log-frontier
                 (reason #(manifest/require-compatible! :node "source" "original"))))
          (is (= :invalid-startup-log-frontier
                 (reason #(manifest/require-compatible! :node "source" nil))))))
      (with-redefs [xt/status (fn [_] (throw failed))]
        (is (identical? failed (try (manifest/require-compatible! :node "source" nil)
                                    nil (catch Exception e e)))))
      (is (empty? @calls)))
    (with-redefs [xt/status (constantly {:latest-submitted-msg-ids {"xtdb" [42]}})
                  xtdb/q (fn [& _] (throw failed))
                  xtdb/execute-tx! (fn [& _] (swap! calls conj :write))]
      (is (identical? failed (try (manifest/require-compatible! :node "source" "original")
                                  nil (catch Exception e e))))
      (is (empty? @calls)))))

(deftest durable-submitted-manifest-is-observed-after-reopening
  (let [root (java.nio.file.Files/createTempDirectory
              "gesso-v2391-manifest-" (make-array java.nio.file.attribute.FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        source "source"
        expected {:xt/id source :profile-version 1 :descriptor "original"}]
    (try
      (with-open [node (xtn/start-node config)]
        ;; Submit without awaiting completion. A new node has no inherited
        ;; client-side await token; the startup frontier must cover this write.
        (xt/submit-tx node [[:put-docs :gesso_publication_manifests expected]]))
      (dotimes [_ 3]
        (with-open [node (xtn/start-node config)]
          (is (nil? (manifest/require-compatible! node source "original")))
          (is (= :publication-manifest-mismatch
                 (reason #(manifest/require-compatible! node source nil))))
          (is (= :publication-manifest-mismatch
                 (reason #(manifest/require-compatible! node source "different"))))
          (is (= [expected] (xt/q node ["SELECT _id, profile_version, descriptor FROM gesso_publication_manifests"])))))
      (finally
        (doseq [entry (reverse (file-seq (.toFile root)))]
          (io/delete-file entry true))))))

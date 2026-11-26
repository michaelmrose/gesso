(ns gesso.assurance.reference-submission-origin-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-observed-recovery :as recovery]
            [gesso.assurance.reference-historical-cut :as cut]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.model-publication :as publication]
            [xtdb.node :as xtn])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest issued-window-retains-origin-not-writer-capability
  (let [root (Files/createTempDirectory "gesso-submission-origin-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        ;; This test isolates the local policy window, not mapped admission.
        admission (Object.)
        ops [[:put-docs :probe {:xt/id "a" :value 1}]]]
    (try
      (let [handle
            (with-open [node (xtn/start-node config)]
              (let [release (xtdb/register-submission-policy! node (constantly true))
                    adapter (xtdb/adapter-connectable! node)
                    handle (try
                             (let [{:keys [observation capture]}
                                   (publication/capture-framed-submissions!
                                    admission adapter #(xtdb/execute-tx! adapter ops))
                                   copy (into {} capture)]
                               (is (= :returned (:outcome capture)))
                               (is (= 2 (count (:events capture))))
                               (is (identical? capture (:capture (publication/read-framed-submission-observation! admission observation))))
                               (is (publication/framed-submission-origin-matches? admission observation node))
                               (is (publication/framed-submission-origin-matches? admission observation adapter))
                               (is (false? (publication/framed-submission-origin-matches? (Object.) observation node)))
                               (is (false? (publication/framed-submission-origin-matches? admission capture node)))
                               (is (false? (publication/framed-submission-origin-matches? admission (publication/->FramedSubmissionObservation) node)))
                               (is (= capture copy))
                               (is (not (identical? capture copy)))
                               (is (false? (xtdb/policy-capture-origin-matches? copy node)))
                               (is (false? (publication/framed-submission-origin-matches? admission observation (Object.))))
                               (with-redefs [xtdb/policy-capture-origin-matches? (constantly true)]
                                 (is (false? (publication/framed-submission-origin-matches? admission observation node))))
                               observation)
                             (finally (release)))]
                (is (publication/framed-submission-origin-matches? admission handle node))
                (let [compare! #(recovery/compare-local-activated! node #{} admission nil nil [] % nil false)]
                  (is (= :unissued-retained-acquisition (:reason (compare! handle))))
                  (is (= :submission-origin-mismatch (:reason (compare! (Object.)))))
                  (with-redefs [publication/read-framed-submission-observation! (constantly {})]
                    (is (= :local-recovery-boundary-replaced (:reason (compare! handle))))))
                (let [capture! #(recovery/capture-local-recovery-cut! node #{} admission nil nil [] % nil false)]
                  (doseq [[obs reason] [[handle :unissued-retained-acquisition]
                                        [(Object.) :submission-origin-mismatch]]]
                    (let [r (capture! obs)]
                      (is (= reason (:reason r)))
                      (is (not (contains? r :recovery-cut-observation)))
                      (is (= :none (:authority r)))
                      (is (= :not-issued (:certificate r)))))
                  (with-redefs [cut/derive-cut! (constantly {:status :derived :historical-cut {}})]
                    (is (= :recovery-cut-boundary-replaced (:reason (capture! handle)))))
                  (with-redefs [recovery/compare-local-activated! (constantly {:status :matched})]
                    (is (= :recovery-cut-boundary-replaced (:reason (capture! handle))))))
                (is (false? (publication/framed-submission-origin-matches? admission handle adapter)))
                (is (some? (publication/read-framed-submission-observation! admission handle)))
                handle))]
        (with-open [reopened (xtn/start-node config)]
          (is (false? (publication/framed-submission-origin-matches? admission handle reopened)))
          (is (some? (publication/read-framed-submission-observation! admission handle)))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

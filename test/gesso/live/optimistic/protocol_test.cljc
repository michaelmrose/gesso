(ns gesso.live.optimistic.protocol-test
  (:require
   [gesso.live.optimistic.protocol :as protocol]
   #?(:clj [clojure.test :refer [deftest is testing]]
      :cljs [cljs.test :refer-macros [deftest is testing]])))

;; -----------------------------------------------------------------------------
;; Protocol identity and wire vocabulary
;; -----------------------------------------------------------------------------

(deftest protocol-identity-test
  (testing "protocol identity is stable"
    (is (= "2" protocol/version))
    (is (= :optimistic/command protocol/command-event))
    (is (= :optimistic/settlement protocol/settlement-event))
    (is (= "gesso-optimistic-execution"
           protocol/execution-header-name))))

(deftest protocol-attrs-test
  (testing "request/projection attrs have exact browser-facing names"
    (is (= :data-gesso-optimistic-protocol protocol/protocol-attr))
    (is (= :data-gesso-optimistic-transition protocol/transition-attr))
    (is (= :data-gesso-optimistic-template protocol/template-attr))
    (is (= :data-gesso-optimistic-target protocol/target-attr))
    (is (= :data-gesso-optimistic-scope protocol/scope-attr))
    (is (= :data-gesso-optimistic-base-revision protocol/base-revision-attr))
    (is (= :data-gesso-optimistic-label protocol/pending-label-attr))
    (is (= :data-gesso-optimistic-mode protocol/projection-mode-attr)))

  (testing "settlement/canonical attrs have exact browser-facing names"
    (is (= :data-gesso-optimistic-settlement protocol/settlement-attr))
    (is (= :data-gesso-optimistic-execution protocol/execution-attr))
    (is (= :data-gesso-optimistic-outcome protocol/outcome-attr))
    (is (= :data-gesso-optimistic-command-applied
           protocol/command-applied-attr))
    (is (= :data-gesso-optimistic-revision protocol/revision-attr))
    (is (= :data-gesso-optimistic-reason protocol/reason-attr))
    (is (= :data-gesso-optimistic-canonical protocol/canonical-attr))))

(deftest reserved-attrs-test
  (testing "every framework-owned optimistic attr is reserved exactly once"
    (let [expected #{protocol/protocol-attr
                     protocol/transition-attr
                     protocol/template-attr
                     protocol/target-attr
                     protocol/scope-attr
                     protocol/base-revision-attr
                     protocol/revision-attr
                     protocol/pending-label-attr
                     protocol/projection-mode-attr
                     protocol/settlement-attr
                     protocol/execution-attr
                     protocol/outcome-attr
                     protocol/command-applied-attr
                     protocol/reason-attr
                     protocol/canonical-attr}]
      (is (= expected (set protocol/reserved-attrs)))
      (is (= (count expected)
             (count protocol/reserved-attrs))))))

(deftest choreography-payload-contract-test
  (testing "command correlation and payload keys are explicit"
    (is (= #{:execution-id :transition :scope}
           protocol/command-required-keys))
    (is (= #{:base-revision :consistency-token}
           protocol/command-optional-keys))
    (is (= #{:execution-id :scope}
           protocol/command-correlation-keys)))

  (testing "settlement correlation and payload keys are explicit"
    (is (= #{:execution-id :scope :outcome :command-applied? :canonical}
           protocol/settlement-required-keys))
    (is (= #{:revision :reason :consistency-token}
           protocol/settlement-optional-keys))
    (is (= #{:execution-id :scope}
           protocol/settlement-correlation-keys))))

;; -----------------------------------------------------------------------------
;; Semantic names and projection modes
;; -----------------------------------------------------------------------------

(deftest qualified-name-test
  (testing "qualified keyword namespaces survive wire naming"
    (is (= "request/claim"
           (protocol/qualified-name :request/claim))))

  (testing "symbols and ordinary values normalize textually"
    (is (= "request/claim"
           (protocol/qualified-name 'request/claim)))
    (is (= "42"
           (protocol/qualified-name 42)))
    (is (nil? (protocol/qualified-name nil)))))

(deftest normalize-name-test
  (testing "required names normalize to non-blank strings"
    (is (= "request/claim"
           (protocol/normalize-name :transition :request/claim)))
    (is (= "custom"
           (protocol/normalize-name :transition 'custom))))

  (testing "blank and missing names are rejected"
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         #"must not be blank"
         (protocol/normalize-name :transition nil)))
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         #"must not be blank"
         (protocol/normalize-name :transition "   ")))))

(deftest optional-name-test
  (is (nil? (protocol/normalize-optional-name :reason nil)))
  (is (= "conflict"
         (protocol/normalize-optional-name :reason :conflict))))

(deftest projection-mode-test
  (testing "nil means provisional"
    (is (= :provisional
           (protocol/normalize-projection-mode nil)))
    (is (= "provisional"
           (protocol/projection-mode->wire nil))))

  (testing "all declared projection modes round-trip"
    (doseq [mode protocol/projection-modes]
      (is (= mode
             (protocol/wire->projection-mode
              (protocol/projection-mode->wire mode))))))

  (testing "unknown and malformed modes are rejected"
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         #"Invalid Gesso Live optimistic projection mode"
         (protocol/normalize-projection-mode :unknown)))
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         #"wire value must be non-blank"
         (protocol/wire->projection-mode "")))))

;; -----------------------------------------------------------------------------
;; Settlement semantics
;; -----------------------------------------------------------------------------

(deftest settlement-outcomes-test
  (testing "declared outcomes match applied semantics"
    (is (= #{:confirmed :reconciled :rejected :failed}
           protocol/settlement-outcomes))
    (is (= #{:confirmed :reconciled}
           protocol/applied-settlement-outcomes))
    (is (true? (protocol/command-applied-for-outcome? :confirmed)))
    (is (true? (protocol/command-applied-for-outcome? :reconciled)))
    (is (false? (protocol/command-applied-for-outcome? :rejected)))
    (is (false? (protocol/command-applied-for-outcome? :failed))))

  (testing "unknown outcomes are rejected"
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         #"Invalid Gesso Live optimistic settlement outcome"
         (protocol/normalize-settlement-outcome :maybe)))))

(deftest settlement-outcome-wire-test
  (doseq [outcome protocol/settlement-outcomes]
    (is (= outcome
           (protocol/wire->settlement-outcome
            (protocol/settlement-outcome->wire outcome)))))

  (testing "wire decoding is strict"
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         #"wire value is required"
         (protocol/wire->settlement-outcome nil)))
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         #"Invalid Gesso Live optimistic settlement outcome"
         (protocol/wire->settlement-outcome "unknown")))))

(deftest command-applied-wire-test
  (testing "booleans encode and decode exactly"
    (is (= "true" (protocol/command-applied->wire true)))
    (is (= "false" (protocol/command-applied->wire false)))
    (is (true? (protocol/wire->command-applied "true")))
    (is (false? (protocol/wire->command-applied "false"))))

  (testing "non-booleans do not silently coerce"
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         #"must be boolean"
         (protocol/command-applied->wire nil)))
    (doseq [wire [nil "" "TRUE" "0" "yes"]]
      (is (thrown-with-msg?
           #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
           #"Malformed Gesso Live command-applied wire value"
           (protocol/wire->command-applied wire))))))

(deftest settlement-consistency-test
  (testing "outcome and command-applied flag must agree"
    (is (true? (protocol/settlement-consistent? :confirmed true)))
    (is (true? (protocol/settlement-consistent? :reconciled true)))
    (is (true? (protocol/settlement-consistent? :rejected false)))
    (is (true? (protocol/settlement-consistent? :failed false)))
    (is (false? (protocol/settlement-consistent? :confirmed false)))
    (is (false? (protocol/settlement-consistent? :rejected true))))

  (testing "assertion returns true for valid combinations"
    (is (true? (protocol/assert-settlement-consistent! :confirmed true)))
    (is (true? (protocol/assert-settlement-consistent! :failed false))))

  (testing "assertion rejects mismatches and non-booleans"
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         #"disagrees with command-applied"
         (protocol/assert-settlement-consistent! :confirmed false)))
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         #"must be boolean"
         (protocol/assert-settlement-consistent! :failed nil)))))

;; -----------------------------------------------------------------------------
;; Scope identity
;; -----------------------------------------------------------------------------

(deftest wire-scope-test
  (testing "strings and EDN-like data receive distinct type tags"
    (is (= "s:request-1"
           (protocol/wire-scope "request-1")))
    (is (= "e:[:request \"request-1\"]"
           (protocol/wire-scope [:request "request-1"])))
    (is (not= (protocol/wire-scope "[:request \"request-1\"]")
              (protocol/wire-scope [:request "request-1"]))))

  (testing "scope identity is deterministic"
    (is (= (protocol/wire-scope [:request 42])
           (protocol/wire-scope [:request 42]))))

  (testing "nil and blank string scopes are rejected"
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         #"scope is required"
         (protocol/wire-scope nil)))
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         #"scope must not be blank"
         (protocol/wire-scope "   ")))))

;; -----------------------------------------------------------------------------
;; Revisions
;; -----------------------------------------------------------------------------

(deftest normalize-revision-test
  (testing "nil, safe non-negative integers, and non-blank strings are supported"
    (is (nil? (protocol/normalize-revision :revision nil)))
    (is (= 0 (protocol/normalize-revision :revision 0)))
    (is (= protocol/max-safe-integer-revision
           (protocol/normalize-revision
            :revision
            protocol/max-safe-integer-revision)))
    (is (= "opaque-7"
           (protocol/normalize-revision :revision "opaque-7"))))

  (testing "unsafe, negative, blank, and unsupported revisions are rejected"
    (doseq [revision [-1
                      (inc protocol/max-safe-integer-revision)
                      ""
                      "   "
                      :revision-1]]
      (is (thrown-with-msg?
           #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
           #"JavaScript-safe non-negative integer or non-blank string"
           (protocol/normalize-revision :revision revision))))))

(deftest revision-wire-test
  (testing "numeric and opaque revisions keep distinct wire types"
    (is (= "i:42" (protocol/revision->wire 42)))
    (is (= "s:42" (protocol/revision->wire "42")))
    (is (= 42 (protocol/wire->revision "i:42")))
    (is (= "42" (protocol/wire->revision "s:42")))
    (is (nil? (protocol/revision->wire nil)))
    (is (nil? (protocol/wire->revision nil))))

  (testing "supported revisions round-trip"
    (doseq [revision [0 1 42 protocol/max-safe-integer-revision
                      "r1" "00042" "2026-08-10T08:32:00Z"]]
      (is (= revision
             (protocol/wire->revision
              (protocol/revision->wire revision))))))

  (testing "malformed wire revisions are rejected"
    (doseq [wire ["" "42" "x:42" "i:" "i:-1" "i:1.5" "i:9007199254740992" "s:" "s:   "]]
      (is (thrown?
           #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
           (protocol/wire->revision wire))))))

(deftest revision-comparison-test
  (testing "numeric revisions have total ordering"
    (is (= :same (protocol/compare-revisions 7 7)))
    (is (= :newer (protocol/compare-revisions 8 7)))
    (is (= :older (protocol/compare-revisions 6 7))))

  (testing "opaque revisions have equality semantics only"
    (is (= :same (protocol/compare-revisions "r7" "r7")))
    (is (= :incomparable
           (protocol/compare-revisions "r8" "r7"))))

  (testing "mixed numeric/opaque revisions are incomparable"
    (is (= :incomparable
           (protocol/compare-revisions 7 "7"))))

  (testing "wire comparison preserves typed revision semantics"
    (is (= :newer
           (protocol/compare-wire-revisions "i:8" "i:7")))
    (is (= :incomparable
           (protocol/compare-wire-revisions "i:7" "s:7")))))

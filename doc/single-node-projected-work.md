# Gesso R5.1 single-node closure estimate — v2257

Re-estimated November 22, 2026, from the accepted Gessoplan R5.1 Stage 1–17 roadmap (excluding HumanHelp Stage 12 and multi-node Stage 18), Part XI completion scopes, prior v2209/v2229 ledgers, and the clean Gesso `typed-clojure` source at `aabe02630f6e831ea522afb9434ee9a8509cc55e` (v2257). The v2257 archive and GitHub branch match.

**Scope:** Framework-owned single-node construction, operations, Live, browser, certification, independent formal theorem/selected formal transfer, and release. No HumanHelp product development or multi-node operation. A Gesso-owned canonical proving app is *not* a substitute for the HumanHelp-specific normative gate.

**Interpretation:** These are *remaining closure work packages*, not claims that their modules do not exist. The revision allocations are subjective planning estimates, not measured tickets or formal guarantees. A revision is a small atomic, tested, directly committed GitHub change; no one-to-one mapping from required source obligations to revisions is possible.

**Budget:** 62 still-open proposed packages, central **464 additional revisions from v2257** (down from 470 remaining at v2229 and 483 at v2209). This is a cost-of-remaining-obligations estimate, **not** subtraction of 31 intervening commits. No whole package or R5.1 implementation milestone newly qualifies for closure. A favorable case remains roughly 240; the credible planning range remains roughly 300–800, and independent proof/transfer can exceed that. An unconditional universal source-to-production theorem is not necessarily feasible at any finite revision budget.

## Maintenance and re-estimation policy

This is a **dated planning snapshot**, not a live task queue, a release gate,
or the authoritative R5.1 requirement denominator. Actual code, tests,
installed evidence and the original R5.1 obligations remain authoritative.

- **Do not update this document after each source revision, test, or CI run.**
  Ordinary Gesso development proceeds without a corresponding plan edit.
- Revisit it when the project owner **asks for a progress estimate** or
  explicitly requests a change to the selected completion scope. A major
  change in technical feasibility may prompt a discussion, but should not
  silently rewrite the estimate.
- When re-estimating, inspect the then-current branch, the original R5.1
  obligations, the actual evidence and this existing ledger. Record the
  baseline revision/date, *which packages really closed*, new obligations,
  changed scope, revised package costs and revised forecast. Do not merely
  subtract commits elapsed.
- Keep the work-package IDs as stable references where practical. If an item
  splits, merges or is superseded, explain the mapping. A package is closed
  only when its selected acceptance evidence is qualified; additional
  defensive tests alone do not automatically count as closure.
- Maintain a short revision-history note when this planning document is
  deliberately refreshed. Update it in a separate numbered commit, not
  bundled into routine feature or bug-fix commits.

**First publication:** v2210 adds this document to the repository while
preserving its **v2209 estimate baseline**. That documentation-only commit
does not close any work package or change the original forecast.

**Re-estimation:** v2230 revises this document from the **v2229 source
baseline**. It does not count as framework closure work, and the 470-revision
forecast is measured from v2229, not from the documentation commit.

**Re-estimation:** v2258 revises this document from the **v2257 source
baseline**. The planning commit itself does not qualify a new feature or gate,
and the 464-revision estimate is measured from v2257, not v2258.

## Re-estimation ledger — v2229 source baseline (published v2230)

**Evidence interval:** v2210–v2229 on `typed-clojure`, from original
v2209 estimate commit `43bc18dc193e42613fda8f3f1f38b90190bcc440` to
`1b1e1d4a71f7c6075be99ecc40ae21c35af3d372`. The only planning-only
revision in the interval is v2210. v2211–v2225 primarily strengthen the
single-node durable outbox, worker/lease identities, Biff host restart,
receipt reconciliation, partial-emission uncertainty, and physical
late-interest/reconnect/reclaim scenarios. v2226–v2229 add bounded source
error retention, active coalescing state, unresolved puts per consumer and
SSE frame-write timeout/retirement. These are *verified narrower results*,
not a certificate for all applications or subscribers.

**Observed qualified CI at exact v2229:** Typed Clojure assurance
(`45056470053`), Ansatz proof regression (`45056470060`), and strict
Chromium/HTMX/XTDB outbox convergence (`45056470113`) all completed
successfully. These jobs do not constitute a full, predeclared release or
independent S0–S4 production transfer gate. No new work from this interval
establishes R5.1's four independent completion milestones.

**Package accounting:** 62 still open, zero newly *fully closed*. In
particular, #27, #28 and #29 are materially stronger but still depend on
source-to-authority joins, current client permission, a complete
registration/reacquisition seam and declared physical load/recovery bounds.
The v2229 changes expose/handle resource risk inside these *existing*
packages; no R5.1 requirement was dropped or newly invented.

**Remaining-work estimate adjustment (expert planning, not measured velocity):**

| Package(s) | v2209 → v2229 estimated revisions | Evidence-based reason |
|---|---:|---|
| #09 host lifecycle | 5 → 4 | Concrete Biff worker restart, ownership and termination cases |
| #25 served browser correspondence | 7 → 6 | Strict physical browser recovery and reconnect gates, not full served-asset proof |
| #27 durable outbox reconciliation | 7 → 4 | Receipt, lease, worker, cold-host and uncertain-submission evidence |
| #28 source/interest/SSE resources | 7 → 4 | Bounded source errors, coalescing scopes, pending puts and SSE frame timeout |
| #29 real recovery and convergence | 9 → 5 | Restart, late interest, missed-notice, lease-race and real Chromium gates |
| #59 cross-suite qualification | 6 → 5 | Expanded current CI selection; full suite and release gate still needed |
| All other packages | unchanged | The v2229 interval supplies no qualified closure evidence sufficient to lower them |
| **Total** | **483 → 470** | **13 fewer remaining, zero packages closed** |

Stage 9 now estimates 19 revisions remaining instead of 29; the other
changed stages together account for 3 revisions of reduction. Operational
track O falls from 140 to 129, C from 147 to 146, Q from 92 to
91, and F remains 104. The original-range uncertainty remains high. A smaller
remaining estimate does *not* imply that 13 of the 20 elapsed commits
were equivalent to completed mandatory acceptance gates.

## Re-estimation ledger — v2257 source baseline (published v2258)

**Evidence interval:** v2230–v2257, exactly **31 Git commits** after the
v2229 source baseline, including the v2230 estimate edit and **30 source,
test or CI commits** (some commits reuse a revision label, notably v2252
and v2256). The baseline-to-head diff touches **25 files**, with **6,128
insertions / 71 deletions**. This is not 28 independently closed gates.

**Verified advances, by original boundary:**

- v2231–v2244: bound active Live consumers, expose HTTP capacity refusal,
  retire stalled SSE streams, retain failed-release debt, add bounded and
  paced scheduler retries/backoff, and test shutdown and admission races.
  Neither a dead subscriber nor a failed cancellation silently transfers
  its authoritative notice or proves browser adoption.
- v2245–v2249: define concrete JDK listener lifetime and a distinct actual
  Netty final-write completion observer. The latter is **not** TCP delivery,
  producer quiescence, an authorized DOM swap, or general host shutdown.
- v2250–v2257: add a **separate opt-in Aleph 0.9.11 HTTP/1 qualification**
  and tests with real peers. Bind authorized source-backed SSE sessions to
  physical channel-close callbacks, route-owned cancellation/recovery debt,
  bounded reconciler scheduling and later terminal disposition; preserve a
  healthy peer while recovering a failed dead-peer release. The test harness
  exercises a real Biff/XTDB-backed host and actual source interest.
- The exact v2257 head has **four completed successful GitHub Actions lanes**:
  [Typed Clojure assurance](https://github.com/michaelmrose/gesso/actions/runs/45077519299),
  [Ansatz proof regression](https://github.com/michaelmrose/gesso/actions/runs/45077519296),
  [strict Chromium/HTMX/XTDB outbox](https://github.com/michaelmrose/gesso/actions/runs/45077519295),
  and [real Aleph HTTP/1](https://github.com/michaelmrose/gesso/actions/runs/45077519339).
  These attest their **selected** executable scopes, not the full JVM,
  browser, deployment, release or independent S0–S4 transfer denominator.

**What did not change:** No new source-to-selected-installation proof spans
an original admitted command, every guarded production writer, durable
publication intent, a currently authorized subscriber and the exact
browser-adopted result. The optional Aleph adapter is **not automatically
installed** as Gesso's production server. HTTP/2, arbitrary asynchronous
continuations, real load/security bounds, application-wide writer/route
inventory, four release decisions and independently justified S0–S4 transfer
remain open at their respective profiles. In particular, a channel close
is not proof of effect completion, and a green proof regression is not a
universal kernel theorem.

**Package accounting:** IDs **01–62 remain open**; **zero packages closed**
and **zero new packages added**. The v2257 work strengthens the existing
host/resource/recovery/test packets, not their original requirement denominator.
Costs below are *planning judgments* based on reduced implementation risk for
particular supported paths, **not earned progress points**:

| Package | v2229 → v2257 remaining revisions | Justification and surviving gap |
|---|---:|---|
| #09 host lifecycle | 4 → 3 | Actual listener and Biff-backed route cleanup tests; whole installed host, provider and failed compensation still open |
| #28 source/authorized SSE lifecycle | 4 → 2 | Bounded consumers and retries, route-owned release plus physical Netty handoff; source-wide current admission and arbitrary continuations remain open |
| #29 physical recovery/convergence | 5 → 3 | Real disconnect, held healthy peer, recovery and hosted Chromium evidence; no one-application complete adoption/fairness proof |
| #59 selected cross-suite CI | 5 → 4 | Fourth strictly executed real-Aleph lane; no full predeclared complete release gate |
| Other 58 packages | unchanged | No qualifying new closure or measurable supported-scope reduction |
| **Total** | **470 → 464** | **6 fewer estimated remaining revisions over 31 commits; no package closed** |

Stage 4 falls **15 → 14**, Stage 9 **19 → 15**, and cross-stage **21 → 20**;
all other stages are unchanged. Track **O 129 → 124**, **Q 91 → 90**,
**C 146 → 146**, and **F 104 → 104**. The new project arithmetic
(v2257 + 464 = v2721) is **22 revision numbers later** than the earlier
v2229 + 470 = v2699 illustration; this is a calibration warning that
physical qualification can consume more commits than prior packet costs
anticipated. It is **not** a reason to count narrow safety work as failure,
nor to forecast by simple subtraction.

### Completion and release reading against R5.1

R5.1 §11.17 requires independently justified **operational application,
compositional certification, external formal verification and multi-node**
milestones, not a single completion percentage. Stage 12 HumanHelp and
Stage 18 multi-node are **excluded from this forecast**, *not completed*.
The three included framework-owned profiles should be read distinctly:

- **Operational/release profile O+Q:** physical XTDB guards, Choreo and
  Live/browser pieces exist, and the physical SSE/host envelope is stronger.
  Original-operation-to-installed-application coverage, trusted current
  access, full UI adoption, release activation and compatibility remain
  **partial/unestablished**.
- **Compositional profile O+Q+C:** source checking, typed interfaces,
  internal proof checking, substantial physical test witnesses and negative
  cases exist. The **independently supplied requirement/writer inventory**,
  original-source-to-issued semantics, multi-record canonical proving
  application and one joined end-to-end acceptance path remain **open**.
- **Independent formal frontier O+Q+C+F:** Typed Clojure and Ansatz run
  in exact-head CI. They **do not establish** the requested general S0/S1
  theorem or selected S2/S3/S4 transfer and running physical correspondence;
  the independent theorem boundary remains **open**.

The four release decisions of R5.1 §11.02 (candidate, installation,
activation, completion) are still a separate unclosed system; passing CI
or a source-only preflight is not a release admission certificate. This
is an accounting of *supported evidence*, not a newly weakened completion
claim, and it does not pronounce HumanHelp complete by substituting a
Gesso-owned test fixture.

## Upcoming single-node work — recommended execution order

This is an **ordered working hypothesis**, not permission to skip other
R5.1 obligations or replace their original denominator. Each next
revision should select one current source boundary and prove its narrow
claim with a positive and an adversarial control. Re-plan at the next
owner-requested estimation, not after every commit.

| Priority | Current packet(s) | Concrete next boundary and accepted evidence |
|---|---|---|
| 1 | #26, #14–#18, #30–#34, #40–#43 | **Close one selected original-source-to-installed application path:** authoritative actor/operation and guarded XTDB effect, atomic publication intent, selected writer/initializer inventory, authorized interest and one actual browser adoption. The positive and a bypass/adversarial control must reach the real enforcing boundary |
| 2 | #09, #28–#29 | Complete the **actual selected host/transport profile**, joining admitted SSE route, bounded source state, failed release, Netty/producer quiescence and shutdown. Keep optional Aleph qualification separate from automatic installed-host adoption, HTTP/2 and unproved continuations |
| 3 | #25, #28–#29, #31–#32 | Demonstrate **initial authorized interest, source-to-browser identity and post-disconnect reacquisition** across actual served assets. An advisory cannot create a new read/refresh permission, and a physical write is not a DOM adoption |
| 4 | #27, #12, #17 | Reconcile committed/attempted/acknowledged/uncertain command and outbox evidence across cold host, late send and lease reclaim. Retain the selected result and retry scope under failure |
| 5 | #35–#39, #59–#62 | Create a single-node **predeclared release decision and artifact/handler gate**, with failed activation, genuine rollback, directional compatibility, whole selected suite and load/security envelopes |
| Parallel | #03–#07, #40–#58 | Advance Typed Clojure, source-derived certification and **independent Ansatz S0–S4** only against still-open exact proof/transfer obligations. Positive examples or bounded sample checks cannot be promoted to unconditional soundness |

**Near-term stopping rule:** v2231–v2257 has substantially strengthened the
physical SSE/host edge. **Do not continue indefinitely through neighboring
transport wrappers** unless a real observed safety defect requires a fix or
a predeclared gap is actually being closed. Prioritize a complete selected
original-source-to-Biff/XTDB-to-publication-to-authorized-browser path,
including positive and adversarial evidence and the independent writer/
capability denominator. Keep incorporated, attempted, acknowledged,
observed and browser-adopted outcomes distinct.

## Budget by stage

| Stage | Theme | Closure packets | Central additional revisions |
|---|---|---:|---:|
| 1 | Source ownership | 2 | 6 |
| 2 | Typed rule semantics | 3 | 12 |
| 3 | Propositional proof checking and induction | 2 | 10 |
| 4 | Biff/host context | 3 | 14 |
| 5 | Authoritative operations | 3 | 17 |
| 6 | Physical XTDB mediation | 5 | 35 |
| 7 | Choreo and projection | 3 | 22 |
| 8 | Browser effects and continuity | 4 | 28 |
| 9 | Live publication and recovery | 4 | 15 |
| 10 | Application assembly and installed closure | 5 | 44 |
| 11 | Release integrity/compatibility | 5 | 58 |
| 13 | Source-derived certification | 4 | 40 |
| 14 | Mutation and negative coverage | 3 | 22 |
| 15 | Counterexamples and explainability | 3 | 17 |
| 16 | Independent proof-kernel soundness | 4 | 39 |
| 17 | Formal-to-production transfer | 5 | 65 |
| Cross | Cross-stage verification and packaging | 4 | 20 |
| **Total** |  | **62** | **464** |

## Cost by practical track

| Track | Central | Interpretation |
|---|---:|---|
| O — Operational/semantic core | 124 | Finish actual source→host→command→browser/Live runtime contract |
| C — Compositional assurance | 146 | Source completeness, writer coverage, conditional certification, negative conformance |
| Q — Release/integration | 90 | Selected release qualification, activation/compatibility, whole-suite test and packaging |
| F — Independent formal transfer | 104 | S0/S1 theorem and selected S2–S4 production-applicability frontier |

## Source-based stage evidence and remaining packets

### Stage 1 — Source ownership

Existing evidence: Source-owned semantic identities are extensively modeled; independent required product denominator not complete for generic release.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 01 | Freeze original semantic identities and a concrete framework-owned requirement inventory | O | 3 | partial |
| 02 | Trace actor, target, relationship, source owner and operation IDs through consumers | C | 3 | partial |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Stage 2 — Typed rule semantics

Existing evidence: Closed rule language, evaluator, typed semantics and negative tests exist; complete supported use-site scope remains qualified.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 03 | Close rule-reader, absence/nil/false and ordered-evaluation use cases | O | 4 | substantial base |
| 04 | Qualify use-site knowledge, authorization and dynamic premises | C | 4 | partial |
| 05 | Complete eligible/negative portable JVM/CLJS rule corpus | O | 4 | partial |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Stage 3 — Propositional proof checking and induction

Existing evidence: Production seven-rule kernel and conditional checkers exist; universal independent soundness explicitly open (Stage 16).

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 06 | Close limited inference/induction checker contract and unsupported outcomes | C | 5 | substantial base |
| 07 | Bind issued proof conclusions, open premises and source-derived dependency inventory | C | 5 | partial |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Stage 4 — Biff/host context

Existing evidence: Checked Biff hosts/routes and v2214–v2218 worker restart. v2245–v2257 adds physical listener lifetime tracking, real Biff-backed authorized SSE sessions, concrete Netty completion and bounded cleanup/reconciliation under HTTP/1 peer failure. Aleph is opt-in; complete installed-host adoption, other producers and termination under load remain partial.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 08 | Pin actual Biff/Ring/Aleph/XTDB/CLJS provider profiles and host constraints | O | 5 | partial |
| 09 | Close host startup, lifecycle, termination and failed-compensation behavior | O | 3 | advanced partial: physical JDK/Aleph HTTP/1 listener/route recovery; not all host continuations |
| 10 | Prove correct request principal/context forwarding and host/handler adoption paths | O | 6 | partial |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Stage 5 — Authoritative operations

Existing evidence: Command, certified-plan, outcome and recovery modules exist; full source-to-authority adoption remains partial.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 11 | Close the public operation catalog and source-to-command identity | O | 6 | partial |
| 12 | Complete command-unknown recovery, deduplication and settlement semantics | O | 5 | partial |
| 13 | Connect authorization, guard/frame obligations and after-commit effects to originals | C | 6 | partial |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Stage 6 — Physical XTDB mediation

Existing evidence: Numerous physical XTDB gateway tests and correspondence modules; actual installed writer/initializer exhaustiveness and universal refinement remain open.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 14 | Connect one original declared operation to the exact immutable one-shot XTDB submission | O | 7 | substantial base |
| 15 | Qualify multi-document atomic guards, prestate, poststate and full write frame | O | 8 | partial |
| 16 | Establish actual writer and initializer completeness, including bypass-negative controls | C | 8 | open |
| 17 | Bind backend transaction, readback, receipt and commitment identity under failure | O | 6 | partial |
| 18 | Reconcile abstract steps to observed physical histories with explicit trust ledger | C | 6 | partial |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Stage 7 — Choreo and projection

Existing evidence: Executable Choreo semantics, projection and JVM/Node parity tests; whole real-application coverage/correspondence partial.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 19 | Complete Choreo global admission and distributed knowledge/premise checks | O | 7 | substantial base |
| 20 | Establish same-subject global-to-role projection/trace correspondence | C | 8 | partial |
| 21 | Test role-local recovery branches, portable CLJC/CLJS execution and negative histories | O | 7 | partial |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Stage 8 — Browser effects and continuity

Existing evidence: Large adapter/CLJS/browser infrastructure and real Chromium convergence, extended through v2218–v2221 Biff recovery, late-interest and reconnect scenarios; general shell effects/served-generation correspondence remains partial.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 22 | Close pure adapter event/effect/continuity semantics for chosen canonical flow | O | 8 | substantial base |
| 23 | Enforce shell target, generation and last-effect permissions on real browser paths | O | 7 | partial |
| 24 | Close optimistic settlement, supersession, retry and continuity across swaps | O | 7 | partial |
| 25 | Bind the actual served artifact, HTMX/SSE configuration and browser observation | C | 6 | partial: stricter hosted browser/recovery gates |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Stage 9 — Live publication and recovery

Existing evidence: Durable outbox/receipt/worker audits, real Chromium restart/reconnect tests (v2211–v2225), bounded source diagnostics, coalescing, pending puts and frame writes (v2226–v2229). v2231–v2257 adds active-consumer caps, HTTP capacity refusal, source-release recovery, bounded scheduled retries, concrete JDK listener and opt-in real Aleph HTTP/1 physical disconnect/Netty completion/healthy-peer recovery. The exact-head four-lane CI runs passed. This does **not** establish original mutation/publication completeness, authorized initial acquisition, whole application read permissions, physical HTTP/2, a universal load envelope or exact browser adoption for the same subject.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 26 | Prove commit-bound publication intent is captured for every selected mutation | O | 6 | substantial base |
| 27 | Complete outbox uncertainty/receipt reconciliation and supported lease-reclaim races | O | 4 | advanced partial: receipt, lease, worker, cold-host recovery evidence |
| 28 | Qualify Live source/interest, authorized SSE and resource-lifecycle boundaries | O | 2 | advanced partial: bounded consumers, route-owned cleanup and real Aleph channel-close observation |
| 29 | Exercise loss/reconnect/restart, backpressure and genuine multi-client adoption histories | O | 3 | advanced partial: live two-peer HTTP/1 and Chromium paths; not one full app adoption trace |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Stage 10 — Application assembly and installed closure

Existing evidence: Application preflight and original-source checks extensive; cannot infer that every deployed route, writer and provider is covered.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 30 | Inventory independently required app operations, routes and protected capabilities | O | 8 | partial |
| 31 | Close actual installed Biff handler/route/executor/source adoption joins | O | 8 | partial |
| 32 | Bind rendered affordances, generated browser assets and Live requirements to originals | C | 8 | partial |
| 33 | Audit all protected writers, initializers, providers and unchecked escape paths | C | 8 | open |
| 34 | Demonstrate complete positive/negative installed canonical application acceptance | Q | 12 | open |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Stage 11 — Release integrity/compatibility

Existing evidence: Source tree has artifact/host checks but no comparable complete release decision, rolling-compatibility and physical activation system.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 35 | Implement exact selected release intent, four decisions and mandatory gate ledger | Q | 10 | early |
| 36 | Bind current source/build/issuer/cache invalidation and proof-evidence reuse | Q | 12 | early |
| 37 | Implement physical install, activation, failed activation and checked rollback | Q | 12 | early |
| 38 | Qualify directional old/new compatibility and active client/command generations | Q | 12 | early |
| 39 | Qualify continuation, asset retention, retirement and release completion record | Q | 12 | early |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Stage 13 — Source-derived certification

Existing evidence: Many conditional proof and source-derived reports; application assurance report explicitly marks full certification and physical writer/HTMX correspondence unestablished.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 40 | Close original source-derived operation/obligation/certification catalog | C | 8 | partial |
| 41 | Bind proof checker, premises and issuer to current specific installed subjects | C | 10 | partial |
| 42 | Build substantial framework-owned multi-record Claim-like proving application | C | 10 | open |
| 43 | Pass source→guard→XTDB→Live→browser positive/adversarial certification gate | C | 12 | open |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Stage 14 — Mutation and negative coverage

Existing evidence: Large negative regression coverage in selected domains; independent mutation-corpus completeness and first-effect refusal not system-wide.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 44 | Freeze the generic per-boundary mutation corpus and valid control cases | C | 7 | partial |
| 45 | Verify negative cases fail before forbidden physical effects across boundaries | C | 7 | partial |
| 46 | Automate reproducible first-failure provenance and reject synthetic evidence inflation | C | 8 | partial |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Stage 15 — Counterexamples and explainability

Existing evidence: Diagnostic/counterexample interfaces exist; current complete source repair and evidence requalification are not demonstrated.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 47 | Derive source-relative explanations from actual authoritative ledgers | C | 5 | partial |
| 48 | Bind model/checker counterexamples to original-source locations and evidence | C | 6 | partial |
| 49 | Enforce authorized source-level repair and dependent requalification | C | 6 | early |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Stage 16 — Independent proof-kernel soundness

Existing evidence: 15 Ansatz pilot namespaces with passing CI; not equivalent to universally quantified independent S0/S1 theorem or production-checker soundness.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 50 | Obtain actual independently kernel-checked S0 rule-schema soundness theorem | F | 10 | research |
| 51 | Prove S1 arbitrary finite valid derivation with open premises, not samples | F | 12 | research |
| 52 | Define faithful formal syntax/normalization and reject unsupported proof gaps | F | 9 | research |
| 53 | Reproducibly import checked theorem and fail closed on forged/stale evidence | F | 8 | partial |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Stage 17 — Formal-to-production transfer

Existing evidence: Many bounded physical/semantic witness modules; separately justified production S2/S3/S4 transfer proof remains open.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 54 | Qualify S2 production parser, validator, normalizer and checker correspondence | F | 12 | research |
| 55 | Qualify S3 typed atom semantics and actual reader/use-site correspondence | F | 13 | research |
| 56 | Qualify S4 issuer, complete obligation inventory and generation binding | F | 12 | research |
| 57 | Transfer checked abstract safety to selected actual XTDB writer/host execution | F | 14 | research |
| 58 | Qualify selected browser/Live progression and source-to-release evidence assumptions | F | 14 | research |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

### Cross-stage — Cross-stage verification and packaging

Existing evidence: Four distinct selected GitHub Actions workflows green at the **exact v2257 head**: Typed Clojure, Ansatz proof regression, strict Chromium/HTMX/XTDB outbox and the new real Aleph HTTP/1 qualification. These are not a single predeclared full JVM/CLJS/formal/release/deployment certificate.

| # | Work package to close | Track | Estimated revisions | Status at v2257 |
|---:|---|:---:|---:|---|
| 59 | Add predeclared full-suite JVM/CLJS/browser/XTDB/restart coverage with clean exits | Q | 4 | partial: four exact-head selected CI lanes; full denominator still open |
| 60 | Run reproducible load/fault/security bounds and resource limits for selected profile | Q | 6 | early |
| 61 | Write runnable canonical example, supported boundaries and versioned integration docs | Q | 5 | partial |
| 62 | Stabilize dependency/toolchain versions, compatibility checks and release cut | Q | 5 | early |

**Definition of complete:** All selected profile gates represented by these packets have current applicable evidence; negative tests demonstrate refusal at the enforcing boundary; residual assumptions are explicitly qualified, rather than promoted to proven.

## Forecasting restrictions

1. One revision here means a tested, coherent commit, **not** a guaranteed fixed unit of acceptance work. Historical v2166–v2209: 43 revisions, 34 changed files, 4,629 added source/test lines. v2210–v2229: 20 commits including one planning-only commit; 22 changed files and 3,996 added lines (including planning). v2230–v2257: **31 commits**, of which v2230 was planning-only, across **25 changed files, 6,128 added / 71 deleted lines**. Some commits reuse a `v2252` or `v2256` label. No R5.1 release or independent formal-completion claim follows from raw code/commit volume.
2. These 62 packets are an expert decomposition, *not* the original requirement denominator. R5.1 explicitly forbids replacing required original obligations with a generated closed subset.
3. The practical operational release under a narrower predeclared conditional assurance profile may be achieved substantially earlier than the full reusable formal-completion profile. The totals should not be mistaken for the schedule of a critical path; parallel development is possible.
4. Research uncertainty is not Gaussian and is not a strict upper bound. Especially S2/S3/S4 and general physical refinement could demand a changed technical approach. If a theorem is impossible under the selected implementation assumptions, it is not an additional fixed number of commits.
5. A generic framework-owned multi-record proving case does not establish the original HumanHelp Claim milestone. Keep that a separate open milestone until HumanHelp is actually qualified.
6. To make convergence measurable: for each packet record its original gate ID, first enforcement point, positive/negative test references, independently required scope/denominator, current evidence status, and a closure decision. Only then can revision forecasts be recalibrated by observed completed packets.

## Explicit accounting for R5.1 cross-stage Gates A–P

These gates are *included inside* the stage packets above; they are not 16 extra cost rows. The following maps each gate to its principal charged stages. The mapping is a planning hypothesis, not a proof the gate is discharged.

| Gate | Normative question in brief | Charged to stages |
|---|---|---|
| A | No silent semantic downgrade | 2, 3, 5, 13 |
| B | One connected semantic subject | 1, 5, 7, 10, 13 |
| C | Single original source owner | 1, 10, 13 |
| D | Trusted enforcement owns authority | 4, 5, 6, 8 |
| E | Exact artifact and implementation binding | 8, 10, 11, 17 |
| F | Dynamic premises remain live/dynamic | 2, 5, 6 |
| G | Actual physical integration required | 6, 8, 9, 10 |
| H | Contradictions rejected at first knowable boundary | 9, 10, 11, 14 |
| I | Representation never grants admission | 3, 10, 13 |
| J | Required capability set independent of discovered closure | 1, 10, 13 |
| K | Incorporation, delivery, knowledge are distinct | 5, 8, 9 |
| L | Recovery preserves scope and adoption permission | 8, 9, 11 |
| M | Evidence retains scope through composition | 3, 13, 16, 17 |
| N | Policy visibility has an honest inspection boundary | 10, 15 |
| O | Formal assumptions imply physical production obligations | 6, 16, 17 |
| P | Intent review and repair retain evidence trust | 11, 14, 15 |

## Incremental planning profiles

| Profile | Work tracks counted | Central remaining effort from v2257 | Limitation |
|---|---|---:|---|
| Qualified framework-owned single-node operational/release profile | O + Q | **214** | Declares accepted bounded assumptions; not the original HumanHelp operational milestone |
| Same profile with reusable compositional certification | O + Q + C | **360** | A framework-owned Claim-like witness cannot fulfill R5.1's real HumanHelp Claim pilot |
| Same plus selected independent S0–S4 formal frontier | O + Q + C + F | **464** | Source-to-production universal proof beyond a declared supported subset may remain open |

These are cumulative *effort budgets*, not serial implementation waves; results can be reused and tracks may be developed concurrently. Stage 11's rolling compatibility applies to old/new live browser or host generations even in a one-node deployment; general multi-node routing remains out of scope.

## Specific possible re-estimation blockers

- **Source completeness:** If arbitrary application-provided handlers/direct writers are in scope, there is no sound inference from a declared catalog to the absence of undiscovered bypass paths. A narrower admitted/checked application profile or real installation inventory is essential.
- **Physical refinement:** A bounded real XTDB test cannot by itself establish universal implementation refinement; closure may depend on a qualified trusted serialization/transaction premise or a new independently verified gateway.
- **Formal correspondence:** Ansatz pilots and Typed Clojure gates cannot replace an independent S0/S1 theorem and a justified production S2/S3/S4 translation; a formalization or extraction design may take an unpredictable number of iterations.
- **Liveness:** Browser eventual convergence requires explicit scheduler, network, retention and availability premises. No number of tests proves unconditional delivery under arbitrary permanent failure.
- **Actual HumanHelp acceptance:** Excluded from this estimate; a framework-owned demonstration cannot legitimately change its evidence status.

## Planning revision history

| Publication | Source baseline | Open packages | Central additional revisions | Decision |
|---|---|---:|---:|---|
| v2210 | v2209 | 62 | 483 | Initial single-node closure estimate |
| v2230 | v2229 | 62 | 470 | Revised selected Stage 4/8/9 and cross-test costs after real outbox/Live convergence work; none fully closed |
| v2258 | v2257 | 62 | 464 | Real source/SSE capacity and HTTP/1 listener-disconnect/reconciliation evidence reduced #09/#28/#29/#59 costs; none fully closed |

## Revision-number projection and calibration

The **current** additive center **464 from v2257** gives an arithmetic illustration of **v2721** (v2257 + 464), **not** a promised delivery revision. A favorable case around **240** from this baseline would be ~v2497; a planning case of **300–800** spans ~v2557–v3057. Independent proof/transfer may extend beyond that or require a revised claim profile. The earlier v2229 + 470 forecast was v2699, and the original v2209 + 483 forecast was v2692. The fact that the arithmetic center moved outward while important physical safety tests passed illustrates why commit counts must never be used as closure denominators.

Re-estimation must be conditioned on **closed packets**, not revisions consumed. After 40–60 more substantive commits, record the number of closed packages, current scope, each newly discovered obligation and whether full end-to-end source-to-installation acceptance has become executable. If another 40–60 commits retire no package, reconsider the *granularity and forecast calibration*, not just the remaining numbers.
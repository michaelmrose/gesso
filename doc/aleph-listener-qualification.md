# Aleph listener qualification (v2250)

## Scope and status

**Status: NOT QUALIFIED for production.** This is an opt-in, executable
qualification gate for Aleph **0.9.11**, not the default Gesso HTTP server.
The new gate is deliberately outside `:test`'s ordinary test paths, and
Aleph remains outside ordinary JVM, browser and CLJS dependencies.

Run on a machine with access to Clojars (or a complete compatible local
Maven cache), from the repository root:

```sh
bb test:outbox:aleph
# Equivalent:
clojure -M:aleph-integration -m gesso.live.outbox.http-aleph-qualification-main
```

Missing Aleph jars or dependencies must **fail the gate**, not turn it into a
skipped/passing test. The dedicated entry point explicitly requires the real
Aleph test namespace and throws if zero tests run; the ordinary Cognitect
`-n` selector can silently report zero tests when an extra test root is
not included in its discovery paths. The pinned alias is `:aleph-integration` in `deps.edn`.
Qualification also requires checking the selected transitive Netty versions
against XTDB/Biff's classpath; the version pin does not establish compatibility.

## GitHub-hosted real-listener gate (v2251)

The separate `.github/workflows/outbox-aleph.yml` lane runs the strict
qualification entry point on **GitHub-hosted Ubuntu 24.04 with Java 21**.
It is triggered by changes on `typed-clojure` to the optional Aleph
harness, listener/Netty completion integration, dependencies, or workflow
itself. `workflow_dispatch` also permits a manual run.

The job resolves the pinned **real** `aleph/aleph` 0.9.11 artifact, verifies
it appears on the selected classpath, then runs:

```sh
clojure -M:aleph-integration -m gesso.live.outbox.http-aleph-qualification-main
```

There is no fallback to the JDK listener, no mock server, no zero-test
success, and no `continue-on-error` override. A failed dependency download,
qualification assertion, or test-loader error must fail the job. Offline
configuration tests validate these controls but **do not qualify Aleph**.

**Configured is not passed.** Every revision needs its own completed,
successful Actions run before claiming its qualification tests have passed.
An unexecuted, queued, skipped, or failing workflow is not positive evidence.
Even a green HTTP/1 gate does not establish HTTP/2, production source
cancellation, listener retirement under load, or browser delivery.

## Executed HTTP/1 evidence (v2251) and next gate (v2252)

The first actual Aleph qualification completed **successfully** on
2026-11-22 for commit `7898953d95a39d8a61c1d77c73954ab86c490249`:

- [GitHub Actions run 45071447647](https://github.com/michaelmrose/gesso/actions/runs/45071447647)
- Real `aleph/aleph` **0.9.11** jar resolved successfully on Java 21.
- The strict HTTP/1 qualification executed **2 tests / 15 assertions**,
  reporting **0 failures / 0 errors** in the job log.
- The result supports finite HTTP/1 response final-write capture and the
  independently certified streaming-producer quiescence case, in the
  controlled harness only.

Revision v2252 extends this gate with a third test using **two sequential
HTTP/1.1 requests over the same physical keep-alive socket**, confirming
that each response has its own captured final-write completion and that
neither the listener nor the per-channel pending registry leaks ownership.
**This added case needs a separate successful workflow run on v2252**;
a passed v2251 run does not validate the new test.

## Automatic source-backed disconnect cleanup (v2254)

`gesso.live.outbox.http-aleph-live/attach!` accepts a **real, already-authorized**
source-backed SSE session, the Aleph request's Netty channel, its per-response
terminal bridge, and a host-managed executor. It waits for the physical
channel's close future before scheduling the session's actual cancellation
**outside the Netty event loop**. The callback verifies the SSE response body
and eager source subscription have both closed before discharging the bridge.
Errors and rejected executor scheduling remain explicit, manually retryable
debt. A live channel is never canceled by merely registering this hook.

A strict Aleph HTTP/1 test now exercises an actual Gesso source-backed SSE
subscription, receives a source-generated frame, resets the client socket,
and checks automatic tap release and listener drain **without test-supplied
completion callbacks**. This test is unqualified until the v2254 workflow
actually passes. The helper is still opt-in and does not automatically
wrap Gesso's authorized Ring route. Source-backed cancel! returning plus tap
and response closure is not a blanket guarantee for arbitrary application
owned asynchronous continuations; those require their own completion proof.

## What the first gate exercises

The `integration/gesso/live/outbox/http_aleph_integration_test.clj` harness
starts a real HTTP/1 Aleph listener on a local ephemeral port, using the
Aleph 0.9.11 `:http1-pipeline-transform` extension point. It:

- registers a per-response bridge *before* returning the Ring response;
- observes the actual outbound `LastHttpContent` promise, not Ring handler
  return or Manifold `put!` as completion evidence;
- verifies HTTP response bytes using a separate real HTTP client;
- checks that the asynchronous listener's ownership is released only after
  both Netty write completion and application producer quiescence;
- exercises a stream where final Netty disposition and producer-quiescence
  are deliberately independent, then drains the concrete listener.

The finite-response test certifies producer quiescence after the fully
materialized synchronous handler returns, **only for that controlled test
handler**. The streaming test does not infer quiescence from stream closure;
its test owner explicitly certifies when no provider-using continuations
remain. Production Gesso SSE still needs real producer cancellation/cleanup
wiring before it may claim this property.

## Evidence and remaining obligations

The supplied local offline dependency bundle has **no Aleph jar** and
cannot access Clojars. The optional dependency was resolved and the two
original real HTTP/1 tests were successfully run in GitHub Actions for
v2251. This does not independently establish that every combination of
Aleph, Netty, Biff and XTDB is compatible in production, and the v2252
keep-alive extension remains unqualified until its own workflow passes.

Further qualification must exercise HTTP/2 stream channels, disconnects,
interrupted and failed writes, actual application/Live SSE producer cleanup,
Aleph listener acceptance shutdown, and combined host-provider drain under
load. A successful outbound ChannelFuture is only Netty pipeline completion,
not TCP acknowledgment or browser DOM adoption. Do not claim Aleph-backed
production lifecycle safety until those separate gates pass.

## Managed authorized route ownership handoff (v2255)

`wrap-authorized-sse` optionally accepts `:on-session-admitted!`, a trusted
synchronous callback receiving the already-authorized Ring request and a
**route-owned** Live SSE session, before the HTTP 200 stream is returned.
The session's `:cancel!` is mediated by the route's existing cancellation
and recovery ledger; it is **not** a second independent source owner. The
Aleph adapter may give that session to `http-aleph-live/attach!` along with
the Netty channel, its terminal bridge, and a bounded cleanup executor.
The hook is never run for denied, non-SSE or over-capacity requests.

Failed hook installation rolls back the registered session. An immediate
cancellation inside the hook cannot return an already-dead HTTP 200 stream.
After successful release, later disconnect calls are idempotent even if the
route has shut down. Once route shutdown starts, *unresolved* release debt
belongs exclusively to its ordered `:close!` retry; a late channel callback
may not start a separate cancellation attempt. The Aleph cleanup adapter can
retain its own retry obligation until this route drain completes.

The v2255 strict Aleph test starts an actual Biff/XTDB-backed Gesso host and
an authorized managed SSE route, verifies a real source-generated frame on
the wire, resets the client socket and checks the combined route's occupied
capacity, indexed source subscription, pending Netty writes, and listener
ownership drain. This new test is **not qualified** until the corresponding
v2255 GitHub Actions run succeeds. As before, the host must call ordered
route/listener/host shutdown; this hook does not automatically select Aleph
or cover unrelated application-owned asynchronous continuations.

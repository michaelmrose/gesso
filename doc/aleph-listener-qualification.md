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

The supplied offline dependency bundle has **no Aleph jar** and its runtime
cannot access Clojars. As of v2250 the gate's EDN and Clojure forms have
been parsed, the optional-dependency invariants and existing Netty/listener
regressions have been run, but **the real Aleph tests have not run**.

Further qualification must exercise HTTP/2 stream channels, disconnects,
interrupted and failed writes, actual application/Live SSE producer cleanup,
Aleph listener acceptance shutdown, and combined host-provider drain under
load. A successful outbound ChannelFuture is only Netty pipeline completion,
not TCP acknowledgment or browser DOM adoption. Do not claim Aleph-backed
production lifecycle safety until those separate gates pass.

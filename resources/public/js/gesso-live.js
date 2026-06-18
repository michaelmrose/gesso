/*
 * gesso-live.js / Gesso Live client-continuity runtime v0.12.1
 *
 * Framework-owned browser runtime for preserving local interaction context
 * across Gesso Live / HTMX fragment replacement.
 *
 * Expected stable root attrs:
 *
 *   data-gesso-live-fragment="<stable root name/id>"
 *   data-gesso-live-continuity="true"
 *   data-gesso-live-continuity-fragment="<replaceable target id>"
 *   data-gesso-live-continuity-config='{"enabled":true,...}'
 *
 * A future/alternate config carrier is also supported:
 *
 *   <script type="application/json" data-gesso-live-continuity-config>
 *     {"enabled":true,...}
 *   </script>
 *
 * This runtime handles both:
 *
 *   - normal HTMX swaps: htmx:beforeSwap / htmx:afterSwap / htmx:afterSettle
 *   - out-of-band swaps: htmx:oobBeforeSwap / htmx:oobAfterSwap
 *
 * Anchor scroll is preferred, but every scroll capture also records raw scroll
 * positions so a missing/moved anchor cannot degrade into a hostile jump to the
 * top of the page.
 *
 * v0.8.0 additionally height-locks the stable continuity root while HTMX swaps
 * the replaceable target. This prevents the document from briefly collapsing
 * and clamping window scroll to the top before restoration runs.
 *
 * v0.9.0 adds request-start capture for hx-swap="none" + OOB response paths,
 * safer focus preservation, and a short post-restore scroll shield so late
 * browser/HTMX focus/reveal behavior cannot navigate the viewport after
 * continuity restoration.
 *
 * v0.10.0 corrects the direct htmx-ext-sse lifecycle hooks to the documented
 * htmx:sseBeforeMessage / htmx:sseMessage events.
 *
 * v0.11.0 adds the built-in details-open continuity box for preserving native
 * <details open> state across fragment replacement.
 *
 * v0.11.1 restores details-open immediately after swap to avoid visible
 * collapsed/open flicker before the delayed full restore path runs.
 *
 * v0.12.0 adds generic optimistic-template support. Server-rendered hidden
 * <template> elements can be swapped into an existing HTMX target immediately
 * on request start, while the later HTMX response remains authoritative.
 *
 * v0.12.1 adds an immediate optimistic press phase. Elements with optimistic
 * action attrs are marked on pointerdown/click/submit before the HTMX request
 * lifecycle, so the user gets local button-press feedback before the optional
 * optimistic template swap and before the authoritative server/SSE result.
 */
(function () {
  "use strict";

  var ROOT_SELECTOR = "[data-gesso-live-continuity='true']";
  var CONFIG_ATTR = "data-gesso-live-continuity-config";
  var CONFIG_SCRIPT_SELECTOR = "script[type='application/json'][data-gesso-live-continuity-config]";
  var FRAGMENT_ATTR = "data-gesso-live-continuity-fragment";
  var STATE_SLOT = "__gessoLiveContinuity";

  var OPTIMISTIC_TEMPLATE_ATTR = "data-gesso-optimistic-template";
  var OPTIMISTIC_ACTION_ATTR = "data-gesso-optimistic-action";
  var OPTIMISTIC_TARGET_ATTR = "data-gesso-optimistic-target";
  var OPTIMISTIC_ACTIVE_ATTR = "data-gesso-optimistic-active";
  var OPTIMISTIC_PENDING_ATTR = "data-gesso-optimistic-pending";
  var OPTIMISTIC_PRESSED_ATTR = "data-gesso-optimistic-pressed";
  var OPTIMISTIC_TRIGGER_ATTR = "data-gesso-optimistic-trigger";
  var OPTIMISTIC_LABEL_ATTR = "data-gesso-optimistic-label";
  var OPTIMISTIC_ERROR_EVENT = "gesso:optimistic:error";

  // Slots are also indexed globally by replaceable target id so OOB lifecycle
  // events can restore even if HTMX reports the old detached target element.
  var activeSlotsByTargetId = {};
  var optimisticSlotsById = {};
  var optimisticSeq = 0;

  var lastUserScrollIntentAt = 0;

  function markUserScrollIntent() {
    lastUserScrollIntentAt = now();
  }

  function keyCouldScroll(event) {
    var key = event && event.key;
    return key === "ArrowUp" ||
      key === "ArrowDown" ||
      key === "ArrowLeft" ||
      key === "ArrowRight" ||
      key === "PageUp" ||
      key === "PageDown" ||
      key === "Home" ||
      key === "End" ||
      key === " " ||
      key === "Spacebar";
  }

  window.addEventListener("wheel", markUserScrollIntent, { passive: true, capture: true });
  window.addEventListener("touchmove", markUserScrollIntent, { passive: true, capture: true });
  window.addEventListener("pointerdown", markUserScrollIntent, { passive: true, capture: true });
  window.addEventListener("keydown", function (event) {
    if (keyCouldScroll(event)) markUserScrollIntent();
  }, true);

  function now() {
    return Date.now ? Date.now() : new Date().getTime();
  }

  function emit(root, name, detail) {
    if (!root || typeof root.dispatchEvent !== "function") return null;

    var event = null;
    var eventDetail = detail || {};

    try {
      event = new CustomEvent("gesso:live-continuity:" + name, {
        bubbles: true,
        cancelable: false,
        detail: eventDetail
      });
    } catch (_customEventError) {
      event = document.createEvent("CustomEvent");
      event.initCustomEvent("gesso:live-continuity:" + name, true, false, eventDetail);
    }

    try {
      root.dispatchEvent(event);
    } catch (_dispatchError) {
      // Debug/event plumbing must never break HTMX swaps.
    }

    return eventDetail;
  }

  function isElement(value) {
    return value && value.nodeType === 1;
  }

  function contains(root, node) {
    return !!(root && node && (root === node || root.contains(node)));
  }

  function isConnected(element) {
    if (!element) return false;
    if (typeof element.isConnected === "boolean") return element.isConnected;
    return document.documentElement && document.documentElement.contains(element);
  }

  function asArray(value) {
    if (value == null || value === false) return [];
    return Array.isArray(value) ? value : [value];
  }

  function cssEscape(value) {
    var string = String(value);
    if (window.CSS && typeof window.CSS.escape === "function") {
      return window.CSS.escape(string);
    }

    // Minimal fallback sufficient for id selectors in old browsers.
    return string.replace(/[^a-zA-Z0-9_-]/g, function (ch) {
      var hex = ch.charCodeAt(0).toString(16);
      return "\\" + hex + " ";
    });
  }

  function query(root, selector) {
    if (!root || !selector) return null;
    try {
      return root.querySelector(selector);
    } catch (_selectorError) {
      return null;
    }
  }

  function queryAll(root, selector) {
    if (!root || !selector) return [];
    try {
      return Array.prototype.slice.call(root.querySelectorAll(selector));
    } catch (_selectorError) {
      return [];
    }
  }

  function findById(scope, id) {
    if (!scope || !id) return null;
    if (scope.id === id) return scope;

    var selector = "#" + cssEscape(id);
    return query(scope, selector);
  }

  function findByAttr(scope, attr, value) {
    if (!scope || !attr) return null;

    // Do not interpolate arbitrary attr values into a selector. Query by attr
    // presence and compare DOM values directly.
    var candidates = queryAll(scope, "[" + attr + "]");
    var stringValue = String(value);

    for (var i = 0; i < candidates.length; i += 1) {
      if (candidates[i].getAttribute(attr) === stringValue) {
        return candidates[i];
      }
    }

    return null;
  }

  function findByName(scope, name) {
    return findByAttr(scope, "name", name);
  }

  function childConfigScript(root) {
    if (!root || !root.children) return null;

    // Prefer an immediate child so a replaced target cannot accidentally become
    // the root's active config source.
    for (var i = 0; i < root.children.length; i += 1) {
      var child = root.children[i];
      if (child.matches && child.matches(CONFIG_SCRIPT_SELECTOR)) {
        return child;
      }
    }

    return null;
  }

  function parseJson(root, raw, source) {
    if (!raw) return null;

    try {
      return JSON.parse(raw);
    } catch (error) {
      emit(root, "error", {
        phase: "parse-config",
        source: source,
        error: error,
        raw: raw
      });
      return null;
    }
  }

  function parseConfig(root) {
    if (!root) return null;

    var attrConfig = root.getAttribute(CONFIG_ATTR);
    if (attrConfig) return parseJson(root, attrConfig, "attr");

    var script = childConfigScript(root);
    if (script) return parseJson(root, script.textContent || script.innerText || "", "script");

    return null;
  }

  function keyForElement(element, opts) {
    if (!element) return null;

    var keyAttr = opts && (opts.keyAttr || opts.keyAttribute || opts["key-attr"] || opts["key-attribute"]);
    if (keyAttr && element.getAttribute(keyAttr)) {
      return { kind: "attr", attr: keyAttr, value: element.getAttribute(keyAttr) };
    }

    if (element.id) {
      return { kind: "id", value: element.id };
    }

    if (element.getAttribute("data-gesso-continuity-key")) {
      return {
        kind: "attr",
        attr: "data-gesso-continuity-key",
        value: element.getAttribute("data-gesso-continuity-key")
      };
    }

    if (element.getAttribute("data-key")) {
      return { kind: "attr", attr: "data-key", value: element.getAttribute("data-key") };
    }

    if (element.name) {
      return { kind: "name", value: element.name };
    }

    return null;
  }

  function findByKey(scope, key) {
    if (!scope || !key) return null;

    if (key.kind === "id") return findById(scope, key.value);
    if (key.kind === "attr") return findByAttr(scope, key.attr, key.value);
    if (key.kind === "name") return findByName(scope, key.value);

    return null;
  }

  function eventElements(event) {
    var elements = [];
    var detail = event && event.detail;

    function add(value) {
      if (isElement(value) && elements.indexOf(value) === -1) elements.push(value);
    }

    // HTMX versions/extensions differ in which field is the request element and
    // which field is the swap target. OOB events also vary. Treat all known
    // element-bearing fields as candidates and derive the real root/target below.
    if (detail) {
      add(detail.target);
      add(detail.elt);
      add(detail.source);
      add(detail.fragment);
      add(detail.oobElement);
      add(detail.swappedElement);
      add(detail.oobTarget);
      if (detail.requestConfig) add(detail.requestConfig.elt);
    }

    add(event && event.target);

    return elements;
  }

  function eventTarget(event) {
    var detail = event && event.detail;
    if (detail && isElement(detail.target)) return detail.target;
    if (isElement(event && event.target)) return event.target;
    return null;
  }

  function firstElementId(elements) {
    for (var i = 0; i < elements.length; i += 1) {
      if (elements[i] && elements[i].id) return elements[i].id;
    }
    return null;
  }

  function closestContinuityRoot(element) {
    if (!element) return null;

    if (element.matches && element.matches(ROOT_SELECTOR)) {
      return element;
    }

    if (element.closest) {
      return element.closest(ROOT_SELECTOR);
    }

    return null;
  }

  function rootTargetId(root) {
    return root && root.getAttribute(FRAGMENT_ATTR);
  }

  function hxTargetId(root) {
    if (!root) return null;

    var hxTarget = root.getAttribute("hx-target");
    if (hxTarget && hxTarget.charAt(0) === "#") {
      return hxTarget.slice(1);
    }

    return null;
  }

  function continuityTargetId(root) {
    return rootTargetId(root) || hxTargetId(root);
  }

  function rootForTargetId(targetId) {
    if (!targetId) return null;

    var roots = queryAll(document, ROOT_SELECTOR);
    for (var i = 0; i < roots.length; i += 1) {
      if (continuityTargetId(roots[i]) === targetId) return roots[i];
    }

    return null;
  }

  function targetIdFromEvent(event) {
    var elements = eventElements(event);
    var id = firstElementId(elements);
    if (id) return id;

    var detail = event && event.detail;
    if (!detail) return null;

    if (typeof detail.target === "string" && detail.target.charAt(0) === "#") {
      return detail.target.slice(1);
    }

    if (typeof detail.oobTarget === "string" && detail.oobTarget.charAt(0) === "#") {
      return detail.oobTarget.slice(1);
    }

    return null;
  }

  function rootFromEvent(event) {
    var elements = eventElements(event);

    for (var i = 0; i < elements.length; i += 1) {
      var root = closestContinuityRoot(elements[i]);
      if (root) return root;
    }

    var targetId = targetIdFromEvent(event);
    if (targetId && activeSlotsByTargetId[targetId] && activeSlotsByTargetId[targetId].root) {
      return activeSlotsByTargetId[targetId].root;
    }

    return rootForTargetId(targetId);
  }

  function targetMatchesRoot(root, target, fallbackId) {
    if (!root || !target) return false;

    // With hx-swap="outerHTML", lifecycle events can still mention the old
    // target element after it has been detached. Matching by id alone would then
    // restore into a dead subtree. Only accept a target that is currently inside
    // the stable continuity root.
    if (!contains(root, target)) return false;

    var targetId = fallbackId || continuityTargetId(root);
    if (targetId && target.id === targetId) return true;

    return false;
  }

  function findContinuityTarget(root, event, fallbackId) {
    if (!root) return null;

    var targetId = fallbackId || continuityTargetId(root);
    var elements = eventElements(event);

    for (var i = 0; i < elements.length; i += 1) {
      if (targetMatchesRoot(root, elements[i], targetId)) return elements[i];
    }

    if (targetId) {
      // Prefer finding the new/current target inside the stable root. Falling
      // back to document supports unusual markup but avoids using detached event
      // targets as restore destinations.
      return findById(root, targetId) || findById(document, targetId);
    }

    return null;
  }

  function configEnabled(config) {
    return !!config && config.enabled !== false;
  }

  function rawWindowScrollState() {
    return {
      kind: "window",
      x: window.pageXOffset || document.documentElement.scrollLeft || document.body.scrollLeft || 0,
      y: window.pageYOffset || document.documentElement.scrollTop || document.body.scrollTop || 0
    };
  }

  function rawElementScrollState(element, root, opts) {
    if (!element || element === window) return rawWindowScrollState();

    return {
      kind: "element",
      key: keyForElement(element, opts),
      top: element.scrollTop,
      left: element.scrollLeft,
      // If the scroller cannot be keyed, we can still use the same element when
      // it survives, or fall back to window rather than doing nothing.
      transientElement: contains(root, element) ? element : null
    };
  }

  function restoreRawScroll(root, raw) {
    if (!raw) return false;

    if (raw.kind === "window") {
      window.scrollTo(raw.x || 0, raw.y || 0);
      return true;
    }

    if (raw.kind === "element") {
      var element = null;

      if (raw.key) {
        element = findByKey(root || document, raw.key);
      }

      if (!element && raw.transientElement && isConnected(raw.transientElement)) {
        element = raw.transientElement;
      }

      if (element) {
        element.scrollTop = raw.top || 0;
        element.scrollLeft = raw.left || 0;
        return true;
      }
    }

    return false;
  }

  function scrollContainerFor(element, root, box) {
    var selector = box && (box.containerSelector || box["container-selector"] || box.container);
    if (selector) {
      return query(root, selector) || query(document, selector) || window;
    }

    var current = element && element.parentElement;
    while (current && current !== document.documentElement && current !== document.body) {
      var style = window.getComputedStyle(current);
      var overflowY = style.overflowY;
      if ((overflowY === "auto" || overflowY === "scroll") && current.scrollHeight > current.clientHeight) {
        return current;
      }
      current = current.parentElement;
    }

    return window;
  }

  function viewportForScroller(scroller) {
    if (!scroller || scroller === window) {
      return { top: 0, bottom: window.innerHeight || document.documentElement.clientHeight || 0 };
    }

    var rect = scroller.getBoundingClientRect();
    return { top: rect.top, bottom: rect.bottom };
  }

  function visibleScore(element, viewport) {
    var rect = element.getBoundingClientRect();
    var visible = rect.bottom >= viewport.top && rect.top <= viewport.bottom;
    if (!visible) return null;
    return Math.abs(rect.top - viewport.top);
  }

  function chooseAnchor(candidates, scroller) {
    var viewport = viewportForScroller(scroller);
    var best = null;
    var bestScore = Infinity;

    candidates.forEach(function (candidate) {
      var score = visibleScore(candidate, viewport);
      if (score != null && score < bestScore) {
        best = candidate;
        bestScore = score;
      }
    });

    return best || candidates[0] || null;
  }

  function scrollerKey(scroller, root, box) {
    if (!scroller || scroller === window) return null;
    return keyForElement(scroller, box) || keyForElement(scroller) || (contains(root, scroller) ? null : null);
  }

  function scrollBy(scroller, delta) {
    if (!delta) return;

    if (!scroller || scroller === window) {
      window.scrollBy(0, delta);
    } else {
      scroller.scrollTop += delta;
    }
  }

  function inputElementValue(element) {
    if (!element) return null;

    var tag = (element.tagName || "").toLowerCase();
    var type = (element.type || "").toLowerCase();

    if (type === "checkbox" || type === "radio") {
      return { checked: !!element.checked };
    }

    if (tag === "select" && element.multiple) {
      return {
        selectedValues: Array.prototype.slice.call(element.options)
          .filter(function (option) { return option.selected; })
          .map(function (option) { return option.value; })
      };
    }

    if ("value" in element) return { value: element.value };
    return null;
  }

  function restoreInputElementValue(element, saved) {
    if (!element || !saved) return;

    if (Object.prototype.hasOwnProperty.call(saved, "checked")) {
      element.checked = !!saved.checked;
      return;
    }

    if (Array.isArray(saved.selectedValues) && element.options) {
      var values = saved.selectedValues.reduce(function (m, value) {
        m[String(value)] = true;
        return m;
      }, {});

      Array.prototype.slice.call(element.options).forEach(function (option) {
        option.selected = !!values[String(option.value)];
      });
      return;
    }

    if (Object.prototype.hasOwnProperty.call(saved, "value") && "value" in element) {
      element.value = saved.value;
    }
  }

  function isDetailsElement(element) {
    return !!element && (element.tagName || "").toLowerCase() === "details";
  }

  function matchesSelector(element, selector) {
    if (!element || !selector || typeof element.matches !== "function") return false;

    try {
      return element.matches(selector);
    } catch (_selectorError) {
      return false;
    }
  }

  function detailsElements(scope, selector) {
    var selectorValue = selector || "details";
    var elements = queryAll(scope, selectorValue);

    if (isDetailsElement(scope) && matchesSelector(scope, selectorValue)) {
      elements.unshift(scope);
    }

    return elements.filter(isDetailsElement);
  }

  function detailsSingle(box) {
    return box &&
      (box.single === true ||
       box["single?"] === true ||
       box.singleOpen === true ||
       box["single-open"] === true);
  }

  function detailsElementForKey(target, elements, key) {
    var element = null;

    if (key && key.kind === "index") {
      element = elements[key.value];
    } else {
      element = findByKey(target, key);
    }

    return isDetailsElement(element) ? element : null;
  }

  var builtIns = {
    "raw-scroll": {
      capture: function (root, target, box) {
        var scroller = scrollContainerFor(target, root, box);
        return rawElementScrollState(scroller, root, box);
      },

      restore: function (root, _target, _box, state) {
        restoreRawScroll(root, state);
      }
    },

    "anchor-scroll": {
      capture: function (root, target, box) {
        var selector = box.selector || box.anchorSelector || box["anchor-selector"];
        var candidates = selector ? queryAll(target, selector) : [];
        var initialScroller = null;

        if (candidates.length) {
          initialScroller = scrollContainerFor(candidates[0], root, box);
        } else {
          initialScroller = scrollContainerFor(target, root, box);
        }

        var raw = rawElementScrollState(initialScroller, root, box);

        if (!selector || !candidates.length) {
          return {
            rawOnly: true,
            raw: raw,
            reason: selector ? "no-candidates" : "no-selector"
          };
        }

        var anchor = chooseAnchor(candidates, initialScroller);
        if (!anchor) {
          return {
            rawOnly: true,
            raw: raw,
            reason: "no-anchor"
          };
        }

        var scroller = scrollContainerFor(anchor, root, box);
        var key = keyForElement(anchor, box);
        var rawForScroller = rawElementScrollState(scroller, root, box);

        if (!key) {
          return {
            rawOnly: true,
            raw: rawForScroller,
            reason: "no-anchor-key"
          };
        }

        return {
          key: key,
          top: anchor.getBoundingClientRect().top,
          scroller: scrollerKey(scroller, root, box),
          raw: rawForScroller
        };
      },

      restore: function (root, target, box, state) {
        if (!state) return;

        if (!state.rawOnly && state.key) {
          var anchor = findByKey(target, state.key);
          if (anchor) {
            var beforeTop = state.top;
            var afterTop = anchor.getBoundingClientRect().top;
            var delta = afterTop - beforeTop;

            if (delta) {
              var scroller = state.scroller ? findByKey(root, state.scroller) : scrollContainerFor(anchor, root, box);
              scrollBy(scroller, delta);
            }

            return;
          }
        }

        // Fallback: if the anchor disappeared, cannot be keyed, or cannot be
        // found after an OOB swap, restore the raw scroll position captured at
        // the exact pre-swap moment.
        restoreRawScroll(root, state.raw);
      }
    },

    "focus": {
      capture: function (root, _target, box) {
        var active = document.activeElement;
        if (!active || !contains(root, active)) return null;

        var selector = box.selector;
        if (selector && !active.matches(selector)) return null;

        var tag = (active.tagName || "").toLowerCase();
        var type = (active.type || "").toLowerCase();
        var editable =
          tag === "input" ||
          tag === "textarea" ||
          tag === "select" ||
          active.isContentEditable;

        var allowNonEditable =
          box.allowNonEditable === true ||
          box["allow-non-editable"] === true ||
          box.includeButtons === true ||
          box["include-buttons"] === true;

        // Restoring focus to a clicked button/link inside a collaborative list can
        // itself cause the browser to reveal the changed card. Preserve editable
        // focus by default; allow button/link focus only by explicit opt-in.
        if (!editable && !allowNonEditable) return null;

        // Most non-text input types do not have meaningful caret state. They are
        // still editable controls, so focus can be restored without treating them
        // like buttons.
        if (tag === "input" && (type === "button" || type === "submit" || type === "reset")) {
          if (!allowNonEditable) return null;
        }

        var key = keyForElement(active, box);
        if (!key) return null;

        var state = {
          key: key,
          rawWindowScroll: rawWindowScrollState()
        };

        if (typeof active.selectionStart === "number" && typeof active.selectionEnd === "number") {
          state.selectionStart = active.selectionStart;
          state.selectionEnd = active.selectionEnd;
          state.selectionDirection = active.selectionDirection;
        }

        return state;
      },

      restore: function (root, target, _box, state) {
        if (!state || !state.key) return;

        var element = findByKey(target, state.key);
        if (!element || typeof element.focus !== "function") return;

        try {
          element.focus({ preventScroll: true });
        } catch (_focusOptionsError) {
          element.focus();
          restoreRawScroll(root, state.rawWindowScroll);
        }

        if (typeof element.setSelectionRange === "function" && typeof state.selectionStart === "number") {
          try {
            element.setSelectionRange(
              state.selectionStart,
              state.selectionEnd,
              state.selectionDirection || "none"
            );
          } catch (_selectionError) {
            // Some input types do not support text selection ranges.
          }
        }

        // Some browsers still scroll focused controls into view even with
        // preventScroll. Undo that on the next frame.
        if (state.rawWindowScroll) {
          requestAnimationFrame(function () {
            restoreRawScroll(root, state.rawWindowScroll);
          });
        }
      }
    },

    "inputs": {
      capture: function (_root, target, box) {
        var selector = box.selector || "input, textarea, select";
        var elements = queryAll(target, selector);

        return elements.map(function (element, index) {
          var key = keyForElement(element, box) || { kind: "index", value: index };
          return {
            key: key,
            value: inputElementValue(element)
          };
        }).filter(function (item) {
          return item.value != null;
        });
      },

      restore: function (_root, target, box, state) {
        if (!Array.isArray(state)) return;

        var selector = box.selector || "input, textarea, select";
        var elements = queryAll(target, selector);

        state.forEach(function (item) {
          var element = null;

          if (item.key && item.key.kind === "index") {
            element = elements[item.key.value];
          } else {
            element = findByKey(target, item.key);
          }

          restoreInputElementValue(element, item.value);
        });
      }
    },

    "details-open": {
      capture: function (_root, target, box) {
        var selector = box.selector || "details";
        var elements = detailsElements(target, selector);

        return {
          single: detailsSingle(box),
          items: elements.map(function (element, index) {
            return {
              key: keyForElement(element, box) || { kind: "index", value: index },
              open: !!element.open
            };
          })
        };
      },

      restore: function (_root, target, box, state) {
        if (!state || !Array.isArray(state.items)) return;

        var selector = box.selector || "details";
        var elements = detailsElements(target, selector);
        var single = state.single === true || detailsSingle(box);

        if (single) {
          elements.forEach(function (element) {
            element.open = false;
          });

          for (var i = 0; i < state.items.length; i += 1) {
            var item = state.items[i];
            if (!item || item.open !== true) continue;

            var selected = detailsElementForKey(target, elements, item.key);
            if (selected) {
              selected.open = true;
              break;
            }
          }

          return;
        }

        state.items.forEach(function (item) {
          if (!item) return;

          var element = detailsElementForKey(target, elements, item.key);
          if (element) element.open = !!item.open;
        });
      }
    },

    "js": {
      capture: function (root, target, box) {
        var fn = resolvePath(box.capture);
        if (typeof fn !== "function") return null;
        return fn(root, target, box);
      },

      restore: function (root, target, box, state) {
        var fn = resolvePath(box.restore);
        if (typeof fn === "function") fn(root, target, box, state);
      }
    },

    "event": {
      capture: function (root, target, box) {
        var name = box.name || box.event || "custom";
        var detail = emit(root, "capture-box:" + name, {
          root: root,
          target: target,
          box: box,
          state: null
        });
        return detail ? detail.state : null;
      },

      restore: function (root, target, box, state) {
        var name = box.name || box.event || "custom";
        emit(root, "restore-box:" + name, {
          root: root,
          target: target,
          box: box,
          state: state
        });
      }
    },

    "hyperscript": {
      capture: function (root, target, box) {
        // Prefer event-backed Hyperscript custom boxes. A component can install
        // handlers like:
        //   on gesso:live-continuity:capture-box:selected-row
        //     set event.detail.state to ...
        var eventBox = Object.assign({}, box, { type: "event" });
        return builtIns.event.capture(root, target, eventBox);
      },

      restore: function (root, target, box, state) {
        var eventBox = Object.assign({}, box, { type: "event" });
        builtIns.event.restore(root, target, eventBox, state);
      }
    }
  };

  function resolvePath(path) {
    if (!path || typeof path !== "string") return null;

    return path.split(".").reduce(function (value, part) {
      return value && value[part];
    }, window);
  }

  function normalizeBox(box) {
    if (typeof box === "string") return { type: box };
    if (!box || typeof box !== "object") return null;

    var normalized = Object.assign({}, box);
    if (!normalized.type && normalized.name && builtIns[normalized.name]) {
      normalized.type = normalized.name;
    }
    if (!normalized.type) normalized.type = "event";

    return normalized;
  }

  function boxesFromConfig(config) {
    var boxes = [];
    var preserve = config.preserve || {};

    asArray(config.boxes).forEach(function (box) {
      var normalized = normalizeBox(box);
      if (normalized) boxes.push(normalized);
    });

    if (preserve.scroll) {
      if (preserve.scroll === true) {
        boxes.push({ type: "raw-scroll" });
      } else {
        var scroll = preserve.scroll || {};
        var mode = scroll.mode || scroll.type;
        if (mode === "raw" || mode === "position" || mode === "scroll" || mode === "raw-scroll") {
          boxes.push(Object.assign({ type: "raw-scroll" }, scroll));
        } else {
          boxes.push(Object.assign({ type: "anchor-scroll" }, scroll));
        }
      }
    }

    if (preserve.focus) {
      var focus = preserve.focus === true ? {} : preserve.focus;
      boxes.push(Object.assign({ type: "focus" }, focus));
    }

    if (preserve.inputs) {
      var inputs = preserve.inputs === true ? {} : preserve.inputs;
      boxes.push(Object.assign({ type: "inputs" }, inputs));
    }

    return boxes;
  }

  function captureBox(root, target, box) {
    var type = box.type || "event";
    var impl = builtIns[type];

    if (!impl || typeof impl.capture !== "function") {
      emit(root, "error", { phase: "capture", reason: "unknown-box-type", box: box });
      return null;
    }

    try {
      return {
        type: type,
        name: box.name || type,
        box: box,
        state: impl.capture(root, target, box)
      };
    } catch (error) {
      emit(root, "error", { phase: "capture", box: box, error: error });
      return null;
    }
  }

  function restoreBox(root, target, captured) {
    if (!captured) return;

    var impl = builtIns[captured.type];
    if (!impl || typeof impl.restore !== "function") return;

    try {
      impl.restore(root, target, captured.box, captured.state);
    } catch (error) {
      emit(root, "error", { phase: "restore", box: captured.box, error: error });
    }
  }

  function restoreBoxesOfType(context, types) {
    if (!context || !context.target) return;

    var slot = context.slot || context.root[STATE_SLOT];
    if (!slot || !Array.isArray(slot.captured)) return;

    var wanted = {};
    types.forEach(function (type) {
      wanted[type] = true;
    });

    slot.captured.forEach(function (captured) {
      if (captured && wanted[captured.type]) {
        restoreBox(context.root, context.target, captured);
      }
    });
  }

  function optimisticEventRoot(source, target) {
    return closestContinuityRoot(source) ||
      closestContinuityRoot(target) ||
      target ||
      source ||
      document.documentElement;
  }

  function requestElementFromEvent(event) {
    var detail = event && event.detail;

    if (detail && detail.requestConfig && isElement(detail.requestConfig.elt)) {
      return detail.requestConfig.elt;
    }

    if (detail && isElement(detail.elt)) return detail.elt;
    if (isElement(event && event.target)) return event.target;

    return null;
  }

  function closestWithAnyOptimisticAttr(element) {
    if (!element) return null;

    var selector =
      "[" + OPTIMISTIC_TEMPLATE_ATTR + "]," +
      "[" + OPTIMISTIC_ACTION_ATTR + "]," +
      "[" + OPTIMISTIC_TARGET_ATTR + "]";

    if (element.matches && element.matches(selector)) return element;
    if (element.closest) return element.closest(selector);

    return null;
  }

  function optimisticSourceFromEvent(event) {
    return closestWithAnyOptimisticAttr(requestElementFromEvent(event));
  }

  function optimisticTemplateName(source) {
    if (!source) return null;

    return source.getAttribute(OPTIMISTIC_TEMPLATE_ATTR) ||
      source.getAttribute(OPTIMISTIC_ACTION_ATTR);
  }

  function selectorAfterPrefix(value, prefix) {
    if (!value || value.indexOf(prefix) !== 0) return null;
    var selector = value.slice(prefix.length);
    return selector.replace(/^\s+|\s+$/g, "");
  }

  function resolveOptimisticTarget(source) {
    if (!source) return null;

    var spec = source.getAttribute(OPTIMISTIC_TARGET_ATTR);
    if (!spec || spec === "this") return source;

    var closestSelector = selectorAfterPrefix(spec, "closest ");
    if (closestSelector && source.closest) {
      return source.closest(closestSelector);
    }

    var root = closestContinuityRoot(source) || document;
    return query(root, spec) || query(document, spec);
  }

  function templateNameMatches(template, name) {
    if (!template) return false;

    if (name) {
      return template.getAttribute(OPTIMISTIC_TEMPLATE_ATTR) === name ||
        template.getAttribute(OPTIMISTIC_ACTION_ATTR) === name;
    }

    return template.hasAttribute(OPTIMISTIC_TEMPLATE_ATTR) ||
      template.hasAttribute(OPTIMISTIC_ACTION_ATTR);
  }

  function findOptimisticTemplateIn(scope, name) {
    if (!scope) return null;

    if ((scope.tagName || "").toLowerCase() === "template" && templateNameMatches(scope, name)) {
      return scope;
    }

    var templates = queryAll(
      scope,
      "template[" + OPTIMISTIC_TEMPLATE_ATTR + "],template[" + OPTIMISTIC_ACTION_ATTR + "]"
    );

    for (var i = 0; i < templates.length; i += 1) {
      if (templateNameMatches(templates[i], name)) return templates[i];
    }

    return null;
  }

  function findOptimisticTemplate(source, target, name) {
    var template = findOptimisticTemplateIn(target, name);
    if (template) return template;

    if (source && source.parentElement) {
      template = findOptimisticTemplateIn(source.parentElement, name);
      if (template) return template;
    }

    var root = closestContinuityRoot(source) || closestContinuityRoot(target);
    template = findOptimisticTemplateIn(root, name);
    if (template) return template;

    return findOptimisticTemplateIn(document, name);
  }

  function firstTemplateElement(template) {
    if (!template) return null;

    if (template.content && template.content.firstElementChild) {
      return template.content.firstElementChild;
    }

    var wrapper = document.createElement("div");
    wrapper.innerHTML = template.innerHTML || "";
    return wrapper.firstElementChild;
  }

  function snapshotAttributes(element) {
    return Array.prototype.slice.call(element.attributes || []).map(function (attr) {
      return { name: attr.name, value: attr.value };
    });
  }

  function elementSnapshot(element) {
    if (!element) return null;

    return {
      tagName: element.tagName,
      attributes: snapshotAttributes(element),
      innerHTML: element.innerHTML,
      open: isDetailsElement(element) ? !!element.open : null
    };
  }

  function removeAllAttributes(element) {
    Array.prototype.slice.call(element.attributes || []).forEach(function (attr) {
      element.removeAttribute(attr.name);
    });
  }

  function restoreAttributes(element, attributes) {
    removeAllAttributes(element);

    (attributes || []).forEach(function (attr) {
      element.setAttribute(attr.name, attr.value);
    });
  }

  function restoreElementSnapshot(element, snapshot) {
    if (!element || !snapshot) return false;

    restoreAttributes(element, snapshot.attributes);
    element.innerHTML = snapshot.innerHTML || "";

    if (snapshot.open != null && isDetailsElement(element)) {
      element.open = !!snapshot.open;
    }

    return true;
  }

  function copyElementIntoExistingTarget(target, sourceElement, slotId) {
    if (!target || !sourceElement) return false;

    var originalId = target.id;
    var sourceHasId = !!sourceElement.id;

    restoreAttributes(target, snapshotAttributes(sourceElement));

    // Preserve the HTMX target id if the optimistic template forgot to include
    // it. HTMX already has the target element reference, but keeping the id
    // stable helps continuity/OOB lookup during the request lifecycle.
    if (originalId && !sourceHasId) {
      target.id = originalId;
    }

    target.innerHTML = sourceElement.innerHTML;
    target.setAttribute(OPTIMISTIC_ACTIVE_ATTR, slotId);
    target.setAttribute(OPTIMISTIC_PENDING_ATTR, "true");

    if (isDetailsElement(target) && isDetailsElement(sourceElement)) {
      target.open = !!sourceElement.open;
    }

    return true;
  }

  function nextOptimisticSlotId() {
    optimisticSeq += 1;
    return "gesso-optimistic-" + optimisticSeq + "-" + now();
  }

  function storeOptimisticSlot(source, target, snapshot, templateName) {
    var slotId = nextOptimisticSlotId();
    var root = optimisticEventRoot(source, target);

    var slot = {
      id: slotId,
      root: root,
      source: source,
      target: target,
      templateName: templateName,
      snapshot: snapshot,
      startedAt: now()
    };

    optimisticSlotsById[slotId] = slot;

    try {
      source.__gessoOptimisticSlotId = slotId;
    } catch (_sourceSlotError) {
      // Expando bookkeeping is best-effort only.
    }

    return slot;
  }

  function optimisticSlotById(slotId) {
    return slotId ? optimisticSlotsById[slotId] : null;
  }

  function optimisticSlotFromElement(element) {
    if (!element) return null;

    if (element.__gessoOptimisticSlotId) {
      var direct = optimisticSlotById(element.__gessoOptimisticSlotId);
      if (direct) return direct;
    }

    var active = null;

    if (element.getAttribute && element.getAttribute(OPTIMISTIC_ACTIVE_ATTR)) {
      active = element.getAttribute(OPTIMISTIC_ACTIVE_ATTR);
    }

    if (!active && element.closest) {
      var activeElement = element.closest("[" + OPTIMISTIC_ACTIVE_ATTR + "]");
      if (activeElement) active = activeElement.getAttribute(OPTIMISTIC_ACTIVE_ATTR);
    }

    return optimisticSlotById(active);
  }

  function optimisticSlotFromEvent(event) {
    var source = requestElementFromEvent(event);
    var slot = optimisticSlotFromElement(source);
    if (slot) return slot;

    var elements = eventElements(event);
    for (var i = 0; i < elements.length; i += 1) {
      slot = optimisticSlotFromElement(elements[i]);
      if (slot) return slot;
    }

    return null;
  }

  function clearOptimisticSlot(slot, reason) {
    if (!slot) return false;

    delete optimisticSlotsById[slot.id];

    if (slot.source && slot.source.__gessoOptimisticSlotId === slot.id) {
      try {
        delete slot.source.__gessoOptimisticSlotId;
      } catch (_deleteError) {
        slot.source.__gessoOptimisticSlotId = null;
      }
    }

    if (slot.target && isConnected(slot.target) &&
        slot.target.getAttribute &&
        slot.target.getAttribute(OPTIMISTIC_ACTIVE_ATTR) === slot.id) {
      slot.target.removeAttribute(OPTIMISTIC_ACTIVE_ATTR);
      slot.target.removeAttribute(OPTIMISTIC_PENDING_ATTR);
    }

    emit(slot.root, "optimistic-cleared", {
      reason: reason || "clear",
      slotId: slot.id,
      template: slot.templateName,
      target: slot.target
    });

    return true;
  }

  function restoreOptimisticSlot(slot, reason) {
    if (!slot) return false;

    var restored = false;
    if (slot.target && isConnected(slot.target)) {
      restored = restoreElementSnapshot(slot.target, slot.snapshot);
    }

    clearOptimisticSlot(slot, reason || "restore");

    emit(slot.root, "optimistic-restored", {
      reason: reason || "restore",
      slotId: slot.id,
      template: slot.templateName,
      target: slot.target,
      restored: restored
    });

    return restored;
  }

  function emitOptimisticError(root, detail) {
    emit(root || document.documentElement, "optimistic-error", detail);

    try {
      document.dispatchEvent(new CustomEvent(OPTIMISTIC_ERROR_EVENT, {
        bubbles: true,
        cancelable: false,
        detail: detail || {}
      }));
    } catch (_customEventError) {
      // Custom diagnostics must never break the request.
    }
  }

  function optimisticSourceFromDomEvent(event) {
    var target = event && event.target;
    if (!isElement(target)) return null;
    return closestWithAnyOptimisticAttr(target);
  }

  function optimisticTriggerFromEvent(event, source) {
    if (event && isElement(event.submitter)) return event.submitter;

    var target = event && event.target;
    if (!isElement(target)) return null;

    var triggerSelector = "button,input[type='submit'],input[type='button']";
    var trigger = null;

    if (target.matches && target.matches(triggerSelector)) {
      trigger = target;
    } else if (target.closest) {
      trigger = target.closest(triggerSelector);
    }

    if (trigger && source && source.contains && source.contains(trigger)) {
      return trigger;
    }

    return null;
  }

  function triggerTextSnapshot(trigger) {
    if (!trigger) return null;

    if ((trigger.tagName || "").toLowerCase() === "input") {
      return { kind: "value", value: trigger.value };
    }

    return { kind: "text", value: trigger.textContent };
  }

  function restoreTriggerText(trigger, snapshot) {
    if (!trigger || !snapshot) return;

    if (snapshot.kind === "value") {
      trigger.value = snapshot.value;
    } else {
      trigger.textContent = snapshot.value;
    }
  }

  function setTriggerText(trigger, text) {
    if (!trigger || !text) return;

    if ((trigger.tagName || "").toLowerCase() === "input") {
      trigger.value = text;
    } else {
      trigger.textContent = text;
    }
  }

  function clearOptimisticPressed(source) {
    if (!source) return false;

    var state = source.__gessoOptimisticPressState || null;
    var target = state && state.target;
    var trigger = state && state.trigger;

    source.removeAttribute(OPTIMISTIC_PRESSED_ATTR);

    if (target && isConnected(target) && target.getAttribute) {
      target.removeAttribute(OPTIMISTIC_PRESSED_ATTR);
    }

    if (trigger && isConnected(trigger) && trigger.getAttribute) {
      trigger.removeAttribute(OPTIMISTIC_TRIGGER_ATTR);
      restoreTriggerText(trigger, state.triggerText);
    }

    try {
      delete source.__gessoOptimisticPressState;
    } catch (_deletePressError) {
      source.__gessoOptimisticPressState = null;
    }

    return true;
  }

  function markOptimisticPressed(source, trigger) {
    if (!source) return null;

    var existing = source.__gessoOptimisticPressState || null;
    if (existing) return existing;

    var target = resolveOptimisticTarget(source);
    var label = source.getAttribute(OPTIMISTIC_LABEL_ATTR);

    var state = {
      source: source,
      target: target,
      trigger: trigger || null,
      triggerText: triggerTextSnapshot(trigger),
      startedAt: now()
    };

    try {
      source.__gessoOptimisticPressState = state;
    } catch (_pressStateError) {
      // Expando bookkeeping is best-effort only.
    }

    source.setAttribute(OPTIMISTIC_PRESSED_ATTR, "true");

    if (target && target.setAttribute) {
      target.setAttribute(OPTIMISTIC_PRESSED_ATTR, "true");
    }

    if (trigger && trigger.setAttribute) {
      trigger.setAttribute(OPTIMISTIC_TRIGGER_ATTR, "true");
      setTriggerText(trigger, label);
    }

    return state;
  }

  function onOptimisticPress(event) {
    var source = optimisticSourceFromDomEvent(event);
    if (!source) return;

    markOptimisticPressed(source, optimisticTriggerFromEvent(event, source));
  }

  function onOptimisticSubmitCapture(event) {
    var source = optimisticSourceFromDomEvent(event);
    if (!source) return;

    markOptimisticPressed(source, optimisticTriggerFromEvent(event, source));
  }

  function applyOptimisticFromEvent(event) {
    var source = optimisticSourceFromEvent(event);
    if (!source) return null;

    var target = resolveOptimisticTarget(source);
    var templateName = optimisticTemplateName(source);
    var root = optimisticEventRoot(source, target);

    if (!target) {
      emitOptimisticError(root, {
        phase: "apply",
        reason: "no-target",
        source: source,
        template: templateName
      });
      return null;
    }

    if (optimisticSlotFromElement(target)) {
      return null;
    }

    var snapshot = elementSnapshot(target);
    var slot = storeOptimisticSlot(source, target, snapshot, templateName);

    var template = findOptimisticTemplate(source, target, templateName);
    var optimisticElement = firstTemplateElement(template);

    if (optimisticElement) {
      if ((target.tagName || "").toLowerCase() !== (optimisticElement.tagName || "").toLowerCase()) {
        emitOptimisticError(root, {
          phase: "apply",
          reason: "template-root-tag-mismatch",
          source: source,
          target: target,
          template: templateName,
          targetTag: target.tagName,
          templateTag: optimisticElement.tagName
        });
        clearOptimisticSlot(slot, "template-root-tag-mismatch");
        return null;
      }

      copyElementIntoExistingTarget(target, optimisticElement, slot.id);
    } else {
      // Template-less optimistic actions are still useful for CSS-only pending
      // affordances. The server response remains authoritative.
      target.setAttribute(OPTIMISTIC_ACTIVE_ATTR, slot.id);
      target.setAttribute(OPTIMISTIC_PENDING_ATTR, "true");
    }

    emit(root, "optimistic-applied", {
      slotId: slot.id,
      source: source,
      target: target,
      template: templateName,
      hasTemplate: !!optimisticElement
    });

    return slot;
  }

  function onOptimisticAfterRequest(event) {
    var source = optimisticSourceFromEvent(event);
    if (source) clearOptimisticPressed(source);

    var slot = optimisticSlotFromEvent(event);
    if (!slot) return;

    var detail = event && event.detail;
    if (detail && (detail.failed === true || detail.successful === false)) {
      restoreOptimisticSlot(slot, "request-failed");
    } else {
      // Successful HTMX responses are authoritative. If they swapped the target,
      // the old optimistic element may already be detached; if not, the app has
      // deliberately kept the optimistic/pending DOM.
      clearOptimisticSlot(slot, "request-success");
    }
  }

  function onOptimisticRequestFailed(event) {
    var source = optimisticSourceFromEvent(event);
    if (source) clearOptimisticPressed(source);

    var slot = optimisticSlotFromEvent(event);
    if (slot) restoreOptimisticSlot(slot, "request-error");
  }

  function currentWindowScrollY() {
    return window.pageYOffset || document.documentElement.scrollTop || document.body.scrollTop || 0;
  }

  function numericCssPx(value) {
    if (!value) return 0;
    var parsed = parseFloat(value);
    return isNaN(parsed) ? 0 : parsed;
  }

  function lockContinuityHeight(root, target) {
    if (!root || !target || !isConnected(root)) {
      return null;
    }

    var rootRect = null;
    var targetRect = null;

    try {
      rootRect = root.getBoundingClientRect();
      targetRect = target.getBoundingClientRect();
    } catch (_measureError) {
      return null;
    }

    var height = Math.max(
      rootRect && rootRect.height ? rootRect.height : 0,
      targetRect && targetRect.height ? targetRect.height : 0
    );

    if (!height || height < 1) {
      return null;
    }

    var previousMinHeight = root.style.minHeight || "";
    var previousOverflowAnchor = root.style.overflowAnchor || "";
    var previousDataLock = root.getAttribute("data-gesso-live-continuity-height-locked");

    var computed = window.getComputedStyle ? window.getComputedStyle(root) : null;
    var existingMinHeight = computed ? numericCssPx(computed.minHeight) : numericCssPx(previousMinHeight);
    var lockedHeight = Math.ceil(Math.max(height, existingMinHeight));

    root.style.minHeight = lockedHeight + "px";

    // Browser scroll anchoring can fight explicit continuity restoration while a
    // large subtree is being replaced. Disable it only on the stable root and
    // restore the previous inline value afterward.
    root.style.overflowAnchor = "none";
    root.setAttribute("data-gesso-live-continuity-height-locked", "true");

    var released = false;

    return {
      height: lockedHeight,
      release: function () {
        if (released) return;
        released = true;

        root.style.minHeight = previousMinHeight;
        root.style.overflowAnchor = previousOverflowAnchor;

        if (previousDataLock == null) {
          root.removeAttribute("data-gesso-live-continuity-height-locked");
        } else {
          root.setAttribute("data-gesso-live-continuity-height-locked", previousDataLock);
        }
      }
    };
  }

  function releaseHeightLock(root, slot, reason) {
    if (!slot || !slot.heightLock || typeof slot.heightLock.release !== "function") {
      return false;
    }

    try {
      slot.heightLock.release();
      emit(root || slot.root, "height-unlocked", {
        source: slot.source,
        reason: reason || "release",
        root: root || slot.root,
        targetId: slot.targetId,
        height: slot.heightLock.height
      });
      return true;
    } catch (error) {
      emit(root || slot.root, "error", {
        phase: "height-unlock",
        reason: reason || "release",
        error: error
      });
      return false;
    } finally {
      slot.heightLock = null;
    }
  }

  function storeSlot(root, slot) {
    var previous = root && root[STATE_SLOT];
    if (previous && previous !== slot) {
      releaseHeightLock(root, previous, "replace-slot");
    }

    root[STATE_SLOT] = slot;

    if (slot.targetId) {
      activeSlotsByTargetId[slot.targetId] = slot;
    }
  }

  function clearSlot(root, targetId) {
    if (root) delete root[STATE_SLOT];
    if (targetId && activeSlotsByTargetId[targetId]) {
      delete activeSlotsByTargetId[targetId];
    }
  }

  function captureContextForRoot(root, event, source) {
    if (!root) return null;

    var target = findContinuityTarget(root, event);
    if (!target) {
      var targetId = continuityTargetId(root);
      if (targetId) target = findById(root, targetId) || findById(document, targetId);
    }

    if (!target) {
      emit(root, "missed", {
        phase: "capture",
        source: source || "request",
        reason: "no-matching-target",
        root: root,
        targetId: continuityTargetId(root)
      });
      return null;
    }

    var config = parseConfig(root);
    if (!configEnabled(config)) return null;

    return {
      source: source || "request",
      root: root,
      target: target,
      targetId: target.id || continuityTargetId(root),
      config: config
    };
  }

  function captureContext(event, source) {
    if (source !== "oob" && event && event.detail && event.detail.shouldSwap === false) return null;

    var root = rootFromEvent(event);
    if (!root) return null;

    var target = findContinuityTarget(root, event);
    if (!target) {
      emit(root, "missed", {
        phase: "capture",
        source: source || "swap",
        reason: "no-matching-target",
        root: root,
        targetId: continuityTargetId(root)
      });
      return null;
    }

    var config = parseConfig(root);
    if (!configEnabled(config)) return null;

    return {
      source: source || "swap",
      root: root,
      target: target,
      targetId: target.id || continuityTargetId(root),
      config: config
    };
  }

  function restoreContext(event, source) {
    var eventTargetId = targetIdFromEvent(event);
    var root = rootFromEvent(event);

    var slot = null;
    if (root && root[STATE_SLOT]) {
      slot = root[STATE_SLOT];
    }

    if (!slot && eventTargetId && activeSlotsByTargetId[eventTargetId]) {
      slot = activeSlotsByTargetId[eventTargetId];
    }

    if (!slot && root) {
      var rootTarget = continuityTargetId(root);
      if (rootTarget && activeSlotsByTargetId[rootTarget]) {
        slot = activeSlotsByTargetId[rootTarget];
      }
    }

    if (!slot) return null;

    if (!root) {
      root = slot.root || rootForTargetId(slot.targetId);
    }

    if (!root) return null;

    var targetId = slot.targetId || eventTargetId || continuityTargetId(root);
    var newTarget = findContinuityTarget(root, event, targetId);

    var config = parseConfig(root) || slot.config;
    if (!configEnabled(config)) return null;

    return {
      source: source || "swap",
      root: root,
      target: newTarget,
      targetId: targetId,
      config: config,
      slot: slot
    };
  }

  function capture(context) {
    var boxes = boxesFromConfig(context.config);
    if (!boxes.length) return;

    var captured = boxes.map(function (box) {
      return captureBox(context.root, context.target, box);
    }).filter(Boolean);

    var heightLock = lockContinuityHeight(context.root, context.target);

    storeSlot(context.root, {
      source: context.source,
      root: context.root,
      targetId: context.targetId,
      capturedAt: now(),
      config: context.config,
      captured: captured,
      fallbackScroll: rawWindowScrollState(),
      heightLock: heightLock
    });

    emit(context.root, "captured", {
      source: context.source,
      root: context.root,
      target: context.target,
      targetId: context.targetId,
      count: captured.length,
      heightLocked: !!heightLock,
      lockedHeight: heightLock && heightLock.height
    });
  }

  function installPostRestoreScrollShield(root, slot, reason) {
    if (!root || !slot || !slot.fallbackScroll || slot.fallbackScroll.kind !== "window") {
      return;
    }

    var startedAt = now();
    var desired = rawWindowScrollState();
    var framesLeft = 10;
    var tolerance = 2;

    function tick() {
      if (lastUserScrollIntentAt > slot.capturedAt) {
        emit(root, "scroll-shield-cancelled", {
          source: slot.source,
          reason: "user-scroll-intent",
          root: root,
          targetId: slot.targetId
        });
        return;
      }

      var current = rawWindowScrollState();
      var dx = Math.abs((current.x || 0) - (desired.x || 0));
      var dy = Math.abs((current.y || 0) - (desired.y || 0));

      if (dx > tolerance || dy > tolerance) {
        restoreRawScroll(root, desired);
        emit(root, "scroll-shield-restored", {
          source: slot.source,
          reason: reason || "late-scroll",
          root: root,
          targetId: slot.targetId,
          from: current,
          to: desired,
          ageMs: now() - startedAt
        });
      }

      framesLeft -= 1;
      if (framesLeft > 0) {
        requestAnimationFrame(tick);
      }
    }

    requestAnimationFrame(tick);
  }

  function restore(context) {
    var slot = context.slot || context.root[STATE_SLOT];
    if (!slot) return;

    if (slot.targetId && context.targetId && slot.targetId !== context.targetId) return;

    if (context.target) {
      slot.captured.forEach(function (captured) {
        restoreBox(context.root, context.target, captured);
      });
    } else {
      restoreRawScroll(context.root, slot.fallbackScroll);
      emit(context.root, "missed", {
        phase: "restore",
        source: context.source,
        reason: "no-matching-target-fallback-scroll",
        root: context.root,
        targetId: context.targetId
      });
    }

    // Do a second raw fallback pass on the next frame only if the browser/HTMX
    // clamped to the very top while we had previously captured a non-top scroll.
    // This avoids fighting successful anchor restoration, but catches the bad
    // "jump to top after OOB outerHTML" failure mode.
    var fallback = slot.fallbackScroll;
    if (fallback && fallback.kind === "window" && fallback.y > 0) {
      requestAnimationFrame(function () {
        if (currentWindowScrollY() === 0) {
          restoreRawScroll(context.root, fallback);
          emit(context.root, "fallback-restored", {
            source: context.source,
            root: context.root,
            targetId: context.targetId,
            y: fallback.y
          });
        }
      });
    }

    // Keep the min-height lock through one more frame so the browser paints the
    // restored scroll position before the stable root is allowed to shrink.
    installPostRestoreScrollShield(context.root, slot, "post-restore");

    if (slot.heightLock) {
      requestAnimationFrame(function () {
        releaseHeightLock(context.root, slot, "after-restore");
      });
    }

    clearSlot(context.root, slot.targetId);

    emit(context.root, "restored", {
      source: context.source,
      root: context.root,
      target: context.target,
      targetId: context.targetId,
      count: slot.captured.length,
      heightLocked: !!slot.heightLock
    });
  }

  function afterLayoutSettles(fn) {
    requestAnimationFrame(function () {
      requestAnimationFrame(function () {
        fn();
      });
    });
  }

  function clearOnBeforeCleanup(event) {
    var target = eventTarget(event);
    if (!target) return;

    queryAll(target, ROOT_SELECTOR).forEach(function (root) {
      var targetId = continuityTargetId(root);
      releaseHeightLock(root, root[STATE_SLOT], "cleanup-child-root");
      clearSlot(root, targetId);
    });

    if (target.matches && target.matches(ROOT_SELECTOR)) {
      releaseHeightLock(target, target[STATE_SLOT], "cleanup-root");
      clearSlot(target, continuityTargetId(target));
    }
  }

  function onBeforeSwap(event) {
    var context = captureContext(event, "swap");
    if (context) capture(context);
  }

  function onAfterSwap(event) {
    var context = restoreContext(event, "swap");
    if (!context) return;

    // Visual state should be restored as soon as the replacement DOM exists.
    // Scroll/focus restoration stays on afterSettle + layout frames below.
    restoreBoxesOfType(context, ["details-open"]);
  }

  function onAfterSettle(event) {
    var context = restoreContext(event, "swap");
    if (!context) return;

    afterLayoutSettles(function () {
      var refreshed = restoreContext(event, "swap") || context;
      restore(refreshed);
    });
  }

  function onOobBeforeSwap(event) {
    var context = captureContext(event, "oob");
    if (context) capture(context);
  }

  function onOobAfterSwap(event) {
    var context = restoreContext(event, "oob");
    if (!context) return;

    // OOB swaps can visibly collapse native <details> before the delayed full
    // restore path runs. Restore details immediately; the later full restore is
    // idempotent and still handles scroll/focus.
    restoreBoxesOfType(context, ["details-open"]);

    afterLayoutSettles(function () {
      var refreshed = restoreContext(event, "oob") || context;
      restore(refreshed);
    });
  }

  function onBeforeRequest(event) {
    var source = optimisticSourceFromEvent(event);

    if (source) {
      markOptimisticPressed(source, null);
      applyOptimisticFromEvent(event);
      return;
    }

    var elements = eventElements(event);
    var seen = [];

    for (var i = 0; i < elements.length; i += 1) {
      var root = closestContinuityRoot(elements[i]);
      if (!root || seen.indexOf(root) !== -1) continue;
      seen.push(root);

      var context = captureContextForRoot(root, event, "request");
      if (context) capture(context);
    }
  }

  function onSseBeforeMessage(event) {
    var context = captureContext(event, "sse-message");
    if (context) capture(context);
  }

  function onSseMessage(event) {
    var context = restoreContext(event, "sse-message");
    if (!context) return;

    // Direct SSE swaps should also get immediate visual restoration.
    restoreBoxesOfType(context, ["details-open"]);

    afterLayoutSettles(function () {
      var refreshed = restoreContext(event, "sse-message") || context;
      restore(refreshed);
    });
  }

  // Attach to document, not document.body, so this file is safe to load in <head>.
  document.addEventListener("pointerdown", onOptimisticPress, true);
  document.addEventListener("click", onOptimisticPress, true);
  document.addEventListener("submit", onOptimisticSubmitCapture, true);
  document.addEventListener("htmx:beforeRequest", onBeforeRequest);
  document.addEventListener("htmx:afterRequest", onOptimisticAfterRequest);
  document.addEventListener("htmx:responseError", onOptimisticRequestFailed);
  document.addEventListener("htmx:sendError", onOptimisticRequestFailed);
  document.addEventListener("htmx:timeout", onOptimisticRequestFailed);
  document.addEventListener("htmx:abort", onOptimisticRequestFailed);
  document.addEventListener("htmx:beforeSwap", onBeforeSwap);
  document.addEventListener("htmx:afterSwap", onAfterSwap);
  document.addEventListener("htmx:afterSettle", onAfterSettle);

  // Direct HTMX SSE-extension message swaps, for components that use sse-swap rather
  // than using sse:<event> as an hx-trigger for a normal request.
  document.addEventListener("htmx:sseBeforeMessage", onSseBeforeMessage);
  document.addEventListener("htmx:sseMessage", onSseMessage);

  document.addEventListener("htmx:oobBeforeSwap", onOobBeforeSwap);
  document.addEventListener("htmx:oobAfterSwap", onOobAfterSwap);
  document.addEventListener("htmx:beforeCleanupElement", clearOnBeforeCleanup);

  window.gessoLive = window.gessoLive || {};
  window.gessoLive.continuity = {
    version: "0.12.1",
    parseConfig: parseConfig,
    boxesFromConfig: boxesFromConfig,
    captureContext: captureContext,
    restoreContext: restoreContext,
    captureContextForRoot: captureContextForRoot,
    capture: capture,
    restore: restore,
    emit: emit,
    builtIns: builtIns,
    registerBox: function (type, impl) {
      if (!type || !impl) return;
      builtIns[type] = impl;
    }
  };

  window.gessoLive.optimistic = {
    version: "0.12.1",
    applyFromEvent: applyOptimisticFromEvent,
    restoreFromEvent: onOptimisticRequestFailed,
    slotFromEvent: optimisticSlotFromEvent,
    clearSlot: clearOptimisticSlot,
    restoreSlot: restoreOptimisticSlot
  };
}());

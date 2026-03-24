(() => {
  const root = document.documentElement;

  const metaContent = (name) =>
    document.querySelector(`meta[name="${name}"]`)?.getAttribute("content") || null;

  const readDefaultColorTheme = () =>
    root.getAttribute("data-color-theme") ||
    metaContent("gesso-color-theme-default");

  const readDefaultMode = () =>
    root.getAttribute("data-color-theme-mode") ||
    metaContent("gesso-theme-mode-default") ||
    "system";

  const systemDark = () =>
    window.matchMedia &&
    window.matchMedia("(prefers-color-scheme: dark)").matches;

  const applyColorTheme = (colorTheme) => {
    if (colorTheme) {
      root.setAttribute("data-color-theme", colorTheme);
    } else {
      root.removeAttribute("data-color-theme");
    }
  };

  const applyMode = (mode) => {
    if (!mode) return;

    const resolved =
      mode === "system"
        ? (systemDark() ? "dark" : "light")
        : mode;

    root.classList.toggle("dark", resolved === "dark");
  };

  const currentColorTheme = () =>
    readDefaultColorTheme();

  const currentMode = () =>
    readDefaultMode();

  const init = () => {
    applyColorTheme(currentColorTheme());
    applyMode(currentMode());
  };

  document.addEventListener("gesso:theme", (event) => {
    const detail = event?.detail || {};
    const nextColorTheme =
      typeof detail.colorTheme === "string"
        ? detail.colorTheme
        : typeof detail.theme === "string"
          ? detail.theme
          : currentColorTheme();

    let nextMode = detail.mode;
    if (!nextMode) {
      nextMode = currentMode();
    } else if (nextMode === "toggle") {
      nextMode = root.classList.contains("dark") ? "light" : "dark";
    }

    if (typeof nextColorTheme === "string") {
      applyColorTheme(nextColorTheme);
    }

    if (typeof nextMode === "string") {
      applyMode(nextMode);
    }
  });

  if (window.matchMedia) {
    const media = window.matchMedia("(prefers-color-scheme: dark)");
    const syncSystemMode = () => {
      if (readDefaultMode() === "system") {
        applyMode("system");
      }
    };

    if (media.addEventListener) {
      media.addEventListener("change", syncSystemMode);
    } else if (media.addListener) {
      media.addListener(syncSystemMode);
    }
  }

  init();
})();

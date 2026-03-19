(() => {
  const root = document.documentElement;
  const THEME_KEY = "themeName";
  const MODE_KEY = "themeMode";

  const metaContent = (name) =>
    document.querySelector(`meta[name="${name}"]`)?.getAttribute("content") || null;

const readDefaultTheme = () =>
    root.getAttribute("data-theme") || metaContent("gesso-theme-default");

  const readDefaultMode = () =>
    root.getAttribute("data-theme-mode") || metaContent("gesso-theme-mode-default") || "system";
  const readStorage = (key) => {
    try {
      return localStorage.getItem(key);
    } catch (_e) {
      return null;
    }
  };

  const writeStorage = (key, value) => {
    try {
      if (value == null) {
        localStorage.removeItem(key);
      } else {
        localStorage.setItem(key, value);
      }
    } catch (_e) {}
  };

  const systemDark = () =>
    window.matchMedia &&
    window.matchMedia("(prefers-color-scheme: dark)").matches;

  const applyTheme = (theme) => {
    if (theme) {
      root.setAttribute("data-theme", theme);
    } else {
      root.removeAttribute("data-theme");
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

  const currentTheme = () =>
    readStorage(THEME_KEY) || readDefaultTheme();

  const currentMode = () =>
    readStorage(MODE_KEY) || readDefaultMode();

  const init = () => {
    applyTheme(currentTheme());
    applyMode(currentMode());
  };

  document.addEventListener("gesso:theme", (event) => {
    const detail = event?.detail || {};
    const nextTheme =
      typeof detail.theme === "string" ? detail.theme : currentTheme();

    let nextMode = detail.mode;
    if (!nextMode) {
      nextMode = currentMode();
    } else if (nextMode === "toggle") {
      nextMode = root.classList.contains("dark") ? "light" : "dark";
    }

    if (typeof nextTheme === "string") {
      applyTheme(nextTheme);
      writeStorage(THEME_KEY, nextTheme);
    }

    if (typeof nextMode === "string") {
      applyMode(nextMode);
      writeStorage(MODE_KEY, nextMode);
    }
  });

  if (window.matchMedia) {
    const media = window.matchMedia("(prefers-color-scheme: dark)");
    const syncSystemMode = () => {
      if ((readStorage(MODE_KEY) || readDefaultMode()) === "system") {
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

(() => {
  const root = document.documentElement;

  const metaContent = (name) =>
    document.querySelector(`meta[name="${name}"]`)?.getAttribute("content") || null;

  const readDefaultColorTheme = () =>
    root.getAttribute("data-color-theme") ||
    metaContent("gesso-color-theme-default");

  const readDefaultDensity = () =>
    root.getAttribute("data-density") ||
    metaContent("gesso-density-default");

  const readDefaultTypography = () =>
    root.getAttribute("data-typography") ||
    metaContent("gesso-typography-default");

  const readDefaultShape = () =>
    root.getAttribute("data-shape") ||
    metaContent("gesso-shape-default");

  const readDefaultMode = () =>
    root.getAttribute("data-color-theme-mode") ||
    metaContent("gesso-theme-mode-default") ||
    "system";

  const systemDark = () =>
    window.matchMedia &&
    window.matchMedia("(prefers-color-scheme: dark)").matches;

  const applyAttr = (attr, value) => {
    if (value) {
      root.setAttribute(attr, value);
    } else {
      root.removeAttribute(attr);
    }
  };

  const applyColorTheme = (colorTheme) => {
    applyAttr("data-color-theme", colorTheme);
  };

  const applyDensity = (density) => {
    applyAttr("data-density", density);
  };

  const applyTypography = (typography) => {
    applyAttr("data-typography", typography);
  };

  const applyShape = (shape) => {
    applyAttr("data-shape", shape);
  };

  const applyMode = (mode) => {
    if (!mode) return;

    const resolved =
      mode === "system"
        ? (systemDark() ? "dark" : "light")
        : mode;

    root.classList.toggle("dark", resolved === "dark");
    root.setAttribute("data-color-theme-mode", mode);
  };

  const currentColorTheme = () =>
    readDefaultColorTheme();

  const currentDensity = () =>
    readDefaultDensity();

  const currentTypography = () =>
    readDefaultTypography();

  const currentShape = () =>
    readDefaultShape();

  const currentMode = () =>
    readDefaultMode();

  const init = () => {
    applyColorTheme(currentColorTheme());
    applyDensity(currentDensity());
    applyTypography(currentTypography());
    applyShape(currentShape());
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

    const nextDensity =
      typeof detail.density === "string"
        ? detail.density
        : currentDensity();

    const nextTypography =
      typeof detail.typography === "string"
        ? detail.typography
        : currentTypography();

    const nextShape =
      typeof detail.shape === "string"
        ? detail.shape
        : currentShape();

    let nextMode = detail.mode;
    if (!nextMode) {
      nextMode = currentMode();
    } else if (nextMode === "toggle") {
      nextMode = root.classList.contains("dark") ? "light" : "dark";
    }

    if (typeof nextColorTheme === "string") {
      applyColorTheme(nextColorTheme);
    }

    if (typeof nextDensity === "string") {
      applyDensity(nextDensity);
    }

    if (typeof nextTypography === "string") {
      applyTypography(nextTypography);
    }

    if (typeof nextShape === "string") {
      applyShape(nextShape);
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

export function downloadJson(filename, content) {
  const blob = new Blob([content], {type: "application/json"});
  const url = URL.createObjectURL(blob);
  const anchor = document.createElement("a");

  anchor.href = url;
  anchor.download = filename;
  document.body.appendChild(anchor);
  anchor.click();
  anchor.remove();
  URL.revokeObjectURL(url);
}

export function readFileAsText(file, onSuccess, onError) {
  if (!file || file.size > 1024 * 1024) {
    onError("The selected file is too large.");
    return;
  }

  const reader = new FileReader();

  reader.onload = () => {
    if (typeof reader.result === "string") {
      onSuccess(reader.result);
    } else {
      onError("The selected file could not be read.");
    }
  };
  reader.onerror = () => onError("The selected file could not be read.");
  reader.readAsText(file);
}

export function clearFileInput(input) {
  input.value = "";
}

export function clickFileInput(input) {
  input.click();
}

const profilesStorageKey = "loan-calculator-profiles";

export function loadSavedProfiles() {
  try {
    return globalThis.localStorage?.getItem(profilesStorageKey) ?? "";
  } catch {
    return "";
  }
}

export function saveProfiles(content) {
  try {
    globalThis.localStorage?.setItem(profilesStorageKey, content);
  } catch {
    // Storage can be unavailable in private browsing or when disabled.
  }
}

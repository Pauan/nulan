import { crash } from "./error";


export const peek = (a, i) => {
  if (i >= 0 && i < a["length"]) {
    return a[i];
  } else {
    return null;
  }
};

export const all = (a, f) => {
  const length = a["length"];

  for (let i = 0; i < length; ++i) {
    if (!f(a[i])) {
      return false;
    }
  }

  return true;
};

export const each = (a, f) => {
  const length = a["length"];

  for (let i = 0; i < length; ++i) {
    f(a[i], i);
  }
};

export const map = (a, f) => {
  const length = a["length"];
  const out = new Array(length);

  for (let i = 0; i < length; ++i) {
    out[i] = f(a[i], i);
  }

  return out;
};

export const join = (a, s) =>
  a["join"](s);

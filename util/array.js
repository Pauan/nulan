import { crash } from "./error";


export const copy = (a) => {
  const b = new Array(a["length"]);

  for (let i = 0; i < a["length"]; ++i) {
    b[i] = a[i];
  }

  return b;
};

export const remove = (a, x) => {
  const i = a["indexOf"](x);

  if (i === -1) {
    crash(new Error("Cannot remove: element not found"));

  } else {
    a["splice"](i, 1);
  }
};

export const peek = (a, i) => {
  if (i < a["length"]) {
    return a[i];
  } else {
    return null;
  }
};

export const foldl = (init, x, f) => {
  for (let i = 0; i < x["length"]; ++i) {
    init = f(init, x[i]);
  }

  return init;
};

export const all = (a, f) => {
  for (let i = 0; i < a["length"]; ++i) {
    if (!f(a[i])) {
      return false;
    }
  }

  return true;
};

export const each = (a, f) => {
  for (let i = 0; i < a["length"]; ++i) {
    f(a[i], i);
  }
};

export const map = (a, f) => {
  const out = new Array(a["length"]);

  for (let i = 0; i < a["length"]; ++i) {
    out[i] = f(a[i], i);
  }

  return out;
};

export const join = (a, s) =>
  a["join"](s);

export const length = (a) =>
  a["length"];

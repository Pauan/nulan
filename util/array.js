import { crash } from "./error";


export const copy = (a) => {
  const length = a["length"];

  const b = new Array(length);

  for (let i = 0; i < length; ++i) {
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
  const length = x["length"];

  for (let i = 0; i < length; ++i) {
    init = f(init, x[i]);
  }

  return init;
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

export const length = (a) =>
  a["length"];

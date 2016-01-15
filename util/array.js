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

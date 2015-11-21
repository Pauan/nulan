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

export const peek = (a, i) => {
  if (i < a["length"]) {
    return a[i];
  } else {
    return null;
  }
};

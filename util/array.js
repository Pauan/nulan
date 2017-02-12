export const peek = (a, i) => {
  if (i >= 0 && i < a["length"]) {
    return a[i];

  } else {
    return null;
  }
};


// TODO move this into builtin/ffi/list.js ?
export const zip = (a) => {
  const length = a.length;

  const output = [];

  let index = 0;

  for (;;) {
    const inner = [];

    for (let i = 0; i < length; ++i) {
      const x = a[i];

      if (index < x.length) {
        inner.push(x[index]);

      } else {
        return output;
      }
    }

    output.push(inner);
    ++index;
  }
};


// TODO move this into builtin/ffi/list.js ?
export const flatten = (a) => {
  const output = [];

  const length = a.length;

  for (let i = 0; i < length; ++i) {
    const x = a[i];

    const length = x.length;

    for (let i = 0; i < length; ++i) {
      output.push(x[i]);
    }
  }

  return output;
};

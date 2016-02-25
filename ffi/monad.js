import { _null } from "./types";


// TODO is this correct ?
export const sequential = (after, wrap, a) => {
  const length = a["length"];
  // TODO this is probably impure / wrong
  const values = new Array(length);

  const loop = (i) =>
    (i < length
      ? after(a[i], (value) => {
          values[i] = value;
          return loop(i + 1);
        })
      : wrap(values));

  return loop(0);
};


export const sequential_null = (after, wrap, a) => {
  const length = a["length"];

  const loop = (i) =>
    (i < length
      ? after(a[i], (_) => loop(i + 1))
      : wrap(_null));

  return loop(0);
};

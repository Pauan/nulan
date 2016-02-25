import { _null } from "./types";


// TODO is this correct ?
export const sequential = (transform, flatten, wrap, a) => {
  const length = a["length"];
  // TODO this is probably impure / wrong
  const values = new Array(length);

  const loop = (i) =>
    (i < length
      ? flatten(transform(a[i], (value) => {
          values[i] = value;
          return loop(i + 1);
        }))
      : wrap(values));

  return loop(0);
};


export const sequential_null = (transform, flatten, wrap, a) => {
  const length = a["length"];

  const loop = (i) =>
    (i < length
      ? flatten(transform(a[i], (_) =>
          loop(i + 1)))
      : wrap(_null));

  return loop(0);
};

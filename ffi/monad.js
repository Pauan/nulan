import { _null } from "./types";


// TODO is this correct ?
export const sequential = (chain, reply, a) => {
  const length = a["length"];
  // TODO this is probably impure / wrong
  const values = new Array(length);

  const loop = (i) =>
    (i < length
      ? chain(a[i], (value) => {
          values[i] = value;
          return loop(i + 1);
        })
      : reply(values));

  return loop(0);
};


export const sequential_null = (chain, reply, a) => {
  const length = a["length"];

  const loop = (i) =>
    (i < length
      ? chain(a[i], (_) => loop(i + 1))
      : reply(_null));

  return loop(0);
};

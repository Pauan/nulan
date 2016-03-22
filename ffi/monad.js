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
      : reply(null));

  return loop(0);
};


const reduce_left_chain1 = (chain, reply, init, a, i, f) => {
  if (i < a["length"]) {
    return chain(f(init, a[i]), (init) =>
      reduce_left_chain1(chain, reply, init, a, i + 1, f));

  } else {
    return reply(init);
  }
};

export const reduce_left_chain = (chain, reply, init, a, f) =>
  reduce_left_chain1(chain, reply, init, a, 0, f);

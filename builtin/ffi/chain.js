const sequential_null1 = ($chain, $reply, a, i, length) =>
  (i < length
    ? $chain(a[i], (_) =>
        sequential_null1($chain, $reply, a, i + 1, length))
    : $reply(null));

export const sequential_null = ($chain, $reply, a) =>
  sequential_null1($chain, $reply, a, 0, a["length"]);


const reduce_left_chain1 = ($chain, $reply, init, a, i, length, f) =>
  (i < length
    ? $chain(f(init, a[i]), (init) =>
        reduce_left_chain1($chain, $reply, init, a, i + 1, length, f))
    : $reply(init));

export const reduce_left_chain = ($chain, $reply, init, a, f) =>
  reduce_left_chain1($chain, $reply, init, a, 0, a["length"], f);

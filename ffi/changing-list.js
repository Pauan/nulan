const set = (push, state, a, f) => {
  const output = [];

  const length = a["length"];

  const indexes = new Array(length);

  let nones = 0;

  for (let i = 0; i < length; ++i) {
    const x = f(a[i]);

    switch (x.$) {
    // *none
    case 0:
      indexes[i] = -1;
      ++nones;
      break;

    // *some
    case 1:
      indexes[i] = i - nones;
      output["push"](x.a);
      break;
    }
  }

  state.a = indexes;
  state.b = length - nones;

  push({
    $: 0,
    a: output
  });
};


const insert_index = (state, indexes, index) => {
  const length = indexes["length"];

  let first = -1;

  for (let i = index; i < length; ++i) {
    const x = indexes[i];

    if (x !== -1) {
      indexes[i] = x + 1;

      if (first === -1) {
        first = x;
      }
    }
  }

  return (first === -1
           ? state.b
           : first);
};

const remove_index = (indexes, index) => {
  const length = indexes["length"];

  for (let i = index + 1; i < length; ++i) {
    const x = indexes[i];

    // TODO verify that this can never cause the index to become -1
    if (x !== -1) {
      indexes[i] = x - 1;
    }
  }
};


const insert = (push, state, index, value, f) => {
  const indexes = state.a;

  const x = f(value);

  switch (x.$) {
  // *none
  case 0:
    indexes["splice"](index, 0, -1);
    break;

  // *some
  case 1:
    const i = insert_index(state, indexes, index);

    indexes["splice"](index, 0, i);
    ++state.b;

    push({
      $: 1,
      a: i,
      b: x.a
    });
    break;
  }
};


const update = (push, state, index, value, f) => {
  const indexes = state.a;
  const i = indexes[index];

  const x = f(value);

  switch (x.$) {
  // *none
  case 0:
    if (i !== -1) {
      remove_index(indexes, index);

      indexes[index] = -1;
      --state.b;

      push({
        $: 3,
        a: i
      });
    }
    break;

  // *some
  case 1:
    if (i === -1) {
      // TODO test this
      const i = insert_index(state, indexes, index + 1);

      indexes[index] = i;
      ++state.b;

      push({
        $: 1,
        a: i,
        b: x.a
      });

    } else {
      // TODO don't push if the value didn't change ?
      push({
        $: 2,
        a: i,
        b: x.a
      });
    }
    break;
  }
};


const remove = (push, state, index) => {
  const indexes = state.a;
  const i = indexes[index];

  if (i === -1) {
    indexes["splice"](index, 1);

  } else {
    remove_index(indexes, index);

    indexes["splice"](index, 1);
    --state.b;

    push({
      $: 3,
      a: i
    });
  }
};


// TODO test this
export const transform_maybe = (changing, f) =>
  (push) => {
    const state = {
      a: null, // indexes
      b: 0     // length
    };

    return changing((x) => {
      switch (x.$) {
      // *set
      case 0:
        set(push, state, x.a, f);
        break;

      // *insert
      case 1:
        insert(push, state, x.a, x.b, f);
        break;

      // *update
      case 2:
        update(push, state, x.a, x.b, f);
        break;

      // *remove
      case 3:
        remove(push, state, x.a);
        break;
      }
    });
  };



/*
[10, 20, 30, 40, 50, 60, 70]
[20, 40, 50, 60]
[-1,  0, -1,  1,  2,  3, -1]


(*insert 3 35)

[10, 20, 30, 35, 40, 50, 60, 70]
[20, 35, 40, 50, 60]
[-1,  0, -1,  1,  2,  3,  4, -1]

[10, 20, 30, 35, 40, 50, 60, 70]
[20, 40, 50, 60]
[-1,  0, -1, -1,  1,  2,  3, -1]


(*insert 7 65)

[10, 20, 30, 35, 40, 50, 60, 65, 70]
[20, 35, 40, 50, 60, 65]
[-1,  0, -1,  1,  2,  3,  4,  5, -1]

[10, 20, 30, 35, 40, 50, 60, 65, 70]
[20, 40, 50, 60]
[-1,  0, -1, -1,  1,  2,  3, -1, -1]
*/
/*console.log(transform_maybe((push) => {
  push({
    $: 0,
    a: [10, 20, 30, 40, 50, 60, 70]
  });

  push({
    $: 1,
    a: 3,
    b: 35,
  });

  push({
    $: 1,
    a: 8,
    b: 75
  });

  push({
    $: 2,
    a: 0,
    b: 11
  });

  push({
    $: 2,
    a: 8,
    b: 76
  });

  push({
    $: 3,
    a: 1
  });

  push({
    $: 3,
    a: 7
  });
}, (x) => {
  if (Math.random() < 0.5) {
    return { $: 0 };
  } else {
    return { $: 1, a: x + 2 };
  }
})((x) => {
  console.log("observe", x);
}));*/

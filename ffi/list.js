import { crash } from "../util/error";
import { blocking } from "./blocking-task";


export const size = (a) =>
  // TODO is this a good idea ?
  a["length"] | 0;


export const copy = (a) => {
  // TODO Integer overflow ?
  const length = a["length"];
  const output = new Array(length);

  for (let i = 0; i < length; ++i) {
    output[i] = a[i];
  }

  return output;
};


export const get_index = (index1, length) => {
  const index2 = (index1 < 0
                   // TODO Integer functions ?
                   ? index1 + length
                   : index1);

  if (index2 >= 0 && index2 < length) {
    return index2;

  } else {
    return crash(new Error("invalid index: " + index1));
  }
};


export const nth = (a, index) =>
  // TODO Integer overflow ?
  a[get_index(index, a["length"])];


export const push = (a, b) => {
  // TODO Integer overflow ?
  const length = a["length"];
  // TODO Integer functions ?
  const output = new Array(length + 1);

  for (let i = 0; i < length; ++i) {
    output[i] = a[i];
  }

  output[length] = b;

  return output;
};


export const insert = (a, index, b) => {
  // TODO Integer overflow ?
  const length = a["length"];

  // TODO Integer functions ?
  index = get_index(index, length + 1);

  // TODO Integer functions ?
  const output = new Array(length + 1);

  for (let i = 0; i < index; ++i) {
    output[i] = a[i];
  }

  output[index] = b;

  while (index < length) {
    // TODO Integer functions ?
    output[index + 1] = a[index];
    // TODO Integer functions ?
    ++index;
  }

  return output;
};


export const update = (a, index, b) => {
  // TODO Integer overflow ?
  index = get_index(index, a["length"]);

  // TODO test this
  if (a[index] === b) {
    return a;

  } else {
    const output = copy(a);

    output[index] = b;

    return output;
  }
};


export const remove = (a, index) => {
  // TODO Integer overflow ?
  index = get_index(index, a["length"]);

  // TODO Integer functions ?
  const length = a["length"] - 1;

  const output = new Array(length);

  let i = 0;

  while (i < index) {
    output[i] = a[i];
    ++i;
  }

  while (i < length) {
    // TODO Integer functions ?
    output[i] = a[i + 1];
    ++i;
  }

  return output;
};


export const slice = (a, start1, end1) => {
  // TODO Integer overflow ?
  const length = a["length"];

  // TODO Integer functions ?
  const start2 = get_index(start1, length + 1);
  const end2   = get_index(end1, length + 1);

  if (start2 === 0 && end2 === length) {
    return a;

  } else if (start2 > end2) {
    return crash(new Error("start index " + start1 + " is greater than end index " + end1));

  } else {
    // TODO Integer functions ?
    const diff = end2 - start2;
    const out = new Array(diff);

    for (let i = 0; i < diff; ++i) {
      // TODO Integer functions ?
      out[i] = a[i + start2];
    }

    return out;
  }
};


// TODO faster implementation of this ?
// TODO test this
export const chunks = (a, size) => {
  if (size < 1) {
    crash(new Error("chunk size must be 1 or greater"));
  }

  // TODO Integer overflow ?
  const length = a["length"];

  const out = [];

  let i = 0;

  while (i < length) {
    const chunk = [];

    let values = 0;

    // TODO what if the size is 0 ?
    while (i < length && values < size) {
      chunk["push"](a[i]);
      ++i;
      ++values;
    }

    out["push"](chunk);
  }

  return out;
};


export const unsafe_push = (a, b) => {
  a["push"](b);
  return a;
};


export const iterator_from = (a) =>
  // TODO include the list in the state ?
  ({
    a: 0,
    b: (i) => {
      if (i < a["length"]) {
        return { $: 2, a: a[i], b: i + 1 };
      } else {
        return { $: 0 };
      }
    }
  });


export const from_iterator = (a) => {
  let state = a.a;

  const next = a.b;
  const list = [];

  for (;;) {
    const x = next(state);

    switch (x.$) {
    case 0:
      return list;

    case 1:
      state = x.a;
      break;

    case 2:
      list["push"](x.a);
      state = x.b;
      break;
    }
  }
};


export const transform = (a, f) => {
  const length = a["length"];
  const out = new Array(length);

  for (let i = 0; i < length; ++i) {
    out[i] = f(a[i]);
  }

  return out;
};


// TODO test this
// TODO can this be made more efficient ?
// TODO handle incorrect min / max
export const range = (min, max) => {
  const array = new Array(max - min);

  for (let i = min; i < max; ++i) {
    array[i - min] = i;
  }

  return array;
};


// TODO test this
// https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
export const shuffle = (array) =>
  blocking(() => {
    const length = array["length"];

    const out = new Array(length);

    for (let i = 0; i < length; ++i) {
      const j = Math["floor"](Math["random"]() * (i + 1));

      if (j !== i) {
        out[i] = out[j];
      }

      out[j] = array[i];
    }

    return out;
  });


// TODO is this correct ?
export const order = ($order, a, b) => {
  const length1 = a["length"];
  const length2 = b["length"];
  const length = Math["min"](length1, length2);

  for (let i = 0; i < length; ++i) {
    switch ($order(a[i], b[i])) {
    // *less
    case 0:
      return 0;
    // *more
    case 2:
      return 2;
    }
  }

  if (length1 === length2) {
    // *equal
    return 1;

  } else if (length1 < length2) {
    // *less
    return 0;

  } else {
    // *more
    return 2;
  }
};


// Insertion sort (https://en.wikipedia.org/wiki/Insertion_sort)
// TODO use a better algorithm ?
export const sort = (a, order) => {
  const array = copy(a);
  const length = array["length"];

  const operations = [];

  for (let i = 1; i < length; ++i) {
    const x = array[i];

    let j = i - 1;

    // *more
    if (order(array[j], x) === 2) {
      do {
        array[j + 1] = array[j];
        --j;
      // *more
      } while (j >= 0 && order(array[j], x) === 2);

      array[j + 1] = x;

      operations["push"]({
        $: 3,
        a: i
      });

      operations["push"]({
        $: 1,
        a: j + 1,
        b: x
      });
    }
  }

  return {
    // TODO test this
    a: (operations["length"] === 0
         ? a
         : array),
    b: operations
  };
};


export const reduce_left = (init, a, f) => {
  const length = a["length"];

  for (let i = 0; i < length; ++i) {
    init = f(init, a[i]);
  }

  return init;
};

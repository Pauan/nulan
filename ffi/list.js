import { crash } from "../util/error";


export const size = (a) =>
  a["length"];


export const copy = (a) => {
  const length = a["length"];
  const output = new Array(length);

  for (let i = 0; i < length; ++i) {
    output[i] = a[i];
  }

  return output;
};


export const get_index = (index1, length) => {
  const index2 = (index1 < 0
                   ? index1 + length
                   : index1);

  if (index2 >= 0 && index2 < length) {
    return index2;

  } else {
    crash(new Error("invalid index: " + index1));
  }
};


export const nth = (a, index) =>
  a[get_index(index, a["length"])];


export const push = (a, b) => {
  const length = a["length"];
  const output = new Array(length + 1);

  for (let i = 0; i < length; ++i) {
    output[i] = a[i];
  }

  output[length] = b;

  return output;
};


export const insert = (a, index, b) => {
  const length = a["length"];

  index = get_index(index, length + 1);

  const output = new Array(length + 1);

  for (let i = 0; i < index; ++i) {
    output[i] = a[i];
  }

  output[index] = b;

  while (index < length) {
    output[index + 1] = a[index];
    ++index;
  }

  return output;
};


export const update = (a, index, b) => {
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
  index = get_index(index, a["length"]);

  const length = a["length"] - 1;

  const output = new Array(length);

  let i = 0;

  while (i < index) {
    output[i] = a[i];
    ++i;
  }

  while (i < length) {
    output[i] = a[i + 1];
    ++i;
  }

  return output;
};


export const slice = (a, start1, end1) => {
  const length = a["length"];

  const start2 = get_index(start1, length + 1);
  const end2   = get_index(end1, length + 1);

  if (start2 === 0 && end2 === length) {
    return a;

  } else if (start2 > end2) {
    crash(new Error("start index " + start1 + " is greater than end index " + end1));

  } else {
    const diff = end2 - start2;
    const out = new Array(diff);

    for (let i = 0; i < diff; ++i) {
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

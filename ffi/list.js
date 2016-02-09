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


export const get_index = (index, length) => {
  if (index < 0) {
    index += length;
  }

  if (index >= 0 && index < length) {
    return index;

  } else {
    crash(new Error("Invalid index: " + index));
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

  const output = copy(a);

  output[index] = b;

  return output;
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

import { crash } from "../util/error";


const sorted_index = (list, sort, a) => {
  let start = 0;
  let end   = list["length"];

  while (start < end) {
    // TODO is this faster/slower than using Math.floor ?
    const pivot = (start + end) >> 1;

    switch (sort(a, list[pivot])) {
    // *less
    case 0:
      end = pivot;
      break;

    // *equal
    case 1:
      // TODO return the left-most index ?
      return {
        $: 0,
        a: pivot
      };

    // *more
    case 2:
      start = pivot + 1;
      break;
    }
  }

  return {
    $: 1,
    a: start
  };
};


export const insert_sorted_index = (list, sort, a) => {
  const x = sorted_index(list, sort, a);

  if (x.$ === 0) {
    crash(new Error("value already exists in list"));

  } else {
    return x.a;
  }
};


export const get_sorted_index = (list, sort, a) => {
  const x = sorted_index(list, sort, a);

  if (x.$ === 0) {
    return x.a;

  } else {
    crash(new Error("value does not exist in list"));
  }
};

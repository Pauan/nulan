import { crash } from "../util/error";
import { blocking } from "./blocking-task";
import { _null } from "./types";
import { trigger } from "./mutable";
import * as $list from "./list";

export { get } from "./mutable";
export { changing_list_from, changing_from } from "./mutable-list";


// TODO test this
// TODO can this be made more efficient ?
// TODO disallow duplicates ?
const sorted_index = (list, sort, a) => {
  let start = 0;
  let end   = list["length"];

  let found = false;

  while (start < end) {
    // TODO is this faster/slower than using Math.floor ?
    const pivot = (start + end) >> 1;

    const order = sort(a, list[pivot]);

    // *less
    if (order === 0) {
      end = pivot;

    // *equal
    // *more
    } else {
      // *equal
      // Find the right-most index
      if (order === 1) {
        found = true;
      }

      start = pivot + 1;
    }
  }

  if (found) {
    return {
      $: 0,
      a: start - 1
    };

  } else {
    return {
      $: 1,
      a: start
    };
  }
};


const insert_sorted_index = (list, sort, a) => {
  const x = sorted_index(list, sort, a);

  if (x.$ === 0) {
    crash(new Error("value already exists in sorted list"));

  } else {
    return x.a;
  }
};


// TODO rather than returning the last item, instead return the index of `a` ?
const get_sorted_index = (list, sort, a) => {
  const x = sorted_index(list, sort, a);

  if (x.$ === 0) {
    return x.a;

  } else {
    // TODO is this a good idea ?
    crash(new Error("value does not exist in sorted list"));
  }
};


export const mutable_sorted_list = (list, order) => {
  // TODO this can be made more efficient
  const x = $list.sort(list, order).a;

  return blocking(() => {
    return {
      a: x,
      b: [],
      c: order
    };
  });
};


// TODO code duplication with mutable-list.js
export const set = (mutable, list) =>
  blocking(() => {
    // TODO this can be made more efficient
    const a = $list.sort(list, mutable.c).a;

    // TODO does this affect the semantics ?
    // TODO test this
    if (mutable.a !== a) {
      mutable.a = a;

      trigger(mutable.b, {
        $: 0,
        a: a
      });
    }

    return _null;
  });


// TODO code duplication with mutable-list.js
export const insert_sorted = (mutable, a) =>
  blocking(() => {
    const index = insert_sorted_index(mutable.a, mutable.c, a);

    // TODO slightly inefficient
    mutable.a = $list.insert(mutable.a, index, a);

    trigger(mutable.b, {
      $: 1,
      a: index,
      b: a
    });

    return _null;
  });


// TODO code duplication with mutable-list.js
export const remove_sorted = (mutable, a) =>
  blocking(() => {
    const index = get_sorted_index(mutable.a, mutable.c, a);

    // TODO slightly inefficient
    mutable.a = $list.remove(mutable.a, index);

    trigger(mutable.b, {
      $: 3,
      a: index
    });

    return _null;
  });


export const set_sort = (mutable, order) =>
  blocking(() => {
    const x = $list.sort(mutable.a, order);

    mutable.a = x.a;
    mutable.c = order;

    const ops = x.b;
    const length = ops["length"];

    for (let i = 0; i < length; ++i) {
      trigger(mutable.b, ops[i]);
    }

    return _null;
  });

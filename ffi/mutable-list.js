import { blocking } from "./blocking-task";
import { _null } from "./types";
import * as $list from "./list";
import * as $array from "../util/array";


// TODO code duplication with mutable.js
const _trigger = (mutable, value) => {
  const listeners = mutable.b;
  const length = listeners["length"];

  for (let i = 0; i < length; ++i) {
    listeners[i](value);
  }
};


// TODO code duplication with mutable.js
export const mutable_list = (value) =>
  blocking(() => {
    return {
      a: value,
      b: []
    };
  });


// TODO code duplication with mutable.js
export const get = (mutable) =>
  blocking(() => mutable.a);


export const set = (mutable, a) =>
  blocking(() => {
    // TODO does this affect the semantics ?
    // TODO test this
    if (mutable.a !== a) {
      mutable.a = a;

      _trigger(mutable, {
        $: 0,
        a: a
      });
    }

    return _null;
  });


export const push = (mutable, a) =>
  blocking(() => {
    // TODO slightly hacky
    const index = mutable.a["length"];

    mutable.a = $list.push(mutable.a, a);

    _trigger(mutable, {
      $: 1,
      a: index,
      b: a
    });

    return _null;
  });


export const insert = (mutable, index1, a) =>
  blocking(() => {
    // TODO very slightly hacky
    // TODO what about errors ?
    const index2 = $list.get_index(index1, mutable.a["length"] + 1);

    // TODO slightly inefficient
    mutable.a = $list.insert(mutable.a, index2, a);

    _trigger(mutable, {
      $: 1,
      a: index2,
      b: a
    });

    return _null;
  });


export const update = (mutable, index1, a) =>
  blocking(() => {
    // TODO very slightly hacky
    // TODO what about errors ?
    const index2 = $list.get_index(index1, mutable.a["length"]);

    // TODO slightly inefficient
    const b = $list.update(mutable.a, index2, a);

    // TODO test this
    // TODO does this affect the semantics ?
    if (mutable.a !== b) {
      mutable.a = b;

      _trigger(mutable, {
        $: 2,
        a: index2,
        b: a
      });
    }

    return _null;
  });


export const remove = (mutable, index1) =>
  blocking(() => {
    // TODO very slightly hacky
    // TODO what about errors ?
    const index2 = $list.get_index(index1, mutable.a["length"]);

    // TODO slightly inefficient
    mutable.a = $list.remove(mutable.a, index2);

    _trigger(mutable, {
      $: 3,
      a: index2
    });

    return _null;
  });


export const changing_list_from = (mutable) =>
  (push) => {
    push({
      $: 0,
      a: mutable.a
    });

    mutable.b["push"](push);

    return () => {
      $array.remove(mutable.b, push);
    };
  };


export const changing_from = (mutable) =>
  (push) => {
    const on_change = () => {
      push(mutable.a);
    };

    on_change();

    mutable.b["push"](on_change);

    return () => {
      $array.remove(mutable.b, on_change);
    };
  };

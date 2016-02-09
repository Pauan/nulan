import { sync } from "./task";
import { _null } from "./types";
import { crash } from "../util/error";
import { make_stream } from "./stream";
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
  sync(() => {
    return {
      a: value,
      b: []
    };
  });


// TODO code duplication with mutable.js
export const get = (mutable) =>
  sync(() => mutable.a);


export const set = (mutable, a) =>
  sync(() => {
    mutable.a = a;

    _trigger(mutable, {
      $: 0,
      a: a
    });

    return _null;
  });


export const push = (mutable, a) =>
  sync(() => {
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
  sync(() => {
    // TODO very slightly hacky
    // TODO what about errors ?
    const index2 = $list.get_index(index1, mutable.a["length"] + 1);

    mutable.a = $list.insert(mutable.a, index2, a);

    _trigger(mutable, {
      $: 1,
      a: index2,
      b: a
    });

    return _null;
  });


export const update = (mutable, index1, a) =>
  sync(() => {
    // TODO very slightly hacky
    // TODO what about errors ?
    const index2 = $list.get_index(index1, mutable.a["length"]);

    mutable.a = $list.update(mutable.a, index2, a);

    _trigger(mutable, {
      $: 2,
      a: index2,
      b: a
    });

    return _null;
  });


export const remove = (mutable, index1) =>
  sync(() => {
    // TODO very slightly hacky
    // TODO what about errors ?
    const index2 = $list.get_index(index1, mutable.a["length"]);

    mutable.a = $list.remove(mutable.a, index2);

    _trigger(mutable, {
      $: 3,
      a: index2
    });

    return _null;
  });


// TODO is this correct ?
export const stream_from = (mutable) =>
  make_stream((push, complete, error) => {
    // TODO what if an error happens ?
    push({
      $: 0,
      a: mutable.a
    });

    mutable.b["push"](push);

    // TODO what if an error happens ?
    return () => {
      $array.remove(mutable.b, push);
    };
  });

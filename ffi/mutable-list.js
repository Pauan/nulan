import { sync, async_killable, make_thread_pool, kill_thread_pool,
         run_in_thread_pool } from "./task";
import { _null } from "./types";
import { crash } from "../util/error";
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
      // TODO use immutable arrays instead ?
      a: $array.copy(value),
      b: []
    };
  });


// TODO code duplication with mutable.js
export const current = (mutable) =>
  // TODO use immutable arrays instead ?
  sync(() => $array.copy(mutable.a));


export const push = (mutable, a) =>
  sync(() => {
    const index = mutable.a["push"](a) - 1;

    _trigger(mutable, {
      $: 1,
      a: index,
      b: a
    });

    return _null;
  });


export const insert = (mutable, index, a) =>
  sync(() => {
    if (index >= 0 && index <= mutable.a["length"]) {
      mutable.a["splice"](index, 0, a);

      _trigger(mutable, {
        $: 1,
        a: index,
        b: a
      });

      return _null;

    } else {
      crash(new Error("Invalid index: " + index));
    }
  });


export const update = (mutable, index, a) =>
  sync(() => {
    if (index >= 0 && index < mutable.a["length"]) {
      mutable.a[index] = a;

      _trigger(mutable, {
        $: 2,
        a: index,
        b: a
      });

      return _null;

    } else {
      crash(new Error("Invalid index: " + index));
    }
  });


export const remove = (mutable, index) =>
  sync(() => {
    if (index >= 0 && index < mutable.a["length"]) {
      mutable.a["splice"](index, 1);

      _trigger(mutable, {
        $: 3,
        a: index
      });

      return _null;

    } else {
      crash(new Error("Invalid index: " + index));
    }
  });


// TODO is this correct ?
// TODO code duplication with mutable.js
export const observe_list = (mutable, fn) =>
  async_killable((success, error) => {
    const pool = make_thread_pool(error);

    const on_change = (value) => {
      run_in_thread_pool(pool, fn(value));
    };

    on_change({
      $: 0,
      // TODO use immutable arrays instead ?
      a: $array.copy(mutable.a)
    });

    mutable.b["push"](on_change);

    // TODO what if an error happens ?
    return () => {
      $array.remove(mutable.b, on_change);
      kill_thread_pool(pool);
    };
  });

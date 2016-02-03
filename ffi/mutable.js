import { sync, async_killable, make_thread_pool, kill_thread_pool,
         run_in_thread_pool } from "./task";
import { _null } from "./types";
import * as $array from "../util/array";


const _set = (mutable, value) => {
  if (mutable.a !== value) {
    mutable.a = value;

    const listeners = mutable.b;
    const length = listeners["length"];

    for (let i = 0; i < length; ++i) {
      listeners[i](value);
    }
  }
};


export const mutable = (value) =>
  sync(() => {
    return {
      a: value,
      b: []
    };
  });

export const get = (mutable) =>
  sync(() => mutable.a);

export const set = (mutable, value) =>
  sync(() => {
    _set(mutable, value);
    return _null;
  });

export const modify = (mutable, fn) =>
  sync(() => {
    const value = mutable.a;
    _set(mutable, fn(value));
    return _null;
  });

// TODO is this correct ?
export const observe = (mutable, fn) =>
  async_killable((success, error) => {
    const pool = make_thread_pool(error);

    const on_change = (value) => {
      run_in_thread_pool(pool, fn(value));
    };

    on_change(mutable.a);

    mutable.b["push"](on_change);

    // TODO what if an error happens ?
    return () => {
      $array.remove(mutable.b, on_change);
      kill_thread_pool(pool);
    };
  });

/*export const each_latest = (mutable, fn) =>
  async_killable((success, error) => {
    const thread = make_thread();

    const on_change = (value) => {
      // TODO is this correct ?
      run(fn(value), thread, noop, error);
    };

    mutable.b["push"](on_change);

    return () => {
      kill_thread(thread);

      $array.remove(mutable.b, on_change);
    };
  });
*/

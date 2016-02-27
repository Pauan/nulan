import { blocking } from "./blocking-task";
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
  blocking(() => {
    return {
      a: value,
      b: []
    };
  });

export const get = (mutable) =>
  blocking(() => mutable.a);

export const set = (mutable, value) =>
  blocking(() => {
    _set(mutable, value);
    return _null;
  });

export const modify = (mutable, fn) =>
  blocking(() => {
    const value = mutable.a;
    _set(mutable, fn(value));
    return _null;
  });


export const changing_from = (mutable) =>
  (push) => {
    push(mutable.a);

    mutable.b["push"](push);

    return () => {
      $array.remove(mutable.b, push);
    };
  };

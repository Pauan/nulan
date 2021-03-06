import { blocking } from "./blocking-task";
import * as $unsafe_list from "./unsafe/list";


export const trigger = (listeners, value) => {
  const length = listeners["length"];

  for (let i = 0; i < length; ++i) {
    listeners[i](value);
  }
};


const _set = (mutable, value) => {
  if (mutable.a !== value) {
    mutable.a = value;

    return trigger(mutable.b, value);
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
    return null;
  });

export const modify = (mutable, fn) =>
  blocking(() => {
    const value = mutable.a;
    _set(mutable, fn(value));
    return null;
  });


export const changing_from = (mutable) =>
  (push) => {
    push(mutable.a);

    mutable.b["push"](push);

    return () =>
      $unsafe_list.remove_element(mutable.b, push);
  };

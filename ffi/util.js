import { _null } from "./types";


export const noop = () => {};


export const try_catch = (f) => {
  try {
    return { $: 0, a: f(_null) };

  } catch (e) {
    return { $: 1, a: e };
  }
};

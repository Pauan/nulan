export const noop = () => {};


export const try_catch = (f) => {
  try {
    return { $: 0, a: f(null) };

  } catch (e) {
    return { $: 1, a: e };
  }
};

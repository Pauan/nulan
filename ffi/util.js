export const noop = () => {};


export const try_catch = (f, value) => {
  try {
    return { $: 0, a: f(value) };

  } catch (e) {
    return { $: 1, a: e };
  }
};

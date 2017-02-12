export const noop = () => {};


// TODO make this faster ?
export const try_catch = (f) => {
  try {
    const result = f();
    return (success, failure) => success(result);

  } catch (e) {
    return (success, failure) => failure(e);
  }
};

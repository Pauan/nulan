export const run = (f) => {
  f().catch((e) => {
    console.error(e.stack);
  });
};


export const each = async (a, f) => {
  const length = a.length;

  for (let i = 0; i < length; ++i) {
    await f(a[i], i);
  }
};


export const map = async (a, f) => {
  const length = a.length;
  const output = new Array(length);

  for (let i = 0; i < length; ++i) {
    output[i] = await f(a[i], i);
  }

  return output;
};


export const reduce_right = async (a, init, f) => {
  let i = a.length;

  while (i--) {
    init = await f(a[i], init);
  }

  return init;
};

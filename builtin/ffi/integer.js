// int32:    x | 0
// uint32:   x >>> 0
// float32:  Math.fround(x)
// float64:  +x


// TODO this truncates the float, and also wraps around on overflow, is that a good idea ?
export const int_from_float = (a) =>
  a | 0;


export const add = (a, b) =>
  (a + b) | 0;


export const subtract = (a, b) =>
  (a - b) | 0;


export const multiply = (a, b) =>
  Math["imul"](a, b);


// TODO is this correct ?
// TODO handle division by 0 ?
export const divide = (a, b) =>
  (a / b) | 0;


export const negate = (a) =>
  (-a) | 0;


export const order = (a, b) => {
  if (a === b) {
    // *equal
    return 1;

  } else if (a < b) {
    // *less
    return 0;

  } else {
    // *more
    return 2;
  }
};

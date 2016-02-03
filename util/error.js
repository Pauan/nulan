export const crash = (x) => {
  throw x;
};

export const assert = (x) => {
  if (!x) {
    crash(new Error("Assertion failed"));
  }
};

export const lazy = (f) => {
  return {
    a: false,
    b: f
  };
};

export const force = (a) => {
  if (!a.a) {
    a.a = true;
    a.b = a.b(null);
  }

  return a.b;
};

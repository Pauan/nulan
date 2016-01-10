import { _null } from "./types";


export const lazy = (f) => {
  return {
    a: false,
    b: f
  };
};

export const force = (a) => {
  if (!a.a) {
    a.b = a.b(_null);
    a.a = true;
  }

  return a.b;
};

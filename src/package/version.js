export const version = (a, b) =>
  ({ a, b });

export const serialize = (a) =>
  "(version " + a.a + " " + a.b + ")";

export const parse = (a) => {
  const b = /^v([0-9]+)\.([0-9]+)/["exec"](a);
  //const b = /^\(version ([0-9]+) ([0-9]+)\)$/["exec"](a);

  if (b === null) {
    return null;

  } else {
    return version(+b[1], +b[2]);
  }
};

export const order = (a, b) => {
  if (a.a === b.a) {
    if (a.b === b.b) {
      return 0;

    } else if (a.b < b.b) {
      return -1;

    } else {
      return 1;
    }

  } else if (a.a < b.a) {
    return -1;

  } else {
    return 1;
  }
};

export const matches = (a, b) =>
  a.a === b.a && b.b >= a.b;

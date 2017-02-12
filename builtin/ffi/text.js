export const concat = (a, b) =>
  a + b;


export const escape_regexp = (s) =>
  s["replace"](/[\\\^\$\.\|\?\*\+\(\)\[\]\{\}]/g, "\\$&");


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

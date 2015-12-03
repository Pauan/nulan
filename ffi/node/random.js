const $crypto = require("crypto");


export const NUMERIC      = "0123456789";
export const UPPERCASE    = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
export const LOWERCASE    = "abcdefghijklmnopqrstuvwxyz";
export const ALPHANUMERIC = NUMERIC + UPPERCASE + LOWERCASE;

// TODO is this correct ?
export const chars_from_bytes = (chars, bytes) => {
  const a = new Array(bytes["length"]);

  const length = chars["length"];

  for (let i = 0; i < bytes["length"]; ++i) {
    a[i] = chars[bytes[i] % length];
  }

  return a["join"]("");
};

export const random_bytes = (limit, f) => {
  $crypto["randomBytes"](limit, f);
};

export const random_characters = (limit, chars, f) => {
  random_bytes(limit, (err, bytes) => {
    if (err) {
      f(err, bytes);

    } else {
      f(err, chars_from_bytes(chars, bytes));
    }
  });
};

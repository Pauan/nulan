import { async_unkillable, transform } from "../task";
import { callback } from "./util";
import { randomBytes } from "crypto";


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

export const random_bytes = (limit) =>
  async_unkillable((success, error) => {
    randomBytes(limit, callback(success, error));
  });

export const random_characters = (limit, chars) =>
  transform(random_bytes(limit), (bytes) =>
    chars_from_bytes(chars, bytes));

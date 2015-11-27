import { eol } from "./node";


// TODO is it necessary to handle \r or \r\n ?
const eols = /\r\n|[\r\n]/g;

export const lines = (s) =>
  s["split"](eols);

export const repeat = (s, i) =>
  new Array(i + 1)["join"](s);

export const indent = (s, spaces) =>
  s["replace"](eols, eol + spaces);

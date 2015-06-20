import { pretty } from "../common/pretty";
import { isIsa } from "../common/type";

export function match_type(x, type, count) {
  return isIsa(x, type) && x.size() === count;
}

export function match_constant(x, constant) {
  return x === constant;
}

export function match_fail(x) {
  throw new Error("Match failed: " + pretty(x));
}

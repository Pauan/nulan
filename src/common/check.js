import { plural } from "./util";

export function assert(x) {
  if (!x) {
    throw new Error("Assertion failed: " + x);
  }
}

export function check_string(x) {
  if (typeof x === "string") {
    return x;
  } else {
    throw new Error("Expected a string but got " + x);
  }
}

export function check_number(x) {
  if (typeof x === "number") {
    return x;
  } else {
    throw new Error("Expected a number but got " + x);
  }
}

export function check_boolean(x) {
  if (x === true || x === false) {
    return x;
  } else {
    throw new Error("Expected a boolean but got " + x);
  }
}

export function check_type(x, expected) {
  if (typeof x !== expected) {
    throw new Error("Expected " + expected + " but got " + x);
  }
}

export function get_number_or_string(x) {
  var type = typeof x;
  if (type === "string" || type === "number") {
    return type;
  } else {
    throw new Error("Expected string or number but got " + x);
  }
}

export function check_arguments_equal(args, i) {
  if (args !== i) {
    throw new Error("Expected " + plural(i, "argument") + " but got " + args);
  }
}

export function check_arguments_min(args, i) {
  if (args < i) {
    throw new Error("Expected at least " + plural(i, "argument") + " but got " + args);
  }
}

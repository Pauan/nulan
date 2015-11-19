import { crash, pretty, eol } from "../src/util/node";
import { indent } from "../src/util/string";


const isObject = (x) =>
  x !== null && typeof x === "object";

const isArray = (x) =>
  Array["isArray"](x);

export const equal = (x, y) => {
  if (x === y) {
    return true;

  } else if (isObject(x) && isObject(y)) {
    if (isArray(x)) {
      if (isArray(y) && x["length"] === y["length"]) {
        for (let i = 0; i < x["length"]; ++i) {
          if (!equal(x[i], y[i])) {
            return false;
          }
        }

        return true;

      } else {
        return false;
      }

    } else if (isArray(y)) {
      return false;

    // TODO use equal ?
    } else if (Object["getPrototypeOf"](x) === Object["getPrototypeOf"](y)) {
      for (let key in x) {
        if (!(key in y && equal(x[key], y[key]))) {
          return false;
        }
      }

      for (let key in y) {
        if (!(key in x)) {
          return false;
        }
      }

      return true;

    } else {
      return false;
    }

  } else {
    return false;
  }
};

export const assert_equal = (value, expected, message) => {
  if (!equal(value, expected)) {
    crash("Error: " + message + eol +
          "  Expected:" + eol +
          "    " + indent(pretty(expected), "    ") + eol +
          "  Got:" + eol +
          "    " + indent(pretty(value), "    "));
  }
};

export const assert_crash = (f, expected, message) => {
  try {
    f();

  } catch (e) {
    const value = e["message"];

    if (value === expected) {
      return;

    } else {
      crash("Error: " + message + eol +
            "  Expected:" + eol +
            "    " + indent(expected, "    ") + eol +
            "  Got:" + eol +
            "    " + indent(value, "    "));
    }
  }

  crash("Error: " + message + eol +
        "  Expected:" + eol +
        "    " + indent(expected, "    ") + eol +
        "  Got:" + eol);
};

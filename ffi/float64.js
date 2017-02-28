// TODO inline this
export const add = (a, b) => a + b;

// TODO inline this
export const subtract = (a, b) => a - b;

// TODO inline this
export const divide = (a, b) => a / b;

// TODO inline this
export const multiply = (a, b) => a * b;


const exponentialRegexp = /^([0-9]+)(?:\.([0-9]+))?e([\+\-])([0-9]+)$/;

// TODO make this faster somehow ?
const replaceExponent = (_, prefix, suffix, type, exponent) => {
  exponent = +exponent;

  if (suffix == null) {
    suffix = "";
  }

  if (exponent === 0) {
    if (suffix === "") {
      return prefix;

    } else {
      return prefix + "." + suffix;
    }

  } else if (type === "+") {
    if (exponent < suffix.length) {
      var left = suffix.slice(0, exponent);
      var right = suffix.slice(exponent);

      return prefix + left + "." + right;

    } else {
      return prefix + suffix + "0".repeat(exponent - suffix.length);
    }

  } else {
    if (exponent < prefix.length) {
      var left = prefix.slice(0, prefix.length - exponent);
      var right = prefix.slice(prefix.length - exponent);

      return left + "." + right + suffix;

    } else {
      return "0." + "0".repeat(exponent - prefix.length) + prefix + suffix;
    }
  }
};

// TODO unit tests for this
// The same as Number.prototype.toString except it never uses scientific notation
export const toText = (s) =>
  ("" + s).replace(exponentialRegexp, replaceExponent);

/*function test(s) {
  var display = toString(s);

  console.log("" + s, display);

  if (/e/g.test(display)) {
    throw new Error("Failed " + s + " " + display);
  }

  if (s !== +display) {
    throw new Error("Failed " + s + " " + display);
  }
}

test(Number.MAX_VALUE);
test(Number.MIN_VALUE);

test(5e+0);
test(5e+1);
test(5e+2);

test(5e-0);
test(5e-1);
test(5e-2);

test(5.5e+0);
test(5.5e+1);
test(5.5e+2);
test(5.555e+2);

test(5.5e-0);
test(5.5e-1);
test(5.5e-2);
test(5.555e-2);*/

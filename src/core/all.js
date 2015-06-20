import { equal } from "../../node_modules/Immutable/src/Immutable";
import { check_number, check_string, check_boolean, check_type, get_number_or_string } from "../common/check";

function pairwise(f) {
  return function () {
    check_arguments_min(arguments.length, 2);

    var init = arguments[0];

    for (var i = 1; i < arguments.length; ++i) {
      var x = arguments[i];

      if (f(init, x)) {
        init = x;
      } else {
        return false;
      }
    }

    return true;
  }
}

function foldl(check, f) {
  return function () {
    check_arguments_min(arguments.length, 2);

    var init = check(arguments[0]);

    for (var i = 1; i < arguments.length; ++i) {
      init = f(init, check(arguments[i]));
    }

    return init;
  }
}


export var _void = void 0;

/*export function isNaN(x) {
  check_arguments_equal(arguments.length, 1);

  return x !== x;
}*/

export function isString(x) {
  check_arguments_equal(arguments.length, 1);
  return typeof x === "string";
}

export function isNumber(x) {
  check_arguments_equal(arguments.length, 1);
  return typeof x === "number";
}

export function isBoolean(x) {
  check_arguments_equal(arguments.length, 1);
  return x === true || x === false;
}

export function isVoid(x) {
  check_arguments_equal(arguments.length, 1);
  return x == null;
}

export function get() {
  check_arguments_min(arguments.length, 2);

  var obj = arguments[0];

  for (var i = 1; i < arguments.length; ++i) {
    var key = check_string(arguments[i]);

    if (key in obj) {
      obj = obj[key];

    } else {
      throw new Error("Key \"" + key + "\" not found in object " + obj);
    }
  }

  return obj;
}

export var add = foldl(check_number, function (x, y) {
  return x + y;
});

export var divide = foldl(check_number, function (x, y) {
  return x / y;
});

export var multiply = foldl(check_number, function (x, y) {
  return x * y;
});

export function subtract() {
  check_arguments_min(arguments.length, 1);

  var init = check_number(arguments[0]);

  if (arguments.length === 1) {
    return -init;

  } else {
    for (var i = 1; i < arguments.length; ++i) {
      init = init - check_number(arguments[i]);
    }

    return init;
  }
}

export function modulo(x, y) {
  check_arguments_equal(arguments.length, 2);
  return check_number(x) % check_number(y);
}

export function not(x) {
  check_arguments_equal(arguments.length, 1);
  return !check_boolean(x);
}

export var lt = pairwise(function (x, y) {
  // TODO inefficient
  check_type(y, get_number_or_string(x));
  return x < y;
});

export var gt = pairwise(function (x, y) {
  // TODO inefficient
  check_type(y, get_number_or_string(x));
  return x > y;
});

export var lt_is = pairwise(function (x, y) {
  // TODO inefficient
  check_type(y, get_number_or_string(x));
  return x <= y;
});

export var gt_is = pairwise(function (x, y) {
  // TODO inefficient
  check_type(y, get_number_or_string(x));
  return x >= y;
});

export var is = pairwise(equal);

// Can't just compose `not` with `is`, because of (isnt 1 1 2)
export var isnt = pairwise(function (x, y) {
  return !equal(x, y);
});

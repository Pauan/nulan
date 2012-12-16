var NINO = (function (n) {
  "use strict";

  var constants  = {}
    , booleans   = {}

  ;("empty name number boolean null regexp string")
    .split(" ").forEach(function (s) { constants[s] = true })

  ;("empty number boolean null regexp string typeof void array object")
    .split(" ").forEach(function (s) { booleans[s] = true})

  function toBoolean(x) {
    switch (x[0]) {
    case "empty":
    case "null":
    case "void":
      return false
    case "number":
      return x[1] !== "0" && x[1] !== "NaN"
    case "boolean":
      return x[1] === "true"
    case "regexp":
    case "typeof":
    case "array":
    case "object":
      return true
    }
  }

  function wrap(x) {
    switch (x) {
    case true:
    case false:
      return ["boolean", "" + x]
    case null:
      return ["null"]
    case void 0:
      return ["empty"]
    }
    switch (typeof x) {
    case "string":
      return ["string", x]
    case "number":
      return ["number", "" + x]
    }
  }

  function unwrap(x) {
    switch (x[0]) {
    case "empty":
      return void 0
    case "number":
      return +x[1]
    case "boolean":
      return x[1] === "true"
    case "null":
      return null
    case "string":
      return x[1]
    }
  }

  function math(a, info) {
    var s = a[0]
    a = a.slice(1).map(expression)
    if (a.every(function (x) { return info.types.indexOf(x[0]) !== -1 })) {
      return wrap(info.action.apply(null, a.map(unwrap)))
      /* else if (maths[x[0]] && info.types.indexOf(x[2][0]) !== -1) {
        return [x[0], x[1], wrap(info.action(unwrap(x[2]), unwrap(y)))]
      }*/
    }
    return [s].concat(a)
  }

  // () - 1 + 2 + 3 + 4 - 20 + 50 - 20
  // "foo" + "bar" - 50 + 600 * 2 / 50 - 20 + "qux"

  // 1 + 2 + 3 + () + 4 + 5 + 6 + ()

  // (void 0) + "foo" + "bar" - 50 + (void 0) + 600 * 2 / 50 - 20 + "qux" + (void 0)
  // () + "foo" + "qux" + "bar" - 50 + () + 600 * 2 * 60 / 50 - 20 + "qux" + ()
  // 'NaNquxundefined'

  // ((-) 5) + 10

  var maths = {
    // TODO: < > <= >= can technically work on objects/arrays/etc. but it's... hacky to say the least
    // {} < {}
    // {} <= {}
    "u+": {
      types: ["empty", "number", "boolean", "null", "string"],
      action: function (x) { return +x }
    },
    "u-": {
      types: ["empty", "number", "boolean", "null", "string"],
      action: function (x) { return -x }
    },

    "<": {
      types: ["empty", "number", "boolean", "null", "string"],
      action: function (x, y) { return x < y }
    },
    ">": {
      types: ["empty", "number", "boolean", "null", "string"],
      action: function (x, y) { return x > y }
    },
    "<=": {
      types: ["empty", "number", "boolean", "null", "string"],
      action: function (x, y) { return x <= y }
    },
    ">=": {
      types: ["empty", "number", "boolean", "null", "string"],
      action: function (x, y) { return x >= y }
    },
    "+": {
      types: ["empty", "number", "boolean", "null", "string"],
      action: function (x, y) { return x + y }
    },
    "-": {
      types: ["empty", "number", "boolean", "null", "string"],
      action: function (x, y) { return x - y}
    },
    "/": {
      types: ["empty", "number", "boolean", "null", "string"],
      action: function (x, y) { return x / y}
    },
    "*": {
      types: ["empty", "number", "boolean", "null", "string"],
      action: function (x, y) { return x * y}
    }
  }

  /*
  if (maths[x[0]]) {
        return math(x, maths[x[0]])
        */

  /*
  if (booleans[y[0]]) {
          return toBoolean(y)
                   ? blockToExpression(x[2])
                   : blockToExpression(x[3])
        } else {
  */

  n.partial = function (a) {
    return a
  }

  return n
})(NINO || {})

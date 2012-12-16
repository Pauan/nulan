var NINO = (function (n) {
  "use strict";

  var statements = {}
    , constants  = {}
    , booleans   = {}

  ;("break continue debugger do for for-in function-statement " + // if
    "return switch throw try var while with")
    .split(" ").forEach(function (s) { statements[s] = true })

  ;("empty name number boolean null regexp string")
    .split(" ").forEach(function (s) { constants[s] = true })

  ;("empty number boolean null regexp string typeof void array object")
    .split(" ").forEach(function (s) { booleans[s] = true})

  function flatten1(x) {
    // TODO: is there a faster way?
    var r = []
    return r.concat.apply(r, x)
  }

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

  // TODO: make this non-recursive?
  function isStatement(x) {
    if (Array.isArray(x)) {
      if (statements[x[0]]) {
        return true
      } else {
        // TODO
        //return false
        return x.some(isStatement)
      }
    } else {
      return false
    }
  }

  function blockToExpression(x) {
    return x.map(expression).reduce(function (x, y) {
      return [",", x, y]
    })
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

  function expression(x) {
    var y
    if (Array.isArray(x)) {
      if (maths[x[0]]) {
        return math(x, maths[x[0]])
      } else {
        switch (x[0]) {
        case "function":
        case "function-statement":
          return [x[0], x[1], x[2], block(x[3])]

        case "if":
          y = expression(x[1])
          if (booleans[y[0]]) {
            return toBoolean(y)
                     ? blockToExpression(x[2])
                     : blockToExpression(x[3])
          } else {
            if (x[2].some(isStatement) || x[3].some(isStatement)) {
              return [x[0], y,
                       block(x[2]),
                       block(x[3])]
            } else {
              if (x[2].length) {
                if (x[3].length) {
                  return ["?:", y,
                           blockToExpression(x[2]),
                           blockToExpression(x[3])]
                } else {
                  return ["&&", y, blockToExpression(x[2])]
                }
              } else {
                return ["empty"]
              }
            }
          }

        case "while":
          y = expression(x[1])
          if (booleans[y[0]]) {
            return toBoolean(y)
                     ? [x[0], ["boolean", "true"], block(x[2])]
                     : blockToExpression(x[2]) // TODO
          } else {
            return [x[0], y, block(x[2])]
          }

/*
        ->
          | var i = 0
          | if i
              i + 10
              i + 20


var foo

foo <=
  | 5
  | 10
  | 20

*/

        case "try":
          return [x[0], block(x[1])].concat(x.slice(2).map(function (x) {
            if (x[0] === "catch") {
              return [x[0], x[1], block(x[2])]
            } else if (x[0] === "finally") {
              return [x[0], block(x[1])]
            }
          }))

        default:
          return x.map(expression)
        }
      }
    } else {
      return x
    }
  }

  function block(x) {
    function loop(x) {
      var y
      switch (x[0]) {

      case ",":
        return loop(x[1]).concat(loop(x[2]))

      case "=":
        y = loop(x[2])
        y.push([x[0], x[1], y.pop()])
        return y

      case "return":
        y = loop(x[1])
        y.push.apply(y, returns([x[0], y.pop()]))
        return y

      default:
        return [expression(x)]
      }
    }
    return flatten1(x.map(loop))/*.filter(function (x) {
      return !constants[x[0]]
    })*/
  }

  function returns(x) {
    var z, y = x[1]

    switch (y[0]) {
    case "if":
      z = y[2]
      if (z.length) {
        z.push.apply(z, returns([x[0], z.pop()]))
      }

      z = y[3]
      if (z.length) {
        z.push.apply(z, returns([x[0], z.pop()]))
      }
      return [y]

    case "while":
      return [y]

    case "try":
      z = y[1]
      z[z.length - 1] = returns([x[0], z[z.length - 1]])
      return [y]

    case "throw":
      return [y]

    case "var":
      z = y[1]
      z = ["name", z[z.length - 1][0]]
      return [y, [x[0], z]]

    default:
      return [x]
    }
  }

  n.transform = function (a) {
    return block(a)
  }

  return n
})(NINO || {})

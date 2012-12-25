var NINO = (function (n) {
  "use strict";

  var counter = 0

  // TODO
  function uniq() {
    return "_" + counter++
  }

  function isConstant(x) {
    switch (x[0]) {
    case "empty":
    case "name":
    case "number":
    case "boolean":
    case "null":
    case "regexp":
    case "string":
      return true

    default:
      return false
    }
  }

  function invert(x, r) {
    var y, z
    switch (x[0]) {
    case "return":
      y = x[1]
      if (y[0] === "if") {
        if (y[2].length) {
          y[2].push(invert([x[0], y[2].pop()], y[2]))
        }
        if (y[3].length) {
          y[3].push(invert([x[0], y[3].pop()], y[3]))
        }
        return y
      } else {
        return [x[0], invert(x[1], r)]
      }

    // TODO: ew
    case "=":
      y = x[2]
      if (y[0] === "var") {
        z = y[1][y[1].length - 1]
        r.push(y)
        return [x[0], x[1], ["name", z[0]]]
      } else {
        return x
      }

    case "var":
      z = x[1][x[1].length - 1]
      if (z.length === 2) {
        y = z[1]
        if (y[0] === "if") {
          r.push([x[0], [[z[0]]]])
          if (y[2].length) {
            y[2].push(invert(["=", ["name", z[0]], y[2].pop()], y[2]))
          }
          if (y[3].length) {
            y[3].push(invert(["=", ["name", z[0]], y[3].pop()], y[3]))
          }
          return y
        } else if (y[0] === "return") {
          return invert(y, r)
        }
      }
      return x

    default:
      return x
    }
  }

  function expression(x, r) {
    if (Array.isArray(x)) {
      switch (x[0]) {
      case ",":
        r.push(expression(x[1], r))
        return expression(x[2], r)

      case "function":
      case "function-statement":
        return [x[0], x[1], x[2], block(x[3])]

      case "if":
        x = [x[0], expression(x[1], r), block(x[2]), block(x[3])]
        var u = uniq()
        r.push(invert(["var", [[u, x]]], r))
        return ["name", u]

      case "while":
        x = [x[0], expression(x[1], r), block(x[2])]
        r.push(x)
        return ["empty"]

      case "break":
        r.push(x)
        return ["empty"]

      case "var":
        return invert([x[0], x[1].map(function (x) {
          if (x.length === 1) {
            return [x[0]]
          } else {
            return [x[0], expression(x[1], r)]
          }
        })], r)

      default:
        return x.map(function (x) {
          return expression(x, r)
        })
      }
    } else {
      return x
    }
  }

  function statement(x, r) {
    r.push(invert(expression(x, r), r))
  }

  function block(a) {
    var r = []
    a.forEach(function (x) {
      statement(x, r)
    })
    return r
  }


  n.transform = function (a) {
    a = block(a)
    //console.log(require("util").inspect(a, false, null, true))
    return a
  }

  return n
})(NINO || {})
